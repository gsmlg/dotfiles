;;; emacs-agent-transform.el --- Exact text and patch transforms -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure planning for Agent-friendly text transforms, followed by guarded,
;; atomic application through `emacs-agent-edit'.

;;; Code:

(require 'cl-lib)
(require 'emacs-agent-changeset)
(require 'emacs-agent-document)
(require 'emacs-agent-edit)

(cl-defstruct (emacs-agent-transform-plan
               (:constructor emacs-agent-transform-plan--make))
  kind workspace document path expected-revision before after edits ranges diff
  match-count)

(defun emacs-agent-transform--position (document point)
  "Return the public position in DOCUMENT corresponding to POINT."
  (with-current-buffer (emacs-agent-document-buffer document)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char point)
        (list :line (line-number-at-pos point t)
              :column (- point (line-beginning-position)))))))

(defun emacs-agent-transform--contents (document)
  "Return the complete authoritative contents of DOCUMENT."
  (with-current-buffer (emacs-agent-document-buffer document)
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun emacs-agent-transform--open-current
    (workspace path expected-revision)
  "Open PATH in WORKSPACE and validate EXPECTED-REVISION."
  (let ((document (emacs-agent-document-open workspace path)))
    (when (emacs-agent-document-degraded document)
      (emacs-agent-signal
       'external_change_conflict :path path
       :reason 'reconciliation_required))
    (emacs-agent-document-reconcile document)
    (let ((current (emacs-agent-document-revision document)))
      (unless (equal expected-revision current)
        (emacs-agent-signal
         'revision_conflict
         :path (emacs-agent-document-relative-path document)
         :expected_revision expected-revision
         :current_revision current
         :modified_by 'buffer
         :requires_reread t)))
    document))

(defun emacs-agent-transform--replace-after (before replacements new-text)
  "Return BEFORE with REPLACEMENTS changed to NEW-TEXT.
Each replacement is a cons of zero-based start and end offsets."
  (with-temp-buffer
    (insert before)
    (dolist (replacement (reverse replacements))
      (delete-region (1+ (car replacement)) (1+ (cdr replacement)))
      (goto-char (1+ (car replacement)))
      (insert new-text))
    (buffer-string)))

(defun emacs-agent-transform--lines (text)
  "Split TEXT into lines while retaining newline characters."
  (let ((start 0)
        lines)
    (while (< start (length text))
      (let ((newline (string-match "\n" text start)))
        (if newline
            (progn
              (push (substring text start (1+ newline)) lines)
              (setq start (1+ newline)))
          (push (substring text start) lines)
          (setq start (length text)))))
    (nreverse lines)))

(defun emacs-agent-transform--patch-physical-lines (patch)
  "Return PATCH's physical lines without their line terminators."
  (unless (stringp patch)
    (emacs-agent-signal 'invalid_argument :field 'patch))
  (mapcar
   (lambda (line)
     (string-remove-suffix
      "\r" (string-remove-suffix "\n" line)))
   (emacs-agent-transform--lines patch)))

(defun emacs-agent-transform--patch-path (declared)
  "Normalize a DECLARED unified-diff path."
  (let ((path (car (split-string declared "\t"))))
    (cond
     ((string-prefix-p "a/" path) (substring path 2))
     ((string-prefix-p "b/" path) (substring path 2))
     (t path))))

(defun emacs-agent-transform--assert-patch-path (path declared)
  "Require DECLARED patch path to identify PATH exactly."
  (unless (equal path (emacs-agent-transform--patch-path declared))
    (emacs-agent-signal
     'patch_path_mismatch :path path :declared_path declared)))

(defun emacs-agent-transform--parse-hunk-header (line)
  "Parse unified hunk header LINE, or return nil."
  (when
      (string-match
       (concat
        "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
        " +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?"
        " @@\\(?: .+\\)?\\'")
       line)
    (list
     :header line
     :old-start (string-to-number (match-string 1 line))
     :old-count
     (if (match-string 2 line)
         (string-to-number (match-string 2 line))
       1)
     :new-start (string-to-number (match-string 3 line))
     :new-count
     (if (match-string 4 line)
         (string-to-number (match-string 4 line))
       1))))

(defun emacs-agent-transform--parse-patch (path patch)
  "Parse strict single-file unified PATCH for PATH."
  (let* ((lines (emacs-agent-transform--patch-physical-lines patch))
         (count (length lines))
         (index 0)
         old-header new-header hunks)
    (while (< index count)
      (let ((line (nth index lines)))
        (cond
         ((string-prefix-p "diff --git " line)
          (when (or old-header new-header hunks)
            (emacs-agent-signal 'invalid_patch :reason 'multiple_files))
          (let ((parts (split-string (substring line 11) " " t)))
            (unless (= (length parts) 2)
              (emacs-agent-signal 'invalid_patch :line line))
            (emacs-agent-transform--assert-patch-path path (car parts))
            (emacs-agent-transform--assert-patch-path path (cadr parts)))
          (setq index (1+ index)))
         ((string-prefix-p "index " line)
          (when hunks
            (emacs-agent-signal 'invalid_patch :line line))
          (setq index (1+ index)))
         ((string-prefix-p "--- " line)
          (when (or old-header hunks)
            (emacs-agent-signal 'invalid_patch :reason 'multiple_files))
          (setq old-header (substring line 4))
          (emacs-agent-transform--assert-patch-path path old-header)
          (setq index (1+ index)))
         ((string-prefix-p "+++ " line)
          (unless (and old-header (not new-header) (not hunks))
            (emacs-agent-signal 'invalid_patch :line line))
          (setq new-header (substring line 4))
          (emacs-agent-transform--assert-patch-path path new-header)
          (setq index (1+ index)))
         ((emacs-agent-transform--parse-hunk-header line)
          (when (not (eq (not old-header) (not new-header)))
            (emacs-agent-signal 'invalid_patch :reason 'incomplete_headers))
          (let* ((hunk (emacs-agent-transform--parse-hunk-header line))
                 (old-count (plist-get hunk :old-count))
                 (new-count (plist-get hunk :new-count))
                 (old-seen 0)
                 (new-seen 0)
                 operations
                 previous)
            (setq index (1+ index))
            (while (or (< old-seen old-count)
                       (< new-seen new-count))
              (when (>= index count)
                (emacs-agent-signal
                 'invalid_patch :hunk line :reason 'truncated))
              (let* ((data-line (nth index lines))
                     (tag (and (> (length data-line) 0)
                               (aref data-line 0))))
                (when (equal data-line "\\ No newline at end of file")
                  (unless previous
                    (emacs-agent-signal 'invalid_patch :line data-line))
                  (setcdr previous
                          (string-remove-suffix "\n" (cdr previous)))
                  (setq index (1+ index)
                        data-line (and (< index count) (nth index lines))
                        tag (and data-line (> (length data-line) 0)
                                 (aref data-line 0))))
                (unless (memq tag '(?\s ?- ?+))
                  (emacs-agent-signal
                   'invalid_patch :hunk line :line data-line))
                (let ((operation
                       (cons tag (concat (substring data-line 1) "\n"))))
                  (push operation operations)
                  (setq previous operation))
                (pcase tag
                  (?\s
                   (setq old-seen (1+ old-seen)
                         new-seen (1+ new-seen)))
                  (?-
                   (setq old-seen (1+ old-seen)))
                  (?+
                   (setq new-seen (1+ new-seen))))
                (when (or (> old-seen old-count)
                          (> new-seen new-count))
                  (emacs-agent-signal
                   'invalid_patch :hunk line :reason 'count_mismatch))
                (setq index (1+ index))))
            (when (and (< index count)
                       (equal (nth index lines)
                              "\\ No newline at end of file"))
              (unless previous
                (emacs-agent-signal 'invalid_patch :line (nth index lines)))
              (setcdr previous
                      (string-remove-suffix "\n" (cdr previous)))
              (setq index (1+ index)))
            (setq hunk
                  (plist-put hunk :operations (nreverse operations)))
            (push hunk hunks)))
         (t
          (emacs-agent-signal 'invalid_patch :line line)))))
    (unless hunks
      (emacs-agent-signal 'invalid_patch :reason 'missing_hunks))
    (nreverse hunks)))

(defun emacs-agent-transform--patch-conflict
    (path hunk expected actual revision)
  "Signal an exact context conflict for PATH and HUNK.
EXPECTED and ACTUAL describe context at REVISION."
  (emacs-agent-signal
   'patch_conflict
   :path path
   :hunk (plist-get hunk :header)
   :expected_context expected
   :actual_context actual
   :current_revision revision))

(defun emacs-agent-transform--apply-hunks
    (path revision before hunks)
  "Return BEFORE for PATH at REVISION with parsed HUNKS applied exactly."
  (let* ((source (vconcat (emacs-agent-transform--lines before)))
         (source-count (length source))
         (cursor 0)
         (output-count 0)
         output)
    (dolist (hunk hunks)
      (let* ((old-start (plist-get hunk :old-start))
             (new-start (plist-get hunk :new-start))
             (source-index (if (zerop old-start) 0 (1- old-start)))
             (output-index (if (zerop new-start) 0 (1- new-start))))
        (when (or (< source-index cursor)
                  (> source-index source-count)
                  (/= output-index (+ output-count (- source-index cursor))))
          (emacs-agent-transform--patch-conflict
           path hunk
           (format "line %s" old-start)
           (format "line %s" (1+ cursor))
           revision))
        (while (< cursor source-index)
          (push (aref source cursor) output)
          (setq cursor (1+ cursor)
                output-count (1+ output-count)))
        (dolist (operation (plist-get hunk :operations))
          (let ((tag (car operation))
                (text (cdr operation)))
            (pcase tag
              ((or ?\s ?-)
               (let ((actual
                      (and (< cursor source-count)
                           (aref source cursor))))
                 (unless (equal text actual)
                   (emacs-agent-transform--patch-conflict
                    path hunk text actual revision))
                 (setq cursor (1+ cursor))
                 (when (= tag ?\s)
                   (push text output)
                   (setq output-count (1+ output-count)))))
              (?+
               (push text output)
               (setq output-count (1+ output-count))))))))
    (while (< cursor source-count)
      (push (aref source cursor) output)
      (setq cursor (1+ cursor)))
    (mapconcat #'identity (nreverse output) "")))

(defun emacs-agent-transform--line-offset (lines index)
  "Return the character offset before zero-based line INDEX in LINES."
  (let ((offset 0)
        (cursor 0))
    (while (and (< cursor index) (< cursor (length lines)))
      (setq offset (+ offset (length (nth cursor lines)))
            cursor (1+ cursor)))
    offset))

;;;###autoload
(cl-defun emacs-agent-transform-replace-text
    (before old-text new-text
            &key replace-all expected-occurrences path)
  "Purely plan an exact replacement in BEFORE.
Return a plist containing `:after', selected zero-based `:matches', and the
exact `:match_count'.  OLD-TEXT and NEW-TEXT are literal strings.
REPLACE-ALL and EXPECTED-OCCURRENCES have the same meaning as in
`emacs-agent-transform-plan-replace'.  PATH is optional error context."
  (unless (stringp before)
    (emacs-agent-signal 'invalid_argument :field 'before))
  (unless (and (stringp old-text) (not (string-empty-p old-text)))
    (emacs-agent-signal 'invalid_argument :field 'old_text))
  (unless (stringp new-text)
    (emacs-agent-signal 'invalid_argument :field 'new_text))
  (when (and expected-occurrences
             (not (and (integerp expected-occurrences)
                       (> expected-occurrences 0))))
    (emacs-agent-signal
     'invalid_argument :field 'expected_occurrences))
  (let (matches)
    (with-temp-buffer
      (insert before)
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (search-forward old-text nil t)
          (push (cons (- (match-beginning 0) (point-min))
                      (- (match-end 0) (point-min)))
                matches))))
    (setq matches (nreverse matches))
    (let ((count (length matches)))
      (when (zerop count)
        (emacs-agent-signal
         'expected_text_mismatch :path path :actual_occurrences 0))
      (when (and expected-occurrences
                 (/= count expected-occurrences))
        (emacs-agent-signal
         'occurrence_count_mismatch
         :path path
         :expected_occurrences expected-occurrences
         :actual_occurrences count))
      (when (and (not replace-all) (> count 1))
        (emacs-agent-signal
         'ambiguous_text_match :path path :actual_occurrences count))
      (let ((selected (if replace-all matches (list (car matches)))))
        (list
         :before before
         :after
         (emacs-agent-transform--replace-after before selected new-text)
         :matches selected
         :match_count count)))))

;;;###autoload
(cl-defun emacs-agent-transform-plan-replace
    (workspace path expected-revision old-text new-text
               &key replace-all expected-occurrences)
  "Plan an exact OLD-TEXT replacement with NEW-TEXT in PATH.
WORKSPACE and EXPECTED-REVISION identify the authoritative buffer.
REPLACE-ALL permits multiple matches.  EXPECTED-OCCURRENCES, when non-nil,
must equal the exact literal match count."
  (let* ((document
          (emacs-agent-transform--open-current
           workspace path expected-revision))
         (before (emacs-agent-transform--contents document))
         (text-plan
          (emacs-agent-transform-replace-text
           before old-text new-text
           :replace-all replace-all
           :expected-occurrences expected-occurrences
           :path path))
         (selected (plist-get text-plan :matches))
         (count (plist-get text-plan :match_count))
         (after (plist-get text-plan :after))
             edits ranges)
    (dolist (match selected)
      (let* ((start (1+ (car match)))
             (end (1+ (cdr match)))
             (start-position
              (emacs-agent-transform--position document start))
             (end-position
              (emacs-agent-transform--position document end)))
        (push (list :start start-position
                    :end end-position
                    :new_text new-text
                    :expected_text old-text)
              edits)
        (push (list :start start-position :end end-position) ranges)))
    (emacs-agent-transform-plan--make
     :kind 'replace
     :workspace workspace
     :document document
     :path (emacs-agent-document-relative-path document)
     :expected-revision expected-revision
     :before before
     :after after
     :edits (nreverse edits)
     :ranges (nreverse ranges)
     :diff
     (emacs-agent-changeset--diff-text
      (emacs-agent-document-relative-path document) before after)
     :match-count count)))

;;;###autoload
(cl-defun emacs-agent-transform-plan-patch
    (workspace path expected-revision patch &key (fuzz 0))
  "Plan strict single-file unified PATCH for PATH in WORKSPACE.
FUZZ must be zero; context and source line numbers are always exact."
  (unless (and (integerp fuzz) (zerop fuzz))
    (emacs-agent-signal
     'invalid_argument :field 'fuzz :reason 'fuzzy_patch_not_supported))
  (let* ((document
          (emacs-agent-transform--open-current
           workspace path expected-revision))
         (relative-path (emacs-agent-document-relative-path document))
         (before (emacs-agent-transform--contents document))
         (hunks (emacs-agent-transform--parse-patch relative-path patch))
         (after
          (emacs-agent-transform--apply-hunks
           relative-path expected-revision before hunks))
         (source-lines (emacs-agent-transform--lines before))
         ranges)
    (when (equal before after)
      (emacs-agent-signal 'invalid_patch :reason 'no_changes))
    (dolist (hunk hunks)
      (let* ((old-start (plist-get hunk :old-start))
             (old-count (plist-get hunk :old-count))
             (line-index (if (zerop old-start) 0 (1- old-start)))
             (start-offset
              (emacs-agent-transform--line-offset source-lines line-index))
             (end-offset
              (emacs-agent-transform--line-offset
               source-lines (+ line-index old-count))))
        (push
         (list
          :start
          (emacs-agent-transform--position document (1+ start-offset))
          :end
          (emacs-agent-transform--position document (1+ end-offset)))
         ranges)))
    (let ((start-position
           (emacs-agent-transform--position document 1))
          (end-position
           (with-current-buffer (emacs-agent-document-buffer document)
             (emacs-agent-transform--position document (point-max)))))
      (emacs-agent-transform-plan--make
       :kind 'patch
       :workspace workspace
       :document document
       :path relative-path
       :expected-revision expected-revision
       :before before
       :after after
       :edits
       (list
        (list :start start-position
              :end end-position
              :new_text after
              :expected_text before))
       :ranges (nreverse ranges)
       :diff (emacs-agent-changeset--diff-text relative-path before after)))))

(defun emacs-agent-transform--validate-plan (plan)
  "Revalidate PLAN against its authoritative buffer."
  (let* ((document
          (emacs-agent-transform--open-current
           (emacs-agent-transform-plan-workspace plan)
           (emacs-agent-transform-plan-path plan)
           (emacs-agent-transform-plan-expected-revision plan)))
         (current (emacs-agent-transform--contents document)))
    (unless (equal current (emacs-agent-transform-plan-before plan))
      (emacs-agent-signal
       'expected_text_mismatch
       :path (emacs-agent-transform-plan-path plan)))
    document))

(defun emacs-agent-transform--result (plan &optional edit-result)
  "Return the public result for PLAN and optional EDIT-RESULT."
  (let ((applied (and edit-result t)))
    (list
     :path (emacs-agent-transform-plan-path plan)
     :old_revision (emacs-agent-transform-plan-expected-revision plan)
     :new_revision
     (if applied
         (plist-get edit-result :new_revision)
       (emacs-agent-transform-plan-expected-revision plan))
     :changeset_id (and applied (plist-get edit-result :changeset_id))
     :applied applied
     :checkpointed (and applied (plist-get edit-result :checkpointed))
     :modified
     (not (equal (emacs-agent-transform-plan-before plan)
                 (emacs-agent-transform-plan-after plan)))
     :match_count (emacs-agent-transform-plan-match-count plan)
     :ranges (emacs-agent-transform-plan-ranges plan)
     :diff (emacs-agent-transform-plan-diff plan)
     :truncated nil)))

;;;###autoload
(defun emacs-agent-transform-apply (plan &optional dry-run checkpoint)
  "Apply PLAN atomically, or only validate it when DRY-RUN is non-nil.
CHECKPOINT is forwarded to the normal guarded edit path."
  (unless (emacs-agent-transform-plan-p plan)
    (signal 'wrong-type-argument
            (list 'emacs-agent-transform-plan plan)))
  (let* ((document (emacs-agent-transform--validate-plan plan))
         (buffer (emacs-agent-document-buffer document)))
    (if dry-run
        (emacs-agent-transform--result plan)
      ;; Use an insertion-type end marker so a whole-document replacement
      ;; preserves the caller's original restriction through inserted text.
      (with-current-buffer buffer
        (let ((narrowed (buffer-narrowed-p))
              (restriction-start (copy-marker (point-min)))
              (restriction-end (copy-marker (point-max) t))
              result)
          (unwind-protect
              (setq result
                    (emacs-agent-edit-apply
                     (emacs-agent-transform-plan-workspace plan)
                     (emacs-agent-transform-plan-path plan)
                     (emacs-agent-transform-plan-expected-revision plan)
                     (emacs-agent-transform-plan-edits plan)
                     checkpoint))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (widen)
                (when narrowed
                  (narrow-to-region
                   (marker-position restriction-start)
                   (marker-position restriction-end)))))
            (set-marker restriction-start nil)
            (set-marker restriction-end nil))
          (emacs-agent-transform--result plan result))))))

;;;###autoload
(cl-defun emacs-agent-transform-replace
    (workspace path expected-revision old-text new-text
               &key replace-all expected-occurrences dry-run checkpoint)
  "Plan and apply an exact text replacement for PATH in WORKSPACE.
Arguments are as for `emacs-agent-transform-plan-replace'.  DRY-RUN validates
and returns the exact preview without changing the buffer."
  (emacs-agent-transform-apply
   (emacs-agent-transform-plan-replace
    workspace path expected-revision old-text new-text
    :replace-all replace-all
    :expected-occurrences expected-occurrences)
   dry-run checkpoint))

;;;###autoload
(cl-defun emacs-agent-transform-apply-patch
    (workspace path expected-revision patch
               &key (fuzz 0) dry-run checkpoint)
  "Plan and apply strict single-file unified PATCH to PATH in WORKSPACE.
FUZZ must be zero.  DRY-RUN returns the exact preview without mutation."
  (emacs-agent-transform-apply
   (emacs-agent-transform-plan-patch
    workspace path expected-revision patch :fuzz fuzz)
   dry-run checkpoint))

(provide 'emacs-agent-transform)
;;; emacs-agent-transform.el ends here
