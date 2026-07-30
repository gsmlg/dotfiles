;;; emacs-agent-edit.el --- Guarded atomic document edits  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure validation followed by one atomic, undoable buffer mutation.

;;; Code:

(require 'cl-lib)
(require 'emacs-agent-document)
(require 'emacs-agent-project)
(require 'emacs-agent-runtime)

(defvar emacs-agent-edit-record-function nil
  "Optional function called after an edit.

The function receives DOCUMENT, the before text, the after text, the previous
revision, and the new revision.  Its return value becomes `changeset_id'.")

(cl-defstruct (emacs-agent-edit-range
               (:constructor emacs-agent-edit-range--create))
  start end new-text expected-text source)

(defun emacs-agent-edit--normalize (document edit)
  "Validate and normalize EDIT for DOCUMENT."
  (let* ((start-object (emacs-agent-document--field edit 'start))
         (end-object (emacs-agent-document--field edit 'end))
         (new-text (or (emacs-agent-document--field edit 'new_text)
                       (emacs-agent-document--field edit 'new-text)))
         (expected-present
          (or (and (hash-table-p edit)
                   (or (gethash "expected_text" edit)
                       (gethash 'expected_text edit)))
              (and (listp edit)
                   (or (assq 'expected_text edit)
                       (assoc "expected_text" edit)
                       (plist-member edit :expected_text)
                       (plist-member edit :expected-text)))))
         (expected-text
          (or (emacs-agent-document--field edit 'expected_text)
              (emacs-agent-document--field edit 'expected-text)))
         (start (emacs-agent-document-position document start-object))
         (end (emacs-agent-document-position document end-object)))
    (unless (stringp new-text)
      (emacs-agent-signal 'invalid_position
                          :field 'new_text :edit edit))
    (when (> start end)
      (emacs-agent-signal 'invalid_position :edit edit))
    (when (and expected-present (not (stringp expected-text)))
      (emacs-agent-signal 'expected_text_mismatch :edit edit))
    (emacs-agent-edit-range--create
     :start start :end end :new-text new-text
     :expected-text (and expected-present expected-text)
     :source edit)))

(defun emacs-agent-edit--overlap-p (a b)
  "Return non-nil when normalized edits A and B conflict."
  (let ((as (emacs-agent-edit-range-start a))
        (ae (emacs-agent-edit-range-end a))
        (bs (emacs-agent-edit-range-start b))
        (be (emacs-agent-edit-range-end b)))
    (cond
     ((and (= as ae) (= bs be)) (= as bs))
     ((= as ae) (and (>= as bs) (< as be)))
     ((= bs be) (and (>= bs as) (< bs ae)))
     (t (< (max as bs) (min ae be))))))

(defun emacs-agent-edit--validate-ranges (document edits)
  "Return normalized EDITS after all guards pass for DOCUMENT."
  (unless (and (listp edits) edits)
    (emacs-agent-signal 'invalid_position :field 'edits))
  (let* ((ranges (mapcar (lambda (edit)
                           (emacs-agent-edit--normalize document edit))
                         edits))
         (ordered (sort (copy-sequence ranges)
                        (lambda (a b)
                          (< (emacs-agent-edit-range-start a)
                             (emacs-agent-edit-range-start b))))))
    (cl-loop for tail on ordered
             for current = (car tail)
             for next = (cadr tail)
             when (and next (emacs-agent-edit--overlap-p current next))
             do (emacs-agent-signal 'overlapping_edits
                                    :first
                                    (emacs-agent-edit-range-source current)
                                    :second
                                    (emacs-agent-edit-range-source next)))
    (with-current-buffer (emacs-agent-document-buffer document)
      (dolist (range ranges)
        (when-let* ((expected (emacs-agent-edit-range-expected-text range)))
          (unless (equal expected
                         (buffer-substring-no-properties
                          (emacs-agent-edit-range-start range)
                          (emacs-agent-edit-range-end range)))
            (emacs-agent-signal
             'expected_text_mismatch
             :start (emacs-agent-edit-range-start range)
             :end (emacs-agent-edit-range-end range))))))
    ranges))

(defun emacs-agent-edit--snapshot-windows (buffer)
  "Capture display state of windows showing BUFFER."
  (mapcar (lambda (window)
            (list window
                  (copy-marker (window-point window))
                  (copy-marker (window-start window))))
          (get-buffer-window-list buffer nil t)))

(defun emacs-agent-edit--restore-windows (snapshot)
  "Restore live windows from SNAPSHOT."
  (dolist (entry snapshot)
    (unwind-protect
        (when (window-live-p (car entry))
          (set-window-point (car entry) (marker-position (cadr entry)))
          (set-window-start (car entry) (marker-position (caddr entry)) t))
      (set-marker (cadr entry) nil)
      (set-marker (caddr entry) nil))))

(defun emacs-agent-edit--apply-ranges (ranges)
  "Apply normalized RANGES in descending order."
  (dolist (range
           (sort (copy-sequence ranges)
                 (lambda (a b)
                   (if (= (emacs-agent-edit-range-start a)
                          (emacs-agent-edit-range-start b))
                       (> (emacs-agent-edit-range-end a)
                          (emacs-agent-edit-range-end b))
                     (> (emacs-agent-edit-range-start a)
                        (emacs-agent-edit-range-start b))))))
    (goto-char (emacs-agent-edit-range-start range))
    (delete-region (emacs-agent-edit-range-start range)
                   (emacs-agent-edit-range-end range))
    (insert (emacs-agent-edit-range-new-text range))))

(defun emacs-agent-edit--assert-text-result (path text)
  "Reject unsafe resulting TEXT for canonical PATH."
  (when (string-match-p (string 0) text)
    (emacs-agent-signal
     'unsupported_document_type :path path :reason 'binary))
  (when (> (string-bytes text)
           emacs-agent-policy-maximum-document-bytes)
    (emacs-agent-signal 'document_too_large :path path))
  text)

(defun emacs-agent-edit--checkpoint-p (runtime requested)
  "Return whether RUNTIME requires or permits REQUESTED checkpointing."
  (or requested
      (eq (emacs-agent-runtime-save-policy runtime) 'immediate)))

(defun emacs-agent-edit--assert-mutation-allowed (runtime target)
  "Reject mutation of TARGET when RUNTIME is paused or read-only."
  (let ((path (emacs-agent-resolved-target-canonical-path target)))
    (when (emacs-agent-runtime-paused-p runtime)
      (emacs-agent-signal 'runtime_paused :path path))
    (when (eq (emacs-agent-runtime-access-mode runtime) 'read-only)
      (emacs-agent-signal
       'path_denied :path path :reason 'read-only-runtime))))

(defun emacs-agent-edit--revalidate-target (runtime target)
  "Re-resolve TARGET in RUNTIME and reject an identity change."
  (let ((current
         (emacs-agent-project-resolve-target
          runtime
          (emacs-agent-resolved-target-input-path target)
          :project-id
          (emacs-agent-resolved-target-project-id target))))
    (unless
        (equal
         (emacs-agent-resolved-target-canonical-path current)
         (emacs-agent-resolved-target-canonical-path target))
      (emacs-agent-signal
       'external_change_conflict
       :path (emacs-agent-resolved-target-canonical-path target)
       :reason 'target_identity_changed))
    (emacs-agent-policy-assert-document-target runtime current)))

(defun emacs-agent-edit--record-changeset
    (runtime document before after previous-revision new-revision checkpoint)
  "Record a completed edit in RUNTIME and return its change-set ID.
DOCUMENT changed from BEFORE to AFTER and PREVIOUS-REVISION to NEW-REVISION.
CHECKPOINT is non-nil when the resulting document was saved."
  (cond
   ((functionp emacs-agent-edit-record-function)
    (funcall emacs-agent-edit-record-function
             document before after previous-revision new-revision))
   ((fboundp 'emacs-agent-changeset-record)
    (let* ((path (emacs-agent-document-canonical-path document))
           (changeset
            (emacs-agent-changeset-record
             runtime
             :operations
             (list (list :operation 'document_apply_edits
                         :path path))
             :touched-documents (list path)
             :base-revisions
             (list (cons path previous-revision))
             :final-revisions
             (list (cons path new-revision))
             :before-snapshots
             (list (cons path before))
             :checkpoint-state (and checkpoint 'checkpointed))))
      (when (fboundp 'emacs-agent-changeset-changeset-id)
        (emacs-agent-changeset-changeset-id changeset))))))

;;;###autoload
(defun emacs-agent-edit-apply
    (runtime target expected-revision edits &optional checkpoint)
  "Atomically apply guarded EDITS to TARGET in RUNTIME.

EXPECTED-REVISION must equal the current document revision.  When CHECKPOINT is
non-nil, run the normal save path before returning."
  (unless (and (emacs-agent-runtime-p runtime)
               (emacs-agent-resolved-target-p target))
    (signal 'wrong-type-argument
            (list 'emacs-agent-resolved-target target)))
  (emacs-agent-edit--assert-mutation-allowed runtime target)
  (setq checkpoint (emacs-agent-edit--checkpoint-p runtime checkpoint))
  (let* ((path (emacs-agent-resolved-target-canonical-path target))
         (document (emacs-agent-document-open runtime target))
         (_ (when (emacs-agent-document-degraded document)
              (emacs-agent-signal
               'external_change_conflict :path path
               :reason 'reconciliation_required)))
         (_ (emacs-agent-document-reconcile document))
         (current-revision (emacs-agent-document-revision document))
         (buffer (emacs-agent-document-buffer document)))
    (unless (equal expected-revision current-revision)
      (emacs-agent-signal
       'revision_conflict
       :path path
       :expected_revision expected-revision
       :current_revision current-revision
       :modified_by 'buffer
       :requires_reread t))
    (with-current-buffer buffer
      (when buffer-read-only
        (emacs-agent-signal 'path_denied :path path :reason 'read-only))
      (let* ((ranges (save-restriction
                       (widen)
                       (emacs-agent-edit--validate-ranges document edits)))
             (before (save-restriction
                       (widen)
                       (buffer-substring-no-properties
                        (point-min) (point-max))))
             (planned-after
              (with-temp-buffer
                (insert before)
                (emacs-agent-edit--apply-ranges ranges)
                (buffer-string)))
             (planned-modified (not (equal before planned-after)))
             (window-state (emacs-agent-edit--snapshot-windows buffer))
             (saved-point (copy-marker (point)))
             (saved-mark (and (mark t) (copy-marker (mark t))))
             (saved-mark-active mark-active)
             (saved-restriction (cons (point-min-marker) (point-max-marker)))
             after new-revision changeset-id modified group)
        (emacs-agent-edit--assert-text-result path planned-after)
        (unwind-protect
            (save-selected-window
              (save-current-buffer
                (with-current-buffer buffer
                  (undo-boundary)
                  (condition-case error-data
                      (progn
                        (when planned-modified
                          (setq group (prepare-change-group))
                          (activate-change-group group)
                          (save-restriction
                            (widen)
                            (emacs-agent-edit--apply-ranges ranges)))
                        (when checkpoint
                          (emacs-agent-edit--revalidate-target
                           runtime target)
                          (emacs-agent-document-checkpoint document))
                        (when group
                          (accept-change-group group)
                          (setq group nil)))
                    (emacs-agent-error
                     (when group
                       (let ((details
                              (emacs-agent-error-details error-data)))
                         (if (plist-get details :partial_completion)
                             (accept-change-group group)
                           (cancel-change-group group)))
                       (setq group nil))
                     (let ((code (emacs-agent-error-code error-data))
                           (details
                            (emacs-agent-error-details error-data)))
                       (when (or
                              (eq code 'save_failed)
                              (plist-get details
                                         :reconciliation_required))
                         (setf
                          (emacs-agent-document-degraded document) t
                          (emacs-agent-runtime-health-state runtime)
                          'degraded)))
                     (signal (car error-data) (cdr error-data)))
                    (error
                     (when group
                       (cancel-change-group group)
                       (setq group nil))
                     (signal (car error-data) (cdr error-data))))
                  (undo-boundary)
                  (save-restriction
                    (widen)
                    (setq after (buffer-substring-no-properties
                                 (point-min) (point-max))))
                  (emacs-agent-edit--assert-text-result path after)
                  (setq new-revision
                        (emacs-agent-document-revision document))
                  (setq modified (not (equal before after)))
                  (when modified
                    (setq changeset-id
                          (emacs-agent-edit--record-changeset
                           runtime document before after current-revision
                           new-revision checkpoint))
                    (setf (emacs-agent-document-last-changeset-id document)
                          changeset-id)))))
          (when group
            (with-current-buffer buffer
              (ignore-errors (cancel-change-group group)))
            (setq group nil))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (widen)
              (narrow-to-region (marker-position (car saved-restriction))
                                (marker-position (cdr saved-restriction)))
              (goto-char saved-point)
              (if saved-mark
                  (set-mark (marker-position saved-mark))
                (set-marker (mark-marker) nil))
              (setq mark-active saved-mark-active)))
          (emacs-agent-edit--restore-windows window-state)
          (set-marker saved-point nil)
          (when saved-mark (set-marker saved-mark nil))
          (set-marker (car saved-restriction) nil)
          (set-marker (cdr saved-restriction) nil))
        (append
         (emacs-agent-document-output-fields target)
         (list
          :changeset_id changeset-id
          :previous_revision current-revision
          :new_revision new-revision
          :checkpointed (not (buffer-modified-p buffer))
          :modified (and modified t)
          :edit_count (length ranges)
          :diff_summary
          (list :before_chars (length before)
                :after_chars (length after))
          :diagnostics_state "not_requested"))))))

(provide 'emacs-agent-edit)
;;; emacs-agent-edit.el ends here
