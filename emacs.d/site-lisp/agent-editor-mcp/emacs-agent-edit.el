;;; emacs-agent-edit.el --- Guarded atomic document edits  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure validation followed by one atomic, undoable buffer mutation.

;;; Code:

(require 'cl-lib)
(require 'emacs-agent-document)

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

(defun emacs-agent-edit--checkpoint-p (workspace requested)
  "Return whether WORKSPACE requires or permits REQUESTED checkpointing."
  (or requested
      (and (not (stringp workspace))
           (fboundp 'emacs-agent-workspace-save-policy)
           (eq (emacs-agent-workspace-save-policy workspace) 'immediate))))

(defun emacs-agent-edit--assert-mutation-allowed (workspace path)
  "Reject mutation of PATH when WORKSPACE is paused or read-only."
  (when (and (not (stringp workspace))
             (fboundp 'emacs-agent-workspace-paused-p)
             (emacs-agent-workspace-paused-p workspace))
    (emacs-agent-signal 'workspace_paused :path path))
  (when (and (not (stringp workspace))
             (fboundp 'emacs-agent-workspace-access-mode)
             (eq (emacs-agent-workspace-access-mode workspace) 'read-only))
    (emacs-agent-signal 'path_denied :path path :reason 'read-only-workspace)))

(defun emacs-agent-edit--record-changeset
    (workspace document before after previous-revision new-revision checkpoint)
  "Record a completed edit in WORKSPACE and return its change-set ID.
DOCUMENT changed from BEFORE to AFTER and PREVIOUS-REVISION to NEW-REVISION.
CHECKPOINT is non-nil when the resulting document was saved."
  (cond
   ((functionp emacs-agent-edit-record-function)
    (funcall emacs-agent-edit-record-function
             document before after previous-revision new-revision))
   ((and (not (stringp workspace))
         (fboundp 'emacs-agent-changeset-record))
    (let ((changeset
           (emacs-agent-changeset-record
            workspace
            :operations
            (list (list :operation 'document_apply_edits
                        :path (emacs-agent-document-relative-path document)))
            :touched-documents
            (list (emacs-agent-document-relative-path document))
            :base-revisions
            (list (cons (emacs-agent-document-relative-path document)
                        previous-revision))
            :final-revisions
            (list (cons (emacs-agent-document-relative-path document)
                        new-revision))
            :before-snapshots
            (list (cons (emacs-agent-document-relative-path document)
                        before))
            :checkpoint-state (and checkpoint 'checkpointed))))
      (when (fboundp 'emacs-agent-changeset-changeset-id)
        (emacs-agent-changeset-changeset-id changeset))))))

;;;###autoload
(defun emacs-agent-edit-apply
    (workspace path expected-revision edits &optional checkpoint)
  "Atomically apply guarded EDITS to PATH in WORKSPACE.

EXPECTED-REVISION must equal the current document revision.  When CHECKPOINT is
non-nil, run the normal save path before returning."
  (emacs-agent-edit--assert-mutation-allowed workspace path)
  (setq checkpoint (emacs-agent-edit--checkpoint-p workspace checkpoint))
  (let* ((document (emacs-agent-document-open workspace path))
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
       :path (emacs-agent-document-relative-path document)
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
             (window-state (emacs-agent-edit--snapshot-windows buffer))
             (saved-point (copy-marker (point)))
             (saved-mark (and (mark t) (copy-marker (mark t))))
             (saved-mark-active mark-active)
             (saved-restriction (cons (point-min-marker) (point-max-marker)))
             after new-revision changeset-id)
        (unwind-protect
            (save-selected-window
              (save-current-buffer
                (with-current-buffer buffer
                  (undo-boundary)
                  (condition-case error-data
                      (atomic-change-group
                        (save-restriction
                          (widen)
                          (emacs-agent-edit--apply-ranges ranges)
                          (when (save-excursion
                                  (goto-char (point-min))
                                  (search-forward (string 0) nil t))
                            (emacs-agent-signal
                             'unsupported_document_type
                             :path path :reason 'binary))
                          (when (> (string-bytes
                                    (buffer-substring-no-properties
                                     (point-min) (point-max)))
                                   emacs-agent-policy-maximum-document-bytes)
                            (emacs-agent-signal
                             'document_too_large :path path)))
                        (when checkpoint
                          (emacs-agent-policy-resolve workspace path)
                          (save-buffer)))
                    (emacs-agent-error
                     (signal (car error-data) (cdr error-data)))
                    (error
                     (if checkpoint
                       (progn
                       (setf (emacs-agent-document-degraded document) t
                             (emacs-agent-document-disk-fingerprint document)
                             (emacs-agent-document--disk-fingerprint
                              (emacs-agent-document-canonical-path document)))
                       (when (and (not (stringp workspace))
                                  (fboundp
                                   'emacs-agent-workspace-health-state))
                         (setf
                          (emacs-agent-workspace-health-state workspace)
                          'degraded))
                       (emacs-agent-signal
                        'save_failed :path path
                        :message (error-message-string error-data)
                        :reconciliation_required t
                              :filesystem_rollback_guaranteed nil))
                       (signal (car error-data) (cdr error-data)))))
                  (undo-boundary)
                  (save-restriction
                    (widen)
                    (setq after (buffer-substring-no-properties
                                 (point-min) (point-max))))
                  (setq new-revision
                        (emacs-agent-document-revision document))
                  (setf (emacs-agent-document-disk-fingerprint document)
                        (emacs-agent-document--disk-fingerprint
                         (emacs-agent-document-canonical-path document)))
                  (setq changeset-id
                        (emacs-agent-edit--record-changeset
                         workspace document before after current-revision
                         new-revision checkpoint))
                  (setf (emacs-agent-document-last-changeset-id document)
                        changeset-id))))
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
        (list :path (emacs-agent-document-relative-path document)
              :changeset_id changeset-id
              :previous_revision current-revision
              :new_revision new-revision
              :checkpointed (not (buffer-modified-p buffer))
              :edit_count (length ranges)
              :diff_summary
              (list :before_chars (length before)
                    :after_chars (length after))
              :diagnostics_state "not_requested")))))

(provide 'emacs-agent-edit)
;;; emacs-agent-edit.el ends here
