;;; emacs-agent-changeset.el --- Change tracking and rollback -*- lexical-binding: t; -*-

;;; Commentary:

;; One mutating tool call creates one change set.  Before-images remain in
;; memory; journals receive only metadata and diffs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'emacs-agent-policy)
(require 'emacs-agent-workspace)

(define-error 'emacs-agent-changeset-error "Emacs Agent change set error")
(define-error 'emacs-agent-rollback-conflict "Change set cannot be rolled back"
  'emacs-agent-changeset-error)

(cl-defstruct (emacs-agent-changeset
               (:constructor emacs-agent-changeset--make))
  changeset-id workspace-id request-id agent-identity created-at status
  operations touched-documents base-revisions final-revisions before-snapshots
  checkpoint-state diagnostics-before diagnostics-after unified-diff
  insertions deletions)

(defun emacs-agent-changeset--id ()
  "Return a new opaque change-set ID."
  (emacs-agent-workspace--random-id "changeset"))

(cl-defun emacs-agent-changeset-record
    (workspace &key request-id agent-identity operations touched-documents
               base-revisions final-revisions before-snapshots
               checkpoint-state diagnostics-before diagnostics-after
               unified-diff insertions deletions)
  "Record one completed mutation in WORKSPACE and return its change set.
REQUEST-ID and AGENT-IDENTITY attribute OPERATIONS.  TOUCHED-DOCUMENTS,
BASE-REVISIONS, FINAL-REVISIONS, and BEFORE-SNAPSHOTS describe affected state.
CHECKPOINT-STATE records persistence.  DIAGNOSTICS-BEFORE and
DIAGNOSTICS-AFTER record analysis state.  UNIFIED-DIFF may cache a diff, while
INSERTIONS and DELETIONS provide summary counts."
  (let* ((id (emacs-agent-changeset--id))
         (status (if (memq checkpoint-state '(t checkpointed))
                     'checkpointed
                   'applied))
         (changeset
          (emacs-agent-changeset--make
           :changeset-id id
           :workspace-id (emacs-agent-workspace-workspace-id workspace)
           :request-id request-id
           :agent-identity agent-identity
           :created-at (float-time)
           :status status
           :operations operations
           :touched-documents (or touched-documents
                                  (mapcar #'car before-snapshots))
           :base-revisions base-revisions
           :final-revisions final-revisions
           :before-snapshots before-snapshots
           :checkpoint-state checkpoint-state
           :diagnostics-before diagnostics-before
           :diagnostics-after diagnostics-after
           :unified-diff
           (or unified-diff
               (mapconcat
                (lambda (entry)
                  (let ((path (car entry))
                        (snapshot (cdr entry)))
                    (emacs-agent-changeset--diff-text
                     path
                     (emacs-agent-changeset--snapshot-content snapshot)
                     (emacs-agent-changeset--current-content workspace path))))
                before-snapshots ""))
           :insertions (or insertions 0)
           :deletions (or deletions 0))))
    (puthash id changeset
             (emacs-agent-workspace-changeset-registry workspace))
    (emacs-agent-workspace-record-activity
     workspace
     (list :request_id request-id :tool "mutation"
           :status (symbol-name status) :changeset_id id
           :paths (emacs-agent-changeset-touched-documents changeset)))
    changeset))

(defun emacs-agent-changeset-get (workspace changeset-id)
  "Return CHANGESET-ID from WORKSPACE, or signal."
  (or (gethash changeset-id
               (emacs-agent-workspace-changeset-registry workspace))
      (signal 'emacs-agent-changeset-error
              (list (format "Unknown change set: %s" changeset-id)))))

(defun emacs-agent-changeset-list (&optional workspace)
  "Return change records from WORKSPACE, newest first."
  (let (items)
    (maphash
     (lambda (_id changeset) (push changeset items))
     (emacs-agent-workspace-changeset-registry
      (or workspace (emacs-agent-workspace-current))))
    (sort items
          (lambda (left right)
            (> (emacs-agent-changeset-created-at left)
               (emacs-agent-changeset-created-at right))))))

(defun emacs-agent-changeset--snapshot-exists-p (snapshot)
  "Return whether SNAPSHOT represents an existing document."
  (if (stringp snapshot)
      t
    (plist-get snapshot :exists)))

(defun emacs-agent-changeset--snapshot-content (snapshot)
  "Return content stored in SNAPSHOT."
  (if (stringp snapshot)
      snapshot
    (or (plist-get snapshot :content) "")))

(defun emacs-agent-changeset--current-content (workspace path)
  "Return authoritative current content for PATH in WORKSPACE."
  (let* ((absolute
          (emacs-agent-policy-resolve workspace path t))
         (buffer (find-buffer-visiting absolute)))
    (cond
     ((buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (buffer-substring-no-properties (point-min) (point-max)))))
     ((file-readable-p absolute)
      (with-temp-buffer
        (insert-file-contents absolute)
        (buffer-string)))
     (t ""))))

(defun emacs-agent-changeset--diff-text (path before after)
  "Create a unified diff for PATH from BEFORE to AFTER."
  (if (equal before after)
      ""
    (let ((old-file (make-temp-file "emacs-agent-old-"))
          (new-file (make-temp-file "emacs-agent-new-"))
          (output (generate-new-buffer " *emacs-agent-diff*")))
      (unwind-protect
          (progn
            (with-temp-file old-file (insert before))
            (with-temp-file new-file (insert after))
            (if (executable-find "diff")
                (let ((status
                       (process-file
                        (executable-find "diff") nil output nil
                        "-u" "--label" (concat "a/" path)
                        "--label" (concat "b/" path)
                        old-file new-file)))
                  (unless (memq status '(0 1))
                    (signal 'emacs-agent-changeset-error
                            (list "Unable to generate unified diff")))
                  (with-current-buffer output (buffer-string)))
              (format "--- a/%s\n+++ b/%s\n@@ complete file @@\n-%s\n+%s\n"
                      path path before after)))
        (when (file-exists-p old-file) (delete-file old-file))
        (when (file-exists-p new-file) (delete-file new-file))
        (when (buffer-live-p output) (kill-buffer output))))))

(defun emacs-agent-changeset-diff (workspace &optional changeset-id)
  "Return a unified diff for CHANGESET-ID in WORKSPACE.

When CHANGESET-ID is nil, concatenate all active change-set diffs."
  (let ((changesets
         (if changeset-id
             (list (emacs-agent-changeset-get workspace changeset-id))
           (seq-filter
            (lambda (changeset)
              (memq (emacs-agent-changeset-status changeset)
                    '(applied checkpointed reviewed)))
            (reverse (emacs-agent-changeset-list workspace))))))
    (mapconcat
     (lambda (changeset)
       (or
        (emacs-agent-changeset-unified-diff changeset)
        (let ((diff
               (mapconcat
                (lambda (entry)
                  (let ((path (car entry))
                        (snapshot (cdr entry)))
                    (emacs-agent-changeset--diff-text
                     path
                     (emacs-agent-changeset--snapshot-content snapshot)
                     (emacs-agent-changeset--current-content
                      workspace path))))
                (emacs-agent-changeset-before-snapshots changeset)
                "")))
          (setf (emacs-agent-changeset-unified-diff changeset) diff)
          diff)))
     changesets "")))

(defun emacs-agent-changeset-mark-reviewed (workspace changeset-id)
  "Mark CHANGESET-ID in WORKSPACE as reviewed."
  (let ((changeset (emacs-agent-changeset-get workspace changeset-id)))
    (when (eq (emacs-agent-changeset-status changeset) 'rolled-back)
      (signal 'emacs-agent-changeset-error
              (list "A rolled-back change set cannot be reviewed")))
    (setf (emacs-agent-changeset-status changeset) 'reviewed)
    changeset))

(defun emacs-agent-changeset--revision (workspace path)
  "Return current revision for PATH in WORKSPACE."
  (let* ((absolute
          (emacs-agent-policy-resolve workspace path t))
         (buffer (find-buffer-visiting absolute)))
    (cond
     ((and (not (file-exists-p absolute))
           (not (buffer-live-p buffer)))
      nil)
     ((fboundp 'emacs-agent-document-revision-for-path)
      (funcall #'emacs-agent-document-revision-for-path workspace path))
     ((and (fboundp 'emacs-agent-document-open)
           (fboundp 'emacs-agent-document-revision))
      (let ((document
             (funcall #'emacs-agent-document-open workspace path)))
        (when (fboundp 'emacs-agent-document-reconcile)
          (funcall #'emacs-agent-document-reconcile document))
        (funcall #'emacs-agent-document-revision document)))
     ((fboundp 'emacs-agent-document-current-revision)
      (funcall #'emacs-agent-document-current-revision workspace path))
     (t
      (if (buffer-live-p buffer)
          (with-current-buffer buffer
            (secure-hash
             'sha256
             (format "%s:%s"
                     (buffer-chars-modified-tick)
                     (buffer-substring-no-properties
                      (point-min) (point-max)))))
        (when (file-exists-p absolute)
          (secure-hash 'sha256 absolute)))))))

(defun emacs-agent-changeset--validate-rollback (workspace changeset)
  "Validate all revision guards for CHANGESET in WORKSPACE."
  (unless (memq (emacs-agent-changeset-status changeset)
                '(applied checkpointed reviewed))
    (signal 'emacs-agent-rollback-conflict
            (list "Change set is not rollback-compatible")))
  (dolist (entry (emacs-agent-changeset-final-revisions changeset))
    (let ((current (emacs-agent-changeset--revision workspace (car entry))))
      (unless (equal current (cdr entry))
        (setf (emacs-agent-changeset-status changeset) 'conflicted)
        (signal 'emacs-agent-rollback-conflict
                (list (format "Document changed after change set: %s"
                              (car entry))))))))

(defun emacs-agent-changeset--restore-one (workspace entry)
  "Restore one before-image ENTRY in WORKSPACE."
  (let* ((path (car entry))
         (snapshot (cdr entry))
         (absolute
          (emacs-agent-policy-resolve workspace path t))
         (buffer (find-buffer-visiting absolute)))
    (if (emacs-agent-changeset--snapshot-exists-p snapshot)
        (let ((buffer (or buffer (find-file-noselect absolute))))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (atomic-change-group
                (erase-buffer)
                (insert (emacs-agent-changeset--snapshot-content snapshot))))
            (when (eq (emacs-agent-workspace-save-policy workspace)
                      'immediate)
              (save-buffer))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-exists-p absolute)
        (delete-file absolute)))))

(defun emacs-agent-changeset-rollback (workspace changeset-id)
  "Rollback CHANGESET-ID in WORKSPACE when final revisions still match.

The complete guard pass occurs before any mutation.  Under the `immediate'
save policy restored buffers are checkpointed."
  (let ((changeset (emacs-agent-changeset-get workspace changeset-id)))
    (emacs-agent-changeset--validate-rollback workspace changeset)
    ;; Resolve every target before the first mutation, then resolve it again
    ;; immediately before use to catch replaced symlink parents.
    (dolist (entry (emacs-agent-changeset-before-snapshots changeset))
      (emacs-agent-policy-resolve workspace (car entry) t))
    (emacs-agent-workspace-enqueue-mutation
     workspace
     (lambda ()
       (let (restoring-entry restored-paths)
         (condition-case error-data
             (progn
               (dolist (entry
                        (emacs-agent-changeset-before-snapshots changeset))
                 (setq restoring-entry entry)
                 (emacs-agent-changeset--restore-one workspace entry)
                 (push (car entry) restored-paths))
               (setf (emacs-agent-changeset-status changeset) 'rolled-back)
               (emacs-agent-workspace-record-activity
                workspace
                (list :tool "changeset_rollback" :status "completed"
                      :changeset_id changeset-id
                      :paths
                      (emacs-agent-changeset-touched-documents changeset)))
               changeset)
           (error
            (when restoring-entry
              (let* ((path (car restoring-entry))
                     (absolute
                      (ignore-errors
                        (emacs-agent-policy-resolve workspace path t)))
                     (buffer (and absolute
                                  (find-buffer-visiting absolute)))
                     (document
                      (and absolute
                           (gethash
                            absolute
                            (emacs-agent-workspace-document-registry
                             workspace)))))
                (when document
                  (setf (emacs-agent-document-degraded document) t))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (set-buffer-modified-p t)))))
            (setf (emacs-agent-changeset-status changeset) 'conflicted
                  (emacs-agent-workspace-health-state workspace) 'degraded)
            (let ((completed (nreverse restored-paths))
                  (failed (and restoring-entry (car restoring-entry))))
              (emacs-agent-workspace-record-activity
               workspace
               (list :tool "changeset_rollback" :status "partial"
                     :changeset_id changeset-id
                     :restored_paths completed
                     :failed_path failed))
              (signal
               'emacs-agent-rollback-conflict
               (list
                (format "Rollback partially completed before %s failed: %s"
                        failed (error-message-string error-data))
                :partial_completion t
                :restored_paths completed
                :failed_path failed
                :reconciliation_required t))))))))))

(provide 'emacs-agent-changeset)
;;; emacs-agent-changeset.el ends here
