;;; gsmlg-ai-review.el --- AI proposal review and apply -*- lexical-binding: t; -*-

;;; Commentary:
;; Proposal UI, stale detection, per-file and transactional apply.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'diff)
(require 'gsmlg-ai)
(require 'gsmlg-ai-context)
(require 'gsmlg-ai-session)

(defconst gsmlg-ai-review-buffer-name "*GSMLG AI Proposal*"
  "Name of the proposal review buffer.")

(defun gsmlg-ai-review--changed-files (session)
  "Return SESSION files that differ from their originals or are new."
  (cl-remove-if
   (lambda (file)
     (and (not (eq (gsmlg-ai-snapshot-operation file) 'create))
          (equal (gsmlg-ai-snapshot-original-content file)
                 (gsmlg-ai-snapshot-proposed-content file))))
   (gsmlg-ai-session-files session)))

(defun gsmlg-ai-review--live-hash (file)
  "Return the current content hash for FILE's source."
  (cond
   ((eq (gsmlg-ai-snapshot-source-kind file) 'staged-new)
    nil)
   ((buffer-live-p (gsmlg-ai-snapshot-source-buffer file))
    (with-current-buffer (gsmlg-ai-snapshot-source-buffer file)
      (save-restriction
        (widen)
        (gsmlg-ai-context--hash
         (buffer-substring-no-properties (point-min) (point-max))))))
   ((gsmlg-ai-snapshot-canonical-file file)
    (with-temp-buffer
      (insert-file-contents (gsmlg-ai-snapshot-canonical-file file))
      (gsmlg-ai-context--hash (buffer-string))))
   (t nil)))

(defun gsmlg-ai-review-stale-p (file)
  "Return non-nil when FILE's source changed after the snapshot."
  (if (eq (gsmlg-ai-snapshot-source-kind file) 'staged-new)
      (file-exists-p (gsmlg-ai-snapshot-canonical-file file))
    (let ((buffer (gsmlg-ai-snapshot-source-buffer file))
          (file-name (gsmlg-ai-snapshot-canonical-file file)))
      (or
       (and (buffer-live-p buffer)
            (or (not (eql (buffer-chars-modified-tick buffer)
                          (gsmlg-ai-snapshot-source-buffer-tick file)))
                (not (equal (gsmlg-ai-review--live-hash file)
                            (gsmlg-ai-snapshot-source-content-hash file)))))
       (and file-name
            (not (buffer-live-p buffer))
            (or (not (equal (file-attributes file-name)
                            (gsmlg-ai-snapshot-source-file-attributes file)))
                (not (equal (gsmlg-ai-review--live-hash file)
                            (gsmlg-ai-snapshot-source-content-hash file)))))
       (and file-name
            (when-let* ((visiting (find-buffer-visiting file-name)))
              (with-current-buffer visiting
                (save-restriction
                  (widen)
                  (not (equal (gsmlg-ai-context--hash
                               (buffer-substring-no-properties
                                (point-min) (point-max)))
                              (gsmlg-ai-snapshot-source-content-hash
                               file)))))))))))
(defun gsmlg-ai-review--status (file)
  "Return a display status symbol for FILE."
  (cond
   ((eq (gsmlg-ai-snapshot-apply-status file) 'applied) 'applied)
   ((gsmlg-ai-review-stale-p file)
    (setf (gsmlg-ai-snapshot-conflict-reason file) "source changed")
    'stale)
   ((eq (gsmlg-ai-snapshot-operation file) 'create) 'new)
   ((equal (gsmlg-ai-snapshot-original-content file)
           (gsmlg-ai-snapshot-proposed-content file))
    'unchanged)
   (t 'modified)))

(defun gsmlg-ai-review--refresh ()
  "Refresh the proposal tabulated list."
  (when-let* ((buffer (get-buffer gsmlg-ai-review-buffer-name))
              (session gsmlg-ai-session--active))
    (with-current-buffer buffer
      (setq tabulated-list-entries
            (mapcar
             (lambda (file)
               (list (gsmlg-ai-snapshot-id file)
                     (vector
                      (symbol-name (gsmlg-ai-review--status file))
                      (gsmlg-ai-snapshot-display-path file)
                      (format "%d"
                              (gsmlg-ai-snapshot-proposal-revision file))
                      (format "%d→%d"
                              (string-bytes
                               (gsmlg-ai-snapshot-original-content file))
                              (string-bytes
                               (gsmlg-ai-snapshot-proposed-content file)))
                      (or (gsmlg-ai-snapshot-conflict-reason file) ""))))
             (gsmlg-ai-session-files session)))
      (tabulated-list-print t))))

(defvar-keymap gsmlg-ai-review-mode-map
  :doc "Keymap for AI proposal review."
  :parent tabulated-list-mode-map
  "RET" #'gsmlg-ai-review-visit
  "d" #'gsmlg-ai-review-diff-file
  "D" #'gsmlg-ai-review-diff-all
  "e" #'gsmlg-ai-review-ediff-file
  "a" #'gsmlg-ai-review-apply-file
  "A" #'gsmlg-ai-review-apply-all
  "r" #'gsmlg-ai-session-revise
  "g" #'gsmlg-ai-review-show
  "x" #'gsmlg-ai-session-discard
  "q" #'quit-window)

(define-derived-mode gsmlg-ai-review-mode tabulated-list-mode
  "AI-Proposal"
  "Major mode for reviewing staged AI proposals."
  (setq tabulated-list-format
        [("Status" 10 t)
         ("File" 52 t)
         ("Rev" 5 t)
         ("Bytes" 12 t)
         ("Note" 24 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun gsmlg-ai-review--file-at-point ()
  "Return the proposal file at point."
  (let ((id (tabulated-list-get-id))
        (session gsmlg-ai-session--active))
    (and session id
         (cl-find id (gsmlg-ai-session-files session)
                  :key #'gsmlg-ai-snapshot-id
                  :test #'equal))))

(defun gsmlg-ai-review-show ()
  "Show the current staged proposal."
  (interactive)
  (unless gsmlg-ai-session--active
    (user-error "No active AI proposal"))
  (let* ((session gsmlg-ai-session--active)
         (buffer (get-buffer-create gsmlg-ai-review-buffer-name)))
    (with-current-buffer buffer
      (gsmlg-ai-review-mode)
      (let ((inhibit-read-only t))
        (setq header-line-format
              (format
               "Session %s | %s | tools %d/%d | root %s | %s"
               (gsmlg-ai-session-id session)
               (gsmlg-ai-session-state session)
               (gsmlg-ai-session-tool-call-count session)
               gsmlg-ai-max-tool-calls
               (gsmlg-ai-session-creation-root session)
               (or (gsmlg-ai-session-model-summary session) ""))))
      (gsmlg-ai-review--refresh))
    (pop-to-buffer buffer)))

(defun gsmlg-ai-review-visit ()
  "Visit the source or proposed path at point."
  (interactive)
  (when-let* ((file (gsmlg-ai-review--file-at-point)))
    (cond
     ((buffer-live-p (gsmlg-ai-snapshot-source-buffer file))
      (pop-to-buffer (gsmlg-ai-snapshot-source-buffer file)))
     ((gsmlg-ai-snapshot-canonical-file file)
      (find-file (gsmlg-ai-snapshot-canonical-file file)))
     (t (user-error "Nothing to visit")))))

(defun gsmlg-ai-review--diff-buffers (before after label)
  "Show a diff between BEFORE and AFTER text using LABEL."
  (let ((old (get-buffer-create (format "*gsmlg-ai-old:%s*" label)))
        (new (get-buffer-create (format "*gsmlg-ai-new:%s*" label))))
    (with-current-buffer old
      (erase-buffer)
      (insert before))
    (with-current-buffer new
      (erase-buffer)
      (insert after))
    (diff-no-select old new nil nil
                    (get-buffer-create (format "*gsmlg-ai-diff:%s*" label)))
    (dolist (buffer (list old new))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun gsmlg-ai-review-diff-file ()
  "Show a diff for the proposal file at point."
  (interactive)
  (when-let* ((file (gsmlg-ai-review--file-at-point)))
    (gsmlg-ai-review--diff-buffers
     (gsmlg-ai-snapshot-original-content file)
     (gsmlg-ai-snapshot-proposed-content file)
     (file-name-nondirectory (gsmlg-ai-snapshot-display-path file)))))

(defun gsmlg-ai-review-diff-all ()
  "Show a combined diff for all changed proposal files."
  (interactive)
  (unless gsmlg-ai-session--active
    (user-error "No active proposal"))
  (let ((before "")
        (after ""))
    (dolist (file (gsmlg-ai-review--changed-files gsmlg-ai-session--active))
      (setq before
            (concat before
                    (format "===== %s =====\n"
                            (gsmlg-ai-snapshot-display-path file))
                    (gsmlg-ai-snapshot-original-content file)
                    "\n")
            after
            (concat after
                    (format "===== %s =====\n"
                            (gsmlg-ai-snapshot-display-path file))
                    (gsmlg-ai-snapshot-proposed-content file)
                    "\n")))
    (gsmlg-ai-review--diff-buffers before after "proposal")))

(defun gsmlg-ai-review-ediff-file ()
  "Ediff original and proposed content for the file at point."
  (interactive)
  (when-let* ((file (gsmlg-ai-review--file-at-point))
              (old (generate-new-buffer
                    (format "*gsmlg-ai-ediff-a:%s*"
                            (gsmlg-ai-snapshot-id file))))
              (new (generate-new-buffer
                    (format "*gsmlg-ai-ediff-b:%s*"
                            (gsmlg-ai-snapshot-id file)))))
    (with-current-buffer old
      (insert (gsmlg-ai-snapshot-original-content file)))
    (with-current-buffer new
      (insert (gsmlg-ai-snapshot-proposed-content file)))
    (ediff-buffers old new)))

(defun gsmlg-ai-review--preflight (file)
  "Validate FILE can be applied, or signal an error."
  (when (eq (gsmlg-ai-snapshot-apply-status file) 'applied)
    (error "Already applied: %s" (gsmlg-ai-snapshot-display-path file)))
  (when (gsmlg-ai-review-stale-p file)
    (error "Stale proposal for %s"
           (gsmlg-ai-snapshot-display-path file)))
  (pcase (gsmlg-ai-snapshot-source-kind file)
    ('staged-new
     (let ((path (gsmlg-ai-snapshot-canonical-file file)))
       (when (file-exists-p path)
         (error "Target already exists: %s" path))
       (unless (file-directory-p (file-name-directory path))
         (error "Parent directory does not exist: %s"
                (file-name-directory path)))))
    (_
     (let ((buffer
            (or (and (buffer-live-p (gsmlg-ai-snapshot-source-buffer file))
                     (gsmlg-ai-snapshot-source-buffer file))
                (and (gsmlg-ai-snapshot-canonical-file file)
                     (find-file-noselect
                      (gsmlg-ai-snapshot-canonical-file file))))))
       (unless buffer
         (error "No buffer available for %s"
                (gsmlg-ai-snapshot-display-path file)))
       (with-current-buffer buffer
         (when buffer-read-only
           (error "Buffer is read-only: %s" (buffer-name))))))))

(defun gsmlg-ai-review--apply-one (file)
  "Apply FILE's proposed content to its visiting buffer without saving."
  (gsmlg-ai-review--preflight file)
  (let ((buffer (gsmlg-ai-review--buffer-for-apply file)))
    (with-current-buffer buffer
      (when buffer-read-only
        (error "Buffer is read-only: %s" (buffer-name)))
      (undo-boundary)
      (atomic-change-group
        (let ((inhibit-read-only t))
          (save-restriction
            (widen)
            (erase-buffer)
            (insert (gsmlg-ai-snapshot-proposed-content file)))))
      (undo-boundary)
      (set-buffer-modified-p t)
      (when (and (eq (gsmlg-ai-snapshot-source-kind file) 'staged-new)
                 (file-exists-p (gsmlg-ai-snapshot-canonical-file file)))
        (error "Apply created an unexpected disk file: %s"
               (gsmlg-ai-snapshot-canonical-file file))))
    (setf (gsmlg-ai-snapshot-source-buffer file) buffer
          (gsmlg-ai-snapshot-apply-status file) 'applied)))

(defun gsmlg-ai-review-apply-file ()
  "Apply the proposal file at point."
  (interactive)
  (when-let* ((file (gsmlg-ai-review--file-at-point)))
    (gsmlg-ai-review--apply-one file)
    (gsmlg-ai-review--refresh)
    (message "Applied %s (unsaved)"
             (gsmlg-ai-snapshot-display-path file))))

(defun gsmlg-ai-review--buffer-for-apply (file)
  "Return the visiting buffer used to apply FILE."
  (if (eq (gsmlg-ai-snapshot-source-kind file) 'staged-new)
      (find-file-noselect (gsmlg-ai-snapshot-canonical-file file))
    (or (and (buffer-live-p (gsmlg-ai-snapshot-source-buffer file))
             (gsmlg-ai-snapshot-source-buffer file))
        (find-file-noselect (gsmlg-ai-snapshot-canonical-file file)))))

(defun gsmlg-ai-review-apply-all ()
  "Transactionally apply all eligible proposal files."
  (interactive)
  (unless gsmlg-ai-session--active
    (user-error "No active proposal"))
  (let* ((session gsmlg-ai-session--active)
         (targets
          (cl-remove-if
           (lambda (file)
             (or (eq (gsmlg-ai-snapshot-apply-status file) 'applied)
                 (and (not (eq (gsmlg-ai-snapshot-operation file) 'create))
                      (equal (gsmlg-ai-snapshot-original-content file)
                             (gsmlg-ai-snapshot-proposed-content file)))))
           (gsmlg-ai-session-files session)))
         (groups nil)
         (created-buffers nil)
         (failure nil))
    (dolist (file targets)
      (gsmlg-ai-review--preflight file))
    (condition-case err
        (progn
          (dolist (file targets)
            (let* ((new-p (eq (gsmlg-ai-snapshot-source-kind file)
                              'staged-new))
                   (buffer (gsmlg-ai-review--buffer-for-apply file))
                   (group (with-current-buffer buffer
                            (prepare-change-group))))
              (when new-p
                (push buffer created-buffers))
              (with-current-buffer buffer
                (activate-change-group group)
                (undo-boundary)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (gsmlg-ai-snapshot-proposed-content file)))
                (set-buffer-modified-p t)
                (undo-boundary))
              (push (list file buffer group) groups)))
          (dolist (entry groups)
            (accept-change-group (nth 2 entry))
            (setf (gsmlg-ai-snapshot-source-buffer (nth 0 entry)) (nth 1 entry)
                  (gsmlg-ai-snapshot-apply-status (nth 0 entry)) 'applied))
          (setq created-buffers nil
                groups nil)
          (gsmlg-ai-review--refresh)
          (message "Applied %d file(s) (unsaved)" (length targets)))
      (error
       (setq failure err)))
    (when failure
      (dolist (entry groups)
        (ignore-errors (cancel-change-group (nth 2 entry)))
        (setf (gsmlg-ai-snapshot-apply-status (nth 0 entry)) 'pending))
      (dolist (buffer created-buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (gsmlg-ai-review--refresh)
      (user-error "Apply All failed and rolled back: %s"
                 (error-message-string failure)))))
(provide 'gsmlg-ai-review)
;;; gsmlg-ai-review.el ends here
