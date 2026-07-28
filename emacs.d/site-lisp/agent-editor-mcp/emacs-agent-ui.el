;;; emacs-agent-ui.el --- Keyboard review UI for Emacs Agent Editor -*- lexical-binding: t; -*-

;;; Commentary:

;; Tabulated activity and change-set buffers.  Agent operations never invoke
;; these commands; they are explicit human controls.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'emacs-agent-workspace)
(require 'emacs-agent-changeset)

(defconst emacs-agent-activity-buffer-name "*Emacs Agent Activity*")
(defconst emacs-agent-changes-buffer-name "*Emacs Agent Changes*")

(defvar emacs-agent-activity-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'emacs-agent-ui-refresh)
    (define-key map (kbd "P") #'emacs-agent-pause-mutations)
    (define-key map (kbd "R") #'emacs-agent-resume-mutations)
    (define-key map (kbd "a") #'emacs-agent-approve-at-point)
    (define-key map (kbd "x") #'emacs-agent-reject-at-point)
    (define-key map (kbd "k") #'emacs-agent-revoke-writer)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode emacs-agent-activity-mode tabulated-list-mode
  "Agent-Activity"
  "Display Emacs Agent activity and pending approvals."
  (setq tabulated-list-format
        [("Time" 20 t)
         ("Status" 18 t)
         ("Tool" 28 t)
         ("Path" 32 t)
         ("Duration" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Time" t))
  (add-hook 'tabulated-list-revert-hook
            #'emacs-agent-ui--activity-entries nil t)
  (tabulated-list-init-header))

(defvar emacs-agent-changes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'emacs-agent-ui-refresh)
    (define-key map (kbd "RET") #'emacs-agent-open-changed-file)
    (define-key map (kbd "d") #'emacs-agent-view-diff)
    (define-key map (kbd "c") #'emacs-agent-checkpoint-at-point)
    (define-key map (kbd "r") #'emacs-agent-rollback-at-point)
    (define-key map (kbd "v") #'emacs-agent-mark-reviewed-at-point)
    (define-key map (kbd "P") #'emacs-agent-pause-mutations)
    (define-key map (kbd "R") #'emacs-agent-resume-mutations)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode emacs-agent-changes-mode tabulated-list-mode
  "Agent-Changes"
  "Display Emacs Agent change sets."
  (setq tabulated-list-format
        [("Created" 20 t)
         ("Status" 14 t)
         ("Files" 42 t)
         ("Changes" 15 t)
         ("Checkpoint" 14 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Created" t))
  (add-hook 'tabulated-list-revert-hook
            #'emacs-agent-ui--changes-entries nil t)
  (tabulated-list-init-header))

(defun emacs-agent-ui--time (value)
  "Format floating-point timestamp VALUE."
  (if value
      (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time value))
    ""))

(defun emacs-agent-ui--display (value)
  "Convert VALUE to compact display text."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   ((listp value) (mapconcat #'emacs-agent-ui--display value ", "))
   (t (format "%s" value))))

(defun emacs-agent-ui--activity-entries ()
  "Populate the current activity list."
  (setq
   tabulated-list-entries
   (cl-loop
    for entry in (emacs-agent-workspace-recent-activity)
    for ordinal from 0
    collect
    (list
     (or (plist-get entry :approval_request_id)
         (plist-get entry :request_id)
         (format "activity-%d" ordinal))
     (vector
      (emacs-agent-ui--time (plist-get entry :timestamp))
      (emacs-agent-ui--display (plist-get entry :status))
      (emacs-agent-ui--display (plist-get entry :tool))
      (emacs-agent-ui--display
       (or (plist-get entry :path) (plist-get entry :paths)))
      (let ((duration (plist-get entry :duration)))
        (if (numberp duration) (format "%.3fs" duration) "")))))))

(defun emacs-agent-ui--changes-entries ()
  "Populate the current change-set list."
  (setq
   tabulated-list-entries
   (mapcar
    (lambda (changeset)
      (list
       (emacs-agent-changeset-changeset-id changeset)
       (vector
        (emacs-agent-ui--time
         (emacs-agent-changeset-created-at changeset))
        (symbol-name (emacs-agent-changeset-status changeset))
        (emacs-agent-ui--display
         (emacs-agent-changeset-touched-documents changeset))
        (format "+%d/-%d"
                (emacs-agent-changeset-insertions changeset)
                (emacs-agent-changeset-deletions changeset))
        (emacs-agent-ui--display
         (emacs-agent-changeset-checkpoint-state changeset)))))
    (emacs-agent-changeset-list))))

(defun emacs-agent-ui-refresh ()
  "Refresh the current Agent Editor list."
  (interactive)
  (tabulated-list-revert))

;;;###autoload
(defun emacs-agent-show-activity ()
  "Open the keyboard-driven Agent Editor activity buffer."
  (interactive)
  (let ((buffer (get-buffer-create emacs-agent-activity-buffer-name)))
    (with-current-buffer buffer
      (emacs-agent-activity-mode)
      (emacs-agent-ui--activity-entries)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

;;;###autoload
(defun emacs-agent-show-changes ()
  "Open the keyboard-driven Agent Editor change-set buffer."
  (interactive)
  (let ((buffer (get-buffer-create emacs-agent-changes-buffer-name)))
    (with-current-buffer buffer
      (emacs-agent-changes-mode)
      (emacs-agent-ui--changes-entries)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

;;;###autoload
(defun emacs-agent-pause-mutations ()
  "Pause agent mutations in the current workspace."
  (interactive)
  (emacs-agent-workspace-pause)
  (message "Emacs Agent mutations paused")
  (when (derived-mode-p 'tabulated-list-mode)
    (emacs-agent-ui-refresh)))

;;;###autoload
(defun emacs-agent-resume-mutations ()
  "Resume agent mutations in the current workspace."
  (interactive)
  (emacs-agent-workspace-resume)
  (message "Emacs Agent mutations resumed")
  (when (derived-mode-p 'tabulated-list-mode)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-ui--row-id ()
  "Return the ID at point, or signal a user error."
  (or (tabulated-list-get-id)
      (user-error "No Emacs Agent item at point")))

(defun emacs-agent-approve-at-point ()
  "Approve the pending approval request at point."
  (interactive)
  (let ((id (emacs-agent-ui--row-id)))
    (emacs-agent-workspace-approve
     (emacs-agent-workspace-current) id)
    (message "Approved %s" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-reject-at-point ()
  "Reject the pending approval request at point."
  (interactive)
  (let ((id (emacs-agent-ui--row-id)))
    (emacs-agent-workspace-reject
     (emacs-agent-workspace-current) id)
    (message "Rejected %s" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-open-changed-file ()
  "Visit the first file touched by the change set at point."
  (interactive)
  (let* ((workspace (emacs-agent-workspace-current))
         (changeset
          (emacs-agent-changeset-get workspace
                                     (emacs-agent-ui--row-id)))
         (path (car
                (emacs-agent-changeset-touched-documents changeset))))
    (unless path (user-error "Change set has no files"))
    (find-file (expand-file-name path
                                 (emacs-agent-workspace-root workspace)))))

(defun emacs-agent-view-diff ()
  "Display the unified diff for the change set at point."
  (interactive)
  (let* ((workspace (emacs-agent-workspace-current))
         (id (emacs-agent-ui--row-id))
         (buffer (get-buffer-create
                  (format "*Emacs Agent Diff %s*" id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (emacs-agent-changeset-diff workspace id))
        (diff-mode)
        (goto-char (point-min))
        (read-only-mode 1)))
    (pop-to-buffer buffer)))

(defun emacs-agent-checkpoint-at-point ()
  "Checkpoint documents in the change set at point."
  (interactive)
  (let* ((workspace (emacs-agent-workspace-current))
         (changeset
          (emacs-agent-changeset-get workspace
                                     (emacs-agent-ui--row-id))))
    (dolist (path
             (emacs-agent-changeset-touched-documents changeset))
      (let* ((absolute (expand-file-name
                        path (emacs-agent-workspace-root workspace)))
             (buffer (find-buffer-visiting absolute)))
        (when (and (buffer-live-p buffer)
                   (buffer-modified-p buffer))
          (with-current-buffer buffer (save-buffer)))))
    (setf (emacs-agent-changeset-checkpoint-state changeset) 'checkpointed)
    (unless (eq (emacs-agent-changeset-status changeset) 'reviewed)
      (setf (emacs-agent-changeset-status changeset) 'checkpointed))
    (message "Checkpointed %s"
             (emacs-agent-changeset-changeset-id changeset))
    (emacs-agent-ui-refresh)))

(defun emacs-agent-rollback-at-point ()
  "Rollback the compatible change set at point."
  (interactive)
  (let ((id (emacs-agent-ui--row-id)))
    (emacs-agent-changeset-rollback
     (emacs-agent-workspace-current) id)
    (message "Rolled back %s" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-mark-reviewed-at-point ()
  "Mark the change set at point reviewed."
  (interactive)
  (let ((id (emacs-agent-ui--row-id)))
    (emacs-agent-changeset-mark-reviewed
     (emacs-agent-workspace-current) id)
    (message "Marked %s reviewed" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-revoke-writer ()
  "Revoke the current workspace writer lease."
  (interactive)
  (if (fboundp 'emacs-agent-editor-revoke-writer)
      (emacs-agent-editor-revoke-writer)
    (let ((workspace (emacs-agent-workspace-current)))
      (setf (emacs-agent-workspace-writer-lease workspace) nil)
      (emacs-agent-workspace-pause workspace)))
  (emacs-agent-workspace-record-activity
   (emacs-agent-workspace-current)
   (list :tool "writer_revoke" :status "completed"))
  (message "Emacs Agent writer revoked and mutations paused")
  (when (derived-mode-p 'tabulated-list-mode)
    (emacs-agent-ui-refresh)))

(defalias 'emacs-agent-activity-show #'emacs-agent-show-activity)
(defalias 'emacs-agent-changes-show #'emacs-agent-show-changes)

(provide 'emacs-agent-ui)
;;; emacs-agent-ui.el ends here
