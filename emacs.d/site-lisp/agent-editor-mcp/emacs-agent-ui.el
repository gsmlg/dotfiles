;;; emacs-agent-ui.el --- Keyboard review UI for Emacs Agent Editor -*- lexical-binding: t; -*-

;;; Commentary:

;; Tabulated activity and change-set buffers.  Agent operations never invoke
;; these commands; they are explicit human controls.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'pulse)
(require 'tabulated-list)
(require 'emacs-agent-document)
(require 'emacs-agent-workspace)
(require 'emacs-agent-changeset)

(defconst emacs-agent-activity-buffer-name "*Emacs Agent Activity*")
(defconst emacs-agent-changes-buffer-name "*Emacs Agent Changes*")
(defconst emacs-agent-approvals-buffer-name "*Emacs Agent Approvals*")

(defface emacs-agent-change-highlight
  '((t :inherit diff-added :extend t))
  "Face used for current Agent changes."
  :group 'emacs-agent-editor)

(defvar emacs-agent-ui-change-overlays nil
  "Live overlays created by change-set highlighting.")

(defvar-local emacs-agent-ui-changeset-id nil
  "Change-set displayed by the current diff buffer.")

(defvar-local emacs-agent-ui-workspace nil
  "Workspace displayed by the current Agent UI buffer.")

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
    (define-key map (kbd "h") #'emacs-agent-highlight-changeset-at-point)
    (define-key map (kbd "H") #'emacs-agent-ui-clear-highlights)
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

(defvar emacs-agent-approvals-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'emacs-agent-ui-refresh)
    (define-key map (kbd "RET") #'emacs-agent-view-approval-at-point)
    (define-key map (kbd "a") #'emacs-agent-approve-at-point)
    (define-key map (kbd "x") #'emacs-agent-reject-at-point)
    (define-key map (kbd "c") #'emacs-agent-cancel-approval-at-point)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode emacs-agent-approvals-mode tabulated-list-mode
  "Agent-Approvals"
  "Display Emacs Agent approval requests."
  (setq tabulated-list-format
        [("Created" 20 t)
         ("Status" 13 t)
         ("Operation" 28 t)
         ("Impact" 40 t)
         ("TTL" 8 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Created" t))
  (setq header-line-format
        "Approve: a  Reject: x  Cancel: c  Details: RET. Partial accept is not supported.")
  (add-hook 'tabulated-list-revert-hook
            #'emacs-agent-ui--approval-entries nil t)
  (tabulated-list-init-header))

(defvar emacs-agent-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "h") #'emacs-agent-highlight-changeset-at-point)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode emacs-agent-diff-mode diff-mode "Agent-Diff"
  "Display a read-only Agent change-set diff."
  (setq-local revert-buffer-function #'emacs-agent-ui--revert-diff-buffer))

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

(defun emacs-agent-ui--approval-impact (approval)
  "Return a compact impact description for public APPROVAL."
  (let ((path (plist-get approval :path))
        (new-path (plist-get approval :new_path))
        (changeset-id (plist-get approval :changeset_id))
        (document-count (plist-get approval :document_count)))
    (cond
     ((and path new-path) (format "%s -> %s" path new-path))
     (path path)
     (changeset-id (format "changeset %s" changeset-id))
     (document-count (format "%d documents" document-count))
     (t (or (plist-get approval :risk) "")))))

(defun emacs-agent-ui--approval-entries ()
  "Populate the current approval list."
  (setq
   tabulated-list-entries
   (mapcar
    (lambda (approval)
      (list
       (plist-get approval :approval_request_id)
       (vector
        (emacs-agent-ui--time (plist-get approval :created_at))
        (emacs-agent-ui--display (plist-get approval :status))
        (emacs-agent-ui--display (plist-get approval :operation))
        (emacs-agent-ui--approval-impact approval)
        (format "%.0fs" (plist-get approval :ttl_remaining)))))
    (emacs-agent-workspace-approval-list))))

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
(defun emacs-agent-show-approvals ()
  "Open the keyboard-driven Agent Editor approval buffer."
  (interactive)
  (let ((buffer (get-buffer-create emacs-agent-approvals-buffer-name)))
    (with-current-buffer buffer
      (emacs-agent-approvals-mode)
      (setq-local emacs-agent-ui-workspace
                  (emacs-agent-workspace-current))
      (emacs-agent-ui--approval-entries)
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

(defun emacs-agent-cancel-approval-at-point ()
  "Cancel the pending or approved approval request at point."
  (interactive)
  (let ((id (emacs-agent-ui--row-id)))
    (emacs-agent-workspace-approval-cancel
     (emacs-agent-workspace-current) id)
    (message "Cancelled %s" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-view-approval-at-point ()
  "Show the safe approval summary for the request at point."
  (interactive)
  (let* ((workspace (emacs-agent-workspace-current))
         (id (emacs-agent-ui--row-id))
         (approval
          (emacs-agent-workspace-approval-status workspace id))
         (buffer
          (get-buffer-create (format "*Emacs Agent Approval %s*" id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Approval: %s\n" id))
        (insert (format "Status: %s\n"
                        (plist-get approval :status)))
        (insert (format "Operation: %s\n"
                        (plist-get approval :operation)))
        (insert (format "Expires: %s\n"
                        (emacs-agent-ui--time
                         (plist-get approval :expires_at))))
        (insert (format "Risk: %s\n"
                        (plist-get approval :risk)))
        (when-let* ((path (plist-get approval :path)))
          (insert (format "Path: %s\n" path)))
        (when-let* ((new-path (plist-get approval :new_path)))
          (insert (format "New path: %s\n" new-path)))
        (when-let* ((revision
                     (plist-get approval :expected_revision)))
          (insert (format "Expected revision: %s\n" revision)))
        (when-let* ((changeset-id
                     (plist-get approval :changeset_id)))
          (insert (format "Change set: %s\n" changeset-id))
          (condition-case nil
              (let ((diff
                     (emacs-agent-changeset-diff
                      workspace changeset-id)))
                (unless (string-empty-p diff)
                  (insert "\nDiff:\n" diff)))
            (emacs-agent-changeset-error
             (insert "\nDiff: unavailable\n"))))
        (insert "\nPartial accept: unsupported; reject and request a narrower operation.\n")
        (special-mode)))
    (pop-to-buffer buffer)))

(defun emacs-agent-ui--range-position (range key)
  "Return position KEY from public RANGE."
  (emacs-agent-document--field range key))

;;;###autoload
(defun emacs-agent-ui-open-file-at-range (workspace path &optional range)
  "Open PATH in WORKSPACE and move point to the start of public RANGE."
  (interactive
   (let* ((workspace (emacs-agent-workspace-current))
          (root (emacs-agent-workspace-root workspace))
          (absolute (read-file-name "Open workspace file: " root nil t)))
     (list workspace (file-relative-name absolute root) nil)))
  (let* ((document (emacs-agent-document-open workspace path))
         (buffer (emacs-agent-document-buffer document))
         (start-object (and range
                            (emacs-agent-ui--range-position range 'start)))
         (end-object (and range
                          (emacs-agent-ui--range-position range 'end)))
         (start (and start-object
                     (emacs-agent-document-position document start-object)))
         (end (and end-object
                   (emacs-agent-document-position document end-object))))
    (pop-to-buffer buffer)
    (save-restriction
      (widen)
      (goto-char (or start (point-min)))
      (when (and end (> end (or start (point-min))))
        (pulse-momentary-highlight-region (or start (point-min)) end)))
    buffer))

(defalias 'emacs-agent-open-file-at-range
  #'emacs-agent-ui-open-file-at-range)

(defun emacs-agent-ui--diff-hunks (diff)
  "Return new-file hunk locations parsed from unified DIFF."
  (with-temp-buffer
    (insert diff)
    (goto-char (point-min))
    (let (path hunks)
      (while (not (eobp))
        (cond
         ((looking-at "^+++ \\(.+\\)$")
          (setq path
                (string-remove-prefix
                 "b/"
                 (car
                  (split-string
                   (match-string-no-properties 1) "\t" t)))))
         ((and path
               (not (equal path "/dev/null"))
               (looking-at
                "^@@ -[0-9]+\\(?:,[0-9]+\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"))
          (push
           (list :path path
                 :line (string-to-number (match-string 1))
                 :count
                 (if (match-string 2)
                     (string-to-number (match-string 2))
                   1))
           hunks)))
        (forward-line 1))
      (nreverse hunks))))

(defun emacs-agent-ui-clear-highlights (&optional changeset-id)
  "Delete Agent change overlays, optionally only for CHANGESET-ID."
  (interactive)
  (setq
   emacs-agent-ui-change-overlays
   (cl-delete-if
    (lambda (overlay)
      (cond
       ((not (overlayp overlay)) t)
       ((or (null changeset-id)
            (equal changeset-id
                   (overlay-get overlay 'emacs-agent-changeset-id)))
        (delete-overlay overlay)
        t)
       (t nil)))
    emacs-agent-ui-change-overlays)))

(defun emacs-agent-ui--highlight-hunk (buffer changeset-id line count)
  "Highlight LINE and COUNT in BUFFER for CHANGESET-ID."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (forward-line (max 0 (1- line)))
        (let ((start (point))
              end)
          (forward-line count)
          (setq end (point))
          (when (= start end)
            (setq end (min (point-max) (1+ start))))
          (let ((overlay (make-overlay start end buffer nil t)))
            (overlay-put overlay 'face 'emacs-agent-change-highlight)
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'emacs-agent-changeset-id changeset-id)
            (overlay-put
             overlay 'help-echo
             (format "Emacs Agent change set %s" changeset-id))
            (push overlay emacs-agent-ui-change-overlays)
            overlay))))))

;;;###autoload
(defun emacs-agent-highlight-changeset (workspace changeset-id)
  "Highlight current ranges belonging to CHANGESET-ID in WORKSPACE.

Only buffers whose revision still matches the change set are highlighted."
  (let* ((changeset
          (emacs-agent-changeset-get workspace changeset-id))
         (hunks
          (emacs-agent-ui--diff-hunks
           (emacs-agent-changeset-diff workspace changeset-id)))
         (expected-revisions
          (emacs-agent-changeset-final-revisions changeset))
         (paths
          (delete-dups
           (append
            (mapcar (lambda (hunk) (plist-get hunk :path)) hunks)
            (copy-sequence
             (emacs-agent-changeset-touched-documents changeset)))))
         stale-paths
         highlighted)
    (emacs-agent-ui-clear-highlights changeset-id)
    (dolist (path paths)
      (let ((expected (cdr (assoc path expected-revisions))))
        (if
            (or
             (null expected)
             (condition-case nil
                 (not
                  (equal
                   expected
                   (emacs-agent-document-revision-for-path workspace path)))
               (error t)))
            (push path stale-paths)
          (let* ((document (emacs-agent-document-open workspace path))
                 (buffer (emacs-agent-document-buffer document))
                 (path-hunks
                  (cl-remove-if-not
                   (lambda (hunk)
                     (equal path (plist-get hunk :path)))
                   hunks)))
            (if path-hunks
                (dolist (hunk path-hunks)
                  (emacs-agent-ui--highlight-hunk
                   buffer changeset-id
                   (plist-get hunk :line)
                   (plist-get hunk :count))
                  (setq highlighted (1+ (or highlighted 0))))
              (emacs-agent-ui--highlight-hunk
               buffer changeset-id 1
               (with-current-buffer buffer
                 (line-number-at-pos (point-max) t)))
              (setq highlighted (1+ (or highlighted 0))))))))
    (list :changeset_id changeset-id
          :highlighted (or highlighted 0)
          :stale_paths (nreverse stale-paths))))

(defun emacs-agent-highlight-changeset-at-point ()
  "Highlight the change set represented by the current buffer or row."
  (interactive)
  (let* ((id (or emacs-agent-ui-changeset-id
                 (emacs-agent-ui--row-id)))
         (result
          (emacs-agent-highlight-changeset
           (or emacs-agent-ui-workspace
               (emacs-agent-workspace-current))
           id)))
    (message "Highlighted %d ranges%s"
             (plist-get result :highlighted)
             (if (plist-get result :stale_paths)
                 (format "; stale: %s"
                         (emacs-agent-ui--display
                          (plist-get result :stale_paths)))
               ""))))

(defun emacs-agent-open-changed-file ()
  "Visit the first current range touched by the change set at point."
  (interactive)
  (let* ((workspace (emacs-agent-workspace-current))
         (id (emacs-agent-ui--row-id))
         (changeset (emacs-agent-changeset-get workspace id))
         (hunk
          (car
           (emacs-agent-ui--diff-hunks
            (emacs-agent-changeset-diff workspace id))))
         (path
          (or (plist-get hunk :path)
              (car
               (emacs-agent-changeset-touched-documents changeset)))))
    (unless path (user-error "Change set has no files"))
    (emacs-agent-highlight-changeset workspace id)
    (emacs-agent-ui-open-file-at-range
     workspace path
     (and hunk
          (list :start
                (list :line (plist-get hunk :line) :column 0)
                :end
                (list :line
                      (+ (plist-get hunk :line)
                         (max 0 (1- (plist-get hunk :count))))
                      :column 0))))))

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
        (emacs-agent-diff-mode)
        (setq-local emacs-agent-ui-changeset-id id
                    emacs-agent-ui-workspace workspace)
        (goto-char (point-min))
        (read-only-mode 1)))
    (pop-to-buffer buffer)))

(defun emacs-agent-ui--revert-diff-buffer (&rest _ignored)
  "Refresh the current Agent diff buffer from its change set."
  (unless (and emacs-agent-ui-workspace emacs-agent-ui-changeset-id)
    (user-error "This buffer is not bound to a change set"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     (emacs-agent-changeset-diff
      emacs-agent-ui-workspace emacs-agent-ui-changeset-id))
    (goto-char (point-min))))

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
    (emacs-agent-workspace-record-activity
     workspace
     (list :tool "changeset_checkpoint" :status "completed"
           :changeset_id
           (emacs-agent-changeset-changeset-id changeset)
           :paths
           (emacs-agent-changeset-touched-documents changeset)))
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
    (emacs-agent-workspace-record-activity
     (emacs-agent-workspace-current)
     (list :tool "changeset_mark_reviewed" :status "completed"
           :changeset_id id))
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
(defalias 'emacs-agent-approvals-show #'emacs-agent-show-approvals)

(provide 'emacs-agent-ui)
;;; emacs-agent-ui.el ends here
