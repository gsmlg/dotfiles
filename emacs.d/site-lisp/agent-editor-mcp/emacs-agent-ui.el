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
(require 'emacs-agent-changeset)
(require 'emacs-agent-journal)
(require 'emacs-agent-project)
(require 'emacs-agent-runtime)

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

(defvar-local emacs-agent-ui-runtime nil
  "Runtime displayed by the current Agent UI buffer.")

(defvar emacs-agent-activity-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'emacs-agent-ui-refresh)
    (define-key map (kbd "P") #'emacs-agent-pause-mutations)
    (define-key map (kbd "R") #'emacs-agent-resume-mutations)
    (define-key map (kbd "a") #'emacs-agent-approve-at-point)
    (define-key map (kbd "p") #'emacs-agent-partially-approve-at-point)
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
    (define-key map (kbd "p") #'emacs-agent-partially-approve-at-point)
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
        "Approve: a  Partial per-document: p  Reject: x  Cancel: c  Details: RET")
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

(defun emacs-agent-ui--display-paths (value)
  "Convert canonical path VALUE to abbreviated display text."
  (cond
   ((null value) "")
   ((stringp value) (abbreviate-file-name value))
   ((listp value)
    (mapconcat #'emacs-agent-ui--display-paths value ", "))
   (t (format "%s" value))))

(defun emacs-agent-ui--runtime ()
  "Return the runtime bound to the current UI buffer or the active runtime."
  (or emacs-agent-ui-runtime
      (emacs-agent-runtime-current)))

(defun emacs-agent-ui--activity-entries ()
  "Populate the current activity list."
  (setq
   tabulated-list-entries
   (cl-loop
    for entry in (emacs-agent-runtime-recent-activity
                  (emacs-agent-ui--runtime))
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
      (emacs-agent-ui--display-paths
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
        (emacs-agent-ui--display-paths
         (emacs-agent-changeset-touched-documents changeset))
        (format "+%d/-%d"
                (emacs-agent-changeset-insertions changeset)
                (emacs-agent-changeset-deletions changeset))
        (emacs-agent-ui--display
         (emacs-agent-changeset-checkpoint-state changeset)))))
    (emacs-agent-changeset-list (emacs-agent-ui--runtime)))))

(defun emacs-agent-ui--approval-impact (approval)
  "Return a compact impact description for public APPROVAL."
  (let ((path (plist-get approval :path))
        (new-path (plist-get approval :new_path))
        (changeset-id (plist-get approval :changeset_id))
        (document-count (plist-get approval :document_count)))
    (cond
     ((and path new-path)
      (format "%s -> %s"
              (emacs-agent-ui--display-paths path)
              (emacs-agent-ui--display-paths new-path)))
     (path (emacs-agent-ui--display-paths path))
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
    (emacs-agent-runtime-approval-list (emacs-agent-ui--runtime)))))

(defun emacs-agent-ui--journal (runtime event)
  "Append human-control EVENT to RUNTIME's journal."
  (emacs-agent-journal-write runtime event))

(defun emacs-agent-ui--record-event (runtime event)
  "Record and journal human-control EVENT for RUNTIME."
  (emacs-agent-runtime-record-activity runtime event)
  (emacs-agent-ui--journal runtime event)
  event)

(defun emacs-agent-ui-refresh ()
  "Refresh the current Agent Editor list."
  (interactive)
  (tabulated-list-revert))

;;;###autoload
(defun emacs-agent-show-activity ()
  "Open the keyboard-driven Agent Editor activity buffer."
  (interactive)
  (let ((runtime (emacs-agent-runtime-current))
        (buffer (get-buffer-create emacs-agent-activity-buffer-name)))
    (with-current-buffer buffer
      (emacs-agent-activity-mode)
      (setq-local emacs-agent-ui-runtime runtime)
      (emacs-agent-ui--activity-entries)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

;;;###autoload
(defun emacs-agent-show-changes ()
  "Open the keyboard-driven Agent Editor change-set buffer."
  (interactive)
  (let ((runtime (emacs-agent-runtime-current))
        (buffer (get-buffer-create emacs-agent-changes-buffer-name)))
    (with-current-buffer buffer
      (emacs-agent-changes-mode)
      (setq-local emacs-agent-ui-runtime runtime)
      (emacs-agent-ui--changes-entries)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

;;;###autoload
(defun emacs-agent-show-approvals ()
  "Open the keyboard-driven Agent Editor approval buffer."
  (interactive)
  (let ((runtime (emacs-agent-runtime-current))
        (buffer (get-buffer-create emacs-agent-approvals-buffer-name)))
    (with-current-buffer buffer
      (emacs-agent-approvals-mode)
      (setq-local emacs-agent-ui-runtime runtime)
      (emacs-agent-ui--approval-entries)
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

;;;###autoload
(defun emacs-agent-pause-mutations ()
  "Pause agent mutations in the current runtime."
  (interactive)
  (let ((runtime (emacs-agent-ui--runtime)))
    (emacs-agent-runtime-pause runtime)
    (emacs-agent-ui--journal
     runtime
     '(:tool "editor_pause" :status "completed"))
    (message "Emacs Agent mutations paused")
    (when (derived-mode-p 'tabulated-list-mode)
      (emacs-agent-ui-refresh))))

;;;###autoload
(defun emacs-agent-resume-mutations ()
  "Resume agent mutations in the current runtime."
  (interactive)
  (let ((runtime (emacs-agent-ui--runtime)))
    (emacs-agent-runtime-resume runtime)
    (emacs-agent-ui--journal
     runtime
     '(:tool "editor_resume" :status "completed"))
    (message "Emacs Agent mutations resumed")
    (when (derived-mode-p 'tabulated-list-mode)
      (emacs-agent-ui-refresh))))

(defun emacs-agent-ui--row-id ()
  "Return the ID at point, or signal a user error."
  (or (tabulated-list-get-id)
      (user-error "No Emacs Agent item at point")))

(defun emacs-agent-approve-at-point ()
  "Approve the pending approval request at point."
  (interactive)
  (let ((runtime (emacs-agent-ui--runtime))
        (id (emacs-agent-ui--row-id)))
    (emacs-agent-runtime-approve runtime id)
    (emacs-agent-ui--journal
     runtime
     (list :tool "approval_approve" :status "completed"
           :approval_request_id id))
    (message "Approved %s" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-reject-at-point ()
  "Reject the pending approval request at point."
  (interactive)
  (let ((runtime (emacs-agent-ui--runtime))
        (id (emacs-agent-ui--row-id)))
    (emacs-agent-runtime-reject runtime id)
    (emacs-agent-ui--journal
     runtime
     (list :tool "approval_reject" :status "completed"
           :approval_request_id id))
    (message "Rejected %s" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-partially-approve-at-point ()
  "Approve selected documents from the request at point."
  (interactive)
  (let* ((runtime (emacs-agent-ui--runtime))
         (id (emacs-agent-ui--row-id))
         (approval
          (emacs-agent-runtime-approval-status runtime id))
         (targets (plist-get approval :document_paths)))
    (unless (plist-get approval :partial_accept_supported)
      (user-error
       "Partial acceptance is available only for multi-document checkpoints"))
    (let ((selected
           (completing-read-multiple
            "Approve documents: " targets nil t)))
      (unless selected
        (user-error "Select at least one document"))
      (let* ((result
              (emacs-agent-runtime-approval-partial
               runtime id selected))
             (child-id
              (plist-get result :derived_approval_request_id)))
        (message
         "Partially approved %s; retry selected documents with %s"
         id child-id)
        (emacs-agent-ui--journal
         runtime
         (list :tool "approval_partial" :status "completed"
               :approval_request_id id
               :derived_approval_request_id child-id
               :paths selected))
        (emacs-agent-ui-refresh)))))

(defun emacs-agent-cancel-approval-at-point ()
  "Cancel the pending or approved approval request at point."
  (interactive)
  (let ((runtime (emacs-agent-ui--runtime))
        (id (emacs-agent-ui--row-id)))
    (emacs-agent-runtime-approval-cancel runtime id)
    (emacs-agent-ui--journal
     runtime
     (list :tool "approval_cancel" :status "completed"
           :approval_request_id id))
    (message "Cancelled %s" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-view-approval-at-point ()
  "Show the safe approval summary for the request at point."
  (interactive)
  (let* ((runtime (emacs-agent-ui--runtime))
         (id (emacs-agent-ui--row-id))
         (approval
          (emacs-agent-runtime-approval-status runtime id))
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
          (insert (format "Path: %s\n"
                          (emacs-agent-ui--display-paths path))))
        (when-let* ((new-path (plist-get approval :new_path)))
          (insert (format "New path: %s\n"
                          (emacs-agent-ui--display-paths new-path))))
        (when-let* ((revision
                     (plist-get approval :expected_revision)))
          (insert (format "Expected revision: %s\n" revision)))
        (when-let* ((changeset-id
                     (plist-get approval :changeset_id)))
          (insert (format "Change set: %s\n" changeset-id))
          (condition-case nil
              (let ((diff
                     (emacs-agent-changeset-diff
                      runtime changeset-id)))
                (unless (string-empty-p diff)
                  (insert "\nDiff:\n" diff)))
            (emacs-agent-changeset-error
             (insert "\nDiff: unavailable\n"))))
        (if (plist-get approval :partial_accept_supported)
            (insert
             "\nPartial accept: press p in the approvals list to select documents.\n")
          (insert
           "\nPartial accept: unavailable for this operation; reject and request a narrower operation.\n"))
        (special-mode)))
    (pop-to-buffer buffer)))

(defun emacs-agent-ui--range-position (range key)
  "Return position KEY from public RANGE."
  (emacs-agent-document--field range key))

;;;###autoload
(defun emacs-agent-ui-open-file-at-range (runtime path &optional range)
  "Open canonical PATH in RUNTIME and move point to public RANGE."
  (interactive
   (list
    (emacs-agent-runtime-current)
    (read-file-name "Open file: " nil nil t)
    nil))
  (let* ((target
          (emacs-agent-project-resolve-target runtime path))
         (document (emacs-agent-document-open runtime target))
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
(defun emacs-agent-highlight-changeset (runtime changeset-id)
  "Highlight current ranges belonging to CHANGESET-ID in RUNTIME.

Only buffers whose revision still matches the change set are highlighted."
  (let* ((changeset
          (emacs-agent-changeset-get runtime changeset-id))
         (hunks
          (emacs-agent-ui--diff-hunks
           (emacs-agent-changeset-diff runtime changeset-id)))
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
      (let* ((target
              (emacs-agent-project-resolve-target runtime path))
             (expected (cdr (assoc path expected-revisions))))
        (if
            (or
             (null expected)
             (condition-case nil
                 (not
                  (equal
                   expected
                   (emacs-agent-document-revision-for-target
                    runtime target)))
               (error t)))
            (push path stale-paths)
          (let* ((document (emacs-agent-document-open runtime target))
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
           (emacs-agent-ui--runtime)
           id)))
    (message "Highlighted %d ranges%s"
             (plist-get result :highlighted)
             (if (plist-get result :stale_paths)
                 (format "; stale: %s"
                         (emacs-agent-ui--display-paths
                          (plist-get result :stale_paths)))
               ""))))

(defun emacs-agent-open-changed-file ()
  "Visit the first current range touched by the change set at point."
  (interactive)
  (let* ((runtime (emacs-agent-ui--runtime))
         (id (emacs-agent-ui--row-id))
         (changeset (emacs-agent-changeset-get runtime id))
         (hunk
          (car
           (emacs-agent-ui--diff-hunks
            (emacs-agent-changeset-diff runtime id))))
         (path
          (or (plist-get hunk :path)
              (car
               (emacs-agent-changeset-touched-documents changeset)))))
    (unless path (user-error "Change set has no files"))
    (emacs-agent-highlight-changeset runtime id)
    (emacs-agent-ui-open-file-at-range
     runtime path
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
  (let* ((runtime (emacs-agent-ui--runtime))
         (id (emacs-agent-ui--row-id))
         (buffer (get-buffer-create
                  (format "*Emacs Agent Diff %s*" id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (emacs-agent-changeset-diff runtime id))
        (emacs-agent-diff-mode)
        (setq-local emacs-agent-ui-changeset-id id
                    emacs-agent-ui-runtime runtime)
        (goto-char (point-min))
        (read-only-mode 1)))
    (pop-to-buffer buffer)))

(defun emacs-agent-ui--revert-diff-buffer (&rest _ignored)
  "Refresh the current Agent diff buffer from its change set."
  (unless (and emacs-agent-ui-runtime emacs-agent-ui-changeset-id)
    (user-error "This buffer is not bound to a change set"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     (emacs-agent-changeset-diff
      emacs-agent-ui-runtime emacs-agent-ui-changeset-id))
    (goto-char (point-min))))

(defun emacs-agent-checkpoint-at-point ()
  "Checkpoint documents in the change set at point."
  (interactive)
  (let* ((runtime (emacs-agent-ui--runtime))
         (changeset
          (emacs-agent-changeset-get runtime
                                     (emacs-agent-ui--row-id)))
         checkpointed-paths checkpointing-document)
    (condition-case error-data
        (dolist (path
                 (emacs-agent-changeset-touched-documents changeset))
          (let* ((target
                  (emacs-agent-project-resolve-target runtime path))
                 (canonical
                  (emacs-agent-resolved-target-canonical-path target))
                 (document
                  (gethash
                   canonical
                   (emacs-agent-runtime-document-registry runtime)))
                 (buffer
                  (or
                   (and document
                        (emacs-agent-document-buffer document))
                   (find-buffer-visiting canonical))))
            (when (and (buffer-live-p buffer)
                       (buffer-modified-p buffer))
              (unless document
                (setq document
                      (emacs-agent-document-open runtime target)))
              (setq checkpointing-document document)
              (emacs-agent-document-checkpoint document)
              (push canonical checkpointed-paths)
              (let* ((revision
                      (emacs-agent-document-revision document))
                     (final-entry
                      (assoc
                       canonical
                       (emacs-agent-changeset-final-revisions
                        changeset))))
                (if final-entry
                    (setcdr final-entry revision)
                  (push
                   (cons canonical revision)
                   (emacs-agent-changeset-final-revisions
                    changeset)))))))
      (emacs-agent-error
       (let* ((code (emacs-agent-error-code error-data))
              (details
               (copy-sequence
                (emacs-agent-error-details error-data)))
              (partial
               (or checkpointed-paths
                   (plist-get details :partial_completion)))
              (degraded
               (or
                checkpointed-paths
                (eq code 'save_failed)
                (plist-get details :reconciliation_required))))
         (when degraded
           (setf (emacs-agent-runtime-health-state runtime) 'degraded)
           (when checkpointing-document
             (setf
              (emacs-agent-document-degraded checkpointing-document)
              t)))
         (setq
          details
          (plist-put
           details :partial_completion (and partial t)))
         (setq
          details
          (plist-put
           details :checkpointed_paths
           (nreverse checkpointed-paths)))
         (apply #'emacs-agent-signal code details)))
      (error
       (setf (emacs-agent-runtime-health-state runtime) 'degraded)
       (when checkpointing-document
         (setf
          (emacs-agent-document-degraded checkpointing-document) t))
       (emacs-agent-signal
        'save_failed
        :message (error-message-string error-data)
        :partial_completion (and checkpointed-paths t)
        :checkpointed_paths (nreverse checkpointed-paths)
        :reconciliation_required t
        :filesystem_rollback_guaranteed nil)))
    ;; The checkpoint advances revision guards, but the recorded review diff
    ;; remains the immutable mutation snapshot.
    (setf (emacs-agent-changeset-checkpoint-state changeset) 'checkpointed)
    (unless (eq (emacs-agent-changeset-status changeset) 'reviewed)
      (setf (emacs-agent-changeset-status changeset) 'checkpointed))
    (emacs-agent-ui--record-event
     runtime
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
  (let ((runtime (emacs-agent-ui--runtime))
        (id (emacs-agent-ui--row-id)))
    (emacs-agent-changeset-rollback runtime id)
    (emacs-agent-ui--journal
     runtime
     (list :tool "changeset_rollback" :status "completed"
           :changeset_id id))
    (message "Rolled back %s" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-mark-reviewed-at-point ()
  "Mark the change set at point reviewed."
  (interactive)
  (let ((runtime (emacs-agent-ui--runtime))
        (id (emacs-agent-ui--row-id)))
    (emacs-agent-changeset-mark-reviewed runtime id)
    (emacs-agent-ui--record-event
     runtime
     (list :tool "changeset_mark_reviewed" :status "completed"
           :changeset_id id))
    (message "Marked %s reviewed" id)
    (emacs-agent-ui-refresh)))

(defun emacs-agent-revoke-writer ()
  "Revoke the current runtime writer lease."
  (interactive)
  (let ((runtime (emacs-agent-ui--runtime)))
    (setf (emacs-agent-runtime-writer-lease runtime) nil)
    (emacs-agent-runtime-pause runtime)
    (emacs-agent-ui--record-event
     runtime
     '(:tool "writer_revoke" :status "completed"))
    (message "Emacs Agent writer revoked and mutations paused")
    (when (derived-mode-p 'tabulated-list-mode)
      (emacs-agent-ui-refresh))))

(provide 'emacs-agent-ui)
;;; emacs-agent-ui.el ends here
