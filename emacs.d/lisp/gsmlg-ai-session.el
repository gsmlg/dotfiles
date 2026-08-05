;;; gsmlg-ai-session.el --- AI workbench request sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; Ask/review/edit session state machine and gptel request orchestration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gsmlg-ai)
(require 'gsmlg-ai-context)

(defvar gptel-use-tools)
(defvar gptel-tools)
(declare-function gptel-tool-function "gptel-request" (tool))
(declare-function gptel-tool-args "gptel-request" (tool))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function gsmlg-ai-tools-register "gsmlg-ai-tools" (session))
(declare-function gsmlg-ai-tools-unregister "gsmlg-ai-tools" (session))
(declare-function gsmlg-ai-tools-make-gptel-tools "gsmlg-ai-tools" (session))
(declare-function gsmlg-ai-review-show "gsmlg-ai-review" ())

(cl-defstruct (gsmlg-ai-session
               (:constructor gsmlg-ai-session--create)
               (:copier nil))
  id kind state user-prompt system-directive context-id files
  creation-root backend-summary request-buffer tool-call-count
  tool-token revision-round model-summary warnings errors
  created-at updated-at)

(defvar gsmlg-ai-session--active nil
  "The active workbench session, if any.")

(defvar gsmlg-ai-session--oneshot nil
  "The active ask/review session, if any.")

(defconst gsmlg-ai-session-result-buffer-name "*GSMLG AI Result*"
  "Buffer name for ask/review results.")

(defun gsmlg-ai-session-active ()
  "Return the active edit session."
  gsmlg-ai-session--active)

(defun gsmlg-ai-session--touch (session)
  "Update SESSION timestamps."
  (setf (gsmlg-ai-session-updated-at session) (gsmlg-ai-context--now)))

(defun gsmlg-ai-session--make (kind prompt directive entries &optional root)
  "Create a session of KIND for PROMPT with DIRECTIVE and ENTRIES.
Optional ROOT is the creation root for staged new files."
  (let* ((snapshots (gsmlg-ai-context-snapshot-entries entries))
         (session
          (gsmlg-ai-session--create
           :id (gsmlg-ai-context--new-id "ses")
           :kind kind
           :state 'preparing
           :user-prompt prompt
           :system-directive directive
           :context-id (gsmlg-ai-context-id (gsmlg-ai-context-ensure))
           :files snapshots
           :creation-root root
           :backend-summary (gsmlg-ai--backend-summary)
           :request-buffer nil
           :tool-call-count 0
           :tool-token nil
           :revision-round 0
           :model-summary nil
           :warnings nil
           :errors nil
           :created-at (gsmlg-ai-context--now)
           :updated-at (gsmlg-ai-context--now))))
    session))

(defun gsmlg-ai-session--result-buffer (session)
  "Return or create the result buffer for SESSION."
  (let ((buffer (get-buffer-create gsmlg-ai-session-result-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (format "Request: %s\nBackend: %s\nContext entries: %d\nState: %s\n\nPrompt:\n%s\n\nResponse:\n"
                 (gsmlg-ai-session-kind session)
                 (gsmlg-ai-session-backend-summary session)
                 (length (gsmlg-ai-session-files session))
                 (gsmlg-ai-session-state session)
                 (gsmlg-ai-session-user-prompt session))))
      (setq buffer-read-only t)
      (goto-char (point-max)))
    (setf (gsmlg-ai-session-request-buffer session) buffer)
    buffer))

(defun gsmlg-ai-session--append-result (session text)
  "Append TEXT to SESSION's result buffer."
  (when-let* ((buffer (gsmlg-ai-session-request-buffer session)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))))

(defun gsmlg-ai-session--set-state (session state)
  "Set SESSION state to STATE and refresh result header when present."
  (setf (gsmlg-ai-session-state session) state)
  (gsmlg-ai-session--touch session)
  (when-let* ((buffer (gsmlg-ai-session-request-buffer session)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^State: .*$" nil t)
            (replace-match (format "State: %s" state))))))))

(defun gsmlg-ai-session--oneshot-callback (session)
  "Return a gptel callback closed over SESSION."
  (lambda (response info)
    (cond
     ((eq response 'abort)
      (gsmlg-ai-session--set-state session 'cancelled)
      (gsmlg-ai-session--append-result session "\n[cancelled]\n"))
     ((stringp response)
      (gsmlg-ai-session--set-state session 'complete)
      (gsmlg-ai-session--append-result session response))
     (t
      (gsmlg-ai-session--set-state session 'failed)
      (push (or (plist-get info :status) "request failed")
            (gsmlg-ai-session-errors session))
      (gsmlg-ai-session--append-result
       session
       (format "\n[failed] %s\n" (or (plist-get info :status) "unknown")))))))

(defun gsmlg-ai-session--dispatch-oneshot (session prompt)
  "Send PROMPT for SESSION using gptel or the test stub."
  (let ((buffer (gsmlg-ai-session--result-buffer session)))
    (gsmlg-ai-session--set-state session 'waiting)
    (pop-to-buffer buffer)
    (let ((gptel-use-tools nil)
          (gptel-tools nil))
      (ignore gptel-use-tools gptel-tools)
      (gsmlg-ai--request
       prompt
       :callback (gsmlg-ai-session--oneshot-callback session)
       :buffer buffer
       :system (gsmlg-ai-session-system-directive session)))))

(defun gsmlg-ai-session-ask (prompt)
  "Ask PROMPT about the active context or fallback selection."
  (interactive "sAsk AI: ")
  (gsmlg-ai--ensure-gptel)
  (let* ((entries (gsmlg-ai-context-default-entries))
         (paths (gsmlg-ai-context-paths entries)))
    (unless (gsmlg-ai--confirm-send paths)
      (user-error "Ask cancelled"))
    (let* ((snapshots (gsmlg-ai-context-snapshot-entries entries))
           (body (gsmlg-ai-context-serialize snapshots))
           (session
            (gsmlg-ai-session--make
             'ask prompt gsmlg-ai-ask-system-directive entries)))
      (setq gsmlg-ai-session--oneshot session)
      (gsmlg-ai-session--dispatch-oneshot
       session
       (format "%s\n\nUntrusted editor context follows.\n\n%s"
               prompt body)))))

(defun gsmlg-ai-session-review (prompt)
  "Review context with optional PROMPT focus."
  (interactive "sReview focus (optional): ")
  (gsmlg-ai--ensure-gptel)
  (let* ((entries (gsmlg-ai-context-default-entries))
         (paths (gsmlg-ai-context-paths entries)))
    (unless (gsmlg-ai--confirm-send paths)
      (user-error "Review cancelled"))
    (let* ((snapshots (gsmlg-ai-context-snapshot-entries entries))
           (body (gsmlg-ai-context-serialize snapshots))
           (session
            (gsmlg-ai-session--make
             'review
             (if (string-empty-p prompt) "Review the context." prompt)
             gsmlg-ai-review-system-directive
             entries)))
      (setq gsmlg-ai-session--oneshot session)
      (gsmlg-ai-session--dispatch-oneshot
       session
       (format "%s\n\nUntrusted editor context follows.\n\n%s"
               (gsmlg-ai-session-user-prompt session) body)))))

(defun gsmlg-ai-session--creation-root (&optional choose)
  "Return the creation root, prompting when CHOOSE is non-nil."
  (cond
   (choose
    (read-directory-name "Creation root: " nil nil t))
   ((project-current)
    (project-root (project-current)))
   (t default-directory)))

(defun gsmlg-ai-session--tool-arg-values (tool args)
  "Return positional argument values for TOOL from ARGS."
  (cond
   ((and (listp args) args (not (keywordp (car args))))
    args)
   ((listp args)
    (mapcar
     (lambda (spec)
       (let* ((name (plist-get spec :name))
              (key (intern (concat ":" name))))
         (or (plist-get args key)
             (plist-get args (intern name)))))
     (gptel-tool-args tool)))
   (t nil)))

(defun gsmlg-ai-session--edit-callback (session)
  "Return the edit-session gptel callback for SESSION."
  (lambda (response info)
    (cond
     ((eq response 'abort)
      (gsmlg-ai-session--set-state session 'cancelled)
      (require 'gsmlg-ai-tools)
      (gsmlg-ai-tools-unregister session)
      (message "AI edit cancelled"))
     ((and (consp response) (eq (car response) 'tool-call))
      (gsmlg-ai-session--set-state session 'tooling)
      (dolist (call (cdr response))
        (pcase-let ((`(,tool ,args ,cb) call))
          (condition-case err
              (let ((result
                     (apply (gptel-tool-function tool)
                            (gsmlg-ai-session--tool-arg-values tool args))))
                (when (functionp cb)
                  (funcall cb result)))
            (error
             (when (functionp cb)
               (funcall cb (format "Tool error: %s"
                                   (error-message-string err))))
             (push (error-message-string err)
                   (gsmlg-ai-session-errors session)))))))
     ((and (consp response) (eq (car response) 'tool-result))
      (gsmlg-ai-session--set-state session 'waiting))
     ((stringp response)
      (unless (eq (gsmlg-ai-session-state session) 'ready)
        (setf (gsmlg-ai-session-model-summary session) response)
        (gsmlg-ai-session--set-state session 'ready))
      (require 'gsmlg-ai-review)
      (gsmlg-ai-review-show))
     ((eq response t))
     (t
      (gsmlg-ai-session--set-state session 'failed)
      (push (or (plist-get info :status) "edit failed")
            (gsmlg-ai-session-errors session))
      (require 'gsmlg-ai-tools)
      (gsmlg-ai-tools-unregister session)
      (message "AI edit failed: %s"
               (car (gsmlg-ai-session-errors session)))))))

(defun gsmlg-ai-session-edit (prompt &optional choose-root)
  "Start a staged multi-file edit session for PROMPT.
With prefix argument CHOOSE-ROOT, prompt for the creation root."
  (interactive
   (list (read-string "Edit task: ")
         current-prefix-arg))
  (when (and gsmlg-ai-session--active
             (memq (gsmlg-ai-session-state gsmlg-ai-session--active)
                   '(preparing waiting tooling ready)))
    (unless (yes-or-no-p "Discard the existing AI proposal and start over? ")
      (user-error "Edit session already active")))
  (gsmlg-ai--ensure-gptel)
  (unless (fboundp #'gptel-make-tool)
    (user-error "Edit requires a tool-capable gptel"))
  (require 'gsmlg-ai-tools)
  (require 'gsmlg-ai-review)
  (let* ((entries (gsmlg-ai-context-default-entries t))
         (editable
          (cl-remove-if-not #'gsmlg-ai-context-entry-editable-p entries)))
    (unless editable
      (user-error "No editable file-backed context for edit"))
    (unless (gsmlg-ai--confirm-send (gsmlg-ai-context-paths editable))
      (user-error "Edit cancelled"))
    (let* ((root (gsmlg-ai-session--creation-root choose-root))
           (session
            (gsmlg-ai-session--make
             'edit prompt gsmlg-ai-edit-system-directive editable root))
           (tools (progn
                    (gsmlg-ai-tools-register session)
                    (gsmlg-ai-tools-make-gptel-tools session)))
           (manifest
            (mapconcat
             (lambda (file)
               (format "- id=%s path=%s editable=%s bytes=%d"
                       (gsmlg-ai-snapshot-id file)
                       (gsmlg-ai-snapshot-display-path file)
                       (gsmlg-ai-snapshot-editable-p file)
                       (string-bytes
                        (gsmlg-ai-snapshot-original-content file))))
             (gsmlg-ai-session-files session)
             "\n"))
           (buffer (get-buffer-create "*GSMLG AI Edit*")))
      (setq gsmlg-ai-session--active session)
      (with-current-buffer buffer
        (erase-buffer)
        (insert (format "Edit session %s\nCreation root: %s\nBackend: %s\n\n"
                        (gsmlg-ai-session-id session)
                        root
                        (gsmlg-ai-session-backend-summary session))))
      (setf (gsmlg-ai-session-request-buffer session) buffer)
      (gsmlg-ai-session--set-state session 'waiting)
      (let ((gptel-use-tools t)
            (gptel-tools tools))
        (gsmlg-ai--request
         (format "%s\n\nCreation root: %s\nAuthorized files:\n%s\n\nUse tools to inspect and edit. Call finish_proposal when done."
                 prompt root manifest)
         :callback (gsmlg-ai-session--edit-callback session)
         :buffer buffer
         :system gsmlg-ai-edit-system-directive)))))

(defun gsmlg-ai-session-revise (instruction)
  "Revise the active proposal with follow-up INSTRUCTION."
  (interactive "sRevise proposal: ")
  (let ((session gsmlg-ai-session--active))
    (unless (and session (eq (gsmlg-ai-session-state session) 'ready))
      (user-error "No ready proposal to revise"))
    (require 'gsmlg-ai-tools)
    (setf (gsmlg-ai-session-revision-round session)
          (1+ (gsmlg-ai-session-revision-round session))
          (gsmlg-ai-session-tool-call-count session) 0
          (gsmlg-ai-session-user-prompt session) instruction)
    (gsmlg-ai-tools-register session)
    (let* ((tools (gsmlg-ai-tools-make-gptel-tools session))
           (buffer (gsmlg-ai-session-request-buffer session)))
      (gsmlg-ai-session--set-state session 'waiting)
      (let ((gptel-use-tools t)
            (gptel-tools tools))
        (gsmlg-ai--request
         instruction
         :callback (gsmlg-ai-session--edit-callback session)
         :buffer buffer
         :system gsmlg-ai-edit-system-directive)))))

(defun gsmlg-ai-session-cancel ()
  "Cancel the active workbench request and clean staged state."
  (interactive)
  (dolist (session (delq nil (list gsmlg-ai-session--active
                                   gsmlg-ai-session--oneshot)))
    (when-let* ((buffer (gsmlg-ai-session-request-buffer session)))
      (gsmlg-ai--abort buffer))
    (gsmlg-ai-session--set-state session 'cancelled)
    (when (eq (gsmlg-ai-session-kind session) 'edit)
      (require 'gsmlg-ai-tools)
      (gsmlg-ai-tools-unregister session)
      (unless (eq (gsmlg-ai-session-state session) 'ready)
        (setq gsmlg-ai-session--active nil))))
  (message "AI request cancelled"))

(defun gsmlg-ai-session-discard ()
  "Discard the active edit proposal after confirmation."
  (interactive)
  (unless gsmlg-ai-session--active
    (user-error "No active proposal"))
  (unless (yes-or-no-p "Discard the AI proposal? ")
    (user-error "Discard cancelled"))
  (require 'gsmlg-ai-tools)
  (gsmlg-ai-tools-unregister gsmlg-ai-session--active)
  (setq gsmlg-ai-session--active nil)
  (message "AI proposal discarded"))

(provide 'gsmlg-ai-session)
;;; gsmlg-ai-session.el ends here
