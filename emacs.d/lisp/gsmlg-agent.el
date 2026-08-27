;;; gsmlg-agent.el --- Agent Editor MCP lifecycle integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Project-optional Agent Editor MCP lifecycle owned by an explicit state
;; machine.  Server advice and hooks are thin sensors that only call
;; `gsmlg-agent-reconcile'; they do not own transitions.

;;; Code:

(require 'subr-x)
(require 'gsmlg-paths)
(require 'server)

(declare-function emacs-agent-editor-running-p "emacs-agent-editor" ())
(declare-function emacs-agent-editor-start
                  "emacs-agent-editor" (&optional port))
(declare-function emacs-agent-editor-stop "emacs-agent-editor" ())
(defvar emacs-agent-editor-host)
(defvar emacs-agent-editor-state-directory)
(defvar emacs-agent-editor--connection-file)

(defgroup gsmlg-agent nil
  "GSMLG integration for Agent Editor MCP."
  :group 'gsmlg)

(defcustom gsmlg-agent-port 9876
  "Default loopback port for Agent Editor MCP.
`EMACS_AGENT_PORT' takes precedence over this value.

Named Emacs daemons that share this default may contend for the port; a
bind failure is recorded as state `failed' without aborting Emacs."
  :type '(integer 1 65535)
  :group 'gsmlg-agent)

(defcustom gsmlg-agent-autostart t
  "Whether to start Agent Editor MCP with the interactive Emacs server.
Autostart follows the server lifecycle by default and is always disabled in
batch mode.  Set this option to nil to opt out; a truthy
`EMACS_AGENT_AUTOSTART' still enables autostart explicitly."
  :type 'boolean
  :group 'gsmlg-agent)

(defvar gsmlg-agent-package-available-p 'unknown
  "Availability of the bundled Agent Editor MCP package.

Values are `unknown' before the first load attempt, t when loaded, or nil
when the package failed to load.")

(defvar gsmlg-agent-state 'disabled
  "Agent Editor MCP lifecycle state.

One of `disabled', `available', `starting', `running', `stopping', or
`failed'.")

(defvar gsmlg-agent-last-error nil
  "Last Agent Editor MCP failure message, or nil.")

(defvar gsmlg-agent--lifecycle-installed nil
  "Non-nil when Agent lifecycle sensors owned by this module are installed.")

(defvar gsmlg-agent--reconciling nil
  "Non-nil while `gsmlg-agent-reconcile' is running.")

(defun gsmlg-agent--resolved-port ()
  "Return the configured Agent Editor MCP port."
  (if-let* ((value (getenv "EMACS_AGENT_PORT")))
      (let ((port
             (and (string-match-p "\\`[[:digit:]]+\\'" value)
                  (string-to-number value))))
        (unless (and port (<= 1 port 65535))
          (user-error
           "EMACS_AGENT_PORT must be an integer from 1 through 65535"))
        port)
    gsmlg-agent-port))

(defun gsmlg-agent--autostart-enabled-p ()
  "Return non-nil when Agent Editor MCP autostart is explicitly enabled."
  (or gsmlg-agent-autostart
      (when-let* ((value (getenv "EMACS_AGENT_AUTOSTART")))
        (member (downcase (string-trim value))
                '("1" "true" "yes" "on")))))

(defun gsmlg-agent--server-owned-p ()
  "Return non-nil when this Emacs process owns a live server."
  (if (fboundp #'gsmlg-server-owned-p)
      (gsmlg-server-owned-p)
    (and server-mode
         (boundp 'server-process)
         (process-live-p server-process))))

(defun gsmlg-agent--desired-state ()
  "Return the desired Agent lifecycle state for the current session."
  (cond
   (noninteractive 'disabled)
   ((and (fboundp #'gsmlg-server-testing-p)
         (gsmlg-server-testing-p)
         (not (gsmlg-agent--server-owned-p)))
    'disabled)
   ((not (gsmlg-agent--autostart-enabled-p)) 'disabled)
   ((gsmlg-agent--server-owned-p) 'running)
   (t 'disabled)))

(defun gsmlg-agent--configure-package ()
  "Restrict Agent Editor MCP to the IPv4 loopback listener.

Do not override `emacs-agent-editor-state-directory' here: the package owns
its XDG state path, and interactive start must preserve an already configured
directory."
  (setq emacs-agent-editor-host "127.0.0.1"))

(defun gsmlg-agent--align-state-directory ()
  "Place Agent Editor MCP connection metadata under Emacs XDG state."
  (setq emacs-agent-editor-state-directory
        (gsmlg-ensure-directory (gsmlg-state-file "agent-editor/"))))

(defun gsmlg-agent--ensure-package ()
  "Load the bundled Agent Editor MCP package when needed.

Return non-nil when the package is available."
  (when (eq gsmlg-agent-package-available-p 'unknown)
    (setq gsmlg-agent-package-available-p
          (condition-case error-data
              (progn
                (require 'emacs-agent-editor)
                t)
            (error
             (setq gsmlg-agent-last-error (error-message-string error-data))
             (message "Agent Editor MCP integration is unavailable: %s"
                      gsmlg-agent-last-error)
             nil))))
  (when gsmlg-agent-package-available-p
    (gsmlg-agent--configure-package))
  gsmlg-agent-package-available-p)

(defun gsmlg-agent--listener-running-p ()
  "Return non-nil when the Agent Editor MCP listener is active."
  (and (gsmlg-agent--ensure-package)
       (fboundp #'emacs-agent-editor-running-p)
       (emacs-agent-editor-running-p)))

(defun gsmlg-agent--observe-actual-state ()
  "Refresh `gsmlg-agent-state' from the live listener when possible."
  (cond
   ((not (gsmlg-agent--ensure-package))
    (setq gsmlg-agent-state 'failed))
   ((and (memq gsmlg-agent-state '(starting stopping)))
    gsmlg-agent-state)
   ((gsmlg-agent--listener-running-p)
    (setq gsmlg-agent-state 'running
          gsmlg-agent-last-error nil))
   ((eq gsmlg-agent-state 'failed)
    'failed)
   (t
    (setq gsmlg-agent-state
          (if gsmlg-agent-package-available-p 'available 'disabled)))))

(defun gsmlg-agent--remove-legacy-connection-file ()
  "Remove obsolete per-daemon connection metadata files.

Keep the singleton connection.json under the agent-editor state directory.
Delete older per-daemon layouts under emacs-agent-editor/ or agent-editor/
when they are not the active connection file."
  (let* ((canonical emacs-agent-editor--connection-file)
         (legacy-roots
          (delq nil
                (list
                 (and (stringp gsmlg-state-directory)
                      (expand-file-name "../emacs-agent-editor/"
                                        gsmlg-state-directory))
                 (and (stringp gsmlg-state-directory)
                      (expand-file-name "agent-editor/"
                                        gsmlg-state-directory))))))
    (dolist (root legacy-roots)
      (when (file-directory-p root)
        (dolist (entry (directory-files root t "\\`[^.]" t))
          (cond
           ;; Old per-daemon directories: <root>/<daemon>/connection.json
           ((file-directory-p entry)
            (let ((legacy (expand-file-name "connection.json" entry)))
              (when (and (file-regular-p legacy)
                         (or (not (stringp canonical))
                             (not (file-equal-p legacy canonical))))
                (delete-file legacy))))
           ;; Do not delete the singleton <root>/connection.json when it is
           ;; the active canonical target.
           ((and (equal (file-name-nondirectory entry) "connection.json")
                 (file-regular-p entry)
                 (stringp canonical)
                 (not (file-equal-p entry canonical))
                 ;; Only treat sibling-tree top-level files as legacy.
                 (string-suffix-p "/emacs-agent-editor/"
                                 (file-name-as-directory root)))
            (delete-file entry))))))))

(defun gsmlg-agent--transition-start (&optional noerror)
  "Start the Agent Editor MCP listener and update lifecycle state.

When NOERROR is non-nil, capture failures into `failed' without signaling."
  (unless (gsmlg-agent--ensure-package)
    (setq gsmlg-agent-state 'failed)
    (user-error "Agent Editor MCP is unavailable; inspect startup messages"))
  (setq gsmlg-agent-state 'starting)
  (condition-case error-data
      (prog1 (emacs-agent-editor-start (gsmlg-agent--resolved-port))
        (gsmlg-agent--remove-legacy-connection-file)
        (setq gsmlg-agent-state 'running
              gsmlg-agent-last-error nil))
    (error
     (setq gsmlg-agent-state 'failed
           gsmlg-agent-last-error (error-message-string error-data))
     (if noerror
         (progn
           (message "Agent Editor MCP start failed: %s" gsmlg-agent-last-error)
           nil)
       (signal (car error-data) (cdr error-data))))))

(defun gsmlg-agent--transition-stop (&optional noerror)
  "Stop the Agent Editor MCP listener and update lifecycle state.

When NOERROR is non-nil, capture failures into `failed' without signaling."
  (unless (gsmlg-agent--ensure-package)
    (setq gsmlg-agent-state 'failed)
    (unless noerror
      (user-error "Agent Editor MCP is unavailable; inspect startup messages"))
    nil)
  (setq gsmlg-agent-state 'stopping)
  (condition-case error-data
      (prog1 (emacs-agent-editor-stop)
        (setq gsmlg-agent-state
              (if gsmlg-agent-package-available-p 'available 'disabled)
              gsmlg-agent-last-error nil))
    (error
     (setq gsmlg-agent-state 'failed
           gsmlg-agent-last-error (error-message-string error-data))
     (if noerror
         (progn
           (message "Agent Editor MCP shutdown failed: %s"
                    gsmlg-agent-last-error)
           nil)
       (signal (car error-data) (cdr error-data))))))

;;;###autoload
(defun gsmlg-agent-reconcile (&optional force)
  "Reconcile Agent Editor MCP actual state with the desired server policy.

Desired state is running only when this process owns a live Emacs server and
autostart is enabled.  Sensors share this entry point.  When FORCE is non-nil,
clear a sticky `failed' state before reconciling."
  (interactive "P")
  (unless gsmlg-agent--reconciling
    (let ((gsmlg-agent--reconciling t)
          (desired (gsmlg-agent--desired-state)))
      (when force
        (setq gsmlg-agent-last-error nil)
        (when (eq gsmlg-agent-state 'failed)
          (setq gsmlg-agent-state 'available)))
      (gsmlg-agent--observe-actual-state)
      (cond
       ((and (eq desired 'running)
             (eq gsmlg-agent-state 'running))
        'running)
       ((and (eq desired 'running)
             (memq gsmlg-agent-state '(available disabled failed)))
        (gsmlg-agent--transition-start t))
       ((and (eq desired 'disabled)
             (eq gsmlg-agent-state 'running))
        (gsmlg-agent--transition-stop t))
       ((and (eq desired 'disabled)
             (eq gsmlg-agent-state 'failed)
             (gsmlg-agent--listener-running-p))
        (gsmlg-agent--transition-stop t))
       (t
        gsmlg-agent-state)))))

;;;###autoload
(defun gsmlg-agent-start ()
  "Start the project-optional Agent Editor MCP runtime."
  (interactive)
  (unless (gsmlg-agent--ensure-package)
    (user-error "Agent Editor MCP is unavailable; inspect startup messages"))
  (or (and (gsmlg-agent--listener-running-p)
           (setq gsmlg-agent-state 'running)
           'running)
      (gsmlg-agent--transition-start)))

;;;###autoload
(defun gsmlg-agent-stop ()
  "Stop Agent Editor MCP without terminating the Emacs process."
  (interactive)
  (unless (gsmlg-agent--ensure-package)
    (user-error "Agent Editor MCP is unavailable; inspect startup messages"))
  (gsmlg-agent--transition-stop))

(defun gsmlg-agent-autostart-maybe ()
  "Start Agent Editor MCP when explicit interactive autostart is enabled.

Unlike `gsmlg-agent-reconcile', this command does not require a live Emacs
server.  Server lifecycle sensors call reconcile instead.  Returns the
listener start result when this call transitions into `running', otherwise
nil."
  (unless noninteractive
    (when (gsmlg-agent--autostart-enabled-p)
      (let ((was-running (gsmlg-agent--listener-running-p)))
        (cond
         (was-running nil)
         (t
          (condition-case error-data
              (let ((result (gsmlg-agent--transition-start t)))
                (and (eq gsmlg-agent-state 'running) result))
            (error
             (message "Agent Editor MCP autostart failed: %s"
                      (error-message-string error-data))
             nil))))))))

(defalias 'gsmlg/agent-editor-mcp-autostart
  #'gsmlg-agent-autostart-maybe)

(defun gsmlg-agent-start-for-server-maybe ()
  "Reconcile Agent Editor MCP when this Emacs process owns a live server."
  (gsmlg-agent-reconcile))

(defun gsmlg-agent--sensor-reconcile (&rest _arguments)
  "Thin lifecycle sensor that only reconciles Agent Editor MCP."
  (gsmlg-agent-reconcile))

(defun gsmlg-agent--sensor-reconcile-stopped (stopped)
  "Thin `server-stop' sensor; reconcile when STOPPED is non-nil."
  (when stopped
    (gsmlg-agent-reconcile))
  stopped)

(defun gsmlg-agent-install-lifecycle ()
  "Install idempotent Agent-owned lifecycle sensors and hooks."
  (unless gsmlg-agent--lifecycle-installed
    (add-hook 'after-init-hook #'gsmlg-agent-start-for-server-maybe)
    (advice-add 'server-start :after #'gsmlg-agent--sensor-reconcile)
    (advice-add 'server-stop :filter-return #'gsmlg-agent--sensor-reconcile-stopped)
    (advice-add 'server-force-delete :after #'gsmlg-agent--sensor-reconcile)
    (add-hook 'kill-emacs-hook #'gsmlg-agent--sensor-reconcile)
    (setq gsmlg-agent--lifecycle-installed t))
  (gsmlg-agent--ensure-package)
  (when gsmlg-agent-package-available-p
    (gsmlg-agent--align-state-directory))
  (when (and gsmlg-agent-package-available-p
             (not (memq gsmlg-agent-state '(running starting stopping))))
    (setq gsmlg-agent-state 'available))
  gsmlg-agent--lifecycle-installed)

(defun gsmlg-agent-remove-lifecycle ()
  "Remove Agent-owned lifecycle sensors and stop a running listener."
  (when gsmlg-agent--lifecycle-installed
    (remove-hook 'after-init-hook #'gsmlg-agent-start-for-server-maybe)
    (advice-remove 'server-start #'gsmlg-agent--sensor-reconcile)
    (advice-remove 'server-stop #'gsmlg-agent--sensor-reconcile-stopped)
    (advice-remove 'server-force-delete #'gsmlg-agent--sensor-reconcile)
    (remove-hook 'kill-emacs-hook #'gsmlg-agent--sensor-reconcile)
    (setq gsmlg-agent--lifecycle-installed nil))
  (when (gsmlg-agent--listener-running-p)
    (gsmlg-agent--transition-stop))
  (setq gsmlg-agent-state 'disabled))

(gsmlg-agent-install-lifecycle)

(provide 'gsmlg-agent)
;;; gsmlg-agent.el ends here
