;;; gsmlg-agent.el --- Agent Editor MCP integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Explicit startup integration for the bundled Agent Editor MCP package.

;;; Code:

(require 'subr-x)
(require 'gsmlg-paths)
(require 'server)

(declare-function emacs-agent-editor-running-p "emacs-agent-editor" ())
(declare-function emacs-agent-editor-start
                  "emacs-agent-editor" (&optional port))
(declare-function emacs-agent-editor-stop "emacs-agent-editor" ())
(defvar emacs-agent-editor-host)
(defvar emacs-agent-editor--connection-file)

(defvar gsmlg-agent-package-available-p
  (condition-case error-data
      (progn
        (require 'emacs-agent-editor)
        t)
    (error
     (message "Agent Editor MCP integration is unavailable: %s"
              (error-message-string error-data))
     nil))
  "Non-nil when the bundled Agent Editor MCP package loaded successfully.")

(defgroup gsmlg-agent nil
  "GSMLG integration for Agent Editor MCP."
  :group 'gsmlg)

(defcustom gsmlg-agent-port 9876
  "Default loopback port for Agent Editor MCP.
`EMACS_AGENT_PORT' takes precedence over this value."
  :type '(integer 1 65535)
  :group 'gsmlg-agent)

(defcustom gsmlg-agent-autostart t
  "Whether to start Agent Editor MCP with the interactive Emacs server.
Autostart follows the server lifecycle by default and is always disabled in
batch mode.  Set this option to nil to opt out; a truthy
`EMACS_AGENT_AUTOSTART' still enables autostart explicitly."
  :type 'boolean
  :group 'gsmlg-agent)

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

(defun gsmlg-agent--configure-package ()
  "Restrict Agent Editor MCP to the IPv4 loopback listener."
  (setq emacs-agent-editor-host "127.0.0.1"))

(defun gsmlg-agent--ensure-package ()
  "Signal a user-facing error when Agent Editor MCP is unavailable."
  (unless (and gsmlg-agent-package-available-p
               (fboundp #'emacs-agent-editor-start))
    (user-error "Agent Editor MCP is unavailable; inspect startup messages")))

(defun gsmlg-agent--remove-legacy-connection-file ()
  "Remove the exact connection file published by the former integration."
  (when-let* ((canonical emacs-agent-editor--connection-file)
              ((stringp canonical))
              (daemon
               (file-name-nondirectory
                (directory-file-name
                 (file-name-directory canonical))))
              (legacy
               (expand-file-name
                (format "agent-editor/%s/connection.json" daemon)
                gsmlg-state-directory))
              ((file-exists-p legacy))
              ((not (file-equal-p legacy canonical))))
    (delete-file legacy)))

;;;###autoload
(defun gsmlg-agent-start ()
  "Start the project-optional Agent Editor MCP runtime."
  (interactive)
  (gsmlg-agent--ensure-package)
  (gsmlg-agent--configure-package)
  (prog1 (emacs-agent-editor-start (gsmlg-agent--resolved-port))
    (gsmlg-agent--remove-legacy-connection-file)))

;;;###autoload
(defun gsmlg-agent-stop ()
  "Stop Agent Editor MCP without terminating the Emacs process."
  (interactive)
  (gsmlg-agent--ensure-package)
  (emacs-agent-editor-stop))

(defun gsmlg-agent-autostart-maybe ()
  "Start Agent Editor MCP when explicit interactive autostart is enabled."
  (unless noninteractive
    (when (and (gsmlg-agent--autostart-enabled-p)
               gsmlg-agent-package-available-p
               (fboundp #'emacs-agent-editor-running-p)
               (not (emacs-agent-editor-running-p)))
      (condition-case error-data
          (gsmlg-agent-start)
        (error
         (message "Agent Editor MCP autostart failed: %s"
                  (error-message-string error-data))
         nil)))))

(defalias 'gsmlg/agent-editor-mcp-autostart
  #'gsmlg-agent-autostart-maybe)

(defun gsmlg-agent-start-for-server-maybe ()
  "Start Agent Editor MCP when this Emacs process owns a live server."
  (when (and server-mode
             (process-live-p server-process))
    (gsmlg-agent-autostart-maybe)))

(defun gsmlg-agent--after-server-start (&rest _arguments)
  "Start Agent Editor MCP after startup of the local Emacs server.
Emacs 30.2 provides no `server-start-hook', and `server-start' does not run
`server-mode-hook', so lifecycle synchronization requires named advice."
  (gsmlg-agent-start-for-server-maybe))

(defun gsmlg-agent--stop-if-running ()
  "Stop Agent Editor MCP when its package and listener are active."
  (when (and gsmlg-agent-package-available-p
             (fboundp #'emacs-agent-editor-running-p)
             (fboundp #'emacs-agent-editor-stop)
             (emacs-agent-editor-running-p))
    (condition-case error-data
        (emacs-agent-editor-stop)
      (error
       (message "Agent Editor MCP shutdown failed: %s"
                (error-message-string error-data))
       nil))))

(defun gsmlg-agent--after-server-stop (stopped)
  "Stop Agent Editor MCP after the Emacs server when STOPPED is non-nil.
Emacs 30.2 provides no `server-stop-hook', and `server-stop' does not run
`server-mode-hook', so lifecycle synchronization requires named advice."
  (when stopped
    (gsmlg-agent--stop-if-running))
  stopped)

(gsmlg-agent--configure-package)
(add-hook 'after-init-hook #'gsmlg-agent-start-for-server-maybe)
(advice-add 'server-start :after #'gsmlg-agent--after-server-start)
(advice-add 'server-stop :filter-return #'gsmlg-agent--after-server-stop)

(provide 'gsmlg-agent)
;;; gsmlg-agent.el ends here
