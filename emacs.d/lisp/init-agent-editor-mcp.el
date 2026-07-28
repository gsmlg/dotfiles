;;; init-agent-editor-mcp.el --- Agent editor MCP setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Load the agent editor MCP package and start it for daemon sessions.

;;; Code:

(require 'emacs-agent-editor)

(defconst gsmlg/agent-editor-mcp-launch-directory
  (file-name-as-directory (expand-file-name default-directory))
  "Directory inherited by Emacs when this configuration was loaded.")

(defun gsmlg/agent-editor-mcp-autostart ()
  "Start Agent Editor MCP for a daemon launched in a local workspace."
  (when (daemonp)
    (condition-case error-data
        (emacs-agent-editor-start gsmlg/agent-editor-mcp-launch-directory)
      (error
       (message "Agent Editor MCP autostart skipped: %s"
                (error-message-string error-data))))))

(add-hook 'after-init-hook #'gsmlg/agent-editor-mcp-autostart)

(provide 'init-agent-editor-mcp)
;;; init-agent-editor-mcp.el ends here
