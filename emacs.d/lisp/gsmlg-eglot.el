;;; gsmlg-eglot.el --- Guarded Eglot server selection and startup -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure built-in Eglot without installing external servers.  Executable
;; discovery, formatting, tree-sitter helpers, and Dape live in dedicated
;; modules.  Language capabilities come from `gsmlg-language-registry'.

;;; Code:

(require 'cl-lib)
(require 'gsmlg-bootstrap)
(require 'gsmlg-language-registry)
(require 'gsmlg-language-tools)
(require 'eglot)
(require 'flymake)
(require 'project)

(declare-function eglot-current-server "eglot" ())
(declare-function eglot-reconnect "eglot" (server &optional interactive))
(declare-function eglot-shutdown "eglot" (server &optional sync preserve-buffers))
(declare-function envrc-reload "envrc" ())

(defcustom gsmlg-eglot-command-overrides nil
  "Alist mapping language symbols to explicit language-server command lists.

Commands are executed from the current project root in its local or TRAMP
environment.  Relative program paths are resolved from that root.  A
TRAMP-qualified program must name the same remote connection as the project;
it is normalized to its remote-local name before Eglot launches it.  Useful
keys include `elixir', `erlang', `typescript', `rust', `go', `python',
`ruby', `zig', `nix', `c-cpp', `yaml', `docker', and `terraform'."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'gsmlg)

(defcustom gsmlg-eglot-auto-start t
  "Whether supported project buffers should start an available server."
  :type 'boolean
  :group 'gsmlg)

(defconst gsmlg-eglot-supported-modes
  (gsmlg-language-registry-supported-modes)
  "Programming modes eligible for guarded Eglot startup.")

(defvar gsmlg-eglot-unavailable-cache (make-hash-table :test #'equal)
  "Servers already found unavailable in the current Emacs process.")

(defun gsmlg-eglot--language ()
  "Return the language-server family for the current buffer."
  (gsmlg-language-registry-language-for-buffer))

(defun gsmlg-eglot-server-command (&optional project)
  "Return the preferred server command for current mode and PROJECT."
  (let* ((language (gsmlg-eglot--language))
         (props (and language (gsmlg-language-registry-entry language)))
         (override (alist-get language gsmlg-eglot-command-overrides))
         (root (gsmlg-eglot--project-root project)))
    (cond
     ((not language) nil)
     (override
      (gsmlg-eglot-normalize-override
       override (or root default-directory)))
     (t
      (or
       (when-let* ((program (plist-get props :project-local))
                   (root)
                   (resolved
                    (gsmlg-eglot-project-executable program root)))
         (append (list resolved)
                 (plist-get props :project-local-args)))
       (gsmlg-eglot--available-command
        (plist-get props :commands)))))))

(defun gsmlg-eglot-contact (_interactive project)
  "Return an Eglot contact for PROJECT without local fallback."
  (gsmlg-eglot-server-command project))

(defun gsmlg-eglot--cache-key ()
  "Return an unavailable-server cache key for the current buffer."
  (list major-mode
        (gsmlg-eglot--language)
        (or (gsmlg-eglot--project-root) default-directory)
        (file-remote-p default-directory)))

;;;###autoload
(defun gsmlg-eglot-ensure-maybe (&optional interactive)
  "Start Eglot when this project buffer has a supported server.

With INTERACTIVE non-nil, explain why no server can be started.  Automatic
calls never prompt repeatedly for unavailable executables."
  (interactive (list t))
  (let ((language (gsmlg-eglot--language))
        (supported (memq major-mode gsmlg-eglot-supported-modes))
        (project (project-current nil)))
    (cond
     ((not (and supported language))
      (when interactive
        (message "Eglot is not configured for %s" major-mode)))
     ((not project)
      (when interactive
        (message "Eglot needs a project for %s" major-mode)))
     ((eglot-managed-p))
     (t
      (let ((key (gsmlg-eglot--cache-key)))
        (when interactive
          (remhash key gsmlg-eglot-unavailable-cache))
        (cond
         ((gethash key gsmlg-eglot-unavailable-cache)
          (when interactive
            (message "No configured %s language server is available near %s"
                     language
                     default-directory)))
         ((gsmlg-eglot-server-command project)
          (remhash key gsmlg-eglot-unavailable-cache)
          (condition-case error-data
              (eglot-ensure)
            (error
             (puthash key t gsmlg-eglot-unavailable-cache)
             (if interactive
                 (user-error "Unable to start Eglot: %s"
                             (error-message-string error-data))
               (message "GSMLG Eglot autostart failed: %s"
                        (error-message-string error-data))))))
         (t
          (puthash key t gsmlg-eglot-unavailable-cache)
          (when interactive
            (message "No configured %s language server is available near %s"
                     language
                     default-directory)))))))))

(defun gsmlg-eglot-auto-start-maybe ()
  "Run guarded Eglot startup when `gsmlg-eglot-auto-start' is enabled."
  (when gsmlg-eglot-auto-start
    (gsmlg-eglot-ensure-maybe)))

(defun gsmlg-eglot-environment-changed ()
  "Clear a negative server lookup after activating a buffer environment."
  (when (memq major-mode gsmlg-eglot-supported-modes)
    (remhash (gsmlg-eglot--cache-key) gsmlg-eglot-unavailable-cache)))

;;;###autoload
(defun gsmlg-envrc-reload-and-refresh-eglot ()
  "Reload envrc for this buffer, then ensure or reconnect Eglot.

Clears the negative server cache so a newly available executable can be
discovered.  When the buffer is already Eglot-managed, reconnect; otherwise
attempt guarded startup."
  (interactive)
  (unless (fboundp #'envrc-reload)
    (user-error "Envrc is unavailable"))
  (envrc-reload)
  (gsmlg-eglot-environment-changed)
  (cond
   ((eglot-managed-p)
    (if (fboundp #'eglot-reconnect)
        (eglot-reconnect (eglot-current-server))
      (let ((server (eglot-current-server)))
        (when server
          (eglot-shutdown server))
        (eglot-ensure)))
    (message "Reloaded envrc and refreshed Eglot"))
   (t
    (gsmlg-eglot-ensure-maybe t))))

;;;###autoload
(defun gsmlg-eglot-organize-imports ()
  "Ask the active language server to organize imports."
  (interactive)
  (unless (eglot-managed-p)
    (user-error "The current buffer is not managed by Eglot"))
  (eglot-code-action-organize-imports (point-min) (point-max)))

(defun gsmlg-eglot-configure-server-programs ()
  "Install GSMLG's remote-aware server command resolver."
  (dolist (mode (gsmlg-language-registry-eglot-modes))
    (setf (alist-get mode eglot-server-programs)
          #'gsmlg-eglot-contact)))

(gsmlg-eglot-configure-server-programs)
(add-hook 'find-file-hook #'gsmlg-eglot-auto-start-maybe 90)
(with-eval-after-load 'envrc
  (add-hook 'envrc-mode-hook #'gsmlg-eglot-environment-changed 90))

(provide 'gsmlg-eglot)
;;; gsmlg-eglot.el ends here
