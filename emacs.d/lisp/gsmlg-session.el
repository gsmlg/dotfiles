;;; gsmlg-session.el --- Persistent state and explicit server control -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep session data outside the Git checkout and make server creation an
;; explicit, batch-safe policy.

;;; Code:

(require 'gsmlg-paths)

(defvar eshell-directory-name)
(defvar nsm-settings-file)

(defcustom gsmlg-desktop-save-enabled t
  "Whether interactive Emacs sessions should persist desktop state."
  :type 'boolean
  :group 'gsmlg)

(defcustom gsmlg-server-autostart
  t
  "Whether a normal interactive GUI session should start an Emacs server.

Daemon sessions already own a server.  Batch sessions always ignore this
option."
  :type 'boolean
  :group 'gsmlg)

(defun gsmlg-recentf-remote-p (file)
  "Return non-nil when FILE is remote and should be excluded from recentf."
  (file-remote-p file))

(require 'server)

(setq savehist-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "history/savehist"))
      save-place-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "history/save-place"))
      recentf-save-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "history/recentf"))
      bookmark-default-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "bookmarks"))
      project-list-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "projects"))
      tramp-persistency-file-name
      (gsmlg-ensure-parent-directory (gsmlg-state-file "tramp"))
      auto-save-list-file-prefix
      (gsmlg-ensure-parent-directory
       (gsmlg-cache-file "auto-save-list/.saves-"))
      eshell-directory-name
      (gsmlg-ensure-directory (gsmlg-state-file "eshell/"))
      nsm-settings-file
      (gsmlg-ensure-parent-directory
       (gsmlg-state-file "network-security.data"))
      transient-levels-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "transient/levels.el"))
      transient-values-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "transient/values.el"))
      transient-history-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "transient/history.el"))
      desktop-dirname
      (gsmlg-ensure-directory (gsmlg-state-file "desktop/"))
      desktop-path
      (list desktop-dirname)
      desktop-base-file-name "desktop.el"
      desktop-save t
      desktop-files-not-to-save "\\`/[^/:]+:"
      desktop-restore-frames t
      recentf-auto-cleanup 'never
      recentf-exclude '(gsmlg-recentf-remote-p))

(let ((server-directory
       (gsmlg-ensure-directory (gsmlg-state-file "server/"))))
  (set-file-modes server-directory #o700)
  (setopt server-auth-dir server-directory
          server-socket-dir server-directory))

(let ((auto-save-directory
       (gsmlg-ensure-directory (gsmlg-cache-file "auto-save/")))
      (backup-directory
       (gsmlg-ensure-directory (gsmlg-state-file "backups/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-directory t))
        tramp-auto-save-directory auto-save-directory
        backup-directory-alist `(("." . ,backup-directory))
        tramp-backup-directory-alist `(("." . ,backup-directory))))

(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)

(defun gsmlg-session-apply-desktop-policy ()
  "Enable and restore desktop state after applying local overrides."
  (when (and gsmlg-desktop-save-enabled
             (not noninteractive))
    (desktop-save-mode 1)
    ;; Desktop's built-in `after-init-hook' has already run by the time this
    ;; startup policy applies, so restore the configured desktop explicitly.
    (desktop-read desktop-dirname)))

(add-hook 'emacs-startup-hook #'gsmlg-session-apply-desktop-policy 85)

(defun gsmlg-server-start ()
  "Start an Emacs server explicitly when the current process can host one."
  (interactive)
  (when noninteractive
    (user-error "An Emacs server cannot start in batch mode"))
  (if (daemonp)
      (message "This Emacs daemon already provides an emacsclient server")
    (if (server-running-p)
        (message "The Emacs server is already running")
      (server-start)
      (message "Emacs server started"))))

(defun gsmlg-server-start-maybe ()
  "Start a server only when the explicit autostart policy permits it."
  (when (and gsmlg-server-autostart
             (not noninteractive)
             (not (daemonp)))
    (condition-case error-data
        (gsmlg-server-start)
      (error
       (message "GSMLG server autostart failed: %s"
                (error-message-string error-data))))))

(add-hook 'emacs-startup-hook #'gsmlg-server-start-maybe 95)

(provide 'gsmlg-session)
;;; gsmlg-session.el ends here
