;;; gsmlg-session.el --- Persistent state and global desktop session -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep session data outside the Git checkout.  One formal Emacs server owns a
;; single desktop file; emacsclient frames inherit that buffer set and do not
;; restore historical GUI geometry from a headless daemon.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-server)

(defvar eshell-directory-name)
(defvar nsm-settings-file)

(defcustom gsmlg-desktop-save-enabled t
  "Whether the formal interactive Emacs server should persist desktop state."
  :type 'boolean
  :group 'gsmlg)

(defun gsmlg-recentf-remote-p (file)
  "Return non-nil when FILE is remote and should be excluded from recentf."
  (file-remote-p file))

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
      ;; Frames are created by emacsclient and the current display; do not
      ;; restore stale monitor coordinates from a headless daemon session.
      desktop-restore-frames nil
      recentf-auto-cleanup 'never
      recentf-exclude '(gsmlg-recentf-remote-p))

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
  "Enable and restore the global desktop after applying local overrides.

Desktop restore and save belong to the long-lived server process.  Closing an
emacsclient frame does not kill that process, so it does not rewrite desktop."
  (when (and gsmlg-desktop-save-enabled
             (not noninteractive))
    (desktop-save-mode 1)
    ;; Desktop's built-in `after-init-hook' has already run by the time this
    ;; startup policy applies, so restore the configured desktop explicitly.
    (desktop-read desktop-dirname)))

(add-hook 'emacs-startup-hook #'gsmlg-session-apply-desktop-policy 85)

(provide 'gsmlg-session)
;;; gsmlg-session.el ends here
