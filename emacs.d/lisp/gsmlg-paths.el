;;; gsmlg-paths.el --- XDG paths for mutable Emacs data -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralize configuration, package data, caches, and mutable state.  Nothing
;; in this module depends on a package manager.

;;; Code:

(require 'subr-x)
(require 'url-vars)

(defgroup gsmlg nil
  "GSMLG Emacs configuration."
  :group 'environment)

(defun gsmlg-paths--xdg-base (variable fallback)
  "Return absolute XDG VARIABLE, or FALLBACK when it is unset or empty."
  (let ((value (getenv variable)))
    (cond
     ((or (null value) (string-empty-p value))
      (expand-file-name fallback "~"))
     ((file-name-absolute-p value)
      value)
     (t
      (error "%s must name an absolute directory: %s" variable value)))))

(defun gsmlg-paths--xdg-directory (variable fallback)
  "Return the Emacs directory below XDG VARIABLE or FALLBACK."
  (file-name-as-directory
   (expand-file-name
    "emacs"
    (gsmlg-paths--xdg-base variable fallback))))

(defcustom gsmlg-config-directory
  (file-name-as-directory user-emacs-directory)
  "Directory containing the immutable Emacs configuration."
  :type 'directory
  :group 'gsmlg)

(defcustom gsmlg-data-directory
  (gsmlg-paths--xdg-directory "XDG_DATA_HOME" ".local/share")
  "Directory containing package repositories and persistent application data."
  :type 'directory
  :group 'gsmlg)

(defcustom gsmlg-cache-directory
  (gsmlg-paths--xdg-directory "XDG_CACHE_HOME" ".cache")
  "Directory containing disposable Emacs caches."
  :type 'directory
  :group 'gsmlg)

(defcustom gsmlg-state-directory
  (gsmlg-paths--xdg-directory "XDG_STATE_HOME" ".local/state")
  "Directory containing mutable Emacs state."
  :type 'directory
  :group 'gsmlg)

(defun gsmlg-data-file (name)
  "Return NAME below `gsmlg-data-directory'."
  (expand-file-name name gsmlg-data-directory))

(defun gsmlg-cache-file (name)
  "Return NAME below `gsmlg-cache-directory'."
  (expand-file-name name gsmlg-cache-directory))

(defun gsmlg-state-file (name)
  "Return NAME below `gsmlg-state-directory'."
  (expand-file-name name gsmlg-state-directory))

(defun gsmlg-ensure-parent-directory (file)
  "Create FILE's parent directory and return FILE."
  (make-directory (file-name-directory file) t)
  file)

(defun gsmlg-ensure-directory (directory)
  "Create DIRECTORY and return its normalized name."
  (let ((directory (file-name-as-directory directory)))
    (make-directory directory t)
    directory))

(dolist (directory (list gsmlg-data-directory
                         gsmlg-cache-directory
                         gsmlg-state-directory))
  (make-directory directory t))

(defcustom gsmlg-local-file
  (let ((environment-file (getenv "GSMLG_EMACS_LOCAL")))
    (cond
     ((and environment-file (not (string-empty-p environment-file)))
      (expand-file-name environment-file))
     (t
      (expand-file-name
       "gsmlg/emacs-local.el"
       (gsmlg-paths--xdg-base "XDG_CONFIG_HOME" ".config")))))
  "Optional machine-local configuration loaded after all other modules.

`GSMLG_EMACS_LOCAL' takes precedence over the XDG default."
  :type 'file
  :group 'gsmlg)

(setq custom-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "custom.el"))
      url-configuration-directory
      (gsmlg-ensure-directory (gsmlg-cache-file "url/")))

(provide 'gsmlg-paths)
;;; gsmlg-paths.el ends here
