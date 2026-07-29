;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings here must take effect before the package and frame systems finish
;; initializing.  Application and language configuration belongs in init.el.

;;; Code:

(declare-function menu-bar-mode "menu-bar" (&optional argument))
(declare-function scroll-bar-mode "scroll-bar" (&optional argument))
(declare-function tool-bar-mode "tool-bar" (&optional argument))

(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar gsmlg-early-init-loaded t
  "Non-nil after the GSMLG early init file has loaded.")

(defun gsmlg-early-init--xdg-base (variable fallback)
  "Return absolute XDG VARIABLE, or FALLBACK when it is unset or empty."
  (let ((value (getenv variable)))
    (cond
     ((or (null value) (equal value ""))
      (expand-file-name fallback "~"))
     ((file-name-absolute-p value)
      value)
     (t
      (error "%s must name an absolute directory: %s" variable value)))))

(let ((cache-root
       (file-name-as-directory
        (expand-file-name
         "emacs"
         (gsmlg-early-init--xdg-base "XDG_CACHE_HOME" ".cache")))))
  (make-directory cache-root t)
  (when (boundp 'native-comp-eln-load-path)
    (let ((eln-directory (expand-file-name "eln-cache/" cache-root)))
      (make-directory eln-directory t)
      (startup-redirect-eln-cache eln-directory))))

(dolist (parameter '((menu-bar-lines . 0)
                     (tool-bar-lines . 0)
                     (vertical-scroll-bars)))
  (add-to-list 'default-frame-alist parameter))

(when (fboundp #'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp #'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp #'scroll-bar-mode)
  (scroll-bar-mode -1))

(provide 'early-init)
;;; early-init.el ends here
