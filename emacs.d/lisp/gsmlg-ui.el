;;; gsmlg-ui.el --- Theme, frames, and native mode line -*- lexical-binding: t; -*-

;;; Commentary:
;; A font-optional UI using the Duskmoon Moonlight theme, Nerd Font glyphs
;; when available, and native mode-line machinery.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)
(require 'project)

(declare-function set-fontset-font
                  "fontset" (name target font-spec &optional frame add))
(defvar org-mode-line-string)

(defcustom gsmlg-default-font-family "Source Code Pro"
  "Preferred default font family, used only when it is installed."
  :type 'string
  :group 'gsmlg)

(defcustom gsmlg-default-font-height 160
  "Preferred default face height in tenths of a point."
  :type 'integer
  :group 'gsmlg)

(defcustom gsmlg-cjk-font-family "Hiragino Sans GB"
  "Preferred CJK font family, used only when it is installed."
  :type 'string
  :group 'gsmlg)

(defcustom gsmlg-nerd-font-family "JetBrainsMono Nerd Font Mono"
  "Preferred Nerd Font family for UI glyphs."
  :type 'string
  :group 'gsmlg)

(defvar gsmlg-ui-nerd-font-available nil
  "Non-nil when the configured Nerd Font is available.")

(defun gsmlg-ui-apply-fonts (&optional frame)
  "Apply optional configured fonts to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (setq gsmlg-ui-nerd-font-available
            (and (find-font (font-spec :family gsmlg-nerd-font-family)) t))
      (when (find-font (font-spec :family gsmlg-default-font-family))
        (set-face-attribute
         'default nil
         :family gsmlg-default-font-family
         :height gsmlg-default-font-height))
      (when (find-font (font-spec :family gsmlg-cjk-font-family))
        (dolist (charset '(kana han cjk-misc bopomofo))
          (set-fontset-font t charset
                            (font-spec :family gsmlg-cjk-font-family)))))))

(defun gsmlg-ui-enable-theme ()
  "Enable Duskmoon Moonlight without prompting."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'duskmoon-moonlight t))

(defun gsmlg-ui-icon (glyph)
  "Return Nerd Font GLYPH, or an empty string when unavailable."
  (if gsmlg-ui-nerd-font-available
      (propertize glyph 'face `(:family ,gsmlg-nerd-font-family))
    ""))

(defun gsmlg-ui-mode-line-file-icon ()
  "Return the mode-line file icon, when Nerd Font glyphs are available."
  (when buffer-file-name
    (gsmlg-ui-icon " ")))

(defun gsmlg-ui-mode-line-position-icon ()
  "Return the mode-line position icon, when Nerd Font glyphs are available."
  (gsmlg-ui-icon " "))

(defun gsmlg-ui-mode-line-vc ()
  "Return version-control status with a Nerd Font branch icon."
  (when vc-mode
    (list (gsmlg-ui-icon " ") vc-mode)))

(defun gsmlg-ui-open-header-directory (directory)
  "Open header breadcrumb DIRECTORY."
  (dired directory))

(defun gsmlg-ui--header-path-button (label directory)
  "Return a header-line button named LABEL that opens DIRECTORY."
  (make-text-button
   label nil
   'action #'gsmlg-ui-open-header-directory
   'button-data directory
   'follow-link t
   'gsmlg-directory directory
   'help-echo (format "Open %s" (abbreviate-file-name directory))))

(defun gsmlg-ui-file-breadcrumb ()
  "Return project context and a clickable breadcrumb for the current file."
  (when buffer-file-name
    (let* ((project (project-current nil (file-name-directory
                                          buffer-file-name)))
           (root (if project
                     (project-root project)
                   (concat (file-remote-p buffer-file-name) "/")))
           (relative (file-relative-name buffer-file-name root))
           (parts (split-string relative "/" t))
           (directory root)
            (breadcrumb
            (if project
                (list
                 "["
                 (gsmlg-ui-icon " ")
                 (gsmlg-ui--header-path-button
                  (file-name-nondirectory (directory-file-name root))
                  root)
                 "] - [")
              (list
               "["
               (gsmlg-ui-icon " ")
               (gsmlg-ui--header-path-button
                (abbreviate-file-name root) root)
               " / "))))
      (while (cdr parts)
        (setq directory (expand-file-name (file-name-as-directory (car parts))
                                          directory)
              breadcrumb
              (append breadcrumb
                      (list (gsmlg-ui--header-path-button
                             (car parts) directory)
                            " / "))
              parts (cdr parts)))
      (append breadcrumb
              (list (gsmlg-ui-icon " ") (car parts) "]")))))

(defun gsmlg-ui-header-line ()
  "Return the file breadcrumb and active Org clock for the header line."
  (let ((breadcrumb (gsmlg-ui-file-breadcrumb))
        (clock (and (boundp 'org-mode-line-string)
                    org-mode-line-string)))
    (when (or breadcrumb clock)
      (append '(" ") breadcrumb
              (when clock `("    " ,clock " "))))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified
                  mode-line-remote)
                 display (min-width (6.0)))
                mode-line-frame-identification
                (:eval (gsmlg-ui-mode-line-file-icon))
                mode-line-buffer-identification
                "   "
                (:eval (gsmlg-ui-mode-line-position-icon))
                mode-line-position
                (:eval (gsmlg-ui-mode-line-vc))
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

(setq-default header-line-format
              '((:eval (gsmlg-ui-header-line))))

(when (fboundp #'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(add-hook 'after-make-frame-functions #'gsmlg-ui-apply-fonts)
(add-hook 'emacs-startup-hook #'gsmlg-ui-apply-fonts 80)

(use-package emacs-duskmoon-theme
  :ensure
  (:host github
   :repo "duskmoon-dev/emacs-duskmoon-theme"
   :files ("*.el"))
  :demand t
  :config
  (gsmlg-ui-enable-theme))

(provide 'gsmlg-ui)
;;; gsmlg-ui.el ends here
