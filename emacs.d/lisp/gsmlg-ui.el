;;; gsmlg-ui.el --- Theme, frames, and native mode line -*- lexical-binding: t; -*-

;;; Commentary:
;; A font-optional, icon-free UI using the Duskmoon Moonlight theme and the
;; native mode-line machinery.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)

(declare-function set-fontset-font
                  "fontset" (name target font-spec &optional frame add))

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

(defun gsmlg-ui-apply-fonts (&optional frame)
  "Apply optional configured fonts to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
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

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified
                  mode-line-remote)
                 display (min-width (6.0)))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

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
