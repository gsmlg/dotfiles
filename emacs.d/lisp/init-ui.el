;;; init-ui.el --- Configuration for init-ui -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-ui.

;;; Code:

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Smooth pixel scrolling in modern Emacs 29+
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
;; highlight window
(use-package dimmer
  :ensure t
  :config
  (dimmer-mode))

(defvar neo-theme)
(defvar *is-a-mac*)

;;------------------------------------------------------------------------------
;; add all the icons packages for file icon
;;------------------------------------------------------------------------------
(use-package all-the-icons
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;----------------------------------------------------------------------------
;; Load packages
;;----------------------------------------------------------------------------
(use-package spacemacs-theme
  :ensure t
  :init
  (load-theme 'spacemacs-dark t))

;;------------------------------------------------------------------------------
;; Config modeline
;;------------------------------------------------------------------------------
(use-package spaceline
  :ensure t
  :config
  (progn
    (require 'spaceline-config)
    (spaceline-helm-mode)
    (spaceline-info-mode)))

(use-package spaceline-all-the-icons
  :ensure t
  :init
  (setq spaceline-all-the-icons-slim-render t
	spaceline-all-the-icons-separator-type 'slant
	spaceline-all-the-icons-separator-scale 1.0
	spaceline-all-the-icons-icon-set-sun-time 'sun/moon))

(defun gsmlg/spaceline-all-the-icons ()
  "Enable `spaceline-all-the-icons' mode line theme."
  (interactive)
  (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  (spaceline-all-the-icons-theme
   'input-method
   'buffer-encoding-abbrev
   'org-pomodoro
   'mu4e-alert-segment))

(defun gsmlg/spaceline-spacemacs ()
  "Enable `spaceline-spacemacs' mode line theme."
  (interactive)
  (spaceline-spacemacs-theme))

;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun gsmlg/maybe-suspend-frame ()
  "Suspend frame unless running in GUI mode on macOS."
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

;;----------------------------------------------------------------------------
;; Set the monospaced font size when mixed Chinese and English words
;;----------------------------------------------------------------------------
(defun gsmlg/set-monospaced-font (english chinese english-size chinese-size)
  "Set ENGLISH and CHINESE fonts with ENGLISH-SIZE and CHINESE-SIZE."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font
                        (format "%s:pixelsize=%d" english english-size))
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :size chinese-size)))))




;;----------------------------------------------------------------------------
;; Apply UI theme after init
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook (lambda ()
                             (if (display-graphic-p)
                                 (progn
                                   (gsmlg/spaceline-all-the-icons)
                                   (gsmlg/set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 16 20))
                               (gsmlg/spaceline-spacemacs))
                             (global-set-key (kbd "C-z") 'gsmlg/maybe-suspend-frame)))

;;----------------------------------------------------------------------------
;; Modify minor mode by `diminish'
;;----------------------------------------------------------------------------
(use-package diminish
  :ensure t)
(use-package scratch
  :ensure t)

(provide 'init-ui)
;;; init-ui.el ends here
