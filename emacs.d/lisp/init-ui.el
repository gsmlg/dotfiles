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
;; highlight window
(use-package dimmer
  :ensure t
  :config
  (dimmer-mode))

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
  :no-require t)

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
	spaceline-all-the-icons-separator-type 'wave
	spaceline-all-the-icons-icon-set-sun-time 'sun/moon))

(defun gsmlg/spaceline-all-the-icons ()
  "Enable spaceline-all-the-icons"
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
  "Enable spaceline-spacemacs"
  (interactive)
  (spaceline-spacemacs-theme))

;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun gsmlg/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

;;----------------------------------------------------------------------------
;; Set the monospaced font size when mixed Chinese and English words
;;----------------------------------------------------------------------------
;(defun gsmlg//set-monospaced-font (english chinese english-size chinese-size)
;  (set-face-attribute 'default nil :font
;                      (format   "%s:pixelsize=%d"  english english-size))
;  (dolist (charset '(kana han cjk-misc bopomofo))
;    (set-fontset-font (frame-parameter nil 'font) charset
;                      (font-spec :family chinese :size chinese-size))))



;;----------------------------------------------------------------------------
;; Apply UI theme after init
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook (lambda ()
			     ;(load-theme 'spacemacs-dark)
                             (if (display-graphic-p)
                                 (gsmlg/spaceline-all-the-icons)
                               (gsmlg/spaceline-spacemacs))
			     ;(gsmlg//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 16 20)
			     (global-set-key (kbd "C-z") 'gsmlg/maybe-suspend-frame)))

;;----------------------------------------------------------------------------
;; Modify minor mode by `diminish'
;;----------------------------------------------------------------------------
(use-package diminish
  :ensure t)
(use-package scratch
  :ensure t)


(provide 'init-ui)
