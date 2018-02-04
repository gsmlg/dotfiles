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

;;----------------------------------------------------------------------------
;; Load packages
;;----------------------------------------------------------------------------
(use-package spacemacs-theme
  :ensure t)




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
(defun gsmlg//set-monospaced-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))



;;----------------------------------------------------------------------------
;; Apply UI theme after init
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook (lambda ()
			     (load-theme 'spacemacs-dark)
			     (gsmlg//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 18)
			     (global-set-key (kbd "C-z") 'gsmlg/maybe-suspend-frame)) t)

;;----------------------------------------------------------------------------
;; Modify minor mode by `diminish'
;;----------------------------------------------------------------------------
(use-package diminish
  :ensure t)
(use-package scratch
  :ensure t)


(provide 'init-ui)
