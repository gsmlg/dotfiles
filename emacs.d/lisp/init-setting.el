(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))

(use-package list-unicode-display
  :ensure t)

(use-package dynamic-spaces
  :ensure t
  :config (dynamic-spaces-global-mode t))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

(use-package undo-tree
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-undo-tree-mode)
    (after-load 'undo-tree
      (diminish 'undo-tree-mode))))

(use-package symbol-overlay
  :ensure t
  :config
  (progn
    (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
      (add-hook hook 'symbol-overlay-mode))
    (after-load 'symbol-overlay
      (diminish 'symbol-overlay-mode)
      (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
      (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))))

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

;;----------------------------------------------------------------------------
;; Key Bindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-h") 'delete-backward-char)


;;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)


;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)



(provide 'init-setting)
