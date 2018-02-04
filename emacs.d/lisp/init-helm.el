(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)
	 ("C-c r" . helm-recentf)
	 ("C-c i" . helm-imenu)
         ("C-x b" . helm-buffers-list))
  :config
  (progn
    (when (executable-find "mdfind")
      (setq helm-locate-command "mdfind %s -name %s"))
    ;; Toggle adaptive sorting in all sources
    (helm-adaptive-mode t)
    ;; Global helm mode
    (helm-mode t)))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
	 ("M-I" . helm-swoop-back-to-last-point)
	 ("C-c M-i" . helm-multi-swoop)
	 ("C-x M-i" . helm-multi-swoop-all))
  :config
  (progn
    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)
    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)
    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color nil)))

(provide 'init-helm)
