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
    (helm-mode t)))


(provide 'init-helm)
