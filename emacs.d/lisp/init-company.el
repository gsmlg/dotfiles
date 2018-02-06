(use-package company
  :ensure t
  :init
  (setq-default company-dabbrev-other-buffers 'all
		company-tooltip-align-annotations t)
  :bind ("M-C-/" . company-complete)
  :config
  (progn
    (diminish 'company-mode "CMP")
    (define-key company-mode-map (kbd "M-/") 'company-complete)
    (define-key company-active-map (kbd "M-/") 'company-select-next)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package company-quickhelp
  :ensure t
  :config
  (add-hook 'after-init-hook 'company-quickhelp-mode))

(provide 'init-company)
