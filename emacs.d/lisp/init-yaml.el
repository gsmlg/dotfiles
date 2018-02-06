(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\.erb\\'"
  :config
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))

(provide 'init-yaml)
