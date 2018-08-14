(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package company-nginx
  :ensure t
  :config
  (eval-after-load 'nginx-mode
    '(add-hook 'nginx-mode-hook #'company-nginx-keywords))
  )

(provide 'init-conf)
