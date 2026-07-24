;;; init-conf.el --- Configuration for init-conf -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-conf.

;;; Code:

(declare-function company-nginx-keywords "company-nginx")

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
    '(add-hook 'nginx-mode-hook #'company-nginx-keywords)))

(use-package caddyfile-mode
  :ensure t
  :mode ("Caddyfile\\'" "\\.caddyfile\\'"))

(provide 'init-conf)
;;; init-conf.el ends here
