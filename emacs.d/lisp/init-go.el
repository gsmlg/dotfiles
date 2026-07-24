;; -*- lexical-binding: t; -*-
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . (lambda () (add-hook 'before-save-hook #'gofmt-before-save nil t)))
  :ensure t)

(provide 'init-go)
