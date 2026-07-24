;;; init-go.el --- Configuration for init-go -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-go.

;;; Code:

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . (lambda () (add-hook 'before-save-hook #'gofmt-before-save nil t)))
  :ensure t)

(provide 'init-go)
;;; init-go.el ends here
