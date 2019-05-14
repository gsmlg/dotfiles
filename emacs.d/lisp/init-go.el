(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . (lambda () ((set (make-local-variable 'before-save-hook) '(gofmt-before-save)))))
  :ensure t)

(provide 'init-go)
