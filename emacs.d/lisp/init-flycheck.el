(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :init (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(provide 'init-flycheck)
