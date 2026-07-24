;;; init-flycheck.el --- Configuration for init-flycheck -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-flycheck.

;;; Code:

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :init (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
