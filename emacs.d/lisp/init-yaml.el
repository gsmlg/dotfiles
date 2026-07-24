;;; init-yaml.el --- Configuration for init-yaml -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-yaml.

;;; Code:

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\.erb\\'"
  :config
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))

(provide 'init-yaml)
;;; init-yaml.el ends here
