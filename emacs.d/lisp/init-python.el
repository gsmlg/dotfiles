;;; init-python.el --- Configuration for Python -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module for Python language support.

;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-indent-offset gsmlg/preferred-indent-level))

(use-package pyvenv
  :ensure t
  :after python)

(provide 'init-python)
;;; init-python.el ends here
