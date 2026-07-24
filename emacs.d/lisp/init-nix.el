;;; init-nix.el --- Configuration for Nix -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module for Nix language support.

;;; Code:

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode))

(provide 'init-nix)
;;; init-nix.el ends here
