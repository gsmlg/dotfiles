;;; init-zig.el --- Configuration for Zig -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module for Zig language support.

;;; Code:

(use-package zig-mode
  :ensure t
  :mode ("\\.zig\\'" . zig-mode))

(provide 'init-zig)
;;; init-zig.el ends here
