;;; gsmlg-debug.el --- Interactive debugging through Dape -*- lexical-binding: t; -*-

;;; Commentary:
;; Deferred Dape integration.  The package itself is declared in
;; `gsmlg-app-packages' during core startup; this module activates when a debug
;; command is invoked.

;;; Code:

(require 'gsmlg-bootstrap)

(use-package dape
  :ensure nil
  :commands (dape dape-breakpoint-toggle dape-repl))

(provide 'gsmlg-debug)
;;; gsmlg-debug.el ends here
