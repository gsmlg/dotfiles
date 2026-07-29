;;; gsmlg-lang-elisp.el --- Emacs Lisp development -*- lexical-binding: t; -*-

;;; Commentary:
;; Built-in evaluation, IELM, and structural editing support for Emacs Lisp.

;;; Code:

(require 'elisp-mode)
(require 'gsmlg-bootstrap)

(use-package macrostep
  :commands (macrostep-expand))

(provide 'gsmlg-lang-elisp)
;;; gsmlg-lang-elisp.el ends here
