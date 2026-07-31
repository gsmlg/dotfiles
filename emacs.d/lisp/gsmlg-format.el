;;; gsmlg-format.el --- Buffer formatting through Apheleia and Eglot -*- lexical-binding: t; -*-

;;; Commentary:
;; Prefer an Apheleia formatter when configured for the current major mode,
;; otherwise fall back to an active Eglot server.  Competing save hooks are
;; never enabled automatically.

;;; Code:

(require 'gsmlg-bootstrap)

(declare-function apheleia-format-buffer "apheleia" ())
(defvar apheleia-mode-alist)

;;;###autoload
(defun gsmlg-format-buffer ()
  "Format the buffer through Apheleia, or fall back to active Eglot."
  (interactive)
  (cond
   ((and (require 'apheleia nil t)
         (alist-get major-mode apheleia-mode-alist))
    (apheleia-format-buffer))
   ((and (fboundp #'eglot-managed-p)
         (eglot-managed-p))
    (eglot-format-buffer))
   (t
    (user-error "No Apheleia formatter or Eglot server is active"))))

(use-package apheleia
  :commands (apheleia-format-buffer))

(provide 'gsmlg-format)
;;; gsmlg-format.el ends here
