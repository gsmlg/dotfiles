;;; gsmlg-compat.el --- Compatibility adapters for Emacs APIs -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralize version-sensitive and fallback access to Emacs APIs so feature
;; modules can depend on stable GSMLG entry points instead of private symbols.

;;; Code:

(defun gsmlg-compat-flymake-diagnostic-severity (type)
  "Return the numeric Flymake severity for diagnostic TYPE, or nil.

Prefer the documented symbol properties that Flymake attaches to diagnostic
types and their categories.  Avoid calling Flymake private helpers from UI
code."
  (cond
   ((null type) nil)
   ((and (symbolp type) (get type 'severity)))
   ((and (symbolp type)
         (let ((category (get type 'flymake-category)))
           (and (symbolp category) (get category 'severity)))))
   ;; Fall back to known built-in diagnostic types when properties are absent.
   ((memq type '(flymake-error :error error))
    (warning-numeric-level :error))
   ((memq type '(flymake-warning :warning warning))
    (warning-numeric-level :warning))
   ((memq type '(flymake-note :note :debug note))
    (warning-numeric-level :debug))
   (t nil)))

(provide 'gsmlg-compat)
;;; gsmlg-compat.el ends here
