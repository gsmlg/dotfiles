;;; test-helper.el --- Shared helpers for GSMLG Emacs tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Establish an isolated XDG environment and expose helpers used by ERT tests.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst gsmlg-test-emacs-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to the Emacs configuration under test.")

(defconst gsmlg-test-xdg-root
  (make-temp-file "gsmlg-emacs-test-" t)
  "Temporary root containing all mutable test state.")

(dolist (entry `(("XDG_CONFIG_HOME" . ,(expand-file-name "config" gsmlg-test-xdg-root))
                 ("XDG_DATA_HOME" . ,(expand-file-name "data" gsmlg-test-xdg-root))
                 ("XDG_CACHE_HOME" . ,(expand-file-name "cache" gsmlg-test-xdg-root))
                 ("XDG_STATE_HOME" . ,(expand-file-name "state" gsmlg-test-xdg-root))))
  (make-directory (cdr entry) t)
  (setenv (car entry) (cdr entry)))

(setq user-emacs-directory (file-name-as-directory gsmlg-test-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" gsmlg-test-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lang" gsmlg-test-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "site-lisp/agent-editor-mcp"
                               gsmlg-test-emacs-directory))

(defun gsmlg-test-mode-for-file (name)
  "Return the major mode selected for a temporary file named NAME."
  (let ((file (expand-file-name name gsmlg-test-xdg-root)))
    (write-region "" nil file nil 'silent)
    (with-temp-buffer
      (setq buffer-file-name file)
      (set-auto-mode)
      major-mode)))

(defun gsmlg-test-git (&rest arguments)
  "Run Git with ARGUMENTS and fail the current test on error."
  (let ((status (apply #'call-process "git" nil nil nil arguments)))
    (unless (zerop status)
      (ert-fail (format "git %S exited %s" arguments status)))))

(provide 'test-helper)
;;; test-helper.el ends here
