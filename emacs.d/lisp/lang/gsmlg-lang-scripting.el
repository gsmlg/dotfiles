;;; gsmlg-lang-scripting.el --- Python, Ruby, and shell modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Prefer built-in tree-sitter modes while preserving built-in fallbacks.

;;; Code:

(require 'gsmlg-eglot)

(defun gsmlg-python-mode ()
  "Select Python tree-sitter mode or built-in `python-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'python #'python-ts-mode #'python-mode))

(defun gsmlg-ruby-mode ()
  "Select Ruby tree-sitter mode or built-in `ruby-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'ruby #'ruby-ts-mode #'ruby-mode))

(defun gsmlg-shell-mode ()
  "Select Bash tree-sitter mode or built-in `sh-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'bash #'bash-ts-mode #'sh-mode))

(defun gsmlg-lang-scripting-register-auto-modes ()
  "Place GSMLG scripting dispatchers ahead of package entries."
  (dolist
      (entry
       '(("\\.zsh\\'" . sh-mode)
         ("\\.\\(?:sh\\|bash\\)\\'" . gsmlg-shell-mode)
         ("\\.rb\\'" . gsmlg-ruby-mode)
         ("\\.py\\'" . gsmlg-python-mode)))
    (gsmlg-auto-mode-prepend entry)))

(gsmlg-lang-scripting-register-auto-modes)

(provide 'gsmlg-lang-scripting)
;;; gsmlg-lang-scripting.el ends here
