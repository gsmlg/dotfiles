;;; gsmlg-lang-scripting.el --- Python, Ruby, and shell modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Prefer built-in tree-sitter modes while preserving built-in fallbacks.

;;; Code:

(require 'gsmlg-treesit)

;;;###autoload
(defun gsmlg-python-mode ()
  "Select Python tree-sitter mode or built-in `python-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'python #'python-ts-mode #'python-mode))

;;;###autoload
(defun gsmlg-ruby-mode ()
  "Select Ruby tree-sitter mode or built-in `ruby-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'ruby #'ruby-ts-mode #'ruby-mode))

;;;###autoload
(defun gsmlg-shell-mode ()
  "Select Bash tree-sitter mode or built-in `sh-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'bash #'bash-ts-mode #'sh-mode)
  (when (derived-mode-p 'sh-mode)
    (sh-set-shell "bash")))

;;;###autoload
(defun gsmlg-zsh-mode ()
  "Select built-in `sh-mode' configured for Zsh."
  (interactive)
  (sh-mode)
  (sh-set-shell "zsh"))

;;;###autoload
(defun gsmlg-posix-shell-mode ()
  "Select built-in `sh-mode' configured for POSIX shell syntax."
  (interactive)
  (sh-mode)
  (sh-set-shell "sh"))

(defun gsmlg-lang-scripting-register-auto-modes ()
  "Place GSMLG scripting dispatchers ahead of package entries."
  (dolist
      (entry
       '(("\\(?:\\`\\|/\\)\\.?z\\(?:login\\|logout\\|profile\\|shenv\\|shrc\\)\\'"
          . gsmlg-zsh-mode)
         ("\\.\\(?:zsh\\|zsh-theme\\|zsh-template\\|zshrc\\)\\'"
          . gsmlg-zsh-mode)
         ("\\(?:\\`\\|/\\)\\.?bash\\(?:_login\\|_logout\\|_profile\\|rc\\)\\'"
          . gsmlg-shell-mode)
         ("\\(?:\\`\\|/\\)bash\\.bashrc\\'" . gsmlg-shell-mode)
         ("\\(?:\\`\\|/\\)\\.?\\(?:profile\\|shrc\\)\\'"
          . gsmlg-posix-shell-mode)
         ("\\.\\(?:sh\\|bash\\)\\'" . gsmlg-shell-mode)
         ("\\(?:\\`\\|/\\)oh-my-zsh\\.sh\\'" . gsmlg-zsh-mode)
         ("\\.rb\\'" . gsmlg-ruby-mode)
         ("\\.py\\'" . gsmlg-python-mode)))
    (gsmlg-auto-mode-prepend entry)))

(gsmlg-lang-scripting-register-auto-modes)

(provide 'gsmlg-lang-scripting)
;;; gsmlg-lang-scripting.el ends here
