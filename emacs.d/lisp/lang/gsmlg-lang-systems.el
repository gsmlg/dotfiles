;;; gsmlg-lang-systems.el --- C, C++, Rust, Go, and Zig -*- lexical-binding: t; -*-

;;; Commentary:
;; Systems-language extension dispatch with tree-sitter fallbacks.

;;; Code:

(require 'cc-mode)
(require 'gsmlg-treesit)

;;;###autoload
(defun gsmlg-c-mode ()
  "Select C tree-sitter mode or built-in `c-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'c #'c-ts-mode #'c-mode))

;;;###autoload
(defun gsmlg-c++-mode ()
  "Select C++ tree-sitter mode or built-in `c++-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'cpp #'c++-ts-mode #'c++-mode))

;;;###autoload
(defun gsmlg-rust-mode ()
  "Select Rust tree-sitter mode or maintained `rust-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'rust #'rust-ts-mode #'rust-mode))

;;;###autoload
(defun gsmlg-go-mode ()
  "Select Go tree-sitter mode or maintained `go-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'go #'go-ts-mode #'go-mode))

(defun gsmlg-lang-systems-register-auto-modes ()
  "Place GSMLG systems-language dispatchers ahead of package entries."
  (dolist
      (entry
       '(("\\.zig\\'" . zig-mode)
         ("\\.go\\'" . gsmlg-go-mode)
         ("\\.rs\\'" . gsmlg-rust-mode)
         ("\\.\\(?:cc\\|cpp\\|cxx\\|hh\\|hpp\\|hxx\\)\\'"
          . gsmlg-c++-mode)
         ("\\.c\\'" . gsmlg-c-mode)))
    (gsmlg-auto-mode-prepend entry)))

(gsmlg-lang-systems-register-auto-modes)

(provide 'gsmlg-lang-systems)
;;; gsmlg-lang-systems.el ends here
