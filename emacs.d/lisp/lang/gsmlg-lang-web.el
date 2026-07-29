;;; gsmlg-lang-web.el --- JavaScript, TypeScript, and web data modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Extension dispatch is non-overlapping and chooses tree-sitter only when its
;; grammar is actually ready.

;;; Code:

(require 'gsmlg-eglot)
(require 'gsmlg-bootstrap)

(defun gsmlg-javascript-mode ()
  "Select JavaScript tree-sitter mode or built-in `js-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'javascript #'js-ts-mode #'js-mode))

(defun gsmlg-jsx-mode ()
  "Select JSX-capable tree-sitter mode or maintained `web-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'tsx #'tsx-ts-mode #'web-mode))

(defun gsmlg-typescript-mode ()
  "Select TypeScript tree-sitter mode or maintained `typescript-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback
   'typescript #'typescript-ts-mode #'typescript-mode))

(defun gsmlg-tsx-mode ()
  "Select TSX tree-sitter mode or maintained `web-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'tsx #'tsx-ts-mode #'web-mode))

(defun gsmlg-json-mode ()
  "Select JSON tree-sitter mode or built-in `js-json-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'json #'json-ts-mode #'js-json-mode))

(defun gsmlg-css-mode ()
  "Select CSS tree-sitter mode or built-in `css-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'css #'css-ts-mode #'css-mode))

(defun gsmlg-html-mode ()
  "Select HTML tree-sitter mode or maintained `web-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'html #'html-ts-mode #'web-mode))

(defun gsmlg-lang-web-register-auto-modes ()
  "Place GSMLG web dispatchers ahead of package-provided entries."
  (dolist
      (entry
       '(("\\.\\(?:html?\\|xhtml\\)\\'" . gsmlg-html-mode)
         ("\\.css\\'" . gsmlg-css-mode)
         ("\\.\\(?:json\\|jsonc\\|json5\\)\\'" . gsmlg-json-mode)
         ("\\.tsx\\'" . gsmlg-tsx-mode)
         ("\\.ts\\'" . gsmlg-typescript-mode)
         ("\\.jsx\\'" . gsmlg-jsx-mode)
         ("\\.\\(?:js\\|mjs\\|cjs\\)\\'" . gsmlg-javascript-mode)))
    (gsmlg-auto-mode-prepend entry)))

(gsmlg-lang-web-register-auto-modes)

(use-package web-mode
  :defer t)

(use-package typescript-mode
  :defer t)

(provide 'gsmlg-lang-web)
;;; gsmlg-lang-web.el ends here
