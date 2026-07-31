;;; gsmlg-lang-infra.el --- Infrastructure and documentation modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Maintained modes for Nix, YAML, Docker, Terraform, Markdown, and common
;; configuration formats, with tree-sitter where Emacs supports it.

;;; Code:

(require 'gsmlg-treesit)

;;;###autoload
(defun gsmlg-yaml-mode ()
  "Select YAML tree-sitter mode or maintained `yaml-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'yaml #'yaml-ts-mode #'yaml-mode))

;;;###autoload
(defun gsmlg-toml-mode ()
  "Select TOML tree-sitter mode or built-in `conf-toml-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'toml #'toml-ts-mode #'conf-toml-mode))

(defun gsmlg-lang-infra-register-auto-modes ()
  "Place GSMLG infrastructure dispatchers ahead of package entries."
  (dolist
      (entry
       '(("\\(?:\\`\\|/\\)\\.\\(?:env\\|editorconfig\\|gitconfig\\)\\'"
          . conf-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.\\(?:tf\\|tfvars\\)\\'" . terraform-mode)
         ("\\.hcl\\'" . hcl-mode)
         ("\\(?:\\`\\|/\\)Dockerfile\\(?:\\..+\\)?\\'" . dockerfile-mode)
         ("\\.toml\\'" . gsmlg-toml-mode)
         ("\\.ya?ml\\'" . gsmlg-yaml-mode)
         ("\\.nix\\'" . nix-mode)))
    (gsmlg-auto-mode-prepend entry)))

(gsmlg-lang-infra-register-auto-modes)

(provide 'gsmlg-lang-infra)
;;; gsmlg-lang-infra.el ends here
