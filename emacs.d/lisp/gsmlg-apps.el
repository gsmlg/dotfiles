;;; gsmlg-apps.el --- Deferred application module loader -*- lexical-binding: t; -*-

;;; Commentary:
;; Register autoloads, file associations, and lightweight hooks for application
;; modules without synchronously requiring them on every startup.  Core
;; modules remain required by `init.el'; Org, Elfeed, Dape, and language
;; integrations activate on demand.  Agent Editor MCP is required by `init.el'.

;;; Code:

(require 'gsmlg-treesit)

(defconst gsmlg-apps-features
  '(gsmlg-org
    gsmlg-elfeed
    gsmlg-debug
    gsmlg-lang-elisp
    gsmlg-lang-beam
    gsmlg-lang-web
    gsmlg-lang-systems
    gsmlg-lang-scripting
    gsmlg-lang-infra)
  "Application features deferred from the core startup path.")

(defun gsmlg-apps-require (feature)
  "Load deferred application FEATURE and finish any queued package work."
  (unless (featurep feature)
    (require feature)
    (when (fboundp #'gsmlg-bootstrap-wait)
      (gsmlg-bootstrap-wait)))
  feature)

(defun gsmlg-apps--prepare-org ()
  "Load Org configuration when the built-in Org library activates."
  (gsmlg-apps-require 'gsmlg-org))

(defun gsmlg-apps--prepare-elfeed ()
  "Load Elfeed configuration when the Elfeed library activates."
  (gsmlg-apps-require 'gsmlg-elfeed))

(defun gsmlg-apps--prepare-elisp ()
  "Load Emacs Lisp tooling when `emacs-lisp-mode' activates."
  (gsmlg-apps-require 'gsmlg-lang-elisp))

(defun gsmlg-apps-register-language-autoloads ()
  "Autoload language dispatch commands without loading their modules."
  (dolist
      (spec
       '((gsmlg-elixir-mode gsmlg-lang-beam)
         (gsmlg-heex-mode gsmlg-lang-beam)
         (gsmlg-erlang-mode gsmlg-lang-beam)
         (gsmlg-javascript-mode gsmlg-lang-web)
         (gsmlg-jsx-mode gsmlg-lang-web)
         (gsmlg-typescript-mode gsmlg-lang-web)
         (gsmlg-tsx-mode gsmlg-lang-web)
         (gsmlg-json-mode gsmlg-lang-web)
         (gsmlg-css-mode gsmlg-lang-web)
         (gsmlg-html-mode gsmlg-lang-web)
         (gsmlg-c-mode gsmlg-lang-systems)
         (gsmlg-c++-mode gsmlg-lang-systems)
         (gsmlg-rust-mode gsmlg-lang-systems)
         (gsmlg-go-mode gsmlg-lang-systems)
         (gsmlg-python-mode gsmlg-lang-scripting)
         (gsmlg-ruby-mode gsmlg-lang-scripting)
         (gsmlg-shell-mode gsmlg-lang-scripting)
         (gsmlg-yaml-mode gsmlg-lang-infra)
         (gsmlg-toml-mode gsmlg-lang-infra)))
    (autoload (car spec) (symbol-name (cadr spec)) nil t)))

(defun gsmlg-apps-register-language-auto-modes ()
  "Install file associations for deferred language dispatch commands."
  (dolist
      (entry
       '(("\\.\\(?:erl\\|hrl\\)\\'" . gsmlg-erlang-mode)
         ("\\.heex\\'" . gsmlg-heex-mode)
         ("\\.exs?\\'" . gsmlg-elixir-mode)
         ("\\.\\(?:html?\\|xhtml\\)\\'" . gsmlg-html-mode)
         ("\\.css\\'" . gsmlg-css-mode)
         ("\\.\\(?:json\\|jsonc\\|json5\\)\\'" . gsmlg-json-mode)
         ("\\.tsx\\'" . gsmlg-tsx-mode)
         ("\\.ts\\'" . gsmlg-typescript-mode)
         ("\\.jsx\\'" . gsmlg-jsx-mode)
         ("\\.\\(?:js\\|mjs\\|cjs\\)\\'" . gsmlg-javascript-mode)
         ("\\.zig\\'" . zig-mode)
         ("\\.go\\'" . gsmlg-go-mode)
         ("\\.rs\\'" . gsmlg-rust-mode)
         ("\\.\\(?:cc\\|cpp\\|cxx\\|hh\\|hpp\\|hxx\\)\\'" . gsmlg-c++-mode)
         ("\\.c\\'" . gsmlg-c-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.\\(?:sh\\|bash\\)\\'" . gsmlg-shell-mode)
         ("\\.rb\\'" . gsmlg-ruby-mode)
         ("\\.py\\'" . gsmlg-python-mode)
         ("\\(?:\\`\\|/\\)\\.\\(?:env\\|editorconfig\\|gitconfig\\)\\'"
          . conf-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.\\(?:tf\\|tfvars\\)\\'" . terraform-mode)
         ("\\.hcl\\'" . hcl-mode)
         ("\\(?:\\`\\|/\\)Dockerfile\\(?:\\..+\\)?\\'" . dockerfile-mode)
         ("\\.toml\\'" . gsmlg-toml-mode)
         ("\\.ya?ml\\'" . gsmlg-yaml-mode)
         ("\\.nix\\'" . nix-mode)))
    (gsmlg-auto-mode-prepend entry)))

(autoload 'dape "gsmlg-debug" nil t)
(autoload 'dape-breakpoint-toggle "gsmlg-debug" nil t)
(autoload 'dape-repl "gsmlg-debug" nil t)
(autoload 'elfeed "elfeed" nil t)

(with-eval-after-load 'org
  (gsmlg-apps--prepare-org))
(with-eval-after-load 'elfeed
  (gsmlg-apps--prepare-elfeed))
(add-hook 'emacs-lisp-mode-hook #'gsmlg-apps--prepare-elisp)

(gsmlg-apps-register-language-autoloads)
(gsmlg-apps-register-language-auto-modes)

(provide 'gsmlg-apps)
;;; gsmlg-apps.el ends here
