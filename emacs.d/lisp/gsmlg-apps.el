;;; gsmlg-apps.el --- Deferred application module loader -*- lexical-binding: t; -*-

;;; Commentary:
;; Register autoloads, file associations, and lightweight hooks for application
;; modules without synchronously requiring them on every startup.  Core
;; modules remain required by `init.el'; Org, Org Note, Elfeed, Agent Editor
;; MCP, Dape, and language integrations activate on demand.

;;; Code:

(require 'gsmlg-treesit)

(defconst gsmlg-apps-features
  '(gsmlg-org
    org-note
    gsmlg-elfeed
    gsmlg-agent
    gsmlg-ai
    gsmlg-ai-completion
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

(defun gsmlg-apps--prepare-agent ()
  "Load Agent Editor MCP integration for interactive sessions."
  (unless noninteractive
    (gsmlg-apps-require 'gsmlg-agent)
    (gsmlg-agent-reconcile)))

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
         (gsmlg-zsh-mode gsmlg-lang-scripting)
         (gsmlg-posix-shell-mode gsmlg-lang-scripting)
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
         ("\\(?:\\`\\|/\\)\\.?z\\(?:login\\|logout\\|profile\\|shenv\\|shrc\\)\\'"
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
(autoload 'gsmlg-agent-start "gsmlg-agent" nil t)
(autoload 'gsmlg-agent-stop "gsmlg-agent" nil t)
(autoload 'gsmlg-agent-reconcile "gsmlg-agent" nil t)

(dolist
    (command
     '(org-note-workspaces
       org-note-documents
       org-note-document-open
       org-note-document-create
       org-note-configure-agenda-workspaces
       org-note-configure-queue-workspaces
       org-note-queue
       org-note-agenda
       org-note-events
       org-note-item-context
       org-note-item-dispatch))
  (autoload command "org-note" nil t))

(dolist
    (command
     '(gsmlg-ai-chat
       gsmlg-ai-menu
       gsmlg-ai-ask
       gsmlg-ai-review
       gsmlg-ai-rewrite-region
       gsmlg-ai-edit
       gsmlg-ai-context-show
       gsmlg-ai-context-add-buffer
       gsmlg-ai-context-add-region
       gsmlg-ai-context-add-file
       gsmlg-ai-context-add-project-files
       gsmlg-ai-context-add-dired
       gsmlg-ai-context-clear
       gsmlg-ai-proposal-show
       gsmlg-ai-cancel))
  (autoload command "gsmlg-ai" nil t))

(dolist
    (command
     '(gsmlg-ai-completion-show
       gsmlg-ai-completion-mode
       gsmlg-ai-global-completion-mode
       gsmlg-ai-completion-diagnose))
  (autoload command "gsmlg-ai-completion" nil t))

(with-eval-after-load 'org
  (gsmlg-apps--prepare-org))
(with-eval-after-load 'elfeed
  (gsmlg-apps--prepare-elfeed))
(add-hook 'emacs-lisp-mode-hook #'gsmlg-apps--prepare-elisp)
(add-hook 'after-init-hook #'gsmlg-apps--prepare-agent)

(gsmlg-apps-register-language-autoloads)
(gsmlg-apps-register-language-auto-modes)

(provide 'gsmlg-apps)
;;; gsmlg-apps.el ends here
