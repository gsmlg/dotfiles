;;; init.el --- GSMLG Emacs configuration orchestrator -*- lexical-binding: t; -*-

;;; Commentary:
;; Load responsibility-based modules in explicit dependency order.  Core
;; modules are required synchronously.  Application modules are registered by
;; `gsmlg-apps' and activate through autoloads, hooks, or explicit commands.

;;; Code:

(when (version< emacs-version "30.2")
  (error "GSMLG Emacs requires GNU Emacs 30.2 or newer; found %s"
         emacs-version))

(defconst gsmlg-init-directory
  (file-name-as-directory
   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the active GSMLG Emacs configuration.")

(dolist (directory '("lisp" "lisp/lang" "site-lisp/agent-editor-mcp"
                     "site-lisp/org-note"))
  (add-to-list 'load-path (expand-file-name directory gsmlg-init-directory)))

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)
(require 'gsmlg-package-lock)

(require 'gsmlg-core)
(require 'gsmlg-server)
(require 'gsmlg-session)
(require 'gsmlg-ui)
(require 'gsmlg-completion)
(require 'gsmlg-editing)
(require 'gsmlg-tramp)
(require 'gsmlg-project)
(require 'gsmlg-vcs)
(require 'gsmlg-language-registry)
(require 'gsmlg-language-tools)
(require 'gsmlg-treesit)
(require 'gsmlg-eglot)
(require 'gsmlg-format)
(require 'gsmlg-lang-packages)
(require 'gsmlg-app-packages)
;; Agent Editor MCP is a core Server capability; load with the interactive
;; server path rather than deferring until the first MCP request or frame.
(require 'gsmlg-agent)

;; Application modules (Org, Elfeed, Dape, language dispatch) register
;; autoloads and hooks here instead of loading on every startup.
(require 'gsmlg-apps)

;; Complete the configuration package phase before installing bindings that
;; assert package keymaps and commands.
(gsmlg-bootstrap-wait)
(gsmlg-package-lock-install-archive-ref-method)
(require 'gsmlg-keybindings)

(when (file-readable-p custom-file)
  (load custom-file nil 'nomessage))

(when (file-readable-p gsmlg-local-file)
  (load gsmlg-local-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
