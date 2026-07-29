;;; init.el --- GSMLG Emacs configuration orchestrator -*- lexical-binding: t; -*-

;;; Commentary:
;; Load responsibility-based modules in explicit dependency order.  Package
;; installation and mutable paths are owned by their dedicated modules.

;;; Code:

(when (version< emacs-version "30.2")
  (error "GSMLG Emacs requires GNU Emacs 30.2 or newer; found %s"
         emacs-version))

(defconst gsmlg-init-directory
  (file-name-as-directory
   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the active GSMLG Emacs configuration.")

(dolist (directory '("lisp" "lisp/lang" "site-lisp/agent-editor-mcp"))
  (add-to-list 'load-path (expand-file-name directory gsmlg-init-directory)))

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)

(require 'gsmlg-core)
(require 'gsmlg-session)
(require 'gsmlg-ui)
(require 'gsmlg-completion)
(require 'gsmlg-editing)
(require 'gsmlg-tramp)
(require 'gsmlg-project)
(require 'gsmlg-vcs)
(require 'gsmlg-eglot)

(require 'gsmlg-lang-elisp)
(require 'gsmlg-lang-beam)
(require 'gsmlg-lang-web)
(require 'gsmlg-lang-systems)
(require 'gsmlg-lang-scripting)
(require 'gsmlg-lang-infra)

(require 'gsmlg-org)
(require 'gsmlg-elfeed)
(require 'gsmlg-agent)

;; Complete the configuration package phase before installing bindings that
;; assert package keymaps and commands.
(gsmlg-bootstrap-wait)
(require 'gsmlg-keybindings)
(gsmlg-lang-beam-register-auto-modes)
(gsmlg-lang-web-register-auto-modes)
(gsmlg-lang-systems-register-auto-modes)
(gsmlg-lang-scripting-register-auto-modes)
(gsmlg-lang-infra-register-auto-modes)

(when (file-readable-p custom-file)
  (load custom-file nil 'nomessage))

(when (file-readable-p gsmlg-local-file)
  (load gsmlg-local-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
