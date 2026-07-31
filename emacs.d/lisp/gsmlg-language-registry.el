;;; gsmlg-language-registry.el --- Declarative language capability registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Single source of truth for modes, language-server candidates, project-local
;; executables, tree-sitter grammars, formatters, and debug adapters.  Eglot,
;; format, treesit, and debug modules derive their behavior from this table.

;;; Code:

(require 'cl-lib)

(defvar sh-shell)

(defconst gsmlg-language-registry
  '((elixir
     :modes (elixir-mode elixir-ts-mode heex-ts-mode)
     :web-mode-extensions ("\\.heex\\'")
     :commands (("expert" "--stdio")
                ("elixir-ls")
                ("language_server.sh"))
     :treesit (elixir heex))
    (erlang
     :modes (erlang-mode erlang-ts-mode)
     :commands (("elp"))
     :treesit (erlang))
    (typescript
     :modes (js-mode js-ts-mode typescript-mode typescript-ts-mode
             tsx-ts-mode)
     :web-mode-extensions ("\\.\\(?:jsx\\|tsx\\)\\'")
     :commands (("typescript-language-server" "--stdio"))
     :project-local "typescript-language-server"
     :project-local-args ("--stdio")
     :treesit (javascript typescript tsx))
    (rust
     :modes (rust-mode rust-ts-mode)
     :commands (("rust-analyzer"))
     :treesit (rust))
    (go
     :modes (go-mode go-ts-mode)
     :commands (("gopls"))
     :treesit (go))
    (zig
     :modes (zig-mode)
     :commands (("zls")))
    (python
     :modes (python-mode python-ts-mode)
     :commands (("basedpyright-langserver" "--stdio")
                ("pyright-langserver" "--stdio"))
     :treesit (python))
    (ruby
     :modes (ruby-mode ruby-ts-mode)
     :commands (("ruby-lsp"))
     :treesit (ruby))
    (c-cpp
     :modes (c-mode c-ts-mode c++-mode c++-ts-mode)
     :commands (("clangd"))
     :treesit (c cpp))
    (nix
     :modes (nix-mode)
     :commands (("nixd") ("nil")))
    (yaml
     :modes (yaml-mode yaml-ts-mode)
     :commands (("yaml-language-server" "--stdio"))
     :treesit (yaml))
    (docker
     :modes (dockerfile-mode)
     :commands (("docker-langserver" "--stdio")))
    (terraform
     :modes (hcl-mode terraform-mode)
     :commands (("terraform-ls" "serve")))
    (shell
     :modes (bash-ts-mode)
     :eglot-modes (sh-mode bash-ts-mode)
     :mode-match gsmlg-language-registry--shell-mode-p
     :commands (("bash-language-server" "start"))
     :treesit (bash))
    (css
     :treesit (css))
    (html
     :treesit (html))
    (json
     :treesit (json))
    (toml
     :treesit (toml)))
  "Declarative registry of language tooling capabilities.

Each entry is (LANGUAGE . PLIST).  Recognized keys:

`:modes'
    Major modes that map directly to LANGUAGE for Eglot.
`:web-mode-extensions'
    File-name regexps that select LANGUAGE when `major-mode' is `web-mode'.
`:mode-match'
    Function of no arguments returning non-nil when the current buffer
    matches LANGUAGE beyond `:modes'.
`:commands'
    Candidate language-server command lists probed near `default-directory'.
`:project-local'
    Optional project-local executable name under `node_modules/.bin/'.
`:project-local-args'
    Arguments appended to a resolved project-local executable.
`:treesit'
    Tree-sitter grammar symbols associated with LANGUAGE.
`:formatter'
    Reserved for formatter identity (Apheleia owns mode dispatch today).
`:debug'
    Reserved for debug adapter identity (Dape owns adapter tables today).")

(defun gsmlg-language-registry--shell-mode-p ()
  "Return non-nil when the current buffer is a Bourne-compatible shell."
  (and (eq major-mode 'sh-mode)
       (boundp 'sh-shell)
       (memq sh-shell '(sh bash bash2 dash))))

(defun gsmlg-language-registry-entry (language)
  "Return the registry plist for LANGUAGE, or nil."
  (cdr (assq language gsmlg-language-registry)))

(defun gsmlg-language-registry-languages ()
  "Return every language symbol declared in the registry."
  (mapcar #'car gsmlg-language-registry))

(defun gsmlg-language-registry-supported-modes ()
  "Return major modes eligible for guarded Eglot startup."
  (delete-dups
   (append
    '(web-mode)
    (cl-loop for (_language . props) in gsmlg-language-registry
             append (or (plist-get props :eglot-modes)
                        (plist-get props :modes))))))

(defun gsmlg-language-registry-treesit-languages ()
  "Return every tree-sitter grammar declared in the registry."
  (delete-dups
   (cl-loop for (_language . props) in gsmlg-language-registry
            append (plist-get props :treesit))))

(defun gsmlg-language-registry-eglot-modes ()
  "Return modes that should install the GSMLG Eglot contact resolver."
  (gsmlg-language-registry-supported-modes))

(defun gsmlg-language-registry-language-for-buffer ()
  "Return the language-server family for the current buffer."
  (cl-loop for (language . props) in gsmlg-language-registry
           when (or (memq major-mode (plist-get props :modes))
                    (when-let* ((matcher (plist-get props :mode-match)))
                      (funcall matcher))
                    (and (eq major-mode 'web-mode)
                         buffer-file-name
                         (cl-loop for pattern in
                                  (plist-get props :web-mode-extensions)
                                  thereis (string-match-p
                                           pattern buffer-file-name))))
           return language))

(provide 'gsmlg-language-registry)
;;; gsmlg-language-registry.el ends here
