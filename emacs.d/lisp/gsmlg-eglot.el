;;; gsmlg-eglot.el --- Eglot, Flymake, formatting, and tree-sitter -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure built-in language intelligence without installing external
;; servers.  Executable discovery follows `default-directory', so TRAMP
;; buffers resolve and launch tools remotely.

;;; Code:

(require 'cl-lib)
(require 'gsmlg-bootstrap)
(require 'eglot)
(require 'flymake)
(require 'project)
(require 'treesit)

(declare-function apheleia-format-buffer "apheleia" ())
(defvar apheleia-mode-alist)
(defvar sh-shell)

(defcustom gsmlg-eglot-command-overrides nil
  "Alist mapping language symbols to explicit language-server command lists.

Commands are executed from the current project root in its local or TRAMP
environment.  Relative program paths are resolved from that root.  A
TRAMP-qualified program must name the same remote connection as the project;
it is normalized to its remote-local name before Eglot launches it.  Useful
keys include `elixir', `erlang', `typescript', `rust', `go', `python',
`ruby', `zig', `nix', `c-cpp', `yaml', `docker', and `terraform'."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'gsmlg)

(defcustom gsmlg-eglot-auto-start t
  "Whether supported project buffers should start an available server."
  :type 'boolean
  :group 'gsmlg)

(defconst gsmlg-eglot-supported-modes
  '(elixir-mode elixir-ts-mode heex-ts-mode erlang-mode erlang-ts-mode
    js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode web-mode
    c-mode c-ts-mode c++-mode c++-ts-mode
    rust-mode rust-ts-mode go-mode go-ts-mode zig-mode
    python-mode python-ts-mode ruby-mode ruby-ts-mode
    sh-mode bash-ts-mode
    nix-mode yaml-mode yaml-ts-mode
    dockerfile-mode hcl-mode terraform-mode)
  "Programming modes eligible for guarded Eglot startup.")

(defconst gsmlg-treesit-languages
  '(bash c cpp css elixir erlang heex html javascript json go python ruby rust
    tsx typescript yaml toml)
  "Tree-sitter grammars used by this configuration when available.")

(defvar gsmlg-eglot-unavailable-cache (make-hash-table :test #'equal)
  "Servers already found unavailable in the current Emacs process.")

(defun gsmlg-treesit-ready-p (language)
  "Return non-nil when the grammar for LANGUAGE is installed and usable."
  (and (treesit-available-p)
       (condition-case nil
           (treesit-ready-p language t)
         (error nil))))

(defun gsmlg-treesit-or-fallback (language tree-mode fallback-mode)
  "Activate TREE-MODE for LANGUAGE when ready, otherwise FALLBACK-MODE."
  (funcall
   (if (and (fboundp tree-mode)
            (gsmlg-treesit-ready-p language))
       tree-mode
     fallback-mode)))

(defun gsmlg-auto-mode-prepend (entry)
  "Move auto-mode ENTRY to the front of `auto-mode-alist'."
  (setq auto-mode-alist (cons entry (delete entry auto-mode-alist))))

(defun gsmlg-treesit-report ()
  "Display availability of every tree-sitter grammar used here."
  (interactive)
  (with-help-window "*GSMLG Tree-sitter Report*"
    (princ "GSMLG tree-sitter grammar report\n\n")
    (dolist (language gsmlg-treesit-languages)
      (princ (format "%-12s %s\n"
                     language
                     (if (gsmlg-treesit-ready-p language)
                         "ready"
                       "missing"))))))

(defun gsmlg-treesit-install-language-grammar (language)
  "Explicitly install the tree-sitter grammar for LANGUAGE."
  (interactive
   (list
    (intern
     (completing-read
      "Install grammar: "
      (mapcar #'symbol-name gsmlg-treesit-languages)
      nil t))))
  (treesit-install-language-grammar language))

(defun gsmlg-eglot-find-executable (program)
  "Find PROGRAM in the environment nearest `default-directory'."
  (executable-find program (and (file-remote-p default-directory) t)))

(defun gsmlg-eglot-project-executable (program root)
  "Return project-local PROGRAM below ROOT in an executable command form.

Probe the full local or TRAMP filename.  For a remote project, return a path
relative to ROOT because Eglot launches the server with the remote project
root as `default-directory'; passing a literal TRAMP filename to the remote
shell would not name an executable there."
  (let* ((relative (concat "node_modules/.bin/" program))
         (remote-prefix (file-remote-p root))
         (candidate (expand-file-name relative root)))
    (when (file-executable-p candidate)
      (if remote-prefix
          (concat "./" relative)
        candidate))))

(defun gsmlg-eglot--language ()
  "Return the language-server family for the current buffer."
  (cond
   ((memq major-mode '(elixir-mode elixir-ts-mode heex-ts-mode)) 'elixir)
   ((memq major-mode '(erlang-mode erlang-ts-mode)) 'erlang)
   ((memq major-mode
          '(js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode))
    'typescript)
   ((eq major-mode 'web-mode)
    (cond
     ((and buffer-file-name
           (string-match-p "\\.heex\\'" buffer-file-name))
      'elixir)
     ((and buffer-file-name
           (string-match-p "\\.\\(?:jsx\\|tsx\\)\\'" buffer-file-name))
      'typescript)))
   ((memq major-mode '(rust-mode rust-ts-mode)) 'rust)
   ((memq major-mode '(go-mode go-ts-mode)) 'go)
   ((eq major-mode 'zig-mode) 'zig)
   ((memq major-mode '(python-mode python-ts-mode)) 'python)
   ((memq major-mode '(ruby-mode ruby-ts-mode)) 'ruby)
   ((memq major-mode '(c-mode c-ts-mode c++-mode c++-ts-mode)) 'c-cpp)
   ((eq major-mode 'nix-mode) 'nix)
   ((memq major-mode '(yaml-mode yaml-ts-mode)) 'yaml)
   ((eq major-mode 'dockerfile-mode) 'docker)
   ((memq major-mode '(hcl-mode terraform-mode)) 'terraform)
   ((eq major-mode 'bash-ts-mode) 'shell)
   ((and (eq major-mode 'sh-mode)
         (boundp 'sh-shell)
         (memq sh-shell '(sh bash bash2 dash)))
    'shell)))

(defun gsmlg-eglot--project-root (&optional project)
  "Return root of PROJECT or the current project without prompting."
  (when-let* ((project (or project (project-current nil))))
    (project-root project)))

(defun gsmlg-eglot--available-command (candidates)
  "Return the first executable command list from CANDIDATES."
  (cl-loop for candidate in candidates
           when (gsmlg-eglot-find-executable (car candidate))
           return candidate))

(defun gsmlg-eglot-command-executable-p (program &optional root)
  "Return non-nil when PROGRAM is executable from project ROOT.

ROOT defaults to `default-directory'.  File-name handlers therefore perform
both local and TRAMP probes near the data rather than on the client host."
  (let* ((root (file-name-as-directory (or root default-directory)))
         (root-remote (file-remote-p root))
         (program-remote (file-remote-p program)))
    (cond
     (program-remote
      (and root-remote
           (equal program-remote root-remote)
           (file-executable-p program)))
     ((file-name-directory program)
      (file-executable-p
       (if (and root-remote (file-name-absolute-p program))
           (concat root-remote program)
         (expand-file-name program root))))
     (t
      (let ((default-directory root))
        (gsmlg-eglot-find-executable program))))))

(defun gsmlg-eglot-normalize-override (override root)
  "Validate OVERRIDE from ROOT and return an Eglot-safe command.

For a command explicitly qualified with the same TRAMP connection as ROOT,
strip the TRAMP prefix before handing it to the remote shell."
  (let ((program (car override)))
    (when (gsmlg-eglot-command-executable-p program root)
      (if (file-remote-p program)
          (cons (file-remote-p program 'localname) (cdr override))
        override))))

(defun gsmlg-eglot-server-command (&optional project)
  "Return the preferred server command for current mode and PROJECT."
  (let* ((language (gsmlg-eglot--language))
         (override (alist-get language gsmlg-eglot-command-overrides))
         (root (gsmlg-eglot--project-root project)))
    (if override
        (gsmlg-eglot-normalize-override
         override (or root default-directory))
      (pcase language
        ('elixir
         (gsmlg-eglot--available-command
          '(("expert" "--stdio")
            ("elixir-ls")
            ("language_server.sh"))))
        ('erlang
         (gsmlg-eglot--available-command '(("elp"))))
        ('typescript
         (or
          (when root
            (when-let* ((program
                         (gsmlg-eglot-project-executable
                          "typescript-language-server" root)))
              (list program "--stdio")))
          (gsmlg-eglot--available-command
           '(("typescript-language-server" "--stdio")))))
        ('rust
         (gsmlg-eglot--available-command '(("rust-analyzer"))))
        ('go
         (gsmlg-eglot--available-command '(("gopls"))))
        ('python
         (gsmlg-eglot--available-command
          '(("basedpyright-langserver" "--stdio")
            ("pyright-langserver" "--stdio"))))
        ('ruby
         (gsmlg-eglot--available-command '(("ruby-lsp"))))
        ('zig
         (gsmlg-eglot--available-command '(("zls"))))
        ('nix
         (gsmlg-eglot--available-command '(("nixd") ("nil"))))
        ('c-cpp
         (gsmlg-eglot--available-command '(("clangd"))))
        ('yaml
         (gsmlg-eglot--available-command
          '(("yaml-language-server" "--stdio"))))
        ('docker
         (gsmlg-eglot--available-command
          '(("docker-langserver" "--stdio"))))
        ('terraform
         (gsmlg-eglot--available-command
          '(("terraform-ls" "serve"))))
        ('shell
         (gsmlg-eglot--available-command
          '(("bash-language-server" "start"))))))))

(defun gsmlg-eglot-contact (_interactive project)
  "Return an Eglot contact for PROJECT without local fallback."
  (gsmlg-eglot-server-command project))

(defun gsmlg-eglot--cache-key ()
  "Return an unavailable-server cache key for the current buffer."
  (list major-mode
        (gsmlg-eglot--language)
        (or (gsmlg-eglot--project-root) default-directory)
        (file-remote-p default-directory)))

(defun gsmlg-eglot-ensure-maybe (&optional interactive)
  "Start Eglot when this project buffer has a supported server.

With INTERACTIVE non-nil, explain why no server can be started.  Automatic
calls never prompt repeatedly for unavailable executables."
  (interactive (list t))
  (let ((language (gsmlg-eglot--language))
        (supported (memq major-mode gsmlg-eglot-supported-modes))
        (project (project-current nil)))
    (cond
     ((not (and supported language))
      (when interactive
        (message "Eglot is not configured for %s" major-mode)))
     ((not project)
      (when interactive
        (message "Eglot needs a project for %s" major-mode)))
     ((eglot-managed-p))
     (t
      (let ((key (gsmlg-eglot--cache-key)))
        (when interactive
          (remhash key gsmlg-eglot-unavailable-cache))
        (cond
         ((gethash key gsmlg-eglot-unavailable-cache)
          (when interactive
            (message "No configured %s language server is available near %s"
                     language
                     default-directory)))
         ((gsmlg-eglot-server-command project)
          (remhash key gsmlg-eglot-unavailable-cache)
          (condition-case error-data
              (eglot-ensure)
            (error
             (puthash key t gsmlg-eglot-unavailable-cache)
             (if interactive
                 (user-error "Unable to start Eglot: %s"
                             (error-message-string error-data))
               (message "GSMLG Eglot autostart failed: %s"
                        (error-message-string error-data))))))
         (t
          (puthash key t gsmlg-eglot-unavailable-cache)
          (when interactive
            (message "No configured %s language server is available near %s"
                     language
                     default-directory)))))))))

(defun gsmlg-eglot-auto-start-maybe ()
  "Run guarded Eglot startup when `gsmlg-eglot-auto-start' is enabled."
  (when gsmlg-eglot-auto-start
    (gsmlg-eglot-ensure-maybe)))

(defun gsmlg-eglot-environment-changed ()
  "Clear a negative server lookup after activating a buffer environment."
  (when (memq major-mode gsmlg-eglot-supported-modes)
    (remhash (gsmlg-eglot--cache-key) gsmlg-eglot-unavailable-cache)))

(defun gsmlg-eglot-organize-imports ()
  "Ask the active language server to organize imports."
  (interactive)
  (unless (eglot-managed-p)
    (user-error "The current buffer is not managed by Eglot"))
  (eglot-code-action-organize-imports (point-min) (point-max)))

(defun gsmlg-format-buffer ()
  "Format the buffer through Apheleia, or fall back to active Eglot."
  (interactive)
  (cond
   ((and (require 'apheleia nil t)
         (alist-get major-mode apheleia-mode-alist))
    (apheleia-format-buffer))
   ((eglot-managed-p)
    (eglot-format-buffer))
   (t
    (user-error "No Apheleia formatter or Eglot server is active"))))

(defun gsmlg-eglot-configure-server-programs ()
  "Install GSMLG's remote-aware server command resolver."
  (let ((modes
         '(elixir-mode elixir-ts-mode heex-ts-mode
           erlang-mode erlang-ts-mode
           js-mode js-ts-mode typescript-mode typescript-ts-mode
           tsx-ts-mode web-mode
           rust-mode rust-ts-mode go-mode go-ts-mode zig-mode
           python-mode python-ts-mode ruby-mode ruby-ts-mode
           c-mode c-ts-mode c++-mode c++-ts-mode
           sh-mode bash-ts-mode
           nix-mode yaml-mode yaml-ts-mode
           dockerfile-mode hcl-mode terraform-mode)))
    (dolist (mode modes)
      (setf (alist-get mode eglot-server-programs)
            #'gsmlg-eglot-contact))))

(gsmlg-eglot-configure-server-programs)
(add-hook 'find-file-hook #'gsmlg-eglot-auto-start-maybe 90)
(with-eval-after-load 'envrc
  (add-hook 'envrc-mode-hook #'gsmlg-eglot-environment-changed 90))

(use-package apheleia
  :commands (apheleia-format-buffer))

(use-package dape
  :commands (dape dape-breakpoint-toggle dape-repl))

(provide 'gsmlg-eglot)
;;; gsmlg-eglot.el ends here
