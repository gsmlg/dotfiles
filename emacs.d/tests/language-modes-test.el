;;; language-modes-test.el --- Language dispatch tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise tree-sitter and maintained fallback dispatch without downloading
;; grammars.

;;; Code:

(require 'test-helper)
(require 'gsmlg-lang-beam)
(require 'gsmlg-lang-web)
(require 'gsmlg-lang-systems)
(require 'gsmlg-lang-scripting)
(require 'gsmlg-lang-infra)

(declare-function native--compile-async-skip-p
                  "comp-run" (file load selector))

(defun gsmlg-test-select-major-mode (mode)
  "Select MODE without requiring its tree-sitter grammar."
  (setq major-mode mode))

(ert-deftest gsmlg-language-missing-grammars-use-fallbacks ()
  "Representative extensions should choose maintained fallback modes."
  (cl-letf (((symbol-function #'gsmlg-treesit-ready-p)
             (lambda (_language) nil)))
    (dolist (case '(("sample.ex" . elixir-mode)
                    ("sample.exs" . elixir-mode)
                    ("sample.heex" . web-mode)
                    ("sample.erl" . erlang-mode)
                    ("sample.js" . js-mode)
                    ("sample.jsx" . web-mode)
                    ("sample.ts" . typescript-mode)
                    ("sample.tsx" . web-mode)
                    ("sample.json" . js-json-mode)
                    ("sample.css" . css-mode)
                    ("sample.html" . web-mode)
                    ("sample.c" . c-mode)
                    ("sample.cpp" . c++-mode)
                    ("sample.rs" . rust-mode)
                    ("sample.go" . go-mode)
                    ("sample.zig" . zig-mode)
                    ("sample.py" . python-mode)
                    ("sample.rb" . ruby-mode)
                    ("sample.sh" . sh-mode)
                    ("sample.zsh" . sh-mode)
                    ("sample.nix" . nix-mode)
                    ("sample.yaml" . yaml-mode)
                    ("sample.toml" . conf-toml-mode)
                    ("Dockerfile" . dockerfile-mode)
                    ("sample.hcl" . hcl-mode)
                    ("sample.tf" . terraform-mode)
                    ("README.md" . gfm-mode)))
      (should (eq (gsmlg-test-mode-for-file (car case)) (cdr case))))))

(ert-deftest gsmlg-language-ready-grammars-use-tree-sitter ()
  "Available representative grammars should select tree-sitter modes."
  (cl-letf (((symbol-function #'gsmlg-treesit-ready-p)
             (lambda (_language) t))
            ((symbol-function 'elixir-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'elixir-ts-mode))
            ((symbol-function 'heex-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'heex-ts-mode))
            ((symbol-function 'erlang-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'erlang-ts-mode))
            ((symbol-function 'js-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'js-ts-mode))
            ((symbol-function 'tsx-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'tsx-ts-mode))
            ((symbol-function 'typescript-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode
                              'typescript-ts-mode))
            ((symbol-function 'json-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'json-ts-mode))
            ((symbol-function 'css-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'css-ts-mode))
            ((symbol-function 'html-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'html-ts-mode))
            ((symbol-function 'c-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'c-ts-mode))
            ((symbol-function 'c++-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'c++-ts-mode))
            ((symbol-function 'rust-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'rust-ts-mode))
            ((symbol-function 'go-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'go-ts-mode))
            ((symbol-function 'python-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'python-ts-mode))
            ((symbol-function 'ruby-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'ruby-ts-mode))
            ((symbol-function 'bash-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'bash-ts-mode))
            ((symbol-function 'yaml-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'yaml-ts-mode))
            ((symbol-function 'toml-ts-mode)
             (apply-partially #'gsmlg-test-select-major-mode 'toml-ts-mode)))
    (dolist (case '(("sample.ex" . elixir-ts-mode)
                    ("sample.heex" . heex-ts-mode)
                    ("sample.erl" . erlang-ts-mode)
                    ("sample.js" . js-ts-mode)
                    ("sample.jsx" . tsx-ts-mode)
                    ("sample.ts" . typescript-ts-mode)
                    ("sample.tsx" . tsx-ts-mode)
                    ("sample.json" . json-ts-mode)
                    ("sample.css" . css-ts-mode)
                    ("sample.html" . html-ts-mode)
                    ("sample.c" . c-ts-mode)
                    ("sample.cpp" . c++-ts-mode)
                    ("sample.rs" . rust-ts-mode)
                    ("sample.go" . go-ts-mode)
                    ("sample.py" . python-ts-mode)
                    ("sample.rb" . ruby-ts-mode)
                    ("sample.sh" . bash-ts-mode)
                    ("sample.zsh" . sh-mode)
                    ("sample.yaml" . yaml-ts-mode)
                    ("sample.toml" . toml-ts-mode)))
      (should (eq (gsmlg-test-mode-for-file (car case)) (cdr case))))))

(ert-deftest gsmlg-language-eglot-hook-covers-fallback-and-tree-modes ()
  "Both language dispatch paths should reach the guarded Eglot hook."
  (should (memq #'gsmlg-eglot-auto-start-maybe find-file-hook))
  (should-not (memq #'gsmlg-eglot-auto-start-maybe prog-mode-hook))
  (should envrc-remote)
  (dolist (mode '(elixir-mode elixir-ts-mode heex-ts-mode
                  erlang-mode erlang-ts-mode
                  js-mode js-ts-mode web-mode typescript-mode
                  typescript-ts-mode tsx-ts-mode
                  c-mode c-ts-mode c++-mode c++-ts-mode
                  rust-mode rust-ts-mode go-mode go-ts-mode zig-mode
                  python-mode python-ts-mode ruby-mode ruby-ts-mode
                  sh-mode bash-ts-mode nix-mode yaml-mode yaml-ts-mode
                  dockerfile-mode hcl-mode terraform-mode))
    (should (memq mode gsmlg-eglot-supported-modes))))

(ert-deftest gsmlg-language-go-mode-skips-native-jit-until-upstream-fix ()
  "Upstream go-mode should remain byte compiled without native JIT warnings."
  (skip-unless (native-comp-available-p))
  (require 'comp-run)
  (let* ((package (elpaca-get 'go-mode))
         (source
          (and package
               (expand-file-name "go-mode.el"
                                 (elpaca<-build-dir package)))))
    (should package)
    (should (file-readable-p source))
    (should
     (native--compile-async-skip-p source 'late nil))))

(ert-deftest gsmlg-language-eglot-autostart-honors-local-policy ()
  "A local override can disable automatic Eglot startup."
  (let ((gsmlg-eglot-auto-start nil)
        called)
    (cl-letf (((symbol-function #'gsmlg-eglot-ensure-maybe)
               (lambda (&optional _interactive)
                 (setq called t))))
      (gsmlg-eglot-auto-start-maybe)
      (should-not called)))
  (let ((gsmlg-eglot-auto-start t)
        called)
    (cl-letf (((symbol-function #'gsmlg-eglot-ensure-maybe)
               (lambda (&optional _interactive)
                 (setq called t))))
      (gsmlg-eglot-auto-start-maybe)
      (should called))))

(ert-deftest gsmlg-language-shell-eglot-excludes-zsh ()
  "The Bash language server must not be selected for a Zsh buffer."
  (let ((major-mode 'sh-mode)
        (sh-shell 'zsh))
    (should-not (gsmlg-eglot--language)))
  (let ((major-mode 'sh-mode)
        (sh-shell 'sh))
    (should (eq (gsmlg-eglot--language) 'shell)))
  (let ((major-mode 'bash-ts-mode))
    (should (eq (gsmlg-eglot--language) 'shell))))

(ert-deftest gsmlg-language-shell-eglot-cache-is-dialect-safe ()
  "A Zsh visit must not suppress Bash Eglot in the same project."
  (let* ((root (make-temp-file "gsmlg-shell-server-" t))
         (default-directory root)
         (major-mode 'sh-mode)
         (sh-shell 'zsh)
         (project `(transient . ,root))
         looked-up
         ensured)
    (unwind-protect
        (progn
          (clrhash gsmlg-eglot-unavailable-cache)
          (cl-letf (((symbol-function #'project-current)
                     (lambda (&rest _) project))
                    ((symbol-function #'eglot-managed-p)
                     (lambda () nil))
                    ((symbol-function #'gsmlg-eglot-find-executable)
                     (lambda (program)
                       (setq looked-up program)
                       "/usr/bin/bash-language-server"))
                    ((symbol-function #'eglot-ensure)
                     (lambda ()
                       (setq ensured t))))
            (should-not (gsmlg-eglot-ensure-maybe))
            (should-not looked-up)
            (should (= (hash-table-count gsmlg-eglot-unavailable-cache) 0))
            (setq sh-shell 'bash)
            (gsmlg-eglot-ensure-maybe)
            (should (equal looked-up "bash-language-server"))
            (should ensured)))
      (clrhash gsmlg-eglot-unavailable-cache)
      (delete-directory root t))))

(ert-deftest gsmlg-language-web-eglot-cache-separates-server-families ()
  "HEEx and TSX buffers in one project must not share a negative cache key."
  (let ((default-directory "/project/")
        (major-mode 'web-mode)
        (buffer-file-name "/project/view.heex"))
    (let ((heex-key (gsmlg-eglot--cache-key)))
      (setq buffer-file-name "/project/component.tsx")
      (should-not (equal heex-key (gsmlg-eglot--cache-key))))))

(ert-deftest gsmlg-language-eglot-relative-override-uses-project-root ()
  "A relative override is validated where Eglot will launch it."
  (let ((default-directory "/project/src/")
        (major-mode 'python-mode)
        (project '(transient . "/project/"))
        (gsmlg-eglot-command-overrides
         '((python . ("./bin/server" "--stdio"))))
        probed)
    (cl-letf (((symbol-function #'file-executable-p)
               (lambda (file)
                 (setq probed file)
                 t)))
      (should
       (equal (gsmlg-eglot-server-command project)
              '("./bin/server" "--stdio")))
      (should (equal probed "/project/bin/server")))))

(ert-deftest gsmlg-language-missing-server-is-cached-without-prompting ()
  "Unavailable servers should neither prompt nor retry on every hook run."
  (let* ((root (make-temp-file "gsmlg-no-server-" t))
         (default-directory root)
         (major-mode 'python-mode)
         (project `(transient . ,root))
         (lookups 0)
         manual-message)
    (unwind-protect
        (progn
          (clrhash gsmlg-eglot-unavailable-cache)
          (cl-letf (((symbol-function #'project-current)
                     (lambda (&rest _) project))
                    ((symbol-function #'eglot-managed-p)
                     (lambda () nil))
                    ((symbol-function #'gsmlg-eglot-server-command)
                     (lambda (&optional _project)
                       (setq lookups (1+ lookups))
                       nil))
                    ((symbol-function #'eglot-ensure)
                     (lambda ()
                       (ert-fail "Eglot started without a server")))
                    ((symbol-function #'yes-or-no-p)
                     (lambda (&rest _)
                       (ert-fail "Unavailable servers must not prompt")))
                    ((symbol-function #'message)
                     (lambda (format-string &rest arguments)
                       (setq manual-message
                             (apply #'format format-string arguments)))))
            (should-not (gsmlg-eglot-ensure-maybe))
            (should-not (gsmlg-eglot-ensure-maybe))
            (gsmlg-eglot-ensure-maybe t))
          (should (= lookups 2))
          (should (string-match-p
                   "No configured python language server"
                   manual-message)))
      (clrhash gsmlg-eglot-unavailable-cache)
      (delete-directory root t))))

(ert-deftest gsmlg-language-environment-change-clears-negative-cache ()
  "A newly activated project environment should permit another server probe."
  (let* ((root (make-temp-file "gsmlg-env-server-" t))
         (default-directory root)
         (major-mode 'python-mode)
         (project `(transient . ,root)))
    (unwind-protect
        (progn
          (clrhash gsmlg-eglot-unavailable-cache)
          (cl-letf (((symbol-function #'project-current)
                     (lambda (&rest _) project)))
            (puthash (gsmlg-eglot--cache-key)
                     t gsmlg-eglot-unavailable-cache)
            (gsmlg-eglot-environment-changed)
            (should-not
             (gethash (gsmlg-eglot--cache-key)
                      gsmlg-eglot-unavailable-cache))))
      (clrhash gsmlg-eglot-unavailable-cache)
      (delete-directory root t))))

(ert-deftest gsmlg-language-eglot-autostart-isolates-launch-failures ()
  "A broken server executable should not make visiting a file fail."
  (let* ((root (make-temp-file "gsmlg-broken-server-" t))
         (default-directory root)
         (major-mode 'python-mode)
         (project `(transient . ,root)))
    (unwind-protect
        (progn
          (clrhash gsmlg-eglot-unavailable-cache)
          (cl-letf (((symbol-function #'project-current)
                     (lambda (&rest _) project))
                    ((symbol-function #'eglot-managed-p)
                     (lambda () nil))
                    ((symbol-function #'gsmlg-eglot-server-command)
                     (lambda (&optional _project)
                       '("broken-server")))
                    ((symbol-function #'eglot-ensure)
                     (lambda ()
                       (error "Launch failed")))
                    ((symbol-function #'message)
                     (lambda (&rest _arguments) nil)))
            (should-not (gsmlg-eglot-ensure-maybe))
            (should
             (gethash (gsmlg-eglot--cache-key)
                      gsmlg-eglot-unavailable-cache))))
      (clrhash gsmlg-eglot-unavailable-cache)
      (delete-directory root t))))

(provide 'language-modes-test)
;;; language-modes-test.el ends here
