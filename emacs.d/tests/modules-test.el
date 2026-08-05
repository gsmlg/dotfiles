;;; modules-test.el --- Module architecture assertions -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify the new responsibility-based module graph and bootstrap boundaries.

;;; Code:

(require 'test-helper)
(require 'gsmlg-package-maintenance)

(declare-function zlib-decompress-region "zlib"
                  (start end &optional allow-partial))

(defconst gsmlg-test-core-features
  '(gsmlg-paths
    gsmlg-bootstrap
    gsmlg-package-lock
    gsmlg-core
    gsmlg-ui
    gsmlg-completion
    gsmlg-editing
    gsmlg-keybindings
    gsmlg-project
    gsmlg-vcs
    gsmlg-language-registry
    gsmlg-language-tools
    gsmlg-treesit
    gsmlg-eglot
    gsmlg-format
    gsmlg-lang-packages
    gsmlg-app-packages
    gsmlg-apps
    gsmlg-tramp
    gsmlg-session)
  "Features every warm core startup must provide.")

(defconst gsmlg-test-application-features
  '(gsmlg-org
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

(defconst gsmlg-test-required-features
  (append gsmlg-test-core-features
          gsmlg-test-application-features
          '(gsmlg-package-maintenance))
  "Features every module must remain requireable.")

(ert-deftest gsmlg-modules-core-are-loaded-at-startup ()
  "Core features should already be present after orchestrated startup."
  (dolist (feature gsmlg-test-core-features)
    (should (featurep feature))))

(ert-deftest gsmlg-modules-application-are-deferred-from-init ()
  "Application modules must not be synchronously required by init.el."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "init.el" gsmlg-config-directory))
    (dolist (feature gsmlg-test-application-features)
      (goto-char (point-min))
      (should-not
       (re-search-forward
        (format "(require '%s)" (regexp-quote (symbol-name feature)))
        nil t)))))

(ert-deftest gsmlg-modules-maintenance-is-not-on-startup-path ()
  "Package maintenance must stay off the normal startup require graph."
  (dolist (file (list (expand-file-name "init.el" gsmlg-config-directory)
                      (expand-file-name "lisp/gsmlg-bootstrap.el"
                                        gsmlg-config-directory)
                      (expand-file-name "lisp/gsmlg-package-lock.el"
                                        gsmlg-config-directory)
                      (expand-file-name "lisp/gsmlg-apps.el"
                                        gsmlg-config-directory)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should-not
       (re-search-forward
        "(require 'gsmlg-package-maintenance)"
        nil t)))))

(ert-deftest gsmlg-modules-can-be-required ()
  "Every first-party feature should remain requireable."
  (dolist (feature gsmlg-test-required-features)
    (should (require feature nil t))))

(ert-deftest gsmlg-modules-removed-stack-is-absent-from-active-source ()
  "Removed package names should occur only in migration docs or absence tests."
  (let ((removed '("helm" "company" "flycheck" "lsp-mode" "lsp-ui"
                   "helm-lsp" "projectile" "git-gutter" "hydra" "spaceline"
                   "all-the-icons" "undo-tree" "alchemist" "tern" "js2-mode"
                   "rjsx-mode" "js2-refactor"))
        (files
         (append
          (list (expand-file-name "early-init.el" gsmlg-config-directory)
                (expand-file-name "init.el" gsmlg-config-directory))
          (directory-files-recursively
           (expand-file-name "lisp" gsmlg-config-directory) "\\.el\\'")
          (directory-files-recursively
           (expand-file-name "site-lisp/agent-editor-mcp"
                             gsmlg-config-directory)
           "\\.el\\'"))))
    (dolist (file files)
      (unless (string-match-p "/tests?/" file)
        (with-temp-buffer
          (insert-file-contents file)
          (let ((case-fold-search t))
            (dolist (name removed)
              (goto-char (point-min))
              (should-not
               (re-search-forward
                (format "\\_<%s\\_>" (regexp-quote name))
                nil t)))))))))

(ert-deftest gsmlg-modules-do-not-load-legacy-features ()
  "No old init feature should remain active."
  (dolist (feature features)
    (should-not (string-prefix-p "init-" (symbol-name feature)))))

(ert-deftest gsmlg-modules-use-only-explicit-load-paths ()
  "Only the exact local package directory should be on `load-path'."
  (let ((site-root
         (file-truename
          (expand-file-name "site-lisp" gsmlg-config-directory)))
        (agent-root
         (file-truename
          (expand-file-name "site-lisp/agent-editor-mcp"
                            gsmlg-config-directory)))
        (canonical-load-path
         (mapcar #'file-truename
                 (mapcar #'directory-file-name load-path))))
    (should (member agent-root canonical-load-path))
    (should-not (member site-root canonical-load-path))))

(ert-deftest gsmlg-modules-do-not-activate-package-bootstrap ()
  "The active configuration must not contain package.el bootstrap calls."
  (should-not package-enable-at-startup)
  (dolist (file (cons (expand-file-name "init.el" gsmlg-config-directory)
                      (directory-files-recursively
                       (expand-file-name "lisp" gsmlg-config-directory)
                       "\\.el\\'")))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (name '("package-initialize"
                      "package-refresh-contents"
                      "package-install"))
        (goto-char (point-min))
        (should-not (search-forward name nil t))))))

(ert-deftest gsmlg-bootstrap-update-prefers-origin-head ()
  "Package maintenance should use an existing origin tracking HEAD."
  (cl-letf (((symbol-function #'gsmlg-bootstrap--git-output)
             (lambda (_directory &rest _arguments)
               "origin/main")))
    (should
     (equal (gsmlg-bootstrap--remote-default-target "/ignored/")
            "origin/main"))))

(ert-deftest gsmlg-bootstrap-update-resolves-missing-origin-head ()
  "Package maintenance should resolve a remote whose tracking HEAD is absent."
  (let ((calls 0))
    (cl-letf (((symbol-function #'gsmlg-bootstrap--git-output)
               (lambda (_directory &rest _arguments)
                 (setq calls (1+ calls))
                 (if (= calls 1)
                     (error "No refs/remotes/origin/HEAD")
                   (concat "ref: refs/heads/trunk\tHEAD\n"
                           "0123456789012345678901234567890123456789\tHEAD")))))
      (should
       (equal (gsmlg-bootstrap--remote-default-target "/ignored/")
              "origin/trunk"))
      (should (= calls 2)))))

(ert-deftest gsmlg-bootstrap-update-respects-a-recipe-branch ()
  "Package maintenance should retain a package's configured upstream branch."
  (cl-letf (((symbol-function #'gsmlg-bootstrap--remote-default-target)
             (lambda (&rest _arguments)
               (ert-fail "A configured branch should bypass origin/HEAD"))))
    (should
     (equal
      (gsmlg-bootstrap--package-update-target
       "/ignored/" '(:branch "externals/vlf"))
      "origin/externals/vlf"))))

(ert-deftest gsmlg-bootstrap-update-rejects-immutable-archives-before-git ()
  "Archive updates should give exact-ref guidance without invoking Git."
  (cl-letf (((symbol-function #'gsmlg-bootstrap--git-output)
             (lambda (&rest _arguments)
               (ert-fail "Archive update attempted a Git operation"))))
    (should-error
     (gsmlg-elpaca-update-package 'diff-hl)
     :type 'user-error)))

(ert-deftest gsmlg-bootstrap-archive-revision-verifies-header-and-pin ()
  "Archive revision discovery should parse and enforce the exact recipe ref."
  (let* ((root (make-temp-file "gsmlg-archive-ref-" t))
         (revision "0123456789abcdef0123456789abcdef01234567")
         (other "89abcdef0123456789abcdef0123456789abcdef")
         (header (expand-file-name "pax_global_header" root))
         (package (copy-sequence (elpaca-get 'diff-hl))))
    (unwind-protect
        (progn
          (with-temp-file header
            (insert "52 comment=" revision "\n"))
          (should
           (equal
            (gsmlg-bootstrap--archive-header-revision root)
            revision))
          (setf (elpaca<-source-dir package) root
                (elpaca<-recipe package)
                `(:type tar :ref ,other))
          (should-error
           (gsmlg-bootstrap-source-revision package)))
      (delete-directory root t))))

(ert-deftest gsmlg-bootstrap-subprocess-can-decompress-without-emacs-zlib ()
  "Archive workers should use gzip when Emacs lacks built-in zlib support."
  (skip-unless (executable-find "gzip"))
  (let ((original-function
         (and (fboundp #'zlib-decompress-region)
              (symbol-function #'zlib-decompress-region)))
        (gc-cons-percentage gc-cons-percentage)
        (print-circle print-circle)
        (print-level print-level))
    (unwind-protect
        (progn
          (fmakunbound #'zlib-decompress-region)
          (eval elpaca-with-emacs-env-form t)
          (should (fboundp #'zlib-decompress-region))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert "locked archive\n")
            (should
             (zerop
              (call-process-region
               (point-min) (point-max) "gzip" t t nil "-c")))
            (zlib-decompress-region (point-min) (point-max))
            (should (equal (buffer-string) "locked archive\n"))))
      (if original-function
          (fset #'zlib-decompress-region original-function)
        (fmakunbound #'zlib-decompress-region)))))

(ert-deftest gsmlg-modules-all-use-lexical-binding ()
  "Every first-party Emacs Lisp file should enable lexical binding."
  (dolist (file
           (append
            (list (expand-file-name "early-init.el" gsmlg-config-directory)
                  (expand-file-name "init.el" gsmlg-config-directory))
            (directory-files-recursively
             (expand-file-name "lisp" gsmlg-config-directory) "\\.el\\'")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward "lexical-binding: t" (line-end-position) t)))))

(provide 'modules-test)
;;; modules-test.el ends here
