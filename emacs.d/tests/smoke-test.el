;;; smoke-test.el --- End-to-end startup assertions -*- lexical-binding: t; -*-

;;; Commentary:
;; These tests run after the installed configuration and startup hooks load.

;;; Code:

(require 'test-helper)
(require 'seq)

(defvar corfu-terminal-mode)

(defun gsmlg-test-path-below-p (path directory)
  "Return non-nil when PATH is located below DIRECTORY."
  (file-in-directory-p
   (file-truename (expand-file-name path))
   (file-truename (file-name-as-directory directory))))

(ert-deftest gsmlg-smoke-core-modes-are-enabled ()
  "Core built-in and completion modes should be active after startup."
  (should (bound-and-true-p gsmlg-early-init-loaded))
  (should (bound-and-true-p savehist-mode))
  (should (bound-and-true-p save-place-mode))
  (should (bound-and-true-p recentf-mode))
  (should (bound-and-true-p repeat-mode))
  (should (bound-and-true-p vertico-mode))
  (should (bound-and-true-p marginalia-mode))
  (should (bound-and-true-p global-corfu-mode))
  (when (not (display-graphic-p))
    (should (bound-and-true-p corfu-terminal-mode))))

(ert-deftest gsmlg-smoke-corfu-terminal-follows-frame-type ()
  "Terminal rendering should be enabled for TTY frames, not GUI frames."
  (let (arguments)
    (cl-letf (((symbol-function #'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function #'corfu-terminal-mode)
               (lambda (argument)
                 (push argument arguments))))
      (gsmlg-corfu-terminal-update)
      (should-not arguments))
    (cl-letf (((symbol-function #'display-graphic-p)
               (lambda (&optional _frame) nil))
              ((symbol-function #'corfu-terminal-mode)
               (lambda (argument)
                 (push argument arguments))))
      (gsmlg-corfu-terminal-update)
      (should (equal arguments '(1))))))

(ert-deftest gsmlg-smoke-cape-remains-a-low-priority-capf-fallback ()
  "Cape should extend global CAPFs without replacing mode or Eglot CAPFs."
  (let ((global-capfs (default-value 'completion-at-point-functions)))
    (should (memq #'cape-file global-capfs))
    (should (memq #'cape-dabbrev global-capfs))
    (should (< (cl-position #'cape-file global-capfs)
               (cl-position #'cape-dabbrev global-capfs))))
  (with-temp-buffer
    (setq-local completion-at-point-functions
                '(eglot-completion-at-point mode-completion-at-point t))
    (should (equal (seq-take completion-at-point-functions 2)
                   '(eglot-completion-at-point mode-completion-at-point)))))

(ert-deftest gsmlg-smoke-theme-is-enabled ()
  "Duskmoon Moonlight should be loaded without runtime installation."
  (should (memq 'duskmoon-moonlight custom-enabled-themes)))

(ert-deftest gsmlg-smoke-batch-opens-no-listeners ()
  "Batch startup must not create Emacs server or Agent MCP listeners."
  (when (featurep 'server)
    (should-not (server-running-p)))
  (when (fboundp 'emacs-agent-editor-running-p)
    (should-not (emacs-agent-editor-running-p))))

(ert-deftest gsmlg-smoke-state-stays-outside-configuration ()
  "All mutable roots and generated native code should stay outside Git."
  (dolist (directory (list gsmlg-data-directory
                           gsmlg-cache-directory
                           gsmlg-state-directory))
    (should-not
     (file-in-directory-p (file-truename directory)
                          (file-truename gsmlg-config-directory))))
  (should-not
   (directory-files-recursively gsmlg-config-directory "\\.el[cn]\\'")))

(ert-deftest gsmlg-smoke-effective-mutable-paths-follow-xdg-roots ()
  "Every configured mutable file should use its data, cache, or state root."
  (dolist (path (list elpaca-directory
                      elpaca-builds-directory
                      elpaca-sources-directory))
    (should (gsmlg-test-path-below-p path gsmlg-data-directory)))
  (dolist (path (list elpaca-cache-directory
                      url-configuration-directory
                      auto-save-list-file-prefix
                      tramp-auto-save-directory
                      org-persist-directory))
    (should (gsmlg-test-path-below-p path gsmlg-cache-directory)))
  (dolist (path (list custom-file
                      savehist-file
                      save-place-file
                      recentf-save-file
                      bookmark-default-file
                      project-list-file
                      tramp-persistency-file-name
                      transient-levels-file
                      transient-values-file
                      transient-history-file
                      desktop-dirname
                      eshell-directory-name
                      nsm-settings-file
                      mc/list-file
                      org-clock-persist-file
                      org-id-locations-file
                      server-auth-dir
                      server-socket-dir
                      emacs-agent-editor-state-directory))
    (should (gsmlg-test-path-below-p path gsmlg-state-directory))))

(ert-deftest gsmlg-smoke-empty-xdg-values-use-home-fallbacks ()
  "Empty XDG environment values should behave as unset values."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "XDG_CONFIG_HOME" "")
    (setenv "XDG_DATA_HOME" "")
    (setenv "XDG_CACHE_HOME" "")
    (setenv "XDG_STATE_HOME" "")
    (should
     (equal (gsmlg-paths--xdg-directory
             "XDG_DATA_HOME" ".local/share")
            (file-name-as-directory
             (expand-file-name "~/.local/share/emacs"))))
    (should
     (equal (gsmlg-early-init--xdg-base "XDG_CACHE_HOME" ".cache")
            (expand-file-name "~/.cache")))
    (should-error
     (let ((process-environment (copy-sequence process-environment)))
       (setenv "XDG_STATE_HOME" "relative/state")
       (gsmlg-paths--xdg-directory
        "XDG_STATE_HOME" ".local/state")))))

(ert-deftest gsmlg-smoke-desktop-policy-runs-after-local-overrides ()
  "The startup-time desktop policy should honor a late local override."
  (let ((noninteractive nil)
        (gsmlg-desktop-save-enabled t)
        enabled)
    (cl-letf (((symbol-function #'desktop-save-mode)
               (lambda (argument)
                 (setq enabled argument))))
      (gsmlg-session-apply-desktop-policy))
    (should (= enabled 1))))

(ert-deftest gsmlg-smoke-lock-file-is-present ()
  "The committed Elpaca lock file should be configured and readable."
  (should (equal (file-truename elpaca-lock-file)
                 (file-truename
                  (expand-file-name "elpaca-lock.el"
                                    gsmlg-config-directory))))
  (should (file-readable-p elpaca-lock-file)))

(ert-deftest gsmlg-smoke-archive-packages-retain-exact-lock-refs ()
  "Immutable source archives must retain their exact commit refs."
  (dolist (id '(corfu-terminal diff-hl erlang git-timemachine popon zig-mode))
    (let* ((package (elpaca-get id))
           (recipe (and package (elpaca<-recipe package)))
           (revision (plist-get recipe :ref)))
      (should package)
      (should (eq (plist-get recipe :type) 'tar))
      (should (string-match-p
               "\\`[[:xdigit:]]\\{40\\}\\'" revision))
      (should (equal (elpaca-ref package) revision)))))

(provide 'smoke-test)
;;; smoke-test.el ends here
