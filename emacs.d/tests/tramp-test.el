;;; tramp-test.el --- Compute-near-data unit tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Mock TRAMP paths and process discovery; no live SSH endpoint is required.

;;; Code:

(require 'test-helper)
(require 'gsmlg-tramp)
(require 'gsmlg-eglot)
(require 'gsmlg-format)
(require 'gsmlg-project)

(defvar apheleia-mode-alist)

(ert-deftest gsmlg-tramp-executable-lookup-is-remote ()
  "Remote Eglot discovery must ask `executable-find' to probe remotely."
  (let ((default-directory "/ssh:test-host:/srv/app/")
        seen-remote)
    (cl-letf (((symbol-function #'file-remote-p)
               (lambda (&rest _) "/ssh:test-host:"))
              ((symbol-function #'executable-find)
               (lambda (_program &optional remote)
                 (setq seen-remote remote)
                 "/usr/bin/test-server")))
      (should (equal (gsmlg-eglot-find-executable "test-server")
                     "/usr/bin/test-server"))
      (should seen-remote))))

(ert-deftest gsmlg-tramp-project-search-preserves-remote-root ()
  "Consult search must receive the original remote project root."
  (let ((default-directory "/ssh:test-host:/srv/app/")
        seen-directory)
    (cl-letf (((symbol-function #'project-current)
               (lambda (&rest _) '(transient . "/ssh:test-host:/srv/app/")))
              ((symbol-function #'consult-ripgrep)
               (lambda (&optional directory _initial)
                 (setq seen-directory directory))))
      (gsmlg-project-search)
      (should (equal seen-directory "/ssh:test-host:/srv/app/")))))

(ert-deftest gsmlg-tramp-processes-use-process-file ()
  "Remote command helpers must use file handlers through `process-file'."
  (let ((default-directory "/ssh:test-host:/srv/app/")
        called)
    (cl-letf (((symbol-function #'process-file)
               (lambda (program &rest _)
                 (setq called program)
                 0))
              ((symbol-function #'call-process)
               (lambda (&rest _)
                 (ert-fail "call-process bypassed the remote file handler"))))
      (should (zerop (gsmlg-process-file-near-data "git" nil nil nil
                                                   "status")))
      (should (equal called "git")))))

(ert-deftest gsmlg-tramp-server-command-is-valid-on-the-remote-shell ()
  "Remote servers should be probed by TRAMP and launched relative to the root."
  (let ((default-directory "/ssh:test-host:/srv/app/")
        probed)
    (cl-letf (((symbol-function #'file-executable-p)
               (lambda (file)
                 (setq probed file)
                 (string-match-p "node_modules/.bin" file))))
      (should
       (equal
        (gsmlg-eglot-project-executable
         "typescript-language-server" "/ssh:test-host:/srv/app/")
        "./node_modules/.bin/typescript-language-server"))
      (should
       (equal probed
              (concat "/ssh:test-host:/srv/app/"
                      "node_modules/.bin/typescript-language-server"))))))

(ert-deftest gsmlg-tramp-eglot-selection-never-falls-back-locally ()
  "Remote Eglot selection resolves and retains only remote commands."
  (let ((default-directory "/ssh:test-host:/srv/app/")
        (major-mode 'typescript-mode)
        (project '(transient . "/ssh:test-host:/srv/app/"))
        lookup-remote
        ensured)
    (clrhash gsmlg-eglot-unavailable-cache)
    (cl-letf (((symbol-function #'project-current)
               (lambda (&rest _) project))
              ((symbol-function #'file-remote-p)
               (lambda (file &optional identification)
                 (when (string-prefix-p "/ssh:test-host:" file)
                   (if (eq identification 'localname)
                       (substring file (length "/ssh:test-host:"))
                     "/ssh:test-host:"))))
              ((symbol-function #'file-executable-p)
               (lambda (file)
                 (string-prefix-p "/ssh:test-host:" file)))
              ((symbol-function #'executable-find)
               (lambda (_program &optional remote)
                 (setq lookup-remote remote)
                 nil))
              ((symbol-function #'eglot-managed-p)
               (lambda () nil))
              ((symbol-function #'eglot-ensure)
               (lambda () (setq ensured t)))
              ((symbol-function #'start-process)
               (lambda (&rest _)
                 (ert-fail "A local process launcher was selected")))
              ((symbol-function #'call-process)
               (lambda (&rest _)
                 (ert-fail "A local process launcher was selected"))))
      (should
       (equal
        (gsmlg-eglot-server-command project)
        `("./node_modules/.bin/typescript-language-server"
          "--stdio")))
      (gsmlg-eglot-ensure-maybe)
      (should ensured)
      (should-not lookup-remote))))

(ert-deftest gsmlg-tramp-missing-command-override-is-not-launched ()
  "A missing override should be probed remotely and cached as unavailable."
  (let ((default-directory "/ssh:test-host:/srv/app/")
        (major-mode 'python-mode)
        (project '(transient . "/ssh:test-host:/srv/app/"))
        (gsmlg-eglot-command-overrides
         '((python . ("missing-python-server" "--stdio"))))
        seen-remote
        ensured)
    (clrhash gsmlg-eglot-unavailable-cache)
    (cl-letf (((symbol-function #'project-current)
               (lambda (&rest _) project))
              ((symbol-function #'file-remote-p)
               (lambda (file &optional identification)
                 (when (string-prefix-p "/ssh:test-host:" file)
                   (if (eq identification 'localname)
                       (substring file (length "/ssh:test-host:"))
                     "/ssh:test-host:"))))
              ((symbol-function #'executable-find)
               (lambda (_program &optional remote)
                 (setq seen-remote remote)
                 nil))
              ((symbol-function #'eglot-managed-p)
               (lambda () nil))
              ((symbol-function #'eglot-ensure)
               (lambda () (setq ensured t))))
      (should-not (gsmlg-eglot-server-command project))
      (should-not (gsmlg-eglot-ensure-maybe))
      (should seen-remote)
      (should-not ensured))
    (clrhash gsmlg-eglot-unavailable-cache)))

(ert-deftest gsmlg-tramp-qualified-override-is-normalized-for-remote-shell ()
  "A same-host TRAMP override must not be handed to the remote shell twice."
  (let ((default-directory "/ssh:test-host:/srv/app/src/")
        (major-mode 'python-mode)
        (project '(transient . "/ssh:test-host:/srv/app/"))
        (gsmlg-eglot-command-overrides
         '((python . ("/ssh:test-host:/opt/bin/server" "--stdio"))))
        probed)
    (cl-letf (((symbol-function #'file-executable-p)
               (lambda (file)
                 (setq probed file)
                 t)))
      (should
       (equal (gsmlg-eglot-server-command project)
              '("/opt/bin/server" "--stdio")))
      (should (equal probed "/ssh:test-host:/opt/bin/server")))))

(ert-deftest gsmlg-tramp-foreign-host-override-is-rejected ()
  "A command qualified for another host cannot launch in this project."
  (let ((default-directory "/ssh:test-host:/srv/app/")
        (major-mode 'python-mode)
        (project '(transient . "/ssh:test-host:/srv/app/"))
        (gsmlg-eglot-command-overrides
         '((python . ("/ssh:other-host:/opt/bin/server" "--stdio")))))
    (cl-letf (((symbol-function #'file-executable-p)
               (lambda (_file)
                 (ert-fail "A foreign TRAMP path must not be probed"))))
      (should-not (gsmlg-eglot-server-command project)))))

(ert-deftest gsmlg-tramp-formatting-keeps-remote-default-directory ()
  "The selected formatter should execute with the remote buffer directory."
  (should (require 'apheleia nil t))
  (let ((default-directory "/ssh:test-host:/srv/app/")
        (major-mode 'typescript-mode)
        (apheleia-mode-alist '((typescript-mode . prettier)))
        seen-directory)
    (cl-letf (((symbol-function #'apheleia-format-buffer)
               (lambda ()
                 (setq seen-directory default-directory)))
              ((symbol-function #'eglot-managed-p)
               (lambda () nil)))
      (gsmlg-format-buffer)
      (should (equal seen-directory default-directory)))))

(provide 'tramp-test)
;;; tramp-test.el ends here
