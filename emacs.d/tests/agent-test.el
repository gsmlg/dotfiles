;;; agent-test.el --- Tests for Agent Editor MCP integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration tests for the GSMLG Agent Editor MCP startup policy.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-paths)
(require 'gsmlg-agent)

(defmacro gsmlg-agent-test--with-emacs-server (name &rest body)
  "Run BODY with an isolated Emacs server named NAME."
  (declare (indent 1) (debug t))
  `(let* ((root (make-temp-file "gsmlg-agent-server-" t))
          (process-environment (copy-sequence process-environment))
          (noninteractive nil)
          (server-name ,name)
          (server-socket-dir root)
          (server-auth-dir root)
          (server-use-tcp nil)
          (server-process nil)
          (server-clients nil)
          (server-mode nil))
     (unwind-protect
         (progn ,@body)
       (when server-process
         (server-stop t))
       (delete-directory root t))))

(ert-deftest gsmlg-agent-start-uses-compatibility-port ()
  "Manual startup uses port 9876 when no environment override is present."
  (let ((process-environment (copy-sequence process-environment))
        observed-port)
    (setenv "EMACS_AGENT_PORT" nil)
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (port)
                 (setq observed-port port)
                 'server)))
      (should (eq (gsmlg-agent-start) 'server))
      (should (= observed-port 9876)))))

(ert-deftest gsmlg-agent-start-honors-environment-port ()
  "Manual startup honors a valid `EMACS_AGENT_PORT' override."
  (let ((process-environment (copy-sequence process-environment))
        observed-port)
    (setenv "EMACS_AGENT_PORT" "43210")
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (port)
                 (setq observed-port port)
                 'server)))
      (should (eq (gsmlg-agent-start) 'server))
      (should (= observed-port 43210)))))

(ert-deftest gsmlg-agent-start-rejects-invalid-environment-port ()
  "Startup rejects malformed or out-of-range `EMACS_AGENT_PORT' values."
  (let ((process-environment (copy-sequence process-environment)))
    (dolist (port '("not-a-port" "0" "65536"))
      (setenv "EMACS_AGENT_PORT" port)
      (should-error (gsmlg-agent-start) :type 'user-error))))

(ert-deftest gsmlg-agent-start-does-not-prompt-for-a-directory ()
  "Interactive startup passes only the configured port to the package."
  (let ((process-environment (copy-sequence process-environment))
        prompted
        observed-port)
    (setenv "EMACS_AGENT_PORT" nil)
    (cl-letf (((symbol-function #'read-directory-name)
               (lambda (&rest _arguments)
                 (setq prompted t)
                 "/tmp/unexpected/"))
              ((symbol-function #'emacs-agent-editor-start)
               (lambda (port)
                 (setq observed-port port)
                 'server)))
      (should (eq (call-interactively #'gsmlg-agent-start) 'server))
      (should (= observed-port 9876))
      (should-not prompted))))

(ert-deftest gsmlg-agent-start-preserves-package-state-directory ()
  "Startup enforces loopback without overriding the package's XDG state."
  (let ((gsmlg-state-directory "/tmp/gsmlg-emacs-state/")
        (emacs-agent-editor-host "192.0.2.1")
        (emacs-agent-editor-state-directory "/tmp/canonical-state/")
        observed-host
        observed-state)
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (_port)
                 (setq observed-host emacs-agent-editor-host
                       observed-state emacs-agent-editor-state-directory)
                 'server)))
      (should (eq (gsmlg-agent-start) 'server))
      (should (equal observed-host "127.0.0.1"))
      (should
       (equal observed-state
              "/tmp/canonical-state/")))))

(ert-deftest gsmlg-agent-start-removes-only-legacy-connection-file ()
  "Successful startup removes only the former integration metadata file."
  (let* ((root (make-temp-file "gsmlg-agent-state-" t))
         (gsmlg-state-directory
          (file-name-as-directory (expand-file-name "emacs" root)))
         (legacy-directory
          (expand-file-name "agent-editor/interactive/"
                            gsmlg-state-directory))
         (legacy-connection
          (expand-file-name "connection.json" legacy-directory))
         (legacy-sibling
          (expand-file-name "keep.json" legacy-directory))
         (canonical-connection
          (expand-file-name
           "emacs-agent-editor/interactive/connection.json" root))
         (emacs-agent-editor--connection-file nil))
    (unwind-protect
        (progn
          (make-directory legacy-directory t)
          (make-directory (file-name-directory canonical-connection) t)
          (write-region "{}" nil legacy-connection nil 'silent)
          (write-region "{}" nil legacy-sibling nil 'silent)
          (write-region "{}" nil canonical-connection nil 'silent)
          (cl-letf (((symbol-function #'emacs-agent-editor-start)
                     (lambda (_port)
                       (setq emacs-agent-editor--connection-file
                             canonical-connection)
                       'server)))
            (should (eq (gsmlg-agent-start) 'server)))
          (should-not (file-exists-p legacy-connection))
          (should (file-exists-p legacy-sibling))
          (should (file-directory-p legacy-directory))
          (should (file-exists-p canonical-connection)))
      (delete-directory root t))))

(ert-deftest gsmlg-agent-start-keeps-legacy-metadata-on-failure ()
  "Failed startup leaves former integration metadata untouched."
  (let* ((root (make-temp-file "gsmlg-agent-failed-state-" t))
         (gsmlg-state-directory
          (file-name-as-directory (expand-file-name "emacs" root)))
         (legacy-connection
          (expand-file-name
           "agent-editor/interactive/connection.json"
           gsmlg-state-directory)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory legacy-connection) t)
          (write-region "{}" nil legacy-connection nil 'silent)
          (cl-letf (((symbol-function #'emacs-agent-editor-start)
                     (lambda (_port)
                       (error "Listener unavailable"))))
            (should-error (gsmlg-agent-start)))
          (should (file-exists-p legacy-connection)))
      (delete-directory root t))))

(ert-deftest gsmlg-agent-start-never-removes-active-connection-file ()
  "Migration cleanup never removes the active connection metadata target."
  (let* ((root (make-temp-file "gsmlg-agent-active-state-" t))
         (gsmlg-state-directory
          (file-name-as-directory (expand-file-name "emacs" root)))
         (active-connection
          (expand-file-name
           "agent-editor/interactive/connection.json"
           gsmlg-state-directory))
         (emacs-agent-editor--connection-file nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory active-connection) t)
          (write-region "{}" nil active-connection nil 'silent)
          (cl-letf (((symbol-function #'emacs-agent-editor-start)
                     (lambda (_port)
                       (setq emacs-agent-editor--connection-file
                             active-connection)
                       'server)))
            (should (eq (gsmlg-agent-start) 'server)))
          (should (file-exists-p active-connection)))
      (delete-directory root t))))

(ert-deftest gsmlg-agent-autostart-never-runs-in-batch ()
  "Autostart does not open an MCP listener when Emacs is noninteractive."
  (let ((noninteractive t)
        (gsmlg-agent-autostart t)
        started)
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (&rest _arguments)
                 (setq started t))))
      (should-not (gsmlg-agent-autostart-maybe))
      (should-not started))))

(ert-deftest gsmlg-agent-autostart-honors-environment-opt-in ()
  "Interactive startup honors `EMACS_AGENT_AUTOSTART' by itself."
  (let ((process-environment (copy-sequence process-environment))
        (noninteractive nil)
        (gsmlg-agent-autostart nil)
        observed-port)
    (setenv "EMACS_AGENT_AUTOSTART" "true")
    (setenv "EMACS_AGENT_PORT" "45678")
    (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
               (lambda () nil))
              ((symbol-function #'emacs-agent-editor-start)
               (lambda (port)
                 (setq observed-port port)
                 'server)))
      (should (eq (gsmlg-agent-autostart-maybe) 'server))
      (should (= observed-port 45678)))))

(ert-deftest gsmlg-agent-autostart-is-registered-after-init ()
  "Agent autostart policy is evaluated only after initialization."
  (should (memq #'gsmlg-agent-start-for-server-maybe after-init-hook)))

(ert-deftest gsmlg-agent-after-init-waits-for-emacs-server ()
  "Agent autostart waits until this Emacs process owns a server."
  (let ((process-environment (copy-sequence process-environment))
        (noninteractive nil)
        (server-mode nil)
        (server-process nil)
        (gsmlg-agent-autostart t)
        started)
    (setenv "EMACS_AGENT_AUTOSTART" nil)
    (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
               (lambda () nil))
              ((symbol-function #'emacs-agent-editor-start)
               (lambda (&rest _arguments)
                 (setq started t))))
      (run-hooks 'after-init-hook)
      (should-not started))))

(ert-deftest gsmlg-agent-server-start-starts-editor-by-default ()
  "Starting the Emacs server starts Agent Editor by default."
  (gsmlg-agent-test--with-emacs-server "agent-lifecycle-test"
    (let (started)
      (setenv "EMACS_AGENT_AUTOSTART" nil)
      (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
                 (lambda () nil))
                ((symbol-function #'emacs-agent-editor-start)
                 (lambda (_port)
                   (should (process-live-p server-process))
                   (setq started t)
                   'agent-server)))
        (server-start nil t)
        (should started)))))

(ert-deftest gsmlg-agent-server-start-honors-explicit-opt-out ()
  "Starting the Emacs server honors explicit Agent Editor opt-out."
  (gsmlg-agent-test--with-emacs-server "agent-opt-in-test"
    (let ((gsmlg-agent-autostart nil)
          started)
      (setenv "EMACS_AGENT_AUTOSTART" nil)
      (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
                 (lambda () nil))
                ((symbol-function #'emacs-agent-editor-start)
                 (lambda (&rest _arguments)
                   (setq started t))))
        (server-start nil t)
        (should-not started)))))

(ert-deftest gsmlg-agent-server-stop-stops-running-editor ()
  "Stopping the Emacs server stops a running Agent Editor."
  (gsmlg-agent-test--with-emacs-server "agent-lifecycle-stop-test"
    (let ((gsmlg-agent-autostart t)
          running
          stopped)
      (setenv "EMACS_AGENT_AUTOSTART" nil)
      (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
                 (lambda () running))
                ((symbol-function #'emacs-agent-editor-start)
                 (lambda (_port)
                   (setq running t)
                   'agent-server))
                ((symbol-function #'emacs-agent-editor-stop)
                 (lambda ()
                   (should-not server-process)
                   (setq running nil
                         stopped t)
                   'stopped)))
        (server-start nil t)
        (should running)
        (server-stop t)
        (should stopped)))))

(ert-deftest gsmlg-agent-autostart-needs-no-project ()
  "Explicit autostart starts the project-optional editor runtime."
  (let ((process-environment (copy-sequence process-environment))
        (noninteractive nil)
        (gsmlg-agent-autostart t)
        started)
    (setenv "EMACS_AGENT_AUTOSTART" nil)
    (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
               (lambda () nil))
              ((symbol-function #'emacs-agent-editor-start)
               (lambda (_port)
                 (setq started t)
                 'server)))
      (should (eq (gsmlg-agent-autostart-maybe) 'server))
      (should started))))

(ert-deftest gsmlg-agent-autostart-policy-covers-daemon-and-gui-sessions ()
  "Explicit autostart works for named daemons and normal GUI sessions."
  (dolist (daemon-name '(nil "agent-editor"))
    (let ((process-environment (copy-sequence process-environment))
          (noninteractive nil)
          (gsmlg-agent-autostart t)
          started)
      (setenv "EMACS_AGENT_AUTOSTART" nil)
      (cl-letf (((symbol-function #'daemonp)
                 (lambda () daemon-name))
                ((symbol-function #'emacs-agent-editor-running-p)
                 (lambda () nil))
                ((symbol-function #'emacs-agent-editor-start)
                 (lambda (_port)
                   (setq started t)
                   'server)))
        (should (eq (gsmlg-agent-autostart-maybe) 'server))
        (should started)))))

(ert-deftest gsmlg-agent-autostart-does-not-restart-a-running-service ()
  "Autostart leaves an existing Agent Editor MCP listener alone."
  (let ((noninteractive nil)
        (gsmlg-agent-autostart t)
        started)
    (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
               (lambda () t))
              ((symbol-function #'emacs-agent-editor-start)
               (lambda (&rest _arguments)
                 (setq started t))))
      (should-not (gsmlg-agent-autostart-maybe))
      (should-not started))))

(ert-deftest gsmlg-agent-autostart-isolates-listener-failures ()
  "An MCP startup failure never prevents Emacs initialization."
  (let ((noninteractive nil)
        (gsmlg-agent-autostart t))
    (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
               (lambda () nil))
              ((symbol-function #'emacs-agent-editor-start)
               (lambda (&rest _arguments)
                 (error "Listener unavailable"))))
      (should-not (gsmlg-agent-autostart-maybe)))))

(ert-deftest gsmlg-agent-unavailable-package-does-not-break-startup ()
  "A failed bundled integration remains a command-level error, not init failure."
  (let ((noninteractive nil)
        (gsmlg-agent-package-available-p nil)
        (gsmlg-agent-autostart t))
    (should-not (gsmlg-agent-autostart-maybe))
    (should-error (gsmlg-agent-start) :type 'user-error)))

(ert-deftest gsmlg-agent-stop-never-terminates-the-emacs-daemon ()
  "Stopping Agent Editor MCP stops only its listener and session state."
  (let (stopped killed)
    (cl-letf (((symbol-function #'emacs-agent-editor-stop)
               (lambda ()
                 (setq stopped t)
                 'stopped))
              ((symbol-function #'kill-emacs)
               (lambda (&rest _arguments)
                 (setq killed t))))
      (should (eq (gsmlg-agent-stop) 'stopped))
      (should stopped)
      (should-not killed))))

(ert-deftest gsmlg-agent-retains-the-legacy-autostart-function-name ()
  "External configuration can still call the old slash-named function."
  (should
   (eq (indirect-function 'gsmlg/agent-editor-mcp-autostart)
       (indirect-function 'gsmlg-agent-autostart-maybe))))

(provide 'agent-test)
;;; agent-test.el ends here
