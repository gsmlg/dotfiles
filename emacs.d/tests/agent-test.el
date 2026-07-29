;;; agent-test.el --- Tests for Agent Editor MCP integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration tests for the GSMLG Agent Editor MCP startup policy.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-paths)
(require 'gsmlg-agent)

(ert-deftest gsmlg-agent-start-uses-compatibility-port ()
  "Manual startup uses port 9876 when no environment override is present."
  (let ((process-environment (copy-sequence process-environment))
        observed-port)
    (setenv "EMACS_AGENT_PORT" nil)
    (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
               (lambda () nil))
              ((symbol-function #'emacs-agent-editor-start)
               (lambda (_workspace port)
                 (setq observed-port port)
                 'server)))
      (should (eq (gsmlg-agent-start "/tmp/workspace") 'server))
      (should (= observed-port 9876)))))

(ert-deftest gsmlg-agent-start-honors-environment-port ()
  "Manual startup honors a valid `EMACS_AGENT_PORT' override."
  (let ((process-environment (copy-sequence process-environment))
        observed-port)
    (setenv "EMACS_AGENT_PORT" "43210")
    (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
               (lambda () nil))
              ((symbol-function #'emacs-agent-editor-start)
               (lambda (_workspace port)
                 (setq observed-port port)
                 'server)))
      (should (eq (gsmlg-agent-start "/tmp/workspace") 'server))
      (should (= observed-port 43210)))))

(ert-deftest gsmlg-agent-start-rejects-invalid-environment-port ()
  "Startup rejects malformed or out-of-range `EMACS_AGENT_PORT' values."
  (let ((process-environment (copy-sequence process-environment)))
    (dolist (port '("not-a-port" "0" "65536"))
      (setenv "EMACS_AGENT_PORT" port)
      (should-error
       (gsmlg-agent-start "/tmp/workspace")
       :type 'user-error))))

(ert-deftest gsmlg-agent-start-prefers-environment-workspace ()
  "Startup uses `EMACS_AGENT_WORKSPACE' ahead of the customized workspace."
  (let ((process-environment (copy-sequence process-environment))
        (gsmlg-agent-workspace "/tmp/custom-workspace")
        observed-workspace)
    (setenv "EMACS_AGENT_WORKSPACE" "/tmp/environment-workspace")
    (cl-letf (((symbol-function #'emacs-agent-editor-running-p)
               (lambda () nil))
              ((symbol-function #'emacs-agent-editor-start)
               (lambda (workspace _port)
                 (setq observed-workspace workspace)
                 'server)))
      (should (eq (gsmlg-agent-start) 'server))
      (should
       (equal observed-workspace
              (file-name-as-directory
               (expand-file-name "/tmp/environment-workspace")))))))

(ert-deftest gsmlg-agent-start-uses-customized-workspace ()
  "Startup uses `gsmlg-agent-workspace' when the environment is unset."
  (let ((process-environment (copy-sequence process-environment))
        (gsmlg-agent-workspace "/tmp/custom-workspace")
        observed-workspace)
    (setenv "EMACS_AGENT_WORKSPACE" nil)
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (workspace _port)
                 (setq observed-workspace workspace)
                 'server)))
      (should (eq (gsmlg-agent-start) 'server))
      (should
       (equal observed-workspace
              (file-name-as-directory
               (expand-file-name "/tmp/custom-workspace")))))))

(ert-deftest gsmlg-agent-start-ignores-empty-environment-workspace ()
  "An empty workspace environment falls back to the customized directory."
  (let ((process-environment (copy-sequence process-environment))
        (gsmlg-agent-workspace "/tmp/custom-workspace")
        observed-workspace)
    (setenv "EMACS_AGENT_WORKSPACE" "")
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (workspace _port)
                 (setq observed-workspace workspace)
                 'server)))
      (should (eq (gsmlg-agent-start) 'server))
      (should
       (equal observed-workspace
              (file-name-as-directory
               (expand-file-name "/tmp/custom-workspace")))))))

(ert-deftest gsmlg-agent-start-confines-listener-and-state ()
  "Startup enforces loopback and writes metadata under GSMLG XDG state."
  (let ((gsmlg-state-directory "/tmp/gsmlg-emacs-state/")
        (emacs-agent-editor-host "192.0.2.1")
        (emacs-agent-editor-state-directory "/tmp/wrong-state/")
        observed-host
        observed-state)
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (_workspace _port)
                 (setq observed-host emacs-agent-editor-host
                       observed-state emacs-agent-editor-state-directory)
                 'server)))
      (should (eq (gsmlg-agent-start "/tmp/workspace") 'server))
      (should (equal observed-host "127.0.0.1"))
      (should
       (equal observed-state
              "/tmp/gsmlg-emacs-state/agent-editor/")))))

(ert-deftest gsmlg-agent-autostart-never-runs-in-batch ()
  "Autostart does not open an MCP listener when Emacs is noninteractive."
  (let ((noninteractive t)
        (gsmlg-agent-autostart t)
        (gsmlg-agent-workspace "/tmp/workspace")
        started)
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (&rest _arguments)
                 (setq started t))))
      (should-not (gsmlg-agent-autostart-maybe))
      (should-not started))))

(ert-deftest gsmlg-agent-autostart-honors-environment-opt-in ()
  "Interactive startup honors `EMACS_AGENT_AUTOSTART' with a workspace."
  (let ((process-environment (copy-sequence process-environment))
        (noninteractive nil)
        (gsmlg-agent-autostart nil)
        (gsmlg-agent-workspace nil)
        started)
    (setenv "EMACS_AGENT_AUTOSTART" "true")
    (setenv "EMACS_AGENT_WORKSPACE" "/tmp/environment-workspace")
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (&rest _arguments)
                 (setq started t)
                 'server)))
      (should (eq (gsmlg-agent-autostart-maybe) 'server))
      (should started))))

(ert-deftest gsmlg-agent-autostart-is-registered-after-init ()
  "Agent autostart policy is evaluated only after initialization."
  (should (memq #'gsmlg-agent-autostart-maybe after-init-hook)))

(ert-deftest gsmlg-agent-autostart-requires-an-explicit-workspace ()
  "Autostart never captures an incidental startup `default-directory'."
  (let ((process-environment (copy-sequence process-environment))
        (noninteractive nil)
        (default-directory "/tmp/")
        (gsmlg-agent-autostart t)
        (gsmlg-agent-workspace nil)
        started)
    (setenv "EMACS_AGENT_WORKSPACE" nil)
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (&rest _arguments)
                 (setq started t))))
      (should-not (gsmlg-agent-autostart-maybe))
      (should-not started))))

(ert-deftest gsmlg-agent-autostart-ignores-an-empty-workspace-environment ()
  "An empty `EMACS_AGENT_WORKSPACE' never resolves to `default-directory'."
  (let ((process-environment (copy-sequence process-environment))
        (noninteractive nil)
        (default-directory "/tmp/")
        (gsmlg-agent-autostart t)
        (gsmlg-agent-workspace nil)
        started)
    (setenv "EMACS_AGENT_WORKSPACE" "")
    (cl-letf (((symbol-function #'emacs-agent-editor-start)
               (lambda (&rest _arguments)
                 (setq started t))))
      (should-not (gsmlg-agent-autostart-maybe))
      (should-not started))))

(ert-deftest gsmlg-agent-autostart-policy-covers-daemon-and-gui-sessions ()
  "Explicit autostart works for named daemons and normal GUI sessions."
  (dolist (daemon-name '(nil "workspace-daemon"))
    (let ((process-environment (copy-sequence process-environment))
          (noninteractive nil)
          (gsmlg-agent-autostart t)
          (gsmlg-agent-workspace "/tmp/configured-workspace")
          started)
      (setenv "EMACS_AGENT_AUTOSTART" nil)
      (setenv "EMACS_AGENT_WORKSPACE" nil)
      (cl-letf (((symbol-function #'daemonp)
                 (lambda () daemon-name))
                ((symbol-function #'emacs-agent-editor-start)
                 (lambda (&rest _arguments)
                   (setq started t)
                   'server)))
        (should (eq (gsmlg-agent-autostart-maybe) 'server))
        (should started)))))

(ert-deftest gsmlg-agent-autostart-does-not-restart-a-running-service ()
  "Autostart leaves an existing Agent Editor MCP listener alone."
  (let ((noninteractive nil)
        (gsmlg-agent-autostart t)
        (gsmlg-agent-workspace "/tmp/configured-workspace")
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
        (gsmlg-agent-autostart t)
        (gsmlg-agent-workspace "/tmp/configured-workspace"))
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
        (gsmlg-agent-autostart t)
        (gsmlg-agent-workspace "/tmp/configured-workspace"))
    (should-not (gsmlg-agent-autostart-maybe))
    (should-error
     (gsmlg-agent-start "/tmp/configured-workspace")
     :type 'user-error)))

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
