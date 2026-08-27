;;; server-test.el --- Canonical Emacs server assertions -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify the singleton server identity and testing bypass.

;;; Code:

(require 'ert)
(require 'gsmlg-server)

(ert-deftest gsmlg-server-uses-canonical-name ()
  "The formal interactive server name is fixed to main."
  (should (equal gsmlg-server-name "main")))

(ert-deftest gsmlg-server-testing-detects-harness ()
  "Isolated test harnesses must not join the user singleton."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "GSMLG_EMACS_TESTING" "1")
    (should (gsmlg-server-testing-p)))
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "GSMLG_EMACS_TESTING" nil)
    (setenv "GSMLG_EMACS_TEST_ROOT" "/tmp/gsmlg-emacs-suite")
    (should (gsmlg-server-testing-p))))

(ert-deftest gsmlg-server-identity-reports-pid ()
  "Server identity always exposes the current Emacs PID."
  (let ((identity (gsmlg-server-identity)))
    (should (equal (alist-get 'pid identity) (emacs-pid)))
    (should (stringp (alist-get 'server_name identity)))))

(provide 'server-test)
;;; server-test.el ends here
