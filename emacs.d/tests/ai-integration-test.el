;;; ai-integration-test.el --- Offline AI workbench integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Autoload/deferral and stubbed ask/edit orchestration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-ai)
(require 'gsmlg-ai-context)
(require 'gsmlg-ai-session)

(ert-deftest gsmlg-ai-loading-does-not-load-providers ()
  "Requiring the facade does not load gptel or minuet."
  (should-not (featurep 'gptel))
  (should-not (featurep 'minuet))
  (require 'gsmlg-ai)
  (should-not (featurep 'gptel))
  (should-not (featurep 'minuet)))

(ert-deftest gsmlg-ai-ask-uses-stubbed-request-and-unsaved-context ()
  "Ask serializes unsaved buffer content through the request stub."
  (gsmlg-ai-context-clear-all t)
  (let (prompt-seen)
    (with-temp-buffer
      (insert "unsaved-ask-body")
      (let ((gsmlg-ai--ensure-gptel-function #'ignore)
            (gsmlg-ai-confirm-before-send 'never)
            (gsmlg-ai--request-function
             (lambda (prompt &rest _args)
               (setq prompt-seen prompt)
               nil)))
        (gsmlg-ai-session-ask "What is this?")
        (should (string-match-p "unsaved-ask-body" prompt-seen))
        (should (eq (gsmlg-ai-session-state gsmlg-ai-session--oneshot)
                    'waiting))))))

(ert-deftest gsmlg-ai-edit-rejects-empty-nonfile-context ()
  "Edit requires editable file-backed context."
  (gsmlg-ai-context-clear-all t)
  (with-temp-buffer
    (insert "not a file")
    (let ((gsmlg-ai--ensure-gptel-function #'ignore)
          (gsmlg-ai-confirm-before-send 'never))
      (should-error (gsmlg-ai-session-edit "change things")))))

(ert-deftest gsmlg-ai-cancel-clears-incomplete-edit ()
  "Cancel aborts and drops an incomplete edit session."
  (gsmlg-ai-context-clear-all t)
  (let ((aborted nil))
    (setq gsmlg-ai-session--active
          (gsmlg-ai-session--create
           :id "c" :kind 'edit :state 'waiting :user-prompt "x"
           :system-directive "x" :context-id "c" :files nil
           :creation-root default-directory :backend-summary "stub"
           :request-buffer (get-buffer-create "*gsmlg-ai-cancel-test*")
           :tool-call-count 0 :tool-token nil :revision-round 0
           :model-summary nil :warnings nil :errors nil
           :created-at 0 :updated-at 0))
    (let ((gsmlg-ai--abort-function
           (lambda (_buffer) (setq aborted t))))
      (gsmlg-ai-session-cancel)
      (should aborted)
      (should-not gsmlg-ai-session--active))))

(provide 'ai-integration-test)
;;; ai-integration-test.el ends here
