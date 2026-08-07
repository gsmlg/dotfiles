;;; ai-integration-test.el --- Offline AI workbench integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Autoload/deferral and stubbed ask/edit orchestration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-ai)
(require 'gsmlg-ai-context)
(require 'gsmlg-ai-session)
(require 'gsmlg-ai-tools)

(defvar gptel-backend)
(defvar gptel-model)

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
  (let ((aborted nil)
        (token nil))
    (require 'gsmlg-ai-tools)
    (setq gsmlg-ai-session--active
          (gsmlg-ai-session--create
           :id "c" :kind 'edit :state 'waiting :user-prompt "x"
           :system-directive "x" :context-id "c" :files nil
           :creation-root default-directory :backend-summary "stub"
           :request-buffer (get-buffer-create "*gsmlg-ai-cancel-test*")
           :tool-call-count 0 :tool-token nil :revision-round 0
           :model-summary nil :warnings nil :errors nil
           :created-at 0 :updated-at 0))
    (setq token (gsmlg-ai-tools-register gsmlg-ai-session--active))
    (let ((gsmlg-ai--abort-function
           (lambda (_buffer) (setq aborted t))))
      (gsmlg-ai-session-cancel)
      (should aborted)
      (should-not gsmlg-ai-session--active)
      (should-error (gsmlg-ai-tools--session token)))))

(ert-deftest gsmlg-ai-cancel-preserves-ready-proposal ()
  "Cancel must not clear a ready edit proposal."
  (gsmlg-ai-context-clear-all t)
  (require 'gsmlg-ai-tools)
  (setq gsmlg-ai-session--active
        (gsmlg-ai-session--create
         :id "ready" :kind 'edit :state 'ready :user-prompt "x"
         :system-directive "x" :context-id "c" :files nil
         :creation-root default-directory :backend-summary "stub"
         :request-buffer (get-buffer-create "*gsmlg-ai-ready-cancel*")
         :tool-call-count 0 :tool-token nil :revision-round 0
         :model-summary "done" :warnings nil :errors nil
         :created-at 0 :updated-at 0))
  (let ((token (gsmlg-ai-tools-register gsmlg-ai-session--active))
        (gsmlg-ai--abort-function
         (lambda (_buffer) (error "Should not abort ready"))))
    (gsmlg-ai-session-cancel)
    (should gsmlg-ai-session--active)
    (should (eq (gsmlg-ai-session-state gsmlg-ai-session--active) 'ready))
    (should (eq (gsmlg-ai-tools--session token)
                gsmlg-ai-session--active))
    (gsmlg-ai-tools-unregister gsmlg-ai-session--active)
    (setq gsmlg-ai-session--active nil)))

(ert-deftest gsmlg-ai-edit-restart-unregisters-previous-session ()
  "Confirming start-over retires the previous tool registry entry."
  (gsmlg-ai-context-clear-all t)
  (require 'gsmlg-ai-tools)
  (let* ((old
          (gsmlg-ai-session--create
           :id "old" :kind 'edit :state 'waiting :user-prompt "old"
           :system-directive "x" :context-id "c" :files nil
           :creation-root default-directory :backend-summary "stub"
           :request-buffer (get-buffer-create "*gsmlg-ai-old-edit*")
           :tool-call-count 0 :tool-token nil :revision-round 0
           :model-summary nil :warnings nil :errors nil
           :created-at 0 :updated-at 0))
         (old-token (progn
                      (setq gsmlg-ai-session--active old)
                      (gsmlg-ai-tools-register old)))
         (aborted nil)
         (file (make-temp-file "gsmlg-ai-restart-" nil ".el" "body\n")))
    (unwind-protect
        (progn
          (find-file-noselect file)
          (with-current-buffer (get-file-buffer file)
            (gsmlg-ai-context-add-current-buffer))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (let ((gsmlg-ai--ensure-gptel-function #'ignore)
                  (gsmlg-ai-confirm-before-send 'never)
                  (gsmlg-ai--abort-function
                   (lambda (_buffer) (setq aborted t)))
                  (gsmlg-ai--request-function
                   (lambda (&rest _) nil)))
              (cl-letf (((symbol-function 'gptel-make-tool)
                         (lambda (&rest _) 'fake-tool))
                        ((symbol-function 'gsmlg-ai-tools-make-gptel-tools)
                         (lambda (_session) nil)))
                (gsmlg-ai-session-edit "new task")
                (should aborted)
                (should-error (gsmlg-ai-tools--session old-token))
                (should gsmlg-ai-session--active)
                (should-not (eq gsmlg-ai-session--active old))))))
      (when-let* ((buf (get-file-buffer file)))
        (with-current-buffer buf
          (set-buffer-modified-p nil)
          (kill-buffer buf)))
      (when (file-exists-p file)
        (delete-file file))
      (when gsmlg-ai-session--active
        (gsmlg-ai-tools-unregister gsmlg-ai-session--active)
        (setq gsmlg-ai-session--active nil))
      (gsmlg-ai-context-clear-all t))))

(ert-deftest gsmlg-ai-configures-deepseek-default ()
  "DeepSeek default registration uses env-backed key and flash model."
  (let ((gsmlg-ai--deepseek-configured nil)
        (gsmlg-ai-configure-deepseek-default t)
        (gsmlg-ai-deepseek-model 'deepseek-v4-flash)
        (captured nil)
        (old-backend (bound-and-true-p gptel-backend))
        (old-model (bound-and-true-p gptel-model)))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel-make-deepseek)
                   (lambda (name &rest keys)
                     (setq captured (cons name keys))
                     'deepseek-backend)))
          (setq gptel-backend nil
                gptel-model nil)
          (gsmlg-ai--configure-deepseek)
          (should (eq gptel-backend 'deepseek-backend))
          (should (eq gptel-model 'deepseek-v4-flash))
          (should (equal (car captured) "DeepSeek"))
          (should (eq (plist-get (cdr captured) :key)
                      #'gsmlg-ai--deepseek-api-key))
          (should gsmlg-ai--deepseek-configured)
          ;; Idempotent.
          (gsmlg-ai--configure-deepseek)
          (should (eq gptel-backend 'deepseek-backend)))
      (setq gptel-backend old-backend
            gptel-model old-model
            gsmlg-ai--deepseek-configured nil))))

(provide 'ai-integration-test)
;;; ai-integration-test.el ends here
