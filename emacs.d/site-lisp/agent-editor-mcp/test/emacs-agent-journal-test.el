;;; emacs-agent-journal-test.el --- Runtime journal tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for runtime-scoped, redacted audit metadata.

;;; Code:

(require 'ert)
(require 'emacs-agent-journal)

(ert-deftest emacs-agent-journal-redacts-runtime-secrets ()
  (let* ((state-directory
          (make-temp-file "emacs-agent-journal-state-" t))
         (runtime
          (emacs-agent-runtime-create
           :state-directory state-directory))
         (emacs-agent-journal-enabled t)
         path)
    (unwind-protect
        (progn
          (setq path (emacs-agent-journal-open runtime))
          (emacs-agent-journal-write
           runtime
           '(:tool "document_apply_edits"
             :path "/tmp/example.el"
             :content "source-secret"
             :credential "credential-secret"
             :nested (:token "token-secret" :status "completed")))
          (let ((contents
                 (with-temp-buffer
                   (insert-file-contents path)
                   (buffer-string))))
            (should
             (string-match-p
              (regexp-quote
               (emacs-agent-runtime-instance-id runtime))
              contents))
            (should (string-match-p "document_apply_edits" contents))
            (should-not (string-match-p "source-secret" contents))
            (should-not (string-match-p "credential-secret" contents))
            (should-not (string-match-p "token-secret" contents))))
      (emacs-agent-journal-close runtime)
      (emacs-agent-runtime-clear runtime)
      (delete-directory state-directory t))))

(provide 'emacs-agent-journal-test)
;;; emacs-agent-journal-test.el ends here
