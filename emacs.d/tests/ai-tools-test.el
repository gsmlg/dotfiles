;;; ai-tools-test.el --- Offline AI tool tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Request-scoped tools against in-memory session fixtures.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-ai-context)
(require 'gsmlg-ai-session)
(require 'gsmlg-ai-tools)

(defun gsmlg-ai-test--session-with-content (content &optional editable)
  "Build a fixture session containing CONTENT.
When EDITABLE is the symbol `no', mark the file read-only."
  (let* ((file
          (gsmlg-ai-snapshot--create
           :id "f1"
           :display-path "fixture.el"
           :canonical-file nil
           :source-kind 'live-buffer
           :source-buffer nil
           :source-buffer-tick nil
           :source-content-hash (gsmlg-ai-context--hash content)
           :source-file-attributes nil
           :original-content content
           :proposed-content content
           :proposal-revision 0
           :operation 'unchanged
           :editable-p (if (eq editable 'no) nil t)
           :remote-p nil
           :apply-status 'pending
           :conflict-reason nil))
         (session
          (gsmlg-ai-session--create
           :id "s1"
           :kind 'edit
           :state 'waiting
           :user-prompt "test"
           :system-directive "test"
           :context-id "c1"
           :files (list file)
           :creation-root (make-temp-file "gsmlg-ai-root-" t)
           :backend-summary "stub"
           :request-buffer nil
           :tool-call-count 0
           :tool-token nil
           :revision-round 0
           :model-summary nil
           :warnings nil
           :errors nil
           :created-at 0
           :updated-at 0))
         (token (gsmlg-ai-tools-register session)))
    (cons session token)))

(ert-deftest gsmlg-ai-tools-list-and-read-proposed ()
  "List and read operate on proposed content only."
  (pcase-let* ((`(,session . ,token)
                (gsmlg-ai-test--session-with-content "hello\nworld\n")))
    (should (string-match-p "f1" (gsmlg-ai-tools-list-context-files token)))
    (let ((payload (gsmlg-ai-tools-read-file token "f1" 1 1)))
      (should (string-match-p "\"content\":\"hello\"" payload)))
    (gsmlg-ai-tools-unregister session)
    (delete-directory (gsmlg-ai-session-creation-root session) t)))

(ert-deftest gsmlg-ai-tools-replace-and-search ()
  "Replace_text mutates proposed content and search sees it."
  (pcase-let* ((`(,session . ,token)
                (gsmlg-ai-test--session-with-content "foo bar foo\n")))
    (gsmlg-ai-tools-replace-text token "f1" 0 "foo" "baz" 2)
    (should (string-match-p "baz"
                            (gsmlg-ai-tools-search-files token "baz")))
    (should-error
     (gsmlg-ai-tools-replace-text token "f1" 0 "foo" "x" 1))
    (gsmlg-ai-tools-unregister session)
    (delete-directory (gsmlg-ai-session-creation-root session) t)))

(ert-deftest gsmlg-ai-tools-replace-rejects-zero-and-ambiguous ()
  "Replace_text rejects zero or unexpected match counts."
  (pcase-let* ((`(,session . ,token)
                (gsmlg-ai-test--session-with-content "abc abc\n")))
    (should-error (gsmlg-ai-tools-replace-text token "f1" 0 "zzz" "y" 1))
    (should-error (gsmlg-ai-tools-replace-text token "f1" 0 "abc" "y" 1))
    (gsmlg-ai-tools-unregister session)
    (delete-directory (gsmlg-ai-session-creation-root session) t)))

(ert-deftest gsmlg-ai-tools-create-file-guards ()
  "Create_file rejects absolute paths, traversal, and existing files."
  (pcase-let* ((`(,session . ,token)
                (gsmlg-ai-test--session-with-content "x\n")))
    (should-error (gsmlg-ai-tools-create-file token "/tmp/x" "y"))
    (should-error (gsmlg-ai-tools-create-file token "../x" "y"))
    (let ((existing
           (expand-file-name "exists.el"
                             (gsmlg-ai-session-creation-root session))))
      (with-temp-file existing (insert "disk"))
      (should-error
       (gsmlg-ai-tools-create-file token "exists.el" "nope")))
    (let ((result (gsmlg-ai-tools-create-file token "new.el" "created\n")))
      (should (string-match-p "\"id\":" result))
      (should-not
       (file-exists-p
        (expand-file-name "new.el"
                          (gsmlg-ai-session-creation-root session)))))
    (gsmlg-ai-tools-unregister session)
    (delete-directory (gsmlg-ai-session-creation-root session) t)))

(ert-deftest gsmlg-ai-tools-budget-exhaustion ()
  "Exceeding the tool budget signals an error."
  (pcase-let* ((`(,session . ,token)
                (gsmlg-ai-test--session-with-content "x\n")))
    (let ((gsmlg-ai-max-tool-calls 1))
      (gsmlg-ai-tools-list-context-files token)
      (should-error (gsmlg-ai-tools-list-context-files token)))
    (gsmlg-ai-tools-unregister session)
    (delete-directory (gsmlg-ai-session-creation-root session) t)))

(ert-deftest gsmlg-ai-tools-do-not-touch-live-buffers ()
  "Tool mutations leave live buffers unchanged."
  (with-temp-buffer
    (insert "live")
    (pcase-let* ((`(,session . ,token)
                  (gsmlg-ai-test--session-with-content "proposed\n")))
      (setf (gsmlg-ai-snapshot-source-buffer
             (car (gsmlg-ai-session-files session)))
            (current-buffer))
      (gsmlg-ai-tools-set-file-content token "f1" 0 "changed\n")
      (should (equal (buffer-string) "live"))
      (gsmlg-ai-tools-unregister session)
      (delete-directory (gsmlg-ai-session-creation-root session) t))))

(provide 'ai-tools-test)
;;; ai-tools-test.el ends here
