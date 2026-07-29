;;; emacs-agent-transaction-test.el --- Workspace transaction tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-agent-transaction)

(defmacro emacs-agent-transaction-test--workspace (&rest body)
  "Run BODY in a temporary workspace."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "agent-transaction-" t))
          (workspace
           (emacs-agent-workspace-create
            root :workspace-id
            (concat "test-" (file-name-nondirectory root))))
          (emacs-agent-current-workspace workspace))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file root)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory root t))))

(ert-deftest emacs-agent-transaction-validation-is-all-or-nothing ()
  (emacs-agent-transaction-test--workspace
    (write-region "old a\n" nil (expand-file-name "a.txt" root))
    (write-region "old b\n" nil (expand-file-name "b.txt" root))
    (let* ((a (emacs-agent-document-open workspace "a.txt"))
           (b (emacs-agent-document-open workspace "b.txt"))
           (a-revision (emacs-agent-document-revision a))
           (b-revision (emacs-agent-document-revision b)))
      (should-error
       (emacs-agent-transaction-plan
        workspace
        `(((path . "a.txt") (expected_revision . ,a-revision)
           (edits . (((old_text . "old") (new_text . "new")))))
          ((path . "b.txt") (expected_revision . ,b-revision)
           (edits . (((old_text . "missing") (new_text . "new")))))))
       :type 'emacs-agent-error)
      (with-current-buffer (emacs-agent-document-buffer a)
        (should (equal (buffer-string) "old a\n")))
      (with-current-buffer (emacs-agent-document-buffer b)
        (should (equal (buffer-string) "old b\n"))))))

(ert-deftest emacs-agent-transaction-dry-run-and-apply ()
  (emacs-agent-transaction-test--workspace
    (write-region "old a\n" nil (expand-file-name "a.txt" root))
    (write-region "old b\n" nil (expand-file-name "b.txt" root))
    (let* ((a (emacs-agent-document-open workspace "a.txt"))
           (b (emacs-agent-document-open workspace "b.txt"))
           (documents
            (list
             (list
              (cons 'path "a.txt")
              (cons 'expected_revision
                    (emacs-agent-document-revision a))
              (cons 'edits
                    '(((old_text . "old") (new_text . "new")))))
             (list
              (cons 'path "b.txt")
              (cons 'expected_revision
                    (emacs-agent-document-revision b))
              (cons 'edits
                    '(((old_text . "old") (new_text . "new")))))))
           (plan
            (emacs-agent-transaction-plan workspace documents))
           (preview (emacs-agent-transaction-apply plan t)))
      (should-not (plist-get preview :applied))
      (with-current-buffer (emacs-agent-document-buffer a)
        (should (equal (buffer-string) "old a\n")))
      (let ((result (emacs-agent-transaction-apply plan nil nil)))
        (should (plist-get result :applied))
        (should (stringp (plist-get result :changeset_id))))
      (with-current-buffer (emacs-agent-document-buffer a)
        (should (equal (buffer-string) "new a\n")))
      (with-current-buffer (emacs-agent-document-buffer b)
        (should (equal (buffer-string) "new b\n"))))))

(provide 'emacs-agent-transaction-test)
;;; emacs-agent-transaction-test.el ends here
