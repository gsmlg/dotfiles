;;; emacs-agent-document-test.el --- Document tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-agent-document)

(defmacro emacs-agent-document-test-with-file (content &rest body)
  "Evaluate BODY with a temporary ROOT containing file.txt with CONTENT."
  (declare (indent 1))
  `(let* ((root (make-temp-file "agent-document-" t))
          (path (expand-file-name "file.txt" root))
          (emacs-agent-document-registry (make-hash-table :test #'equal))
          (emacs-agent-document-cursors (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (with-temp-file path (insert ,content))
           ,@body)
       (when-let* ((buffer (get-file-buffer path)))
         (set-buffer-modified-p nil)
         (kill-buffer buffer))
       (delete-directory root t))))

(ert-deftest emacs-agent-document-read-sees-unsaved-buffer ()
  (emacs-agent-document-test-with-file "disk\n"
    (let* ((document (emacs-agent-document-open root "file.txt"))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "human unsaved\n"))
      (let ((result (emacs-agent-document-read root "file.txt")))
        (should (equal (plist-get result :content) "human unsaved\n"))
        (should (plist-get result :modified))
        (should (string-prefix-p "rev:" (plist-get result :revision)))))))

(ert-deftest emacs-agent-document-revision-changes-with-buffer ()
  (emacs-agent-document-test-with-file "one\n"
    (let* ((document (emacs-agent-document-open root "file.txt"))
           (before (emacs-agent-document-revision document)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "two\n"))
      (should-not
       (equal before (emacs-agent-document-revision document))))))

(ert-deftest emacs-agent-document-revision-is-content-stable ()
  (emacs-agent-document-test-with-file "one\n"
    (let* ((document (emacs-agent-document-open root "file.txt"))
           (before (emacs-agent-document-revision document)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "temporary")
        (delete-region (- (point) (length "temporary")) (point)))
      (should (equal before
                     (emacs-agent-document-revision document))))))

(ert-deftest emacs-agent-document-status-does-not-visit-file ()
  (emacs-agent-document-test-with-file "one\n"
    (let ((status (emacs-agent-document-status root "file.txt")))
      (should-not (plist-get status :visited))
      (should (plist-get status :exists_on_disk))
      (should (string-prefix-p "rev:" (plist-get status :revision)))
      (should-not (get-file-buffer path)))))

(ert-deftest emacs-agent-workspace-modified-documents-finds-dirty-buffer ()
  (emacs-agent-document-test-with-file "one\n"
    (let ((document (emacs-agent-document-open root "file.txt")))
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "dirty"))
      (let ((items (emacs-agent-workspace-modified-documents root)))
        (should (= (length items) 1))
        (should (equal (plist-get (car items) :path) "file.txt"))
        (should (plist-get (car items) :modified))))))

(ert-deftest emacs-agent-document-read-paginates-with-revision-cursor ()
  (emacs-agent-document-test-with-file "abcdef\n"
    (let* ((first (emacs-agent-document-read root "file.txt" nil nil 3))
           (second (emacs-agent-document-read
                    root "file.txt" nil nil 3 (plist-get first :cursor))))
      (should (equal (plist-get first :content) "abc"))
      (should (plist-get first :truncated))
      (should (equal (plist-get second :content) "def"))
      (should (plist-get second :truncated)))))

(ert-deftest emacs-agent-document-external-change-reloads-clean-buffer ()
  (emacs-agent-document-test-with-file "old\n"
    (let ((document (emacs-agent-document-open root "file.txt")))
      (sleep-for 0.01)
      (with-temp-file path (insert "new content\n"))
      (emacs-agent-document-reconcile document)
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "new content\n"))))))

(ert-deftest emacs-agent-document-external-change-conflicts-with-dirty-buffer ()
  (emacs-agent-document-test-with-file "old\n"
    (let ((document (emacs-agent-document-open root "file.txt")))
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "dirty"))
      (with-temp-file path (insert "external replacement\n"))
      (should-error (emacs-agent-document-reconcile document)
                    :type 'emacs-agent-error))))

(provide 'emacs-agent-document-test)
;;; emacs-agent-document-test.el ends here
