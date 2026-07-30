;;; emacs-agent-edit-test.el --- Guarded edit tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-agent-edit)
(require 'emacs-agent-project)

;;; Code:

(defmacro emacs-agent-edit-test-with-file (content &rest body)
  "Evaluate BODY with temporary file.txt initialized to CONTENT."
  (declare (indent 1))
  `(let* ((root (make-temp-file "agent-edit-" t))
          (path (expand-file-name "file.txt" root))
          (runtime
           (emacs-agent-runtime-create :save-policy 'manual))
          target)
     (unwind-protect
         (progn
           (with-temp-file path (insert ,content))
           (setq target
                 (emacs-agent-project-resolve-target runtime path))
           ,@body)
       (when-let* ((buffer (get-file-buffer path)))
         (set-buffer-modified-p nil)
         (kill-buffer buffer))
       (emacs-agent-runtime-clear runtime)
       (delete-directory root t))))

(defun emacs-agent-edit-test-edit
    (start-line start-column end-line end-column text &optional expected)
  "Construct one test edit.
START-LINE and START-COLUMN locate its beginning; END-LINE and END-COLUMN
locate its end.  Insert TEXT and optionally require EXPECTED source text."
  (append
   (list :start (list :line start-line :column start-column)
         :end (list :line end-line :column end-column)
         :new_text text)
   (when expected (list :expected_text expected))))

(ert-deftest emacs-agent-edit-applies-unicode-character-columns ()
  (emacs-agent-edit-test-with-file "aλ\tz\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (result
            (emacs-agent-edit-apply
             runtime target revision
             (list (emacs-agent-edit-test-edit 1 1 1 2 "β" "λ")))))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "aβ\tz\n")))
      (should (equal (plist-get result :path) (file-truename path)))
      (should-not (plist-get result :project_id))
      (should-not (plist-get result :relative_path))
      (should-not (equal revision (plist-get result :new_revision))))))

(ert-deftest emacs-agent-edit-returns-project-relative-output-fields ()
  (emacs-agent-edit-test-with-file "old\n"
    (ignore target)
    (let* ((opened (emacs-agent-project-open runtime root))
           (project-id (plist-get opened :project_id))
           (project-target
            (emacs-agent-project-resolve-target
             runtime "file.txt" :project-id project-id))
           (document
            (emacs-agent-document-open runtime project-target))
           (revision (emacs-agent-document-revision document))
           (result
            (emacs-agent-edit-apply
             runtime project-target revision
             (list
              (emacs-agent-edit-test-edit
               1 0 1 3 "new" "old")))))
      (should (equal (plist-get result :path)
                     (file-truename path)))
      (should (equal (plist-get result :project_id)
                     project-id))
      (should (equal (plist-get result :relative_path)
                     "file.txt")))))

(ert-deftest emacs-agent-edit-counts-tab-combining-and-emoji-as-characters ()
  (emacs-agent-edit-test-with-file "a\t中é😀z\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document)))
      ;; Columns are: a=0, tab=1, 中=2, e=3, combining acute=4,
      ;; emoji=5, z=6.  Display width must not affect the public range.
      (emacs-agent-edit-apply
       runtime target revision
       (list (emacs-agent-edit-test-edit 1 4 1 6 "X" "́😀")))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "a\t中eXz\n"))))))

(ert-deftest emacs-agent-edit-crlf-positions-use-logical-lines ()
  (emacs-agent-edit-test-with-file "one\r\ntwo\r\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (= (coding-system-eol-type buffer-file-coding-system) 1)))
      (emacs-agent-edit-apply
       runtime target revision
       (list (emacs-agent-edit-test-edit 2 0 2 3 "TWO" "two")))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "one\nTWO\n"))))))

(ert-deftest emacs-agent-edit-rejects-stale-revision-without-change ()
  (emacs-agent-edit-test-with-file "abc\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (buffer (emacs-agent-document-buffer document)))
      (should-error
       (emacs-agent-edit-apply
        runtime target "rev:stale"
        (list (emacs-agent-edit-test-edit 1 0 1 1 "x")))
       :type 'emacs-agent-error)
      (with-current-buffer buffer
        (should (equal (buffer-string) "abc\n"))))))

(ert-deftest emacs-agent-edit-validates-all-guards-before-change ()
  (emacs-agent-edit-test-with-file "abcdef\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-edit-apply
        runtime target revision
        (list (emacs-agent-edit-test-edit 1 0 1 1 "A" "a")
              (emacs-agent-edit-test-edit 1 5 1 6 "F" "wrong")))
       :type 'emacs-agent-error)
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "abcdef\n"))))))

(ert-deftest emacs-agent-edit-rejects-overlaps ()
  (emacs-agent-edit-test-with-file "abcdef\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-edit-apply
        runtime target revision
        (list (emacs-agent-edit-test-edit 1 0 1 3 "x")
              (emacs-agent-edit-test-edit 1 2 1 4 "y")))
       :type 'emacs-agent-error))))

(ert-deftest emacs-agent-edit-is-one-undo-unit-and-preserves-point ()
  (emacs-agent-edit-test-with-file "abcdef\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (buffer (emacs-agent-document-buffer document))
           revision)
      (with-current-buffer buffer
        (buffer-enable-undo)
        (goto-char 4)
        (setq revision (emacs-agent-document-revision document)))
      (emacs-agent-edit-apply
       runtime target revision
       (list (emacs-agent-edit-test-edit 1 0 1 1 "A")
             (emacs-agent-edit-test-edit 1 5 1 6 "F")))
      (with-current-buffer buffer
        (should (= (point) 4))
        (undo)
        (should (equal (buffer-string) "abcdef\n"))))))

(ert-deftest emacs-agent-edit-point-follows-normal-marker-movement ()
  (emacs-agent-edit-test-with-file "abcdef\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (buffer (emacs-agent-document-buffer document))
           revision)
      (with-current-buffer buffer
        (goto-char 4)
        (setq revision (emacs-agent-document-revision document)))
      (emacs-agent-edit-apply
       runtime target revision
       (list (emacs-agent-edit-test-edit 1 0 1 0 "XX")))
      (with-current-buffer buffer
        (should (= (point) 6))))))

(ert-deftest emacs-agent-edit-save-failure-rolls-back-and-degrades ()
  (emacs-agent-edit-test-with-file "abcdef\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (buffer (emacs-agent-document-buffer document))
           (revision (emacs-agent-document-revision document)))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (error "Test save failure"))))
        (should-error
         (emacs-agent-edit-apply
          runtime target revision
          (list (emacs-agent-edit-test-edit 1 0 1 1 "A"))
          t)
         :type 'emacs-agent-error))
      (with-current-buffer buffer
        (should (equal (buffer-string) "abcdef\n")))
      (should (emacs-agent-document-degraded document))
      (should-error
       (emacs-agent-edit-apply
        runtime target (emacs-agent-document-revision document)
        (list (emacs-agent-edit-test-edit 1 0 1 1 "A")))
       :type 'emacs-agent-error))))

(ert-deftest emacs-agent-edit-rejects-binary-result-atomically ()
  (emacs-agent-edit-test-with-file "abc\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-edit-apply
        runtime target revision
        (list (emacs-agent-edit-test-edit 1 1 1 1 (string 0))))
       :type 'emacs-agent-error)
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "abc\n"))))))

(ert-deftest emacs-agent-edit-rejects-oversized-result-atomically ()
  (emacs-agent-edit-test-with-file "abc\n"
    (let* ((emacs-agent-policy-maximum-document-bytes 5)
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-edit-apply
        runtime target revision
        (list (emacs-agent-edit-test-edit 1 0 1 0 "XX")))
       :type 'emacs-agent-error)
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "abc\n"))))))

(provide 'emacs-agent-edit-test)
;;; emacs-agent-edit-test.el ends here
