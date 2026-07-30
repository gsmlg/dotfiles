;;; emacs-agent-document-test.el --- Document tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-agent-document)
(require 'emacs-agent-project)
(require 'emacs-agent-runtime)

;;; Code:

(defmacro emacs-agent-document-test-with-file (content &rest body)
  "Evaluate BODY with a runtime and temporary file containing CONTENT."
  (declare (indent 1))
  `(let* ((root (make-temp-file "agent-document-" t))
          (path (expand-file-name "file.txt" root))
          (runtime (emacs-agent-runtime-create))
          (emacs-agent-document-cursors (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (with-temp-file path (insert ,content))
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (condition-case nil
                     (file-in-directory-p (file-truename file) root)
                   (file-error
                    (file-in-directory-p (expand-file-name file) root)))
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory root t))))

(defun emacs-agent-document-test-direct-target (runtime path &optional create)
  "Resolve direct absolute PATH in RUNTIME.
When CREATE is non-nil, permit a missing leaf."
  (emacs-agent-policy-resolve-target runtime path :for-create create))

(defun emacs-agent-document-test-project-target (runtime root path)
  "Open ROOT in RUNTIME and resolve project-relative PATH."
  (let* ((opened (emacs-agent-project-open runtime root))
         (project-id (plist-get opened :project_id)))
    (emacs-agent-project-resolve-target
     runtime path :project-id project-id)))

(ert-deftest emacs-agent-document-read-sees-unsaved-buffer ()
  (emacs-agent-document-test-with-file "disk\n"
    (let* ((target (emacs-agent-document-test-direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "human unsaved\n"))
      (let ((result (emacs-agent-document-read runtime target)))
        (should (equal (plist-get result :path) (file-truename path)))
        (should-not (plist-get result :project_id))
        (should-not (plist-get result :relative_path))
        (should (equal (plist-get result :content) "human unsaved\n"))
        (should (plist-get result :modified))
        (should (string-prefix-p "rev:" (plist-get result :revision)))))))

(ert-deftest emacs-agent-document-project-and-direct-target-share-identity ()
  (emacs-agent-document-test-with-file "one\n"
    (let* ((direct (emacs-agent-document-test-direct-target runtime path))
           (project (emacs-agent-document-test-project-target
                     runtime root "file.txt"))
           (direct-document (emacs-agent-document-open runtime direct))
           (project-document (emacs-agent-document-open runtime project))
           (project-fields
            (emacs-agent-document-output-fields project))
           (project-read
            (emacs-agent-document-read runtime project)))
      (should (eq direct-document project-document))
      (should (eq (emacs-agent-document-buffer direct-document)
                  (emacs-agent-document-buffer project-document)))
      (should (= (hash-table-count
                  (emacs-agent-runtime-document-registry runtime))
                 1))
      (should (equal (plist-get project-fields :path)
                     (file-truename path)))
      (should (equal (plist-get project-fields :project_id)
                     (emacs-agent-resolved-target-project-id project)))
      (should (equal (plist-get project-fields :relative_path)
                     "file.txt"))
      (should (equal (plist-get project-read :path)
                     (file-truename path)))
      (should (equal (plist-get project-read :project_id)
                     (emacs-agent-resolved-target-project-id project)))
      (should (equal (plist-get project-read :relative_path)
                     "file.txt")))))

(ert-deftest emacs-agent-document-shares-state-across-direct-and-nested-projects ()
  (emacs-agent-document-test-with-file "unused\n"
    (let* ((nested-root (expand-file-name "nested" root))
           (nested-path (expand-file-name "shared.txt" nested-root)))
      (make-directory nested-root)
      (write-region "disk\n" nil nested-path)
      (let* ((parent-info (emacs-agent-project-open runtime root))
             (nested-info (emacs-agent-project-open runtime nested-root))
             (direct-target
              (emacs-agent-document-test-direct-target
               runtime nested-path))
             (parent-target
              (emacs-agent-project-resolve-target
               runtime "nested/shared.txt"
               :project-id (plist-get parent-info :project_id)))
             (nested-target
              (emacs-agent-project-resolve-target
               runtime "shared.txt"
               :project-id (plist-get nested-info :project_id)))
             (direct-document
              (emacs-agent-document-open runtime direct-target))
             (parent-document
              (emacs-agent-document-open runtime parent-target))
             (nested-document
              (emacs-agent-document-open runtime nested-target))
             (direct-read
              (emacs-agent-document-read runtime direct-target))
             (parent-read
              (emacs-agent-document-read runtime parent-target)))
        (should (eq direct-document parent-document))
        (should (eq direct-document nested-document))
        (should (= 1
                   (hash-table-count
                    (emacs-agent-runtime-document-registry runtime))))
        (should
         (equal
          (plist-get direct-read :revision)
          (plist-get parent-read :revision)))
        (with-current-buffer
            (emacs-agent-document-buffer parent-document)
          (erase-buffer)
          (insert "unsaved through parent\n"))
        (let ((direct-after
               (emacs-agent-document-read runtime direct-target))
              (nested-after
               (emacs-agent-document-read runtime nested-target)))
          (should
           (equal
            (plist-get direct-after :content)
            "unsaved through parent\n"))
          (should
           (equal
            (plist-get nested-after :content)
            "unsaved through parent\n"))
          (should (plist-get direct-after :modified))
          (should (plist-get nested-after :modified))
          (should
           (equal
            (plist-get direct-after :revision)
            (plist-get nested-after :revision))))))))

(ert-deftest emacs-agent-document-canonicalizes-symlink-identity ()
  (emacs-agent-document-test-with-file "one\n"
    (let* ((link (expand-file-name "alias.txt" root))
           (_ (make-symbolic-link path link))
           (real-target
            (emacs-agent-document-test-direct-target runtime path))
           (link-target
            (emacs-agent-document-test-direct-target runtime link)))
      (should (eq (emacs-agent-document-open runtime real-target)
                  (emacs-agent-document-open runtime link-target)))
      (should (= (hash-table-count
                  (emacs-agent-runtime-document-registry runtime))
                 1)))))

(ert-deftest emacs-agent-document-checkpoint-preserves-hard-link-backup-semantics ()
  (emacs-agent-document-test-with-file "old\n"
    (let ((alias (expand-file-name "alias.txt" root)))
      (condition-case error-data
          (add-name-to-file path alias)
        (file-error
         (ert-skip (error-message-string error-data))))
      (let* ((target
              (emacs-agent-document-test-direct-target runtime path))
             (document (emacs-agent-document-open runtime target))
             (buffer (emacs-agent-document-buffer document)))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "new\n")
          (let ((make-backup-files t)
                (backup-inhibited nil)
                (backup-by-copying nil)
                (backup-by-copying-when-linked nil)
                (backup-directory-alist nil)
                (buffer-backed-up nil))
            (emacs-agent-document-checkpoint document)))
        (should
         (equal
          (with-temp-buffer
            (insert-file-contents path)
            (buffer-string))
          "new\n"))
        (should
         (equal
          (with-temp-buffer
            (insert-file-contents alias)
            (buffer-string))
          "old\n"))
        (should-not
         (equal
          (file-attribute-inode-number (file-attributes path 'integer))
          (file-attribute-inode-number
           (file-attributes alias 'integer))))))))

(ert-deftest emacs-agent-document-revision-changes-with-buffer ()
  (emacs-agent-document-test-with-file "one\n"
    (let* ((target (emacs-agent-document-test-direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (before (emacs-agent-document-revision document)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "two\n"))
      (should-not
       (equal before (emacs-agent-document-revision document))))))

(ert-deftest emacs-agent-document-revision-is-content-stable ()
  (emacs-agent-document-test-with-file "one\n"
    (let* ((target (emacs-agent-document-test-direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (before (emacs-agent-document-revision document)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "temporary")
        (delete-region (- (point) (length "temporary")) (point)))
      (should (equal before
                     (emacs-agent-document-revision document))))))

(ert-deftest emacs-agent-document-status-does-not-visit-file ()
  (emacs-agent-document-test-with-file "one\n"
    (let* ((target (emacs-agent-document-test-direct-target runtime path))
           (status (emacs-agent-document-status runtime target)))
      (should (equal (plist-get status :path) (file-truename path)))
      (should-not (plist-get status :project_id))
      (should-not (plist-get status :relative_path))
      (should-not (plist-get status :visited))
      (should (plist-get status :exists_on_disk))
      (should (string-prefix-p "rev:" (plist-get status :revision)))
      (should-not (get-file-buffer path)))))

(ert-deftest emacs-agent-document-status-renders-explicit-project-context ()
  (emacs-agent-document-test-with-file "one\n"
    (let* ((target
            (emacs-agent-document-test-project-target
             runtime root "file.txt"))
           (status (emacs-agent-document-status runtime target)))
      (should (equal (plist-get status :path) (file-truename path)))
      (should (equal (plist-get status :project_id)
                     (emacs-agent-resolved-target-project-id target)))
      (should (equal (plist-get status :relative_path) "file.txt")))))

(ert-deftest emacs-agent-document-modified-documents-uses-runtime-registry ()
  (emacs-agent-document-test-with-file "one\n"
    (let* ((target (emacs-agent-document-test-direct-target runtime path))
           (document (emacs-agent-document-open runtime target)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "dirty"))
      (let ((items (emacs-agent-document-modified-documents runtime)))
        (should (= (length items) 1))
        (should (equal (plist-get (car items) :path)
                       (file-truename path)))
        (should-not (plist-get (car items) :project_id))
        (should-not (plist-get (car items) :relative_path))
        (should (plist-get (car items) :modified))))))

(ert-deftest emacs-agent-document-read-paginates-with-runtime-bound-cursor ()
  (emacs-agent-document-test-with-file "abcdef\n"
    (let* ((target (emacs-agent-document-test-direct-target runtime path))
           (other-runtime (emacs-agent-runtime-create))
           (foreign-first
            (emacs-agent-document-read runtime target nil nil 3))
           (first (emacs-agent-document-read runtime target nil nil 3))
           (_
            (should-error
             (emacs-agent-document-read
              other-runtime
              (emacs-agent-document-test-direct-target other-runtime path)
              nil nil 3 (plist-get foreign-first :cursor))
             :type 'emacs-agent-error))
           (second (emacs-agent-document-read
                    runtime target nil nil 3 (plist-get first :cursor))))
      (should (equal (plist-get first :content) "abc"))
      (should (plist-get first :truncated))
      (should (equal (plist-get second :content) "def"))
      (should (plist-get second :truncated)))))

(ert-deftest emacs-agent-document-read-cursor-preserves-original-range ()
  (emacs-agent-document-test-with-file "aaa\nbbbb\nccc\n"
    (let* ((target
            (emacs-agent-document-test-direct-target runtime path))
           (first
            (emacs-agent-document-read runtime target 2 2 2))
           (second
            (emacs-agent-document-read
             runtime target nil nil 20
             (plist-get first :cursor))))
      (should (equal (plist-get first :content) "bb"))
      (should (equal (plist-get second :content) "bb\n"))
      (should (= (plist-get second :start_line) 2))
      (should (= (plist-get second :end_line) 2)))))

(ert-deftest emacs-agent-document-read-cursor-rejects-conflicting-range ()
  (emacs-agent-document-test-with-file "aaa\nbbbb\nccc\n"
    (let* ((target
            (emacs-agent-document-test-direct-target runtime path))
           (first
            (emacs-agent-document-read runtime target 2 2 2)))
      (should-error
       (emacs-agent-document-read
        runtime target 1 2 20
        (plist-get first :cursor))
       :type 'emacs-agent-error))))

(ert-deftest emacs-agent-document-read-cursor-binds-project-context ()
  (emacs-agent-document-test-with-file "abcdef\n"
    (let* ((direct
            (emacs-agent-document-test-direct-target runtime path))
           (project
            (emacs-agent-document-test-project-target
             runtime root "file.txt"))
           (first
            (emacs-agent-document-read runtime project nil nil 3)))
      (should-error
       (emacs-agent-document-read
        runtime direct nil nil 3
        (plist-get first :cursor))
       :type 'emacs-agent-error))))

(ert-deftest emacs-agent-document-external-change-reloads-clean-buffer ()
  (emacs-agent-document-test-with-file "old\n"
    (let* ((target (emacs-agent-document-test-direct-target runtime path))
           (document (emacs-agent-document-open runtime target)))
      (sleep-for 0.01)
      (with-temp-file path (insert "new content\n"))
      (emacs-agent-document-reconcile document)
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "new content\n"))))))

(ert-deftest emacs-agent-document-external-change-conflicts-with-dirty-buffer ()
  (emacs-agent-document-test-with-file "old\n"
    (let* ((target (emacs-agent-document-test-direct-target runtime path))
           (document (emacs-agent-document-open runtime target)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "dirty"))
      (with-temp-file path (insert "external replacement\n"))
      (should-error (emacs-agent-document-reconcile document)
                    :type 'emacs-agent-error))))

(ert-deftest emacs-agent-document-open-rejects-missing-without-create ()
  (let* ((root (make-temp-file "agent-document-missing-" t))
         (path (expand-file-name "missing.txt" root))
         (runtime (emacs-agent-runtime-create))
         (target
          (emacs-agent-document-test-direct-target runtime path t)))
    (unwind-protect
        (should-error
         (emacs-agent-document-open runtime target)
         :type 'emacs-agent-error)
      (delete-directory root t))))

(ert-deftest emacs-agent-document-open-allows-missing-create-target ()
  (let* ((root (make-temp-file "agent-document-create-" t))
         (path (expand-file-name "missing.txt" root))
         (runtime (emacs-agent-runtime-create))
         (target
          (emacs-agent-document-test-direct-target runtime path t))
         document)
    (unwind-protect
        (progn
          (setq document
                (emacs-agent-document-open runtime target t))
          (should (buffer-live-p
                   (emacs-agent-document-buffer document)))
          (should-not (file-exists-p path)))
      (when (and document
                 (buffer-live-p
                  (emacs-agent-document-buffer document)))
        (with-current-buffer (emacs-agent-document-buffer document)
          (set-buffer-modified-p nil))
        (kill-buffer (emacs-agent-document-buffer document)))
      (delete-directory root t))))

(ert-deftest emacs-agent-document-open-preserves-deleted-visiting-buffer ()
  (emacs-agent-document-test-with-file "disk\n"
    (let ((buffer (find-file-noselect path)))
      (delete-file path)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "unsaved\n"))
      (let* ((target
              (emacs-agent-document-test-direct-target runtime path t))
             (document (emacs-agent-document-open runtime target)))
        (should (eq (emacs-agent-document-buffer document) buffer))
        (with-current-buffer buffer
          (should (equal (buffer-string) "disk\nunsaved\n")))))))

(ert-deftest emacs-agent-document-open-rejects-binary-large-and-special ()
  (let* ((root (make-temp-file "agent-document-types-" t))
         (binary (expand-file-name "binary.dat" root))
         (large (expand-file-name "large.txt" root))
         (runtime (emacs-agent-runtime-create)))
    (unwind-protect
        (progn
          (with-temp-file binary
            (set-buffer-multibyte nil)
            (insert "a" (string 0) "b"))
          (with-temp-file large
            (insert "12345"))
          (should-error
           (emacs-agent-document-open
            runtime
            (emacs-agent-document-test-direct-target runtime binary))
           :type 'emacs-agent-error)
          (let ((emacs-agent-policy-maximum-document-bytes 4))
            (should-error
             (emacs-agent-document-open
              runtime
              (emacs-agent-document-test-direct-target runtime large))
             :type 'emacs-agent-error))
          (should-error
           (emacs-agent-document-open
            runtime
            (emacs-agent-document-test-direct-target runtime root))
           :type 'emacs-agent-error))
      (delete-directory root t))))

(ert-deftest emacs-agent-document-rejects-remote-and-denied-targets ()
  (let ((runtime (emacs-agent-runtime-create)))
    (should-error
     (emacs-agent-policy-resolve-target
      runtime "/ssh:example.invalid:/tmp/file.txt")
     :type 'emacs-agent-error)
    (should-error
     (emacs-agent-policy-resolve-target runtime "/tmp/.env" :for-create t)
     :type 'emacs-agent-error)))

(provide 'emacs-agent-document-test)
;;; emacs-agent-document-test.el ends here
