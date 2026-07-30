;;; emacs-agent-project-test.el --- Project registry tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for optional, explicit project registration.

;;; Code:

(require 'ert)
(require 'emacs-agent-document)
(require 'emacs-agent-project)
(require 'emacs-agent-runtime)

(defmacro emacs-agent-project-test--with-runtime (&rest body)
  "Run BODY with a fresh editor runtime."
  (declare (indent 0) (debug t))
  `(let ((runtime (emacs-agent-runtime-create)))
     (unwind-protect
         (progn ,@body)
       (emacs-agent-runtime-clear runtime))))

(ert-deftest emacs-agent-project-opens-plain-directory-idempotently ()
  (emacs-agent-project-test--with-runtime
    (let* ((root (make-temp-file "emacs-agent-project-" t))
           (original-directory default-directory)
           (original-buffer (current-buffer)))
      (unwind-protect
          (let* ((first (emacs-agent-project-open runtime root))
                 (second (emacs-agent-project-open runtime root))
                 (project-id (plist-get first :project_id)))
            (should (plist-get first :opened))
            (should-not (plist-get second :opened))
            (should (equal project-id
                           (plist-get second :project_id)))
            (should (equal (plist-get first :type) "directory"))
            (should-not (plist-get first :native_project))
            (should (= 1
                       (plist-get
                        (emacs-agent-project-list runtime)
                        :project_count)))
            (should (equal default-directory original-directory))
            (should (eq (current-buffer) original-buffer)))
        (delete-directory root t)))))

(ert-deftest emacs-agent-project-uses-native-object-only-at-exact-root ()
  (skip-unless (executable-find "git"))
  (emacs-agent-project-test--with-runtime
    (let* ((root (make-temp-file "emacs-agent-native-project-" t))
           (nested (expand-file-name "nested" root)))
      (unwind-protect
          (progn
            (make-directory nested)
            (let ((default-directory root))
              (should
               (zerop
                (process-file "git" nil nil nil "init" "-q"))))
            (let* ((root-info
                    (emacs-agent-project-open runtime root))
                   (nested-info
                    (emacs-agent-project-open runtime nested))
                   (root-project
                    (emacs-agent-project-get
                     runtime (plist-get root-info :project_id)))
                   (nested-project
                    (emacs-agent-project-get
                     runtime (plist-get nested-info :project_id))))
              (should (plist-get root-info :native_project))
              (should
               (file-equal-p
                (project-root
                 (emacs-agent-project-project-object root-project))
                root))
              (should-not (plist-get nested-info :native_project))
              (should (equal (plist-get nested-info :type)
                             "directory"))
              (should
               (file-equal-p
                (project-root
                 (emacs-agent-project-project-object nested-project))
                nested))))
        (delete-directory root t)))))

(ert-deftest emacs-agent-project-keeps-multiple-and-nested-projects ()
  (emacs-agent-project-test--with-runtime
    (let* ((parent (make-temp-file "emacs-agent-parent-" t))
           (child (expand-file-name "nested" parent))
           (sibling (make-temp-file "emacs-agent-sibling-" t)))
      (unwind-protect
          (progn
            (make-directory child)
            (let ((parent-info
                   (emacs-agent-project-open runtime parent))
                  (child-info
                   (emacs-agent-project-open runtime child))
                  (sibling-info
                   (emacs-agent-project-open runtime sibling)))
              (should (= 3
                         (plist-get
                          (emacs-agent-project-list runtime)
                          :project_count)))
              (should (= 3
                         (length
                          (delete-dups
                           (list
                            (plist-get parent-info :project_id)
                            (plist-get child-info :project_id)
                            (plist-get sibling-info :project_id))))))))
        (delete-directory parent t)
        (delete-directory sibling t)))))

(ert-deftest emacs-agent-project-list-and-info-return-complete-metadata ()
  (emacs-agent-project-test--with-runtime
    (let ((root (make-temp-file "emacs-agent-project-metadata-" t)))
      (unwind-protect
          (let* ((opened (emacs-agent-project-open runtime root))
                 (project-id (plist-get opened :project_id))
                 (canonical-root
                  (file-name-as-directory (file-truename root)))
                 (expected-name
                  (file-name-nondirectory
                   (directory-file-name canonical-root)))
                 (listed
                  (car
                   (plist-get
                    (emacs-agent-project-list runtime)
                    :projects)))
                 (info
                  (emacs-agent-project-info runtime project-id)))
            (dolist (metadata (list listed info))
              (should
               (equal
                (plist-get metadata :project_id)
                project-id))
              (should
               (equal
                (plist-get metadata :root)
                canonical-root))
              (should
               (equal
                (plist-get metadata :name)
                expected-name))
              (should
               (equal
                (plist-get metadata :type)
                "directory"))
              (should-not
               (plist-get metadata :native_project))
              (should-not
               (plist-get metadata :opened))))
        (delete-directory root t)))))

(ert-deftest emacs-agent-project-close-only-unregisters-context ()
  (emacs-agent-project-test--with-runtime
    (let ((root (make-temp-file "emacs-agent-project-close-" t)))
      (unwind-protect
          (let* ((opened (emacs-agent-project-open runtime root))
                 (project-id (plist-get opened :project_id))
                 (closed
                  (emacs-agent-project-close runtime project-id)))
            (should (plist-get closed :closed))
            (should-error
             (emacs-agent-project-get runtime project-id)
             :type 'emacs-agent-error)
            (let ((reopened
                   (emacs-agent-project-open runtime root)))
              (should (equal project-id
                             (plist-get reopened :project_id)))
              (should (plist-get reopened :opened))))
        (delete-directory root t)))))

(ert-deftest emacs-agent-project-close-isolates-projects-and-preserves-dirty-buffer ()
  (emacs-agent-project-test--with-runtime
    (let* ((root-a (make-temp-file "emacs-agent-project-close-a-" t))
           (root-b (make-temp-file "emacs-agent-project-close-b-" t))
           (path-a (expand-file-name "a.txt" root-a))
           (path-b (expand-file-name "b.txt" root-b))
           document-a
           document-b)
      (unwind-protect
          (progn
            (write-region "a on disk\n" nil path-a)
            (write-region "b on disk\n" nil path-b)
            (let* ((opened-a
                    (emacs-agent-project-open runtime root-a))
                   (opened-b
                    (emacs-agent-project-open runtime root-b))
                   (project-a
                    (plist-get opened-a :project_id))
                   (project-b
                    (plist-get opened-b :project_id))
                   (target-a
                    (emacs-agent-project-resolve-target
                     runtime "a.txt" :project-id project-a))
                   (target-b
                    (emacs-agent-project-resolve-target
                     runtime "b.txt" :project-id project-b)))
              (setq
               document-a
               (emacs-agent-document-open runtime target-a)
               document-b
               (emacs-agent-document-open runtime target-b))
              (with-current-buffer
                  (emacs-agent-document-buffer document-a)
                (erase-buffer)
                (insert "a unsaved\n"))
              (let* ((closed
                      (emacs-agent-project-close runtime project-a))
                     (listed
                      (emacs-agent-project-list runtime))
                     (remaining
                      (car (plist-get listed :projects)))
                     (direct-a
                      (emacs-agent-policy-resolve-target
                       runtime path-a))
                     (direct-read
                      (emacs-agent-document-read runtime direct-a))
                     (project-read
                      (emacs-agent-document-read runtime target-b)))
                (should (plist-get closed :closed))
                (should (= 1
                           (plist-get
                            closed :managed_document_count)))
                (should (= 1 (plist-get listed :project_count)))
                (should
                 (equal
                  (plist-get remaining :project_id)
                  project-b))
                (should-error
                 (emacs-agent-project-get runtime project-a)
                 :type 'emacs-agent-error)
                (should
                 (equal
                  (plist-get
                   (emacs-agent-project-info runtime project-b)
                   :project_id)
                  project-b))
                (should-error
                 (emacs-agent-project-resolve-target
                  runtime "a.txt" :project-id project-a)
                 :type 'emacs-agent-error)
                (should
                 (buffer-live-p
                  (emacs-agent-document-buffer document-a)))
                (should
                 (buffer-modified-p
                  (emacs-agent-document-buffer document-a)))
                (should
                 (equal
                  (plist-get direct-read :content)
                  "a unsaved\n"))
                (should (plist-get direct-read :modified))
                (should
                 (equal
                  (plist-get project-read :content)
                  "b on disk\n"))
                (should (= 2
                           (hash-table-count
                            (emacs-agent-runtime-document-registry
                             runtime)))))))
        (dolist (document (list document-a document-b))
          (when
              (and
               document
               (buffer-live-p
                (emacs-agent-document-buffer document)))
            (with-current-buffer
                (emacs-agent-document-buffer document)
              (set-buffer-modified-p nil))
            (kill-buffer
             (emacs-agent-document-buffer document))))
        (delete-directory root-a t)
        (delete-directory root-b t)))))

(ert-deftest emacs-agent-project-rejects-invalid-roots ()
  (emacs-agent-project-test--with-runtime
    (let ((missing
           (expand-file-name
            "missing"
            (make-temp-file "emacs-agent-project-base-" t))))
      (unwind-protect
          (progn
            (dolist (root (list "" "relative/project" missing
                                "/ssh:host:/tmp/project"))
              (should-error
               (emacs-agent-project-open runtime root)
               :type 'emacs-agent-error)))
        (delete-directory (file-name-directory missing) t)))))

(ert-deftest emacs-agent-project-open-obeys-runtime-allowlist ()
  (let* ((allowed (make-temp-file "emacs-agent-project-allowed-" t))
         (outside (make-temp-file "emacs-agent-project-outside-" t))
         (runtime
          (emacs-agent-runtime-create
           :filesystem-policy 'allowlist
           :allowed-roots (list allowed))))
    (unwind-protect
        (progn
          (should (emacs-agent-project-open runtime allowed))
          (should-error
           (emacs-agent-project-open runtime outside)
           :type 'emacs-agent-error))
      (emacs-agent-runtime-clear runtime)
      (delete-directory allowed t)
      (delete-directory outside t))))

(ert-deftest emacs-agent-project-resolves-explicit-targets ()
  (emacs-agent-project-test--with-runtime
    (let ((root (make-temp-file "emacs-agent-project-target-" t)))
      (unwind-protect
          (let* ((opened (emacs-agent-project-open runtime root))
                 (project-id (plist-get opened :project_id))
                 (target
                  (emacs-agent-project-resolve-target
                   runtime "lib/new.el"
                   :project-id project-id
                   :for-create t)))
            (should
             (equal
              (emacs-agent-resolved-target-canonical-path target)
              (expand-file-name "lib/new.el"
                                (file-name-as-directory
                                 (file-truename root)))))
            (should
             (equal
              (emacs-agent-resolved-target-relative-path target)
              "lib/new.el"))
            (should-error
             (emacs-agent-project-resolve-target
              runtime "lib/new.el"
              :project-id "project_missing"
              :for-create t)
             :type 'emacs-agent-error))
        (delete-directory root t)))))

(provide 'emacs-agent-project-test)
;;; emacs-agent-project-test.el ends here
