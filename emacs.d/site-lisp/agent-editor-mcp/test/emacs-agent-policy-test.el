;;; emacs-agent-policy-test.el --- Policy tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for runtime filesystem policy and explicit document targets.

;;; Code:

(require 'ert)
(require 'emacs-agent-policy)
(require 'emacs-agent-runtime)

(ert-deftest emacs-agent-policy-resolves-contained-path ()
  (let ((root (make-temp-file "agent-policy-" t))
        (runtime (emacs-agent-runtime-create)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" root))
          (should
           (equal
            (emacs-agent-resolved-target-canonical-path
             (emacs-agent-policy-resolve-target
              runtime "src/new.el"
              :project-id "project_test"
              :project-root root
              :for-create t))
            (expand-file-name
             "src/new.el"
             (file-name-as-directory (file-truename root))))))
      (delete-directory root t))))

(ert-deftest emacs-agent-policy-rejects-parent-and-git ()
  (let ((root (make-temp-file "agent-policy-" t))
        (runtime (emacs-agent-runtime-create)))
    (unwind-protect
        (progn
          (should-error
           (emacs-agent-policy-resolve-target
            runtime "../secret"
            :project-id "project_test"
            :project-root root
            :for-create t)
           :type 'emacs-agent-error)
          (make-directory (expand-file-name ".git" root))
          (should-error
           (emacs-agent-policy-resolve-target
            runtime ".git/config"
            :project-id "project_test"
            :project-root root
            :for-create t)
           :type 'emacs-agent-error))
      (delete-directory root t))))

(ert-deftest emacs-agent-policy-rejects-escaping-symlink ()
  (let ((root (make-temp-file "agent-policy-root-" t))
        (outside (make-temp-file "agent-policy-outside-" t))
        (runtime (emacs-agent-runtime-create)))
    (unwind-protect
        (progn
          (make-symbolic-link outside (expand-file-name "escape" root))
          (should-error
           (emacs-agent-policy-resolve-target
            runtime "escape/new.el"
            :project-id "project_test"
            :project-root root
            :for-create t)
           :type 'emacs-agent-error))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest emacs-agent-policy-rejects-binary-and-secret ()
  (let ((root (make-temp-file "agent-policy-" t))
        (runtime (emacs-agent-runtime-create)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "binary" root)
            (set-buffer-multibyte nil)
            (insert "a" (string 0) "b"))
          (let ((target
                 (emacs-agent-policy-resolve-target
                  runtime "binary"
                  :project-id "project_test"
                  :project-root root)))
            (should-error
             (emacs-agent-policy-assert-document-target runtime target)
             :type 'emacs-agent-error))
          (should-error
           (emacs-agent-policy-resolve-target
            runtime ".env"
            :project-id "project_test"
            :project-root root
            :for-create t)
           :type 'emacs-agent-error))
      (delete-directory root t))))

(ert-deftest emacs-agent-policy-resolves-direct-absolute-targets ()
  (let* ((root (make-temp-file "agent-policy-direct-" t))
         (existing (expand-file-name "existing.el" root))
         (runtime (emacs-agent-runtime-create)))
    (unwind-protect
        (progn
          (write-region "existing\n" nil existing)
          (let ((target
                 (emacs-agent-policy-resolve-target
                  runtime existing)))
            (should (equal
                     (emacs-agent-resolved-target-canonical-path target)
                     (file-truename existing)))
            (should-not
             (emacs-agent-resolved-target-project-id target))
            (should-not
             (emacs-agent-resolved-target-relative-path target))
            (should
             (equal
              (emacs-agent-policy-target-fields target)
              (list :path (file-truename existing)
                    :project_id nil :relative_path nil))))
          (let* ((missing (expand-file-name "new/sub/file.el" root)))
            (make-directory (file-name-directory missing) t)
            (should
             (equal
              (emacs-agent-resolved-target-canonical-path
               (emacs-agent-policy-resolve-target
                runtime missing :for-create t))
              (expand-file-name
               "file.el"
               (file-name-as-directory
                (file-truename
                 (file-name-directory missing))))))))
      (delete-directory root t))))

(ert-deftest emacs-agent-policy-requires-project-context-for-relative-path ()
  (let ((runtime (emacs-agent-runtime-create)))
    (should-error
     (emacs-agent-policy-resolve-target runtime "lib/example.el")
     :type 'emacs-agent-error)))

(ert-deftest emacs-agent-policy-validates-explicit-project-context ()
  (let* ((root (make-temp-file "agent-policy-project-" t))
         (outside (make-temp-file "agent-policy-outside-" t))
         (runtime (emacs-agent-runtime-create))
         (canonical-root
          (file-name-as-directory (file-truename root))))
    (unwind-protect
        (progn
          (write-region "inside\n" nil
                        (expand-file-name "inside.el" root))
          (let ((target
                 (emacs-agent-policy-resolve-target
                  runtime "inside.el"
                  :project-id "project_test"
                  :project-root canonical-root)))
            (should
             (equal
              (emacs-agent-resolved-target-relative-path target)
              "inside.el"))
            (should
             (equal
              (emacs-agent-resolved-target-project-id target)
              "project_test")))
          (should-error
           (emacs-agent-policy-resolve-target
            runtime "../escape.el"
            :project-id "project_test"
            :project-root canonical-root
            :for-create t)
           :type 'emacs-agent-error)
          (should-error
           (emacs-agent-policy-resolve-target
            runtime (expand-file-name "outside.el" outside)
            :project-id "project_test"
            :project-root canonical-root
            :for-create t)
           :type 'emacs-agent-error))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest emacs-agent-policy-accepts-absolute-path-in-matching-project ()
  (let* ((root (make-temp-file "agent-policy-project-absolute-" t))
         (path (expand-file-name "inside.el" root))
         (runtime (emacs-agent-runtime-create))
         (canonical-root
          (file-name-as-directory (file-truename root))))
    (unwind-protect
        (progn
          (write-region "inside\n" nil path)
          (let ((target
                 (emacs-agent-policy-resolve-target
                  runtime path
                  :project-id "project_test"
                  :project-root canonical-root)))
            (should
             (equal
              (emacs-agent-resolved-target-canonical-path target)
              (file-truename path)))
            (should
             (equal
              (emacs-agent-resolved-target-project-id target)
              "project_test"))
            (should
             (equal
              (emacs-agent-resolved-target-relative-path target)
              "inside.el"))))
      (delete-directory root t))))

(ert-deftest emacs-agent-policy-enforces-runtime-allowlist ()
  (let* ((allowed (make-temp-file "agent-policy-allowed-" t))
         (outside (make-temp-file "agent-policy-denied-" t))
         (runtime
          (emacs-agent-runtime-create
           :filesystem-policy 'allowlist
           :allowed-roots (list allowed))))
    (unwind-protect
        (progn
          (should
           (emacs-agent-policy-resolve-target
            runtime (expand-file-name "new.el" allowed)
            :for-create t))
          (should-error
           (emacs-agent-policy-resolve-target
            runtime (expand-file-name "new.el" outside)
            :for-create t)
           :type 'emacs-agent-error)
          (should-error
           (emacs-agent-policy-resolve-target
            runtime (expand-file-name ".env" allowed)
            :for-create t)
           :type 'emacs-agent-error))
      (delete-directory allowed t)
      (delete-directory outside t))))

(ert-deftest emacs-agent-policy-rejects-direct-symlink-escape ()
  (let* ((allowed (make-temp-file "agent-policy-link-root-" t))
         (outside (make-temp-file "agent-policy-link-outside-" t))
         (runtime
          (emacs-agent-runtime-create
           :filesystem-policy 'allowlist
           :allowed-roots (list allowed))))
    (unwind-protect
        (progn
          (make-symbolic-link outside
                              (expand-file-name "escape" allowed))
          (should-error
           (emacs-agent-policy-resolve-target
            runtime (expand-file-name "escape/new.el" allowed)
            :for-create t)
           :type 'emacs-agent-error))
      (delete-directory allowed t)
      (delete-directory outside t))))

(ert-deftest emacs-agent-policy-rejects-dangling-symlink-create-target ()
  (let* ((allowed (make-temp-file "agent-policy-link-root-" t))
         (outside (make-temp-file "agent-policy-link-outside-" t))
         (link (expand-file-name "dangling.el" allowed))
         (runtime (emacs-agent-runtime-create)))
    (unwind-protect
        (progn
          (make-symbolic-link
           (expand-file-name ".env" outside) link)
          (should-error
           (emacs-agent-policy-resolve-target
            runtime link :for-create t)
           :type 'emacs-agent-error)
          (should-error
           (emacs-agent-policy-resolve-target
            runtime "dangling.el"
            :project-id "project_test"
            :project-root allowed
            :for-create t)
           :type 'emacs-agent-error))
      (delete-directory allowed t)
      (delete-directory outside t))))

(provide 'emacs-agent-policy-test)
;;; emacs-agent-policy-test.el ends here
