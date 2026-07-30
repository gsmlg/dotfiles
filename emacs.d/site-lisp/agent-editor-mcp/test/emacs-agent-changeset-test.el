;;; emacs-agent-changeset-test.el --- Runtime change-set tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'emacs-agent-changeset)
(require 'emacs-agent-project)
(require 'emacs-agent-runtime)

(defmacro emacs-agent-changeset-test--runtime (&rest body)
  "Run BODY with a runtime and three temporary filesystem roots."
  (declare (indent 0) (debug t))
  `(let* ((root-a (make-temp-file "emacs-agent-changeset-a-" t))
          (root-b (make-temp-file "emacs-agent-changeset-b-" t))
          (direct-root
           (make-temp-file "emacs-agent-changeset-direct-" t))
          (runtime
           (emacs-agent-runtime-create
            :instance-id
            (concat "test-" (file-name-nondirectory direct-root))))
          (emacs-agent-current-runtime runtime))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when
               (cl-some
                (lambda (root)
                  (condition-case nil
                      (file-in-directory-p file root)
                    (file-error nil)))
                (list root-a root-b direct-root))
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (emacs-agent-runtime-clear runtime)
       (dolist (root (list root-a root-b direct-root))
         (when (file-directory-p root)
           (delete-directory root t))))))

(defun emacs-agent-changeset-test--write (path content)
  "Write CONTENT to PATH."
  (make-directory (file-name-directory path) t)
  (write-region content nil path nil 'silent))

(defun emacs-agent-changeset-test--target
    (runtime path &optional project-id)
  "Resolve PATH in RUNTIME with optional PROJECT-ID."
  (emacs-agent-project-resolve-target
   runtime path :project-id project-id :for-create t))

(defun emacs-agent-changeset-test--revision
    (runtime path &optional project-id)
  "Return the current revision for PATH in RUNTIME."
  (emacs-agent-document-revision-for-target
   runtime
   (emacs-agent-changeset-test--target
    runtime path project-id)))

(defun emacs-agent-changeset-test--replace
    (runtime path content &optional project-id)
  "Replace PATH with CONTENT through its canonical buffer in RUNTIME."
  (let* ((target
          (emacs-agent-changeset-test--target
           runtime path project-id))
         (document
          (emacs-agent-document-open runtime target t)))
    (with-current-buffer (emacs-agent-document-buffer document)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)))
    (emacs-agent-document-checkpoint document)
    document))

(defun emacs-agent-changeset-test--dirty-after-save ()
  "Make a rollback target dirty once after it is saved."
  (remove-hook
   'after-save-hook
   #'emacs-agent-changeset-test--dirty-after-save t)
  (goto-char (point-max))
  (insert "dirty after rollback save\n"))

(defun emacs-agent-changeset-test--format-before-save ()
  "Mutate a rollback target once before its checkpoint."
  (remove-hook
   'before-save-hook
   #'emacs-agent-changeset-test--format-before-save t)
  (goto-char (point-max))
  (insert "formatted during rollback\n"))

(defun emacs-agent-changeset-test--record
    (runtime paths before-contents base-revisions &optional unified-diff)
  "Record PATHS after mutation in RUNTIME.
BEFORE-CONTENTS and BASE-REVISIONS correspond positionally to PATHS."
  (emacs-agent-changeset-record
   runtime
   :request-id "request-1"
   :agent-identity "agent-1"
   :operations
   (mapcar
    (lambda (path)
      (list :type 'edit :path path))
    paths)
   :touched-documents paths
   :base-revisions (cl-mapcar #'cons paths base-revisions)
   :final-revisions
   (mapcar
    (lambda (path)
      (cons path
            (emacs-agent-changeset-test--revision runtime path)))
    paths)
   :before-snapshots
   (cl-mapcar
    (lambda (path content)
      (cons path (list :exists t :content content)))
    paths before-contents)
   :checkpoint-state 'checkpointed
   :unified-diff unified-diff))

(ert-deftest emacs-agent-changeset-records-runtime-canonical-frozen-state ()
  (emacs-agent-changeset-test--runtime
    (let* ((spelled
           (expand-file-name "nested/../direct.el" direct-root))
           (canonical
            (file-truename
             (expand-file-name "direct.el" direct-root))))
      (emacs-agent-changeset-test--write canonical "before\n")
      (let ((base
             (emacs-agent-changeset-test--revision runtime spelled)))
        (emacs-agent-changeset-test--replace
         runtime spelled "after\n")
        (let* ((changeset
                (emacs-agent-changeset-test--record
                 runtime (list spelled) '("before\n") (list base)))
               (id
                (emacs-agent-changeset-changeset-id changeset))
               (frozen
                (emacs-agent-changeset-diff runtime id)))
          (should
           (equal
            (emacs-agent-runtime-instance-id runtime)
            (emacs-agent-changeset-runtime-instance-id changeset)))
          (should
           (equal
            (emacs-agent-changeset-touched-documents changeset)
            (list canonical)))
          (should
           (equal
            (caar (emacs-agent-changeset-final-revisions changeset))
            canonical))
          (should (string-match-p "before" frozen))
          (should (string-match-p "after" frozen))
          (with-current-buffer
              (emacs-agent-document-buffer
               (gethash
                canonical
                (emacs-agent-runtime-document-registry runtime)))
            (goto-char (point-max))
            (insert "later human edit\n"))
          (should
           (equal frozen
                  (emacs-agent-changeset-diff runtime id))))))))

(ert-deftest emacs-agent-changeset-rolls-back-direct-file ()
  (emacs-agent-changeset-test--runtime
    (let ((path (expand-file-name "direct.el" direct-root)))
      (emacs-agent-changeset-test--write path "before\n")
      (let ((base
             (emacs-agent-changeset-test--revision runtime path)))
        (emacs-agent-changeset-test--replace runtime path "after\n")
        (let* ((changeset
                (emacs-agent-changeset-test--record
                 runtime (list path) '("before\n") (list base) "frozen"))
               (id
                (emacs-agent-changeset-changeset-id changeset))
               (canonical (file-truename path)))
          (emacs-agent-changeset-rollback runtime id)
          (should
           (eq (emacs-agent-changeset-status changeset) 'rolled-back))
          (should
           (equal
            "before\n"
            (with-current-buffer
                (emacs-agent-document-buffer
                 (gethash
                  canonical
                  (emacs-agent-runtime-document-registry runtime)))
              (buffer-string))))
          (should
           (equal
            "before\n"
            (with-temp-buffer
              (insert-file-contents path)
              (buffer-string)))))))))

(ert-deftest emacs-agent-changeset-rollback-rejects-dirty-after-save ()
  (emacs-agent-changeset-test--runtime
    (let ((path (expand-file-name "rollback-hook.el" direct-root)))
      (emacs-agent-changeset-test--write path "before\n")
      (let ((base
             (emacs-agent-changeset-test--revision runtime path)))
        (emacs-agent-changeset-test--replace runtime path "after\n")
        (let* ((changeset
                (emacs-agent-changeset-test--record
                 runtime (list path) '("before\n") (list base) "frozen"))
               (id
                (emacs-agent-changeset-changeset-id changeset))
               (canonical (file-truename path))
               (document
                (gethash
                 canonical
                 (emacs-agent-runtime-document-registry runtime)))
               (buffer (emacs-agent-document-buffer document)))
          (with-current-buffer buffer
            (add-hook
             'after-save-hook
             #'emacs-agent-changeset-test--dirty-after-save nil t))
          (should-error
           (emacs-agent-changeset-rollback runtime id)
           :type 'emacs-agent-rollback-conflict)
          (should
           (eq (emacs-agent-changeset-status changeset) 'conflicted))
          (should (emacs-agent-document-degraded document))
          (should
           (eq
            (emacs-agent-runtime-health-state runtime)
            'degraded))
          (should (buffer-modified-p buffer))
          (should
           (equal
            (emacs-agent-document--buffer-content buffer)
            "before\ndirty after rollback save\n"))
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents path)
              (buffer-string))
            "before\n")))))))

(ert-deftest emacs-agent-changeset-rollback-rejects-before-save-mutation ()
  (emacs-agent-changeset-test--runtime
    (let ((path (expand-file-name "rollback-format.el" direct-root)))
      (emacs-agent-changeset-test--write path "before\n")
      (let ((base
             (emacs-agent-changeset-test--revision runtime path)))
        (emacs-agent-changeset-test--replace runtime path "after\n")
        (let* ((changeset
                (emacs-agent-changeset-test--record
                 runtime (list path) '("before\n") (list base) "frozen"))
               (id
                (emacs-agent-changeset-changeset-id changeset))
               (canonical (file-truename path))
               (document
                (gethash
                 canonical
                 (emacs-agent-runtime-document-registry runtime)))
               (buffer (emacs-agent-document-buffer document)))
          (with-current-buffer buffer
            (add-hook
             'before-save-hook
             #'emacs-agent-changeset-test--format-before-save nil t))
          (should-error
           (emacs-agent-changeset-rollback runtime id)
           :type 'emacs-agent-rollback-conflict)
          (should
           (eq (emacs-agent-changeset-status changeset) 'conflicted))
          (should
           (equal
            (emacs-agent-changeset-diff runtime id)
            "frozen"))
          (should (emacs-agent-document-degraded document))
          (should
           (eq
            (emacs-agent-runtime-health-state runtime)
            'degraded))
          (should (buffer-modified-p buffer))
          (should
           (equal
            (emacs-agent-document--buffer-content buffer)
            "before\n"))
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents path)
              (buffer-string))
            "before\nformatted during rollback\n")))))))

(ert-deftest emacs-agent-changeset-rolls-back-across-project-roots ()
  (emacs-agent-changeset-test--runtime
    (let* ((project-a
            (plist-get
             (emacs-agent-project-open runtime root-a)
             :project_id))
           (project-b
            (plist-get
             (emacs-agent-project-open runtime root-b)
             :project_id))
           (path-a (expand-file-name "lib/a.el" root-a))
           (path-b (expand-file-name "src/b.el" root-b)))
      (emacs-agent-changeset-test--write path-a "a-before\n")
      (emacs-agent-changeset-test--write path-b "b-before\n")
      (let ((canonical-a (file-truename path-a))
            (canonical-b (file-truename path-b))
            (base-a
             (emacs-agent-changeset-test--revision
              runtime "lib/a.el" project-a))
            (base-b
             (emacs-agent-changeset-test--revision
              runtime "src/b.el" project-b)))
        (emacs-agent-changeset-test--replace
         runtime "lib/a.el" "a-after\n" project-a)
        (emacs-agent-changeset-test--replace
         runtime "src/b.el" "b-after\n" project-b)
        (let* ((changeset
                (emacs-agent-changeset-test--record
                 runtime
                 (list path-a path-b)
                 '("a-before\n" "b-before\n")
                 (list base-a base-b)
                 "cross-project"))
               (id
                (emacs-agent-changeset-changeset-id changeset)))
          (emacs-agent-changeset-rollback runtime id)
          (should
           (equal
            (emacs-agent-changeset-touched-documents changeset)
            (list canonical-a canonical-b)))
          (should
           (equal
            '("a-before\n" "b-before\n")
            (mapcar
             (lambda (path)
               (with-temp-buffer
                 (insert-file-contents path)
                 (buffer-string)))
             (list path-a path-b)))))))))

(ert-deftest emacs-agent-changeset-stale-guard-aborts-before-rollback ()
  (emacs-agent-changeset-test--runtime
    (let ((path-a (expand-file-name "a.el" root-a))
          (path-b (expand-file-name "b.el" root-b)))
      (emacs-agent-changeset-test--write path-a "a-before\n")
      (emacs-agent-changeset-test--write path-b "b-before\n")
      (let ((base-a
             (emacs-agent-changeset-test--revision runtime path-a))
            (base-b
             (emacs-agent-changeset-test--revision runtime path-b)))
        (emacs-agent-changeset-test--replace
         runtime path-a "a-after\n")
        (emacs-agent-changeset-test--replace
         runtime path-b "b-after\n")
        (let* ((changeset
                (emacs-agent-changeset-test--record
                 runtime
                 (list path-a path-b)
                 '("a-before\n" "b-before\n")
                 (list base-a base-b)
                 "guarded"))
               (id
                (emacs-agent-changeset-changeset-id changeset)))
          (emacs-agent-changeset-test--replace
           runtime path-b "human-change\n")
          (should-error
           (emacs-agent-changeset-rollback runtime id)
           :type 'emacs-agent-rollback-conflict)
          (should
           (eq (emacs-agent-changeset-status changeset) 'conflicted))
          (should
           (equal
            '("a-after\n" "human-change\n")
            (mapcar
             (lambda (path)
               (with-temp-buffer
                 (insert-file-contents path)
                 (buffer-string)))
             (list path-a path-b)))))))))

(ert-deftest emacs-agent-changeset-cursors-are-runtime-scoped ()
  (emacs-agent-changeset-test--runtime
    (let* ((path (expand-file-name "cursor.el" direct-root))
           (other-runtime
            (emacs-agent-runtime-create :instance-id "other-runtime")))
      (emacs-agent-changeset-test--write path "before\n")
      (let ((base
             (emacs-agent-changeset-test--revision runtime path)))
        (emacs-agent-changeset-test--replace runtime path "after-one\n")
        (emacs-agent-changeset-test--record
         runtime (list path) '("before\n") (list base)
         "123456789")
        (let ((base-two
               (emacs-agent-changeset-test--revision runtime path)))
          (emacs-agent-changeset-test--replace
           runtime path "after-two\n")
          (let* ((changeset
                  (emacs-agent-changeset-test--record
                   runtime (list path) '("after-one\n")
                   (list base-two) "abcdefghi"))
                 (page
                  (emacs-agent-changeset-query
                   runtime :path path :limit 1))
                 (cursor (plist-get page :cursor))
                 (detail
                  (emacs-agent-changeset-detail
                   runtime
                   (emacs-agent-changeset-changeset-id changeset)
                   :max-chars 4))
                 (diff-cursor
                  (plist-get detail :diff_cursor)))
            (should cursor)
            (should diff-cursor)
            (should-error
             (emacs-agent-changeset-query
              other-runtime :path path :limit 1 :cursor cursor)
             :type 'emacs-agent-changeset-error)
            (should-error
             (emacs-agent-changeset-detail
              other-runtime
              (emacs-agent-changeset-changeset-id changeset)
              :max-chars 4 :cursor diff-cursor)
             :type 'emacs-agent-changeset-error)))))))

(ert-deftest emacs-agent-changeset-partial-failure-degrades-runtime ()
  (emacs-agent-changeset-test--runtime
    (let ((path-a (expand-file-name "partial-a.el" root-a))
          (path-b (expand-file-name "partial-b.el" root-b)))
      (emacs-agent-changeset-test--write path-a "a-before\n")
      (emacs-agent-changeset-test--write path-b "b-before\n")
      (let ((canonical-a (file-truename path-a))
            (canonical-b (file-truename path-b))
            (base-a
             (emacs-agent-changeset-test--revision runtime path-a))
            (base-b
             (emacs-agent-changeset-test--revision runtime path-b)))
        (emacs-agent-changeset-test--replace
         runtime path-a "a-after\n")
        (emacs-agent-changeset-test--replace
         runtime path-b "b-after\n")
        (let* ((changeset
                (emacs-agent-changeset-test--record
                 runtime
                 (list path-a path-b)
                 '("a-before\n" "b-before\n")
                 (list base-a base-b)
                 "partial"))
               (id
                (emacs-agent-changeset-changeset-id changeset))
               (restore
                (symbol-function
                 'emacs-agent-changeset--restore-one)))
          (cl-letf
              (((symbol-function
                'emacs-agent-changeset--restore-one)
                (lambda (active-runtime entry)
                  (if (equal (car entry) canonical-b)
                      (error "Forced second restore failure")
                    (funcall restore active-runtime entry)))))
            (should-error
             (emacs-agent-changeset-rollback runtime id)
             :type 'emacs-agent-rollback-conflict))
          (should
           (eq (emacs-agent-changeset-status changeset) 'conflicted))
          (should
           (eq (emacs-agent-runtime-health-state runtime) 'degraded))
          (should
           (emacs-agent-document-degraded
            (gethash
             canonical-b
             (emacs-agent-runtime-document-registry runtime))))
          (let ((activity
                 (car (emacs-agent-runtime-recent-activity runtime))))
            (should (equal (plist-get activity :status) "partial"))
            (should
             (equal
              (plist-get activity :restored_paths)
              (list canonical-a)))
            (should
             (equal
              (plist-get activity :failed_path)
              canonical-b))))))))

(provide 'emacs-agent-changeset-test)
;;; emacs-agent-changeset-test.el ends here
