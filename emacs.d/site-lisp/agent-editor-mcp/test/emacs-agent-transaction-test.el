;;; emacs-agent-transaction-test.el --- Runtime transaction tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-agent-transaction)

;;; Code:
(require 'emacs-agent-project)

(defmacro emacs-agent-transaction-test--runtime (&rest body)
  "Run BODY with a temporary runtime and directory."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "agent-transaction-" t))
          (runtime
           (emacs-agent-runtime-create :save-policy 'manual)))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file root)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (emacs-agent-runtime-clear runtime)
       (delete-directory root t))))

(defun emacs-agent-transaction-test--format-before-save ()
  "Insert deterministic formatter text once before saving."
  (remove-hook
   'before-save-hook
   #'emacs-agent-transaction-test--format-before-save t)
  (goto-char (point-max))
  (insert "formatted by hook\n"))

(defun emacs-agent-transaction-test--dirty-after-save ()
  "Make the current transaction buffer dirty once after saving."
  (remove-hook
   'after-save-hook
   #'emacs-agent-transaction-test--dirty-after-save t)
  (goto-char (point-max))
  (insert "dirty after save\n"))

(ert-deftest emacs-agent-transaction-validation-is-all-or-nothing ()
  (emacs-agent-transaction-test--runtime
   (let ((a-path (expand-file-name "a.txt" root))
         (b-path (expand-file-name "b.txt" root)))
     (write-region "old a\n" nil a-path)
     (write-region "old b\n" nil b-path)
     (let* ((a-target
             (emacs-agent-project-resolve-target runtime a-path))
            (b-target
             (emacs-agent-project-resolve-target runtime b-path))
            (a (emacs-agent-document-open runtime a-target))
            (b (emacs-agent-document-open runtime b-target))
            (a-revision (emacs-agent-document-revision a))
            (b-revision (emacs-agent-document-revision b)))
       (should-error
        (emacs-agent-transaction-plan
         runtime
         `(((path . ,a-path) (expected_revision . ,a-revision)
            (edits . (((old_text . "old") (new_text . "new")))))
           ((path . ,b-path) (expected_revision . ,b-revision)
            (edits . (((old_text . "missing") (new_text . "new")))))))
        :type 'emacs-agent-error)
       (with-current-buffer (emacs-agent-document-buffer a)
         (should (equal (buffer-string) "old a\n")))
       (with-current-buffer (emacs-agent-document-buffer b)
         (should (equal (buffer-string) "old b\n")))))))

(ert-deftest emacs-agent-transaction-rejects-unsafe-result-before-mutation ()
  (emacs-agent-transaction-test--runtime
   (let* ((a-path (expand-file-name "a.txt" root))
          (b-path (expand-file-name "b.txt" root))
          (emacs-agent-policy-maximum-document-bytes 8))
     (write-region "old a\n" nil a-path)
     (write-region "old b\n" nil b-path)
     (let* ((a-target
             (emacs-agent-project-resolve-target runtime a-path))
            (b-target
             (emacs-agent-project-resolve-target runtime b-path))
            (a (emacs-agent-document-open runtime a-target))
            (b (emacs-agent-document-open runtime b-target)))
       (should-error
        (emacs-agent-transaction-plan
         runtime
         `(((path . ,a-path)
            (expected_revision
             . ,(emacs-agent-document-revision a))
            (edits . (((old_text . "old") (new_text . "new")))))
           ((path . ,b-path)
            (expected_revision
             . ,(emacs-agent-document-revision b))
            (edits
             . (((old_text . "old") (new_text . "much-too-long")))))))
        :type 'emacs-agent-error)
       (with-current-buffer (emacs-agent-document-buffer a)
         (should (equal (buffer-string) "old a\n")))
       (with-current-buffer (emacs-agent-document-buffer b)
         (should (equal (buffer-string) "old b\n")))))))

(ert-deftest emacs-agent-transaction-dry-run-and-apply ()
  (emacs-agent-transaction-test--runtime
   (let ((a-path (expand-file-name "a.txt" root))
         (b-path (expand-file-name "b.txt" root)))
     (write-region "old a\n" nil a-path)
     (write-region "old b\n" nil b-path)
     (let* ((a-target
             (emacs-agent-project-resolve-target runtime a-path))
            (b-target
             (emacs-agent-project-resolve-target runtime b-path))
            (a (emacs-agent-document-open runtime a-target))
            (b (emacs-agent-document-open runtime b-target))
            (documents
             (list
              (list
               (cons 'path a-path)
               (cons 'expected_revision
                     (emacs-agent-document-revision a))
               (cons 'edits
                     '(((old_text . "old") (new_text . "new")))))
              (list
               (cons 'path b-path)
               (cons 'expected_revision
                     (emacs-agent-document-revision b))
               (cons 'edits
                     '(((old_text . "old") (new_text . "new")))))))
            (plan
             (emacs-agent-transaction-plan runtime documents))
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
         (should (equal (buffer-string) "new b\n")))))))

(ert-deftest emacs-agent-transaction-zero-item-plan-is-a-true-noop ()
  (emacs-agent-transaction-test--runtime
    (let* ((plan
            (emacs-agent-transaction-plan--make
             :runtime runtime :items nil))
           (preview (emacs-agent-transaction-apply plan t))
           (result (emacs-agent-transaction-apply plan)))
      (should-not (plist-get preview :modified))
      (should (equal (plist-get preview :diff) ""))
      (should-not (plist-get preview :documents))
      (should (plist-get result :applied))
      (should-not (plist-get result :modified))
      (should-not (plist-get result :checkpointed))
      (should-not (plist-get result :changeset_id))
      (should (equal (plist-get result :diff) ""))
      (should-not (plist-get result :documents))
      (should
       (zerop
        (hash-table-count
         (emacs-agent-runtime-changeset-registry runtime)))))))

(ert-deftest emacs-agent-transaction-noop-item-checkpoints-without-rewrite ()
  (emacs-agent-transaction-test--runtime
    (let* ((path (expand-file-name "noop.txt" root))
           (_ (write-region "old\n" nil path))
           (target (emacs-agent-project-resolve-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (buffer (emacs-agent-document-buffer document))
           original-erase
           (erase-count 0))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "dirty\n"))
      (let* ((revision (emacs-agent-document-revision document))
             (plan
              (emacs-agent-transaction-plan
               runtime
               `(((path . ,path)
                  (expected_revision . ,revision)
                  (edits
                   . (((old_text . "old")
                       (new_text . "old"))))))))
             result)
        (setq original-erase (symbol-function 'erase-buffer))
        (cl-letf
            (((symbol-function 'erase-buffer)
              (lambda ()
                (setq erase-count (1+ erase-count))
                (funcall original-erase))))
          (setq result
                (emacs-agent-transaction-apply plan nil t)))
        (should (zerop erase-count))
        (should (plist-get result :applied))
        (should-not (plist-get result :modified))
        (should (plist-get result :checkpointed))
        (should-not (plist-get result :changeset_id))
        (should (equal (plist-get result :diff) ""))
        (let ((entry (car (plist-get result :documents))))
          (should entry)
          (should-not (plist-get entry :modified))
          (should (plist-get entry :checkpointed)))
        (should-not (buffer-modified-p buffer))
        (should
         (equal
          (with-temp-buffer
            (insert-file-contents path)
            (buffer-string))
          "old\ndirty\n"))
        (should
         (zerop
          (hash-table-count
           (emacs-agent-runtime-changeset-registry runtime))))))))

(ert-deftest emacs-agent-transaction-noop-checkpoint-failure-has-no-history ()
  (emacs-agent-transaction-test--runtime
    (let* ((path (expand-file-name "noop.txt" root))
           (_ (write-region "old\n" nil path))
           (target (emacs-agent-project-resolve-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "dirty\n"))
      (let* ((revision (emacs-agent-document-revision document))
             (plan
              (emacs-agent-transaction-plan
               runtime
               `(((path . ,path)
                  (expected_revision . ,revision)
                  (edits
                   . (((old_text . "old")
                       (new_text . "old"))))))))
             result)
        (cl-letf (((symbol-function 'save-buffer)
                   (lambda (&rest _arguments)
                     (error "Injected no-op checkpoint failure"))))
          (setq result
                (emacs-agent-transaction-apply plan nil t)))
        (should (plist-get result :applied))
        (should-not (plist-get result :modified))
        (should-not (plist-get result :checkpointed))
        (should-not (plist-get result :changeset_id))
        (should (plist-get result :checkpoint_error))
        (should (emacs-agent-document-degraded document))
        (should
         (eq (emacs-agent-runtime-health-state runtime) 'degraded))
        (should
         (zerop
          (hash-table-count
           (emacs-agent-runtime-changeset-registry runtime))))))))

(ert-deftest emacs-agent-transaction-noop-before-save-change-has-history ()
  (emacs-agent-transaction-test--runtime
    (let* ((path (expand-file-name "formatted.txt" root))
           (_ (write-region "old\n" nil path))
           (target (emacs-agent-project-resolve-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (set-buffer-modified-p t)
        (add-hook
         'before-save-hook
         #'emacs-agent-transaction-test--format-before-save nil t))
      (let* ((revision (emacs-agent-document-revision document))
             (plan
              (emacs-agent-transaction-plan
               runtime
               `(((path . ,path)
                  (expected_revision . ,revision)
                  (edits
                   . (((old_text . "old")
                       (new_text . "old"))))))))
             (result
              (emacs-agent-transaction-apply plan nil t)))
        (should (plist-get result :applied))
        (should (plist-get result :modified))
        (should (plist-get result :checkpointed))
        (should (stringp (plist-get result :changeset_id)))
        (should
         (string-match-p
          "formatted by hook"
          (plist-get result :diff)))
        (let ((entry (car (plist-get result :documents))))
          (should (plist-get entry :modified))
          (should (plist-get entry :checkpointed)))
        (should-not (buffer-modified-p buffer))
        (should
         (equal
          (emacs-agent-document--buffer-content buffer)
          "old\nformatted by hook\n"))
        (should
         (equal
          (with-temp-buffer
            (insert-file-contents path)
            (buffer-string))
          "old\nformatted by hook\n"))
        (should
         (= 1
            (hash-table-count
             (emacs-agent-runtime-changeset-registry runtime))))))))

(ert-deftest emacs-agent-transaction-after-save-dirty-is-not-checkpointed ()
  (emacs-agent-transaction-test--runtime
    (let* ((path (expand-file-name "dirty-hook.txt" root))
           (_ (write-region "old\n" nil path))
           (target (emacs-agent-project-resolve-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (add-hook
         'after-save-hook
         #'emacs-agent-transaction-test--dirty-after-save nil t))
      (let* ((revision (emacs-agent-document-revision document))
             (plan
              (emacs-agent-transaction-plan
               runtime
               `(((path . ,path)
                  (expected_revision . ,revision)
                  (edits
                   . (((old_text . "old")
                       (new_text . "new"))))))))
             (result
              (emacs-agent-transaction-apply plan nil t))
             (checkpoint-error
              (plist-get result :checkpoint_error))
             (entry (car (plist-get result :documents))))
        (should (plist-get result :applied))
        (should (plist-get result :modified))
        (should-not (plist-get result :checkpointed))
        (should (stringp (plist-get result :changeset_id)))
        (should
         (eq (plist-get checkpoint-error :code)
             'checkpoint_failed))
        (should-not
         (plist-get checkpoint-error :checkpointed_paths))
        (should
         (plist-get checkpoint-error :partial_completion))
        (should-not
         (plist-get checkpoint-error :reconciliation_required))
        (should-not (plist-get entry :checkpointed))
        (should (plist-get entry :modified))
        (should
         (equal
          (with-temp-buffer
            (insert-file-contents path)
            (buffer-string))
          "new\n"))
        (should
         (equal
          (emacs-agent-document--buffer-content buffer)
          "new\ndirty after save\n"))
        (should (buffer-modified-p buffer))
        (should (emacs-agent-document-degraded document))
        (should
         (eq
          (emacs-agent-runtime-health-state runtime)
          'degraded))))))

(ert-deftest emacs-agent-transaction-cross-project-and-direct-file ()
  (emacs-agent-transaction-test--runtime
   (let* ((project-root (expand-file-name "project" root))
          (direct-root (expand-file-name "direct" root))
          (project-path (expand-file-name "a.txt" project-root))
          (direct-path (expand-file-name "b.txt" direct-root)))
     (make-directory project-root)
     (make-directory direct-root)
     (write-region "old a\n" nil project-path)
     (write-region "old b\n" nil direct-path)
     (let* ((opened
             (emacs-agent-project-open runtime project-root))
            (project-id (plist-get opened :project_id))
            (project-target
             (emacs-agent-project-resolve-target
              runtime "a.txt" :project-id project-id))
            (direct-target
             (emacs-agent-project-resolve-target runtime direct-path))
            (a (emacs-agent-document-open runtime project-target))
            (b (emacs-agent-document-open runtime direct-target))
            (plan
             (emacs-agent-transaction-plan
              runtime
              `(((project_id . ,project-id)
                 (path . "a.txt")
                 (expected_revision
                  . ,(emacs-agent-document-revision a))
                 (edits
                  . (((old_text . "old") (new_text . "new")))))
                ((path . ,direct-path)
                 (expected_revision
                  . ,(emacs-agent-document-revision b))
                 (edits
                  . (((old_text . "old") (new_text . "new"))))))))
            (result (emacs-agent-transaction-apply plan)))
       (should (plist-get result :applied))
       (let ((documents (plist-get result :documents)))
         (should (= (length documents) 2))
         (should
          (equal
           (mapcar (lambda (entry)
                     (plist-get entry :path))
                   documents)
           (list (file-truename project-path)
                 (file-truename direct-path))))
         (should (equal (plist-get (car documents) :project_id)
                        project-id))
         (should-not (plist-get (cadr documents) :project_id)))))))

(ert-deftest emacs-agent-transaction-spans-two-registered-projects ()
  (emacs-agent-transaction-test--runtime
    (let* ((first-root (expand-file-name "first" root))
           (second-root (expand-file-name "second" root))
           (first-path (expand-file-name "a.txt" first-root))
           (second-path (expand-file-name "b.txt" second-root)))
      (make-directory first-root)
      (make-directory second-root)
      (write-region "old a\n" nil first-path)
      (write-region "old b\n" nil second-path)
      (let* ((first-id
              (plist-get
               (emacs-agent-project-open runtime first-root)
               :project_id))
             (second-id
              (plist-get
               (emacs-agent-project-open runtime second-root)
               :project_id))
             (first-target
              (emacs-agent-project-resolve-target
               runtime "a.txt" :project-id first-id))
             (second-target
              (emacs-agent-project-resolve-target
               runtime "b.txt" :project-id second-id))
             (first-document
              (emacs-agent-document-open runtime first-target))
             (second-document
              (emacs-agent-document-open runtime second-target))
             (plan
              (emacs-agent-transaction-plan
               runtime
               `(((project_id . ,first-id)
                  (path . "a.txt")
                  (expected_revision
                   . ,(emacs-agent-document-revision
                       first-document))
                  (edits
                   . (((old_text . "old")
                       (new_text . "new")))))
                 ((project_id . ,second-id)
                  (path . "b.txt")
                  (expected_revision
                   . ,(emacs-agent-document-revision
                       second-document))
                  (edits
                   . (((old_text . "old")
                       (new_text . "new"))))))))
             (result (emacs-agent-transaction-apply plan))
             (documents (plist-get result :documents)))
        (should (plist-get result :applied))
        (should
         (equal
          (mapcar
           (lambda (item)
             (plist-get item :project_id))
           documents)
          (list first-id second-id)))
        (with-current-buffer
            (emacs-agent-document-buffer first-document)
          (should (equal (buffer-string) "new a\n")))
        (with-current-buffer
            (emacs-agent-document-buffer second-document)
          (should (equal (buffer-string) "new b\n")))))))

(ert-deftest emacs-agent-transaction-preserves-partial-checkpoint-paths ()
  (emacs-agent-transaction-test--runtime
    (let (paths documents)
      (dotimes (index 3)
        (let ((path
               (expand-file-name
                (format "%d.txt" index) root)))
          (write-region "old\n" nil path)
          (push path paths)))
      (setq paths (nreverse paths))
      (setq
       documents
       (mapcar
        (lambda (path)
          (let* ((target
                  (emacs-agent-project-resolve-target
                   runtime path))
                 (document
                  (emacs-agent-document-open runtime target)))
            (list path document)))
        paths))
      (let* ((plan
              (emacs-agent-transaction-plan
               runtime
               (mapcar
                (lambda (entry)
                  `((path . ,(car entry))
                    (expected_revision
                     . ,(emacs-agent-document-revision
                         (cadr entry)))
                    (edits
                     . (((old_text . "old")
                         (new_text . "new"))))))
                documents)))
             (original-save-buffer
              (symbol-function 'save-buffer))
             (save-count 0)
             result)
        (cl-letf
            (((symbol-function 'save-buffer)
              (lambda (&rest arguments)
                (setq save-count (1+ save-count))
                (if (= save-count 3)
                    (error "Third save fails")
                  (apply original-save-buffer arguments)))))
          (setq result
                (emacs-agent-transaction-apply
                 plan nil t)))
        (let ((checkpoint-error
               (plist-get result :checkpoint_error))
              (result-documents
               (plist-get result :documents)))
          (should
           (equal
            (plist-get checkpoint-error :checkpointed_paths)
            (mapcar #'file-truename (seq-take paths 2))))
          (should
           (equal
            (mapcar
             (lambda (item)
               (and (plist-get item :checkpointed) t))
             result-documents)
            '(t t nil))))))))

(provide 'emacs-agent-transaction-test)
;;; emacs-agent-transaction-test.el ends here
