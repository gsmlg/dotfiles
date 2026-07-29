;;; emacs-agent-workspace-review-test.el --- Workspace/review tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'emacs-agent-workspace)
(require 'emacs-agent-search)
(require 'emacs-agent-changeset)
(require 'emacs-agent-journal)
(require 'emacs-agent-ui)
(require 'emacs-agent-document nil t)

(defmacro emacs-agent-review-test--workspace (&rest body)
  "Run BODY with a fresh temporary workspace bound."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "emacs-agent-review-" t))
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

(ert-deftest emacs-agent-workspace-review-pause-and-queue ()
  (emacs-agent-review-test--workspace
    (should (emacs-agent-workspace-mutations-allowed-p workspace))
    (should (= 42
               (emacs-agent-workspace-enqueue-mutation
                workspace (lambda () 42))))
    (emacs-agent-workspace-pause workspace)
    (should-error
     (emacs-agent-workspace-enqueue-mutation workspace #'ignore)
     :type 'emacs-agent-workspace-paused)
    (emacs-agent-workspace-resume workspace)
    (should (emacs-agent-workspace-mutations-allowed-p workspace))))

(ert-deftest emacs-agent-workspace-review-approval-is-bound-and-one-use ()
  (emacs-agent-review-test--workspace
    (let* ((arguments '(:path "a.el" :force t))
           (request
            (emacs-agent-workspace-request-approval
             workspace "document_delete" arguments "writer-a"))
           (id (plist-get request :approval_request_id)))
      (emacs-agent-workspace-approve workspace id)
      (should
       (equal
        (emacs-agent-workspace-approval-digest
         "document_delete" '((path . "a.el") (force . t)))
        (emacs-agent-workspace-approval-digest
         "document_delete" '((force . t) (path . "a.el")))))
      (should-error
       (emacs-agent-workspace-consume-approval
        workspace id "document_delete" arguments "writer-b")
       :type 'emacs-agent-approval-error)
      (should-error
       (emacs-agent-workspace-consume-approval
        workspace id "document_delete"
        '(:path "different.el" :force t)
        "writer-a")
       :type 'emacs-agent-approval-error)
      (should
       (emacs-agent-workspace-consume-approval
        workspace id "document_delete" arguments "writer-a"))
      (should-error
       (emacs-agent-workspace-consume-approval
        workspace id "document_delete" arguments "writer-a")
       :type 'emacs-agent-approval-error))))

(ert-deftest emacs-agent-workspace-review-approval-expires ()
  (emacs-agent-review-test--workspace
    (let* ((request
            (emacs-agent-workspace-request-approval
             workspace "document_delete" '(:path "a.el") "writer"))
           (id (plist-get request :approval_request_id))
           (approval
            (gethash id
                     (emacs-agent-workspace-approval-registry workspace))))
      (setf (emacs-agent-approval-expires-at approval)
            (1- (float-time)))
      (should-error
       (emacs-agent-workspace-approve workspace id)
       :type 'emacs-agent-approval-error)
      (should (eq (emacs-agent-approval-status approval) 'expired)))))

(ert-deftest emacs-agent-workspace-review-approval-invalidates-on-revision-change ()
  (emacs-agent-review-test--workspace
    (let* ((path "guarded.el")
           (absolute (expand-file-name path root)))
      (write-region "before\n" nil absolute)
      (let* ((document (emacs-agent-document-open workspace path))
             (revision (emacs-agent-document-revision document))
             (arguments
              (list :path path :expected_revision revision))
             (request
              (emacs-agent-workspace-request-approval
               workspace "document_delete" arguments "writer"))
             (id (plist-get request :approval_request_id))
             (approval
              (gethash
               id (emacs-agent-workspace-approval-registry workspace))))
        (emacs-agent-workspace-approve workspace id)
        (with-current-buffer (emacs-agent-document-buffer document)
          (goto-char (point-max))
          (insert "changed\n"))
        (should-error
         (emacs-agent-workspace-consume-approval
          workspace id "document_delete" arguments "writer")
         :type 'emacs-agent-approval-error)
        (should
         (eq (emacs-agent-approval-status approval) 'invalidated))))))

(ert-deftest emacs-agent-workspace-review-approval-does-not-leak-token ()
  (emacs-agent-review-test--workspace
    (let* ((credential "bearer-super-secret")
           (request
            (emacs-agent-workspace-request-approval
             workspace "document_delete" '(:path "a.el") credential))
           (public
            (concat
             (prin1-to-string request)
             (prin1-to-string
             (emacs-agent-workspace-recent-activity workspace)))))
      (should-not (string-match-p (regexp-quote credential) public)))))

(ert-deftest emacs-agent-workspace-review-approval-status-and-cancel ()
  (emacs-agent-review-test--workspace
    (let* ((arguments '(:path "a.el" :force t))
           (request
            (emacs-agent-workspace-request-approval
             workspace "document_delete" arguments "writer"))
           (id (plist-get request :approval_request_id))
           (pending
            (emacs-agent-workspace-approval-status workspace id))
           (cancelled
            (emacs-agent-workspace-approval-cancel workspace id)))
      (should (eq (plist-get pending :status) 'pending))
      (should (eq (plist-get cancelled :status) 'cancelled))
      (should-not
       (string-match-p "writer" (prin1-to-string cancelled)))
      (should-error
       (emacs-agent-workspace-consume-approval
        workspace id "document_delete" arguments "writer")
       :type 'emacs-agent-approval-error))))

(ert-deftest emacs-agent-workspace-review-files-and-search-cursors ()
  (emacs-agent-review-test--workspace
    (write-region "needle one\n" nil (expand-file-name "a.el" root))
    (write-region "needle two\n" nil (expand-file-name "b.el" root))
    (write-region "ignored\n" nil (expand-file-name "c.txt" root))
    (let* ((first
            (emacs-agent-workspace-files
             workspace :include-globs '("*.el") :max-results 1))
           (cursor (plist-get first :next_cursor))
           (second
            (emacs-agent-workspace-files
             workspace :include-globs '("*.el")
             :max-results 1 :cursor cursor)))
      (should (= 1 (length (plist-get first :results))))
      (should cursor)
      (should (= 1 (length (plist-get second :results))))
      (should-not (plist-get second :next_cursor)))
    (let ((results
           (emacs-agent-workspace-search
            workspace "needle" :include-globs '("*.el"))))
      (should (= 2 (length (plist-get results :results))))
      (should
       (equal '("a.el" "b.el")
              (mapcar
               (lambda (item) (plist-get item :path))
               (plist-get results :results)))))))

(ert-deftest emacs-agent-workspace-search-prefers-dirty-buffer ()
  (emacs-agent-review-test--workspace
    (let* ((path "dirty.el")
           (absolute (expand-file-name path root)))
      (write-region "disk-only\n" nil absolute)
      (let ((document (emacs-agent-document-open workspace path)))
        (with-current-buffer (emacs-agent-document-buffer document)
          (erase-buffer)
          (insert "buffer-only needle\n"))
        (let* ((found
                (emacs-agent-workspace-search workspace "needle"))
               (items (plist-get found :results))
               (item (car items)))
          (should (= (length items) 1))
          (should (equal (plist-get item :path) path))
          (should (equal (plist-get item :source) "buffer"))
          (should (plist-get item :modified))
          (should (stringp (plist-get item :revision))))
        (should
         (= 0
            (length
             (plist-get
              (emacs-agent-workspace-search workspace "disk-only")
              :results))))))))

(ert-deftest emacs-agent-workspace-review-journal-redacts-secrets ()
  (emacs-agent-review-test--workspace
    (let* ((state (make-temp-file "emacs-agent-state-" t))
           (emacs-agent-journal-enabled t))
      (unwind-protect
          (progn
            (setf (emacs-agent-workspace-state-directory workspace) state)
            (let ((path
                   (progn
                     (emacs-agent-journal-write
                      workspace
                      '(:tool "edit" :token "nope" :content "source"
                        :paths ("a.el")))
                     (gethash
                      (emacs-agent-workspace-workspace-id workspace)
                      emacs-agent-journal-files))))
              (should (= #o600 (file-modes path)))
              (with-temp-buffer
                (insert-file-contents path)
                (let ((entry
                       (json-parse-string
                        (buffer-string) :object-type 'alist)))
                  (should (equal "edit" (alist-get 'tool entry)))
                  (should-not (alist-get 'token entry))
                  (should-not (alist-get 'content entry))))))
        (delete-directory state t)))))

(ert-deftest emacs-agent-workspace-review-changeset-rollback ()
  (emacs-agent-review-test--workspace
    (let* ((path "a.el")
           (absolute (expand-file-name path root)))
      (write-region "before\n" nil absolute)
      (let ((buffer (find-file-noselect absolute)))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "after\n")
          (save-buffer))
        (let* ((revision
                (if (fboundp 'emacs-agent-document-revision-for-path)
                    (emacs-agent-document-revision-for-path workspace path)
                  (emacs-agent-changeset--revision workspace path)))
               (changeset
                (emacs-agent-changeset-record
                 workspace
                 :touched-documents (list path)
                 :before-snapshots
                 (list (cons path '(:exists t :content "before\n")))
                 :final-revisions (list (cons path revision))
                 :checkpoint-state 'checkpointed)))
          (should (string-match-p "after"
                                  (emacs-agent-changeset-diff
                                   workspace
                                   (emacs-agent-changeset-changeset-id
                                    changeset))))
          (emacs-agent-changeset-rollback
           workspace (emacs-agent-changeset-changeset-id changeset))
          (should
           (eq 'rolled-back
               (emacs-agent-changeset-status changeset)))
          (should
           (equal "before\n"
                  (with-current-buffer buffer (buffer-string))))
          (should
           (equal "before\n"
                  (with-temp-buffer
                    (insert-file-contents absolute)
                    (buffer-string)))))))))

(ert-deftest emacs-agent-workspace-review-changeset-query-and-detail ()
  (emacs-agent-review-test--workspace
    (let* ((path "a.el")
           (absolute (expand-file-name path root)))
      (write-region "after\n" nil absolute)
      (let* ((revision
              (emacs-agent-document-revision-for-path workspace path))
             (changeset
              (emacs-agent-changeset-record
               workspace
               :operations (list (list :type 'edit :path path))
               :before-snapshots
               (list (cons path '(:exists t :content "before\n")))
               :base-revisions (list (cons path "rev:before"))
               :final-revisions (list (cons path revision))))
             (id (emacs-agent-changeset-changeset-id changeset))
             (page
              (emacs-agent-changeset-query
               workspace :path path :statuses '(applied) :limit 1))
             (detail
              (emacs-agent-changeset-detail
               workspace id :max-chars 8)))
        (should (= (plist-get page :result_count) 1))
        (should
         (equal
          (plist-get (car (plist-get page :changesets)) :changeset_id)
          id))
        (should (stringp (plist-get detail :diff)))
        (should (plist-get detail :diff_truncated))
        (should (stringp (plist-get detail :diff_cursor)))))))

(ert-deftest emacs-agent-workspace-review-rollback-rejects-symlink-escape ()
  (emacs-agent-review-test--workspace
    (let ((outside (make-temp-file "emacs-agent-outside-" t)))
      (unwind-protect
          (progn
            (make-symbolic-link outside (expand-file-name "link" root))
            (should-error
             (emacs-agent-changeset--restore-one
              workspace
              (cons "link/escaped.txt"
                    '(:exists t :content "must stay contained\n")))
             :type 'emacs-agent-error)
            (should-not
             (file-exists-p (expand-file-name "escaped.txt" outside))))
        (delete-directory outside t)))))

(ert-deftest emacs-agent-workspace-review-ui-keymaps ()
  (should (eq (lookup-key emacs-agent-activity-mode-map (kbd "a"))
              #'emacs-agent-approve-at-point))
  (should (eq (lookup-key emacs-agent-changes-mode-map (kbd "r"))
              #'emacs-agent-rollback-at-point)))

(provide 'emacs-agent-workspace-review-test)
;;; emacs-agent-workspace-review-test.el ends here
