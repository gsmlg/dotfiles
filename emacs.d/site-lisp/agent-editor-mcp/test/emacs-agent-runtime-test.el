;;; emacs-agent-runtime-test.el --- Editor runtime tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for process-lifetime editor state, mutation serialization, and
;; approvals.

;;; Code:

(require 'ert)
(require 'emacs-agent-runtime)

(defmacro emacs-agent-runtime-test--with-runtime (&rest body)
  "Run BODY with a fresh bound editor runtime."
  (declare (indent 0) (debug t))
  `(let* ((state-directory
           (make-temp-file "emacs-agent-runtime-state-" t))
          (runtime
           (emacs-agent-runtime-create
            :state-directory state-directory)))
     (unwind-protect
         (progn
           (emacs-agent-runtime-bind runtime)
           ,@body)
       (emacs-agent-runtime-clear runtime)
       (delete-directory state-directory t))))

(ert-deftest emacs-agent-runtime-starts-with-zero-projects ()
  (emacs-agent-runtime-test--with-runtime
    (let ((info (emacs-agent-runtime-info runtime)))
      (should (string-prefix-p
               "editor_" (plist-get info :instance_id)))
      (should (eq (plist-get info :access_mode) 'autonomous))
      (should (eq (plist-get info :save_policy) 'immediate))
      (should (eq (plist-get info :filesystem_policy) 'unrestricted))
      (should (= (plist-get info :project_count) 0))
      (should (= (plist-get info :managed_document_count) 0))
      (should (eq (emacs-agent-runtime-current) runtime)))))

(ert-deftest emacs-agent-runtime-current-requires-a-bound-runtime ()
  (let ((emacs-agent-current-runtime nil))
    (should-error
     (emacs-agent-runtime-current)
     :type 'emacs-agent-runtime-not-started)))

(ert-deftest emacs-agent-runtime-bind-enforces-one-active-runtime ()
  (let ((emacs-agent-current-runtime nil)
        (first (emacs-agent-runtime-create))
        (second (emacs-agent-runtime-create)))
    (unwind-protect
        (progn
          (emacs-agent-runtime-bind first)
          (should-error
           (emacs-agent-runtime-bind second)
           :type 'emacs-agent-runtime-already-started)
          (should (eq (emacs-agent-runtime-current) first)))
      (emacs-agent-runtime-clear first)
      (emacs-agent-runtime-clear second))))

(ert-deftest emacs-agent-runtime-serializes-reentrant-mutations ()
  (emacs-agent-runtime-test--with-runtime
    (let (events)
      (should
       (eq
        (emacs-agent-runtime-enqueue-mutation
         runtime
         (lambda ()
           (push 'outer-start events)
           (should
            (eq
             (emacs-agent-runtime-enqueue-mutation
              runtime (lambda () (push 'inner events)))
             'queued))
           (push 'outer-end events)
           'completed))
        'completed))
      (should (equal (nreverse events)
                     '(outer-start outer-end inner))))))

(ert-deftest emacs-agent-runtime-pause-blocks-mutations ()
  (emacs-agent-runtime-test--with-runtime
    (should (emacs-agent-runtime-mutations-allowed-p runtime))
    (emacs-agent-runtime-pause runtime)
    (should-error
     (emacs-agent-runtime-enqueue-mutation runtime #'ignore)
     :type 'emacs-agent-runtime-paused)
    (emacs-agent-runtime-resume runtime)
    (should (emacs-agent-runtime-mutations-allowed-p runtime))))

(ert-deftest emacs-agent-runtime-read-only-blocks-mutations ()
  (let ((runtime
         (emacs-agent-runtime-create :access-mode 'read-only)))
    (should-not (emacs-agent-runtime-mutations-allowed-p runtime))
    (should-error
     (emacs-agent-runtime-enqueue-mutation runtime #'ignore)
     :type 'emacs-agent-runtime-paused)))

(ert-deftest emacs-agent-runtime-clear-does-not-kill-visiting-buffers ()
  (let* ((root (make-temp-file "emacs-agent-runtime-buffer-" t))
         (path (expand-file-name "managed.txt" root))
         (runtime (emacs-agent-runtime-create))
         buffer)
    (unwind-protect
        (progn
          (write-region "managed\n" nil path)
          (setq buffer (find-file-noselect path))
          (puthash path buffer
                   (emacs-agent-runtime-document-registry runtime))
          (puthash "project_test" t
                   (emacs-agent-runtime-project-registry runtime))
          (emacs-agent-runtime-bind runtime)
          (emacs-agent-runtime-clear runtime)
          (should (buffer-live-p buffer))
          (should (= 0
                     (hash-table-count
                      (emacs-agent-runtime-document-registry runtime))))
          (should (= 0
                     (hash-table-count
                      (emacs-agent-runtime-project-registry runtime))))
          (should-error
           (emacs-agent-runtime-current)
           :type 'emacs-agent-runtime-not-started))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest emacs-agent-runtime-approval-is-bound-and-one-use ()
  (emacs-agent-runtime-test--with-runtime
    (let* ((arguments '(:path "/tmp/a.el" :force t))
           (request
            (emacs-agent-runtime-request-approval
             runtime "document_delete" arguments "writer-a"))
           (id (plist-get request :approval_request_id)))
      (should
       (equal
        (emacs-agent-runtime-approval-digest
         "document_delete" '((path . "/tmp/a.el") (force . t)))
        (emacs-agent-runtime-approval-digest
         "document_delete" '((force . t) (path . "/tmp/a.el")))))
      (emacs-agent-runtime-approve runtime id)
      (should-error
       (emacs-agent-runtime-consume-approval
        runtime id "document_delete" arguments "writer-b")
       :type 'emacs-agent-runtime-approval-error)
      (should
       (emacs-agent-runtime-consume-approval
        runtime id "document_delete" arguments "writer-a"))
      (should-error
       (emacs-agent-runtime-consume-approval
        runtime id "document_delete" arguments "writer-a")
       :type 'emacs-agent-runtime-approval-error))))

(ert-deftest emacs-agent-runtime-approval-expires ()
  (emacs-agent-runtime-test--with-runtime
    (let* ((request
            (emacs-agent-runtime-request-approval
             runtime "document_delete" '(:path "/tmp/a.el") "writer"))
           (id (plist-get request :approval_request_id))
           (approval
            (gethash id
                     (emacs-agent-runtime-approval-registry runtime))))
      (setf (emacs-agent-runtime-approval-expires-at approval)
            (1- (float-time)))
      (should-error
       (emacs-agent-runtime-approve runtime id)
       :type 'emacs-agent-runtime-approval-error)
      (should (eq (emacs-agent-runtime-approval-state approval) 'expired)))))

(ert-deftest emacs-agent-runtime-approval-cancels ()
  (emacs-agent-runtime-test--with-runtime
    (let* ((request
            (emacs-agent-runtime-request-approval
             runtime "document_delete" '(:path "/tmp/a.el") "writer"))
           (id (plist-get request :approval_request_id))
           (status (emacs-agent-runtime-approval-cancel runtime id)))
      (should (eq (plist-get status :status) 'cancelled))
      (should-error
       (emacs-agent-runtime-approve runtime id)
       :type 'emacs-agent-runtime-approval-error))))

(ert-deftest emacs-agent-runtime-approval-status-redacts-secrets ()
  (emacs-agent-runtime-test--with-runtime
    (let* ((request
            (emacs-agent-runtime-request-approval
             runtime
             "document_delete"
             '(:path "/tmp/a.el" :token "argument-secret")
             "credential-secret"))
           (id (plist-get request :approval_request_id))
           (public
            (prin1-to-string
             (emacs-agent-runtime-approval-status runtime id))))
      (should-not (string-match-p "argument-secret" public))
      (should-not (string-match-p "credential-secret" public)))))

(ert-deftest emacs-agent-runtime-approval-partial-creates-bound-child ()
  (emacs-agent-runtime-test--with-runtime
    (let* ((arguments
            '(:documents
              ((:path "/tmp/a.el")
               (:path "/tmp/b.el"))))
           (request
            (emacs-agent-runtime-request-approval
             runtime "editor_checkpoint" arguments "writer"))
           (id (plist-get request :approval_request_id))
           (partial
            (emacs-agent-runtime-approval-partial
             runtime id '("/tmp/a.el")))
           (child (plist-get partial :derived_approval))
           (child-id (plist-get child :approval_request_id)))
      (should (eq (plist-get partial :status) 'partially_approved))
      (should (equal (plist-get partial :accepted_paths)
                     '("/tmp/a.el")))
      (should (eq (plist-get child :status) 'approved))
      (should (equal (plist-get child :parent_approval_request_id) id))
      (should
       (emacs-agent-runtime-consume-approval
        runtime
        child-id
        "editor_checkpoint"
        '(:documents
          ((:path "/tmp/a.el")))
        "writer")))))

(ert-deftest emacs-agent-runtime-approval-invalidates-on-document-change ()
  (require 'emacs-agent-document)
  (require 'emacs-agent-project)
  (emacs-agent-runtime-test--with-runtime
    (let* ((root (make-temp-file "emacs-agent-approval-document-" t))
           (path (expand-file-name "file.txt" root))
           target document request id)
      (unwind-protect
          (progn
            (write-region "before\n" nil path)
            (setq target
                  (emacs-agent-project-resolve-target runtime path)
                  document
                  (emacs-agent-document-open runtime target)
                  request
                  (emacs-agent-runtime-request-approval
                   runtime
                   "document_delete"
                   (list
                    :path path
                    :expected_revision
                    (emacs-agent-document-revision document))
                   "writer")
                  id (plist-get request :approval_request_id))
            (emacs-agent-runtime-approve runtime id)
            (with-current-buffer
                (emacs-agent-document-buffer document)
              (goto-char (point-max))
              (insert "changed\n"))
            (should
             (eq
              (plist-get
               (emacs-agent-runtime-approval-status runtime id)
               :status)
              'invalidated)))
        (when-let* ((buffer (get-file-buffer path)))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-directory root t)))))

(provide 'emacs-agent-runtime-test)
;;; emacs-agent-runtime-test.el ends here
