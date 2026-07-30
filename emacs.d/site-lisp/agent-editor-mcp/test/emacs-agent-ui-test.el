;;; emacs-agent-ui-test.el --- Collaboration UI tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-agent-ui)

(defmacro emacs-agent-ui-test--runtime (&rest body)
  "Run BODY with a fresh bound runtime and temporary filesystem root."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "emacs-agent-ui-runtime-" t))
          (runtime
           (emacs-agent-runtime-create
            :instance-id
            (concat "ui-" (file-name-nondirectory root))))
          (emacs-agent-current-runtime runtime))
     (unwind-protect
         (progn ,@body)
       (emacs-agent-ui-clear-highlights)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when
               (condition-case nil
                   (file-in-directory-p file root)
                 (file-error nil))
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (emacs-agent-runtime-clear runtime)
       (when (file-directory-p root)
         (delete-directory root t)))))

(defun emacs-agent-ui-test--target (runtime path &optional for-create)
  "Resolve absolute PATH for RUNTIME, optionally allowing creation."
  (emacs-agent-project-resolve-target
   runtime path :for-create for-create))

(defun emacs-agent-ui-test--dirty-after-save ()
  "Make a UI checkpoint target dirty once after saving."
  (remove-hook
   'after-save-hook
   #'emacs-agent-ui-test--dirty-after-save t)
  (goto-char (point-max))
  (insert "dirty after UI checkpoint\n"))

(defun emacs-agent-ui-test--format-before-save ()
  "Insert deterministic UI formatter text once before saving."
  (remove-hook
   'before-save-hook
   #'emacs-agent-ui-test--format-before-save t)
  (goto-char (point-max))
  (insert "formatted before UI checkpoint\n"))

(ert-deftest emacs-agent-ui-activity-is-runtime-scoped ()
  (emacs-agent-ui-test--runtime
    (let* ((path (file-truename (expand-file-name "activity.el" root)))
           (event
            (emacs-agent-runtime-record-activity
             runtime
             (list :tool "document_read"
                   :status "completed"
                   :path path))))
      (with-temp-buffer
        (emacs-agent-activity-mode)
        (setq-local emacs-agent-ui-runtime runtime)
        (cl-letf (((symbol-function 'abbreviate-file-name)
                   (lambda (value) (concat "display:" value))))
          (emacs-agent-ui--activity-entries))
        (should (= (length tabulated-list-entries) 1))
        (should
         (equal
          (aref (cadar tabulated-list-entries) 3)
          (concat "display:" path))))
      (should (equal (plist-get event :path) path)))))

(ert-deftest emacs-agent-ui-approval-status-is-safe-and-cancellable ()
  (emacs-agent-ui-test--runtime
    (let* ((path (expand-file-name "safe.el" (file-truename root)))
           (arguments
            `((path . ,path)
              (content . "private source")
              (authorization . "secret")))
           (request
            (emacs-agent-runtime-request-approval
             runtime "document_delete" arguments "credential"))
           (id (plist-get request :approval_request_id))
           (status
            (emacs-agent-runtime-approval-status runtime id)))
      (should (eq (plist-get status :status) 'pending))
      (should (equal (plist-get status :path) path))
      (should-not (string-match-p
                   "private source\\|secret\\|credential"
                   (prin1-to-string status)))
      (setq status
            (emacs-agent-runtime-approval-cancel runtime id))
      (should (eq (plist-get status :status) 'cancelled))
      (should-error
       (emacs-agent-runtime-consume-approval
        runtime id "document_delete" arguments "credential")
       :type 'emacs-agent-runtime-approval-error)
      (should
       (cl-find-if
        (lambda (entry)
          (equal (plist-get entry :status) "cancelled"))
        (emacs-agent-runtime-recent-activity runtime))))))

(ert-deftest emacs-agent-ui-delete-approvals-distinguish-project-targets ()
  (emacs-agent-ui-test--runtime
    (let* ((root-a (expand-file-name "project-a" root))
           (root-b (expand-file-name "project-b" root))
           (relative "same.el")
           (path-a (expand-file-name relative root-a))
           (path-b (expand-file-name relative root-b)))
      (make-directory root-a)
      (make-directory root-b)
      (write-region "a\n" nil path-a)
      (write-region "b\n" nil path-b)
      (let* ((project-a
              (plist-get
               (emacs-agent-project-open runtime root-a)
               :project_id))
             (project-b
              (plist-get
               (emacs-agent-project-open runtime root-b)
               :project_id))
             (canonical-a (file-truename path-a))
             (canonical-b (file-truename path-b))
             (request-a
              (emacs-agent-runtime-request-approval
               runtime "document_delete"
               (list :path relative :project_id project-a)
               "credential"))
             (request-b
              (emacs-agent-runtime-request-approval
               runtime "document_delete"
               (list :path relative :project_id project-b)
               "credential"))
             (id-a (plist-get request-a :approval_request_id))
             (id-b (plist-get request-b :approval_request_id))
             (status-a
              (emacs-agent-runtime-approval-status runtime id-a))
             (status-b
              (emacs-agent-runtime-approval-status runtime id-b)))
        (should (equal (plist-get status-a :path) canonical-a))
        (should (equal (plist-get status-b :path) canonical-b))
        (with-temp-buffer
          (emacs-agent-approvals-mode)
          (setq-local emacs-agent-ui-runtime runtime)
          (emacs-agent-ui--approval-entries)
          (should
           (equal
            (aref (cadr (assoc id-a tabulated-list-entries)) 3)
            (abbreviate-file-name canonical-a)))
          (should
           (equal
            (aref (cadr (assoc id-b tabulated-list-entries)) 3)
            (abbreviate-file-name canonical-b))))))))

(ert-deftest emacs-agent-ui-move-approval-distinguishes-project-targets ()
  (emacs-agent-ui-test--runtime
    (let* ((root-a (expand-file-name "project-a" root))
           (root-b (expand-file-name "project-b" root))
           (relative "same.el")
           (path-a (expand-file-name relative root-a)))
      (make-directory root-a)
      (make-directory root-b)
      (write-region "source\n" nil path-a)
      (let* ((project-a
              (plist-get
               (emacs-agent-project-open runtime root-a)
               :project_id))
             (project-b
              (plist-get
               (emacs-agent-project-open runtime root-b)
               :project_id))
             (canonical-a
              (emacs-agent-resolved-target-canonical-path
               (emacs-agent-project-resolve-target
                runtime relative :project-id project-a)))
             (canonical-b
              (emacs-agent-resolved-target-canonical-path
               (emacs-agent-project-resolve-target
                runtime relative :project-id project-b
                :for-create t)))
             (request
              (emacs-agent-runtime-request-approval
               runtime "document_move"
               (list
                :path relative
                :project_id project-a
                :new_path relative
                :new_project_id project-b)
               "credential"))
             (id (plist-get request :approval_request_id))
             (status
              (emacs-agent-runtime-approval-status runtime id)))
        (should (equal (plist-get status :path) canonical-a))
        (should (equal (plist-get status :new_path) canonical-b))
        (with-temp-buffer
          (emacs-agent-approvals-mode)
          (setq-local emacs-agent-ui-runtime runtime)
          (emacs-agent-ui--approval-entries)
          (should
           (equal
            (aref (cadr (assoc id tabulated-list-entries)) 3)
            (format "%s -> %s"
                    (abbreviate-file-name canonical-a)
                    (abbreviate-file-name canonical-b)))))))))

(ert-deftest emacs-agent-ui-approval-status-expires-on-query ()
  (emacs-agent-ui-test--runtime
    (let* ((emacs-agent-runtime-approval-lifetime -1)
           (request
            (emacs-agent-runtime-request-approval
             runtime "document_delete" nil "credential"))
           (status
            (emacs-agent-runtime-approval-status
             runtime (plist-get request :approval_request_id))))
      (should (eq (plist-get status :status) 'expired))
      (should (= (plist-get status :ttl_remaining) 0.0)))))

(ert-deftest emacs-agent-ui-approval-invalidates-on-revision-change ()
  (emacs-agent-ui-test--runtime
    (let ((absolute (expand-file-name "revision.el" root)))
      (write-region "before\n" nil absolute)
      (let* ((path (file-truename absolute))
             (target (emacs-agent-ui-test--target runtime path))
             (document (emacs-agent-document-open runtime target))
             (revision (emacs-agent-document-revision document))
             (arguments
              `((path . ,path) (expected_revision . ,revision)))
             (request
              (emacs-agent-runtime-request-approval
               runtime "document_delete" arguments "credential"))
             (id (plist-get request :approval_request_id)))
        (emacs-agent-runtime-approve runtime id)
        (with-current-buffer (emacs-agent-document-buffer document)
          (goto-char (point-max))
          (insert "changed\n"))
        (should
         (eq
          (plist-get
           (emacs-agent-runtime-approval-status runtime id)
           :status)
          'invalidated))
        (should-error
         (emacs-agent-runtime-consume-approval
          runtime id "document_delete" arguments "credential")
         :type 'emacs-agent-runtime-approval-error)))))

(ert-deftest emacs-agent-ui-rollback-approval-binds-final-revisions ()
  (emacs-agent-ui-test--runtime
    (let ((absolute (expand-file-name "rollback.el" root)))
      (write-region "current\n" nil absolute)
      (let* ((path (file-truename absolute))
             (target (emacs-agent-ui-test--target runtime path))
             (document (emacs-agent-document-open runtime target))
             (revision (emacs-agent-document-revision document))
             (changeset
              (emacs-agent-changeset-record
               runtime
               :touched-documents (list path)
               :before-snapshots
               (list (cons path '(:exists t :content "before\n")))
               :final-revisions (list (cons path revision))))
             (arguments
              (list :changeset_id
                    (emacs-agent-changeset-changeset-id changeset)))
             (request
              (emacs-agent-runtime-request-approval
               runtime "changeset_rollback" arguments "credential"))
             (id (plist-get request :approval_request_id)))
        (emacs-agent-runtime-approve runtime id)
        (with-current-buffer (emacs-agent-document-buffer document)
          (goto-char (point-max))
          (insert "new edit\n"))
        (should
         (eq
          (plist-get
           (emacs-agent-runtime-approval-status runtime id)
           :status)
          'invalidated))))))

(ert-deftest emacs-agent-ui-partial-approval-derives-exact-child ()
  (emacs-agent-ui-test--runtime
    (let ((absolute-a (expand-file-name "a.el" root))
          (absolute-b (expand-file-name "b.el" root)))
      (write-region "a\n" nil absolute-a)
      (write-region "b\n" nil absolute-b)
      (let* ((path-a (file-truename absolute-a))
             (path-b (file-truename absolute-b))
             (document-a
             (list
               (cons 'path path-a)
               (cons
                'expected_revision
                (emacs-agent-document-revision-for-target
                 runtime
                 (emacs-agent-ui-test--target runtime path-a)))))
             (document-b
              (list
               (cons 'path path-b)
               (cons
                'expected_revision
                (emacs-agent-document-revision-for-target
                 runtime
                 (emacs-agent-ui-test--target runtime path-b)))))
             (arguments
              (list (cons 'documents (list document-a document-b))))
             (request
              (emacs-agent-runtime-request-approval
               runtime "editor_checkpoint" arguments "credential"))
             (id (plist-get request :approval_request_id))
             (parent
              (emacs-agent-runtime--approval runtime id))
             (parent-expiry (+ (float-time) 5))
             (_
              (setf (emacs-agent-runtime-approval-expires-at parent)
                    parent-expiry))
             (pending
              (emacs-agent-runtime-approval-status runtime id))
             (partial
              (emacs-agent-runtime-approval-partial
               runtime id (list path-b)))
             (child-id
              (plist-get partial :derived_approval_request_id))
             (narrowed
              (list (cons 'documents (list document-b)))))
        (should (plist-get pending :partial_accept_supported))
        (should (equal (plist-get pending :document_paths)
                       (list path-a path-b)))
        (should (eq (plist-get partial :status) 'partially_approved))
        (should (stringp child-id))
        (should
         (= (emacs-agent-runtime-approval-expires-at
             (emacs-agent-runtime--approval runtime child-id))
            parent-expiry))
        (should
         (eq
          (plist-get
           (emacs-agent-runtime-approval-status runtime child-id)
           :status)
          'approved))
        (should-error
         (emacs-agent-runtime-consume-approval
          runtime id "editor_checkpoint" narrowed "credential")
         :type 'emacs-agent-runtime-approval-error)
        (should-error
         (emacs-agent-runtime-consume-approval
          runtime child-id "editor_checkpoint" arguments "credential")
         :type 'emacs-agent-runtime-approval-error)
        (should
         (emacs-agent-runtime-consume-approval
          runtime child-id "editor_checkpoint" narrowed "credential"))
        (should
         (cl-find-if
          (lambda (entry)
            (and
             (equal (plist-get entry :status) "partially_approved")
             (equal (plist-get entry :paths) (list path-b))
             (equal
              (plist-get entry :derived_approval_request_id)
              child-id)))
          (emacs-agent-runtime-recent-activity runtime)))
        (should (= 2 (length
                      (alist-get
                       'documents
                       (emacs-agent-runtime-approval-arguments
                        (emacs-agent-runtime--approval runtime id))))))))))

(ert-deftest emacs-agent-ui-partial-approval-rejects-unsafe-selection ()
  (emacs-agent-ui-test--runtime
    (let* ((path-a (expand-file-name "a.el" root))
           (path-b (expand-file-name "b.el" root))
           (arguments
            (list
             :documents
             (list
              (list :path path-a)
              (list :path path-b))))
           (request
            (emacs-agent-runtime-request-approval
             runtime "editor_checkpoint" arguments "credential"))
           (id (plist-get request :approval_request_id)))
      (should-error
       (emacs-agent-runtime-approval-partial
        runtime id nil)
       :type 'emacs-agent-runtime-approval-error)
      (should-error
       (emacs-agent-runtime-approval-partial
        runtime id (list path-a path-b))
       :type 'emacs-agent-runtime-approval-error)
      (should-error
       (emacs-agent-runtime-approval-partial
        runtime id (list (expand-file-name "outside.el" root)))
       :type 'emacs-agent-runtime-approval-error))))

(ert-deftest emacs-agent-ui-partial-approval-command-selects-documents ()
  (emacs-agent-ui-test--runtime
    (let* ((path-a (expand-file-name "a.el" root))
           (path-b (expand-file-name "b.el" root))
           (request
            (emacs-agent-runtime-request-approval
             runtime
             "editor_checkpoint"
             (list :documents
                   (list (list :path path-a)
                         (list :path path-b)))
             "credential"))
           (id (plist-get request :approval_request_id)))
      (cl-letf (((symbol-function 'emacs-agent-ui--row-id)
                (lambda () id))
                ((symbol-function 'completing-read-multiple)
                 (lambda (&rest _ignored) (list path-b)))
                ((symbol-function 'emacs-agent-ui-refresh) #'ignore)
                ((symbol-function 'message) #'ignore))
        (emacs-agent-partially-approve-at-point))
      (let ((status
             (emacs-agent-runtime-approval-status runtime id)))
        (should (eq (plist-get status :status) 'partially_approved))
        (should (equal (plist-get status :accepted_paths) (list path-b)))
        (should
         (stringp
          (plist-get status :derived_approval_request_id)))))))

(ert-deftest emacs-agent-ui-partial-approval-distinguishes-project-targets ()
  (emacs-agent-ui-test--runtime
    (let* ((root-a (expand-file-name "project-a" root))
           (root-b (expand-file-name "project-b" root))
           (relative "same.el")
           (path-a (expand-file-name relative root-a))
           (path-b (expand-file-name relative root-b)))
      (make-directory root-a)
      (make-directory root-b)
      (write-region "a\n" nil path-a)
      (write-region "b\n" nil path-b)
      (let* ((project-a
              (plist-get
               (emacs-agent-project-open runtime root-a)
               :project_id))
             (project-b
              (plist-get
               (emacs-agent-project-open runtime root-b)
               :project_id))
             (canonical-a (file-truename path-a))
             (canonical-b (file-truename path-b))
             (document-a
              (list (cons 'path relative)
                    (cons 'project_id project-a)))
             (document-b
              (list (cons 'path relative)
                    (cons 'project_id project-b)))
             (arguments
              (list (cons 'documents (list document-a document-b))))
             (request
              (emacs-agent-runtime-request-approval
               runtime "editor_checkpoint" arguments "credential"))
             (id (plist-get request :approval_request_id))
             (status
              (emacs-agent-runtime-approval-status runtime id))
             candidates)
        (should
         (equal
          (plist-get status :document_paths)
          (list canonical-a canonical-b)))
        (cl-letf (((symbol-function 'emacs-agent-ui--row-id)
                   (lambda () id))
                  ((symbol-function 'completing-read-multiple)
                   (lambda (_prompt collection &rest _ignored)
                     (setq candidates collection)
                     (list canonical-b)))
                  ((symbol-function 'emacs-agent-ui--journal) #'ignore)
                  ((symbol-function 'emacs-agent-ui-refresh) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (emacs-agent-partially-approve-at-point))
        (should (equal candidates (list canonical-a canonical-b)))
        (setq status
              (emacs-agent-runtime-approval-status runtime id))
        (should (equal (plist-get status :accepted_paths)
                       (list canonical-b)))
        (let ((child-id
               (plist-get status :derived_approval_request_id)))
          (should-error
           (emacs-agent-runtime-consume-approval
            runtime child-id "editor_checkpoint"
            (list (cons 'documents (list document-a)))
            "credential")
           :type 'emacs-agent-runtime-approval-error)
          (should
           (emacs-agent-runtime-consume-approval
            runtime child-id "editor_checkpoint"
            (list (cons 'documents (list document-b)))
            "credential")))))))

(ert-deftest emacs-agent-ui-open-file-jumps-to-range ()
  (emacs-agent-ui-test--runtime
    (let ((path (expand-file-name "jump.el" root)))
      (write-region "zero\none two\nthree\n" nil
                    path)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _ignored)
                   (set-buffer buffer))))
        (emacs-agent-ui-open-file-at-range
         runtime path
         '(:start (:line 2 :column 4)
           :end (:line 2 :column 7))))
      (should (= (line-number-at-pos) 2))
      (should (= (current-column) 4)))))

(ert-deftest emacs-agent-ui-highlights-current-changeset-hunks ()
  (emacs-agent-ui-test--runtime
    (let ((absolute (expand-file-name "highlight.el" root)))
      (write-region "before\nkeep\n" nil absolute)
      (let* ((path (file-truename absolute))
             (target (emacs-agent-ui-test--target runtime path))
             (document (emacs-agent-document-open runtime target)))
        (with-current-buffer (emacs-agent-document-buffer document)
          (erase-buffer)
          (insert "after\nkeep\n"))
        (let* ((revision (emacs-agent-document-revision document))
               (changeset
                (emacs-agent-changeset-record
                 runtime
                 :touched-documents (list path)
                 :before-snapshots
                 (list (cons path '(:exists t :content "before\nkeep\n")))
                 :final-revisions (list (cons path revision))))
               (id (emacs-agent-changeset-changeset-id changeset))
               (result
                (emacs-agent-highlight-changeset runtime id)))
          (should (= (plist-get result :highlighted) 1))
          (should-not (plist-get result :stale_paths))
          (should
           (cl-find-if
            (lambda (overlay)
              (equal id
                     (overlay-get overlay
                                  'emacs-agent-changeset-id)))
            emacs-agent-ui-change-overlays)))))))

(ert-deftest emacs-agent-ui-opens-read-only-changeset-diff-buffer ()
  (emacs-agent-ui-test--runtime
    (let ((absolute (expand-file-name "diff.el" root)))
      (write-region "after\n" nil absolute)
      (let* ((path (file-truename absolute))
             (target (emacs-agent-ui-test--target runtime path))
             (revision
              (emacs-agent-document-revision-for-target runtime target))
             (changeset
              (emacs-agent-changeset-record
               runtime
               :touched-documents (list path)
               :before-snapshots
               (list (cons path '(:exists t :content "before\n")))
               :final-revisions (list (cons path revision))))
             (id (emacs-agent-changeset-changeset-id changeset)))
        (cl-letf (((symbol-function 'emacs-agent-ui--row-id)
                   (lambda () id))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _ignored) buffer)))
          (emacs-agent-view-diff))
        (with-current-buffer (format "*Emacs Agent Diff %s*" id)
          (should (derived-mode-p 'emacs-agent-diff-mode))
          (should buffer-read-only)
          (should (equal emacs-agent-ui-changeset-id id))
          (should (string-match-p "before" (buffer-string))))
        (kill-buffer (format "*Emacs Agent Diff %s*" id))))))

(ert-deftest emacs-agent-ui-runtime-controls-pause-resume-and-revoke ()
  (emacs-agent-ui-test--runtime
    (let (journal-events)
      (setf (emacs-agent-runtime-writer-lease runtime) "writer")
      (with-temp-buffer
        (emacs-agent-activity-mode)
        (setq-local emacs-agent-ui-runtime runtime)
        (cl-letf (((symbol-function 'emacs-agent-journal-write)
                   (lambda (active-runtime event)
                     (should (eq active-runtime runtime))
                     (push event journal-events)))
                  ((symbol-function 'emacs-agent-ui-refresh) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (emacs-agent-pause-mutations)
          (should (emacs-agent-runtime-paused-p runtime))
          (emacs-agent-resume-mutations)
          (should-not (emacs-agent-runtime-paused-p runtime))
          (emacs-agent-revoke-writer)))
      (should-not (emacs-agent-runtime-writer-lease runtime))
      (should (emacs-agent-runtime-paused-p runtime))
      (should
       (equal
        (mapcar (lambda (event) (plist-get event :tool))
                (nreverse journal-events))
        '("editor_pause" "editor_resume" "writer_revoke")))
      (should
       (cl-find-if
        (lambda (event)
          (equal (plist-get event :tool) "writer_revoke"))
        (emacs-agent-runtime-recent-activity runtime))))))

(ert-deftest emacs-agent-ui-changeset-controls-use-canonical-runtime-state ()
  (emacs-agent-ui-test--runtime
    (let ((absolute (expand-file-name "controlled.el" root))
          journal-events)
      (write-region "before\n" nil absolute)
      (let* ((path (file-truename absolute))
             (target (emacs-agent-ui-test--target runtime path))
             (document (emacs-agent-document-open runtime target))
             (base-revision (emacs-agent-document-revision document)))
        (with-current-buffer (emacs-agent-document-buffer document)
          (erase-buffer)
          (insert "after\n"))
        (let* ((final-revision
                (emacs-agent-document-revision document))
               (changeset
                (emacs-agent-changeset-record
                 runtime
                 :touched-documents (list path)
                 :base-revisions (list (cons path base-revision))
                 :final-revisions (list (cons path final-revision))
                 :before-snapshots
                 (list (cons path '(:exists t :content "before\n")))
                 :unified-diff "frozen"))
               (id (emacs-agent-changeset-changeset-id changeset)))
          (cl-letf (((symbol-function 'emacs-agent-ui--row-id)
                     (lambda () id))
                    ((symbol-function 'emacs-agent-journal-write)
                     (lambda (active-runtime event)
                       (should (eq active-runtime runtime))
                       (push event journal-events)))
                    ((symbol-function 'emacs-agent-ui-refresh) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (emacs-agent-checkpoint-at-point)
            (should-not
             (buffer-modified-p
              (emacs-agent-document-buffer document)))
            (should
             (eq (emacs-agent-changeset-status changeset)
                 'checkpointed))
            (emacs-agent-mark-reviewed-at-point)
            (should
             (eq (emacs-agent-changeset-status changeset)
                 'reviewed))
            (emacs-agent-rollback-at-point))
          (should
           (eq (emacs-agent-changeset-status changeset)
               'rolled-back))
          (should
           (equal
            (mapcar (lambda (event) (plist-get event :tool))
                    (nreverse journal-events))
            '("changeset_checkpoint"
              "changeset_mark_reviewed"
              "changeset_rollback")))
          (should
           (equal
            (emacs-agent-changeset-touched-documents changeset)
            (list path))))))))

(ert-deftest emacs-agent-ui-checkpoint-rejects-dirty-after-save ()
  (emacs-agent-ui-test--runtime
    (let ((absolute (expand-file-name "dirty-checkpoint.el" root))
          journal-events)
      (write-region "before\n" nil absolute)
      (let* ((path (file-truename absolute))
             (target (emacs-agent-ui-test--target runtime path))
             (document (emacs-agent-document-open runtime target))
             (base-revision (emacs-agent-document-revision document))
             (buffer (emacs-agent-document-buffer document)))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "after\n"))
        (let* ((final-revision
                (emacs-agent-document-revision document))
               (changeset
                (emacs-agent-changeset-record
                 runtime
                 :touched-documents (list path)
                 :base-revisions (list (cons path base-revision))
                 :final-revisions (list (cons path final-revision))
                 :before-snapshots
                 (list (cons path '(:exists t :content "before\n")))
                 :unified-diff "frozen"))
               (id (emacs-agent-changeset-changeset-id changeset)))
          (with-current-buffer buffer
            (add-hook
             'after-save-hook
             #'emacs-agent-ui-test--dirty-after-save nil t))
          (cl-letf (((symbol-function 'emacs-agent-ui--row-id)
                     (lambda () id))
                    ((symbol-function 'emacs-agent-journal-write)
                     (lambda (_active-runtime event)
                       (push event journal-events)))
                    ((symbol-function 'emacs-agent-ui-refresh) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (let ((error-data
                   (should-error
                    (emacs-agent-checkpoint-at-point)
                    :type 'emacs-agent-error)))
              (should
               (eq
                (emacs-agent-error-code error-data)
                'checkpoint_failed))))
          (should-not
           (emacs-agent-changeset-checkpoint-state changeset))
          (should
           (eq (emacs-agent-changeset-status changeset) 'applied))
          (should-not journal-events)
          (should (buffer-modified-p buffer))
          (should
           (equal
            (emacs-agent-document--buffer-content buffer)
            "after\ndirty after UI checkpoint\n"))
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents path)
              (buffer-string))
            "after\n"))
          (should-not (emacs-agent-document-degraded document))
          (should
           (eq
            (emacs-agent-runtime-health-state runtime)
            'healthy)))))))

(ert-deftest emacs-agent-ui-checkpoint-refreshes-rollback-guard ()
  (emacs-agent-ui-test--runtime
    (let ((absolute (expand-file-name "formatted-checkpoint.el" root)))
      (write-region "before\n" nil absolute)
      (let* ((path (file-truename absolute))
             (target (emacs-agent-ui-test--target runtime path))
             (document (emacs-agent-document-open runtime target))
             (base-revision (emacs-agent-document-revision document))
             (buffer (emacs-agent-document-buffer document)))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "after\n"))
        (let* ((pre-hook-revision
                (emacs-agent-document-revision document))
               (changeset
                (emacs-agent-changeset-record
                 runtime
                 :touched-documents (list path)
                 :base-revisions (list (cons path base-revision))
                 :final-revisions (list (cons path pre-hook-revision))
                 :before-snapshots
                 (list (cons path '(:exists t :content "before\n")))
                 :unified-diff "frozen-review-diff"))
               (id (emacs-agent-changeset-changeset-id changeset)))
          (with-current-buffer buffer
            (add-hook
             'before-save-hook
             #'emacs-agent-ui-test--format-before-save nil t))
          (cl-letf (((symbol-function 'emacs-agent-ui--row-id)
                     (lambda () id))
                    ((symbol-function 'emacs-agent-journal-write)
                     #'ignore)
                    ((symbol-function 'emacs-agent-ui-refresh) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (emacs-agent-checkpoint-at-point))
          (let ((checkpoint-revision
                 (emacs-agent-document-revision document)))
            (should-not (equal pre-hook-revision checkpoint-revision))
            (should
             (equal
              (cdr
               (assoc
                path
                (emacs-agent-changeset-final-revisions changeset)))
              checkpoint-revision)))
          (should
           (equal
            (emacs-agent-changeset-diff runtime id)
            "frozen-review-diff"))
          (emacs-agent-changeset-rollback runtime id)
          (should
           (eq (emacs-agent-changeset-status changeset) 'rolled-back))
          (should
           (equal
            (emacs-agent-document--buffer-content buffer)
            "before\n"))
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents path)
              (buffer-string))
            "before\n")))))))

(ert-deftest emacs-agent-ui-keymaps-expose-review-controls ()
  (should
   (eq (lookup-key emacs-agent-approvals-mode-map (kbd "a"))
       #'emacs-agent-approve-at-point))
  (should
   (eq (lookup-key emacs-agent-approvals-mode-map (kbd "p"))
       #'emacs-agent-partially-approve-at-point))
  (should
   (eq (lookup-key emacs-agent-approvals-mode-map (kbd "x"))
       #'emacs-agent-reject-at-point))
  (should
   (eq (lookup-key emacs-agent-approvals-mode-map (kbd "c"))
       #'emacs-agent-cancel-approval-at-point))
  (should
   (eq (lookup-key emacs-agent-changes-mode-map (kbd "h"))
       #'emacs-agent-highlight-changeset-at-point))
  (should
   (eq (lookup-key emacs-agent-changes-mode-map (kbd "c"))
       #'emacs-agent-checkpoint-at-point))
  (should
   (eq (lookup-key emacs-agent-changes-mode-map (kbd "r"))
       #'emacs-agent-rollback-at-point))
  (should
   (eq (lookup-key emacs-agent-changes-mode-map (kbd "v"))
       #'emacs-agent-mark-reviewed-at-point))
  (should
   (eq (lookup-key emacs-agent-activity-mode-map (kbd "P"))
       #'emacs-agent-pause-mutations))
  (should
   (eq (lookup-key emacs-agent-activity-mode-map (kbd "R"))
       #'emacs-agent-resume-mutations))
  (should
   (eq (lookup-key emacs-agent-activity-mode-map (kbd "k"))
       #'emacs-agent-revoke-writer)))

(provide 'emacs-agent-ui-test)
;;; emacs-agent-ui-test.el ends here
