;;; emacs-agent-ui-test.el --- Collaboration UI tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-agent-ui)

(defmacro emacs-agent-ui-test--workspace (&rest body)
  "Run BODY with a fresh bound workspace."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "emacs-agent-ui-" t))
          (workspace
           (emacs-agent-workspace-create
            root :workspace-id
            (concat "ui-" (file-name-nondirectory root))))
          (emacs-agent-current-workspace workspace))
     (unwind-protect
         (progn ,@body)
       (emacs-agent-ui-clear-highlights)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file root)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory root t))))

(ert-deftest emacs-agent-ui-approval-status-is-safe-and-cancellable ()
  (emacs-agent-ui-test--workspace
    (let* ((arguments
            '((path . "safe.el")
              (content . "private source")
              (authorization . "secret")))
           (request
            (emacs-agent-workspace-request-approval
             workspace "document_delete" arguments "credential"))
           (id (plist-get request :approval_request_id))
           (status
            (emacs-agent-workspace-approval-status workspace id)))
      (should (eq (plist-get status :status) 'pending))
      (should (equal (plist-get status :path) "safe.el"))
      (should-not (string-match-p
                   "private\\|secret\\|credential"
                   (prin1-to-string status)))
      (setq status
            (emacs-agent-workspace-approval-cancel workspace id))
      (should (eq (plist-get status :status) 'cancelled))
      (should-error
       (emacs-agent-workspace-consume-approval
        workspace id "document_delete" arguments "credential")
       :type 'emacs-agent-approval-error)
      (should
       (cl-find-if
        (lambda (entry)
          (equal (plist-get entry :status) "cancelled"))
        (emacs-agent-workspace-recent-activity workspace))))))

(ert-deftest emacs-agent-ui-approval-status-expires-on-query ()
  (emacs-agent-ui-test--workspace
    (let* ((emacs-agent-approval-lifetime -1)
           (request
            (emacs-agent-workspace-request-approval
             workspace "document_delete" nil "credential"))
           (status
            (emacs-agent-workspace-approval-status
             workspace (plist-get request :approval_request_id))))
      (should (eq (plist-get status :status) 'expired))
      (should (= (plist-get status :ttl_remaining) 0.0)))))

(ert-deftest emacs-agent-ui-approval-invalidates-on-revision-change ()
  (emacs-agent-ui-test--workspace
    (let* ((path "revision.el")
           (absolute (expand-file-name path root)))
      (write-region "before\n" nil absolute)
      (let* ((document (emacs-agent-document-open workspace path))
             (revision (emacs-agent-document-revision document))
             (arguments
              `((path . ,path) (expected_revision . ,revision)))
             (request
              (emacs-agent-workspace-request-approval
               workspace "document_delete" arguments "credential"))
             (id (plist-get request :approval_request_id)))
        (emacs-agent-workspace-approve workspace id)
        (with-current-buffer (emacs-agent-document-buffer document)
          (goto-char (point-max))
          (insert "changed\n"))
        (should
         (eq
          (plist-get
           (emacs-agent-workspace-approval-status workspace id)
           :status)
          'invalidated))
        (should-error
         (emacs-agent-workspace-consume-approval
          workspace id "document_delete" arguments "credential")
         :type 'emacs-agent-approval-error)))))

(ert-deftest emacs-agent-ui-rollback-approval-binds-final-revisions ()
  (emacs-agent-ui-test--workspace
    (let* ((path "rollback.el")
           (absolute (expand-file-name path root)))
      (write-region "current\n" nil absolute)
      (let* ((document (emacs-agent-document-open workspace path))
             (revision (emacs-agent-document-revision document))
             (changeset
              (emacs-agent-changeset-record
               workspace
               :touched-documents (list path)
               :before-snapshots
               (list (cons path '(:exists t :content "before\n")))
               :final-revisions (list (cons path revision))))
             (arguments
              (list :changeset_id
                    (emacs-agent-changeset-changeset-id changeset)))
             (request
              (emacs-agent-workspace-request-approval
               workspace "changeset_rollback" arguments "credential"))
             (id (plist-get request :approval_request_id)))
        (emacs-agent-workspace-approve workspace id)
        (with-current-buffer (emacs-agent-document-buffer document)
          (goto-char (point-max))
          (insert "new edit\n"))
        (should
         (eq
          (plist-get
           (emacs-agent-workspace-approval-status workspace id)
           :status)
          'invalidated))))))

(ert-deftest emacs-agent-ui-open-file-jumps-to-range ()
  (emacs-agent-ui-test--workspace
    (let ((path "jump.el"))
      (write-region "zero\none two\nthree\n" nil
                    (expand-file-name path root))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _ignored)
                   (set-buffer buffer))))
        (emacs-agent-ui-open-file-at-range
         workspace path
         '(:start (:line 2 :column 4)
           :end (:line 2 :column 7))))
      (should (= (line-number-at-pos) 2))
      (should (= (current-column) 4)))))

(ert-deftest emacs-agent-ui-highlights-current-changeset-hunks ()
  (emacs-agent-ui-test--workspace
    (let* ((path "highlight.el")
           (absolute (expand-file-name path root)))
      (write-region "before\nkeep\n" nil absolute)
      (let ((document (emacs-agent-document-open workspace path)))
        (with-current-buffer (emacs-agent-document-buffer document)
          (erase-buffer)
          (insert "after\nkeep\n"))
        (let* ((revision (emacs-agent-document-revision document))
               (changeset
                (emacs-agent-changeset-record
                 workspace
                 :touched-documents (list path)
                 :before-snapshots
                 (list (cons path '(:exists t :content "before\nkeep\n")))
                 :final-revisions (list (cons path revision))))
               (id (emacs-agent-changeset-changeset-id changeset))
               (result
                (emacs-agent-highlight-changeset workspace id)))
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
  (emacs-agent-ui-test--workspace
    (let* ((path "diff.el")
           (absolute (expand-file-name path root)))
      (write-region "after\n" nil absolute)
      (let* ((revision
              (emacs-agent-document-revision-for-path workspace path))
             (changeset
              (emacs-agent-changeset-record
               workspace
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

(ert-deftest emacs-agent-ui-keymaps-expose-review-controls ()
  (should
   (eq (lookup-key emacs-agent-approvals-mode-map (kbd "a"))
       #'emacs-agent-approve-at-point))
  (should
   (eq (lookup-key emacs-agent-approvals-mode-map (kbd "x"))
       #'emacs-agent-reject-at-point))
  (should
   (eq (lookup-key emacs-agent-approvals-mode-map (kbd "c"))
       #'emacs-agent-cancel-approval-at-point))
  (should
   (eq (lookup-key emacs-agent-changes-mode-map (kbd "h"))
       #'emacs-agent-highlight-changeset-at-point)))

(provide 'emacs-agent-ui-test)
;;; emacs-agent-ui-test.el ends here
