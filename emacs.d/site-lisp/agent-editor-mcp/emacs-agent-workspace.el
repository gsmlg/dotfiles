;;; emacs-agent-workspace.el --- Workspace state for Emacs Agent Editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Workspace binding, mutation serialization, activity history, and review
;; approvals.  A daemon normally binds exactly one workspace.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)

(defgroup emacs-agent-editor nil
  "Buffer-first editing services for software agents."
  :group 'tools)

(defcustom emacs-agent-activity-limit 200
  "Maximum number of activity entries retained in memory."
  :type 'integer)

(defcustom emacs-agent-approval-lifetime 300
  "Number of seconds an approved request remains usable."
  :type 'integer)

(define-error 'emacs-agent-workspace-error "Emacs Agent workspace error")
(define-error 'emacs-agent-workspace-paused "Workspace mutations are paused"
  'emacs-agent-workspace-error)
(define-error 'emacs-agent-approval-error "Approval is invalid"
  'emacs-agent-workspace-error)

(cl-defstruct (emacs-agent-workspace
               (:constructor emacs-agent-workspace--make))
  workspace-id
  root
  canonical-root
  project
  server-epoch
  access-mode
  save-policy
  writer-lease
  document-registry
  changeset-registry
  mutation-queue
  mutation-active-p
  denied-paths
  allowed-paths
  state-directory
  health-state
  paused-p
  activity-ring
  approval-registry)

(cl-defstruct (emacs-agent-approval
               (:constructor emacs-agent-approval--make))
  id operation digest credential created-at expires-at status)

(defvar emacs-agent-workspace-registry (make-hash-table :test #'equal)
  "Registry of configured workspaces, keyed by workspace ID.")

(defvar emacs-agent-current-workspace nil
  "Workspace currently bound to the editor service.")

(defun emacs-agent-workspace--random-id (prefix)
  "Return an opaque identifier beginning with PREFIX."
  (unless (and (file-readable-p "/dev/urandom")
               (executable-find "dd"))
    (signal 'emacs-agent-workspace-error
            (list "Secure OS entropy is unavailable")))
  (let ((bytes
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (unless
               (zerop
                (call-process
                 (executable-find "dd") nil (list (current-buffer) nil) nil
                 "if=/dev/urandom" "bs=32" "count=1"))
             (signal 'emacs-agent-workspace-error
                     (list "Unable to read secure OS entropy")))
           (unless (= (buffer-size) 32)
             (signal 'emacs-agent-workspace-error
                     (list "Secure OS entropy returned too few bytes")))
           (buffer-string))))
    (format "%s_%s" prefix (secure-hash 'sha256 bytes))))

(defun emacs-agent-workspace--canonical-root (root)
  "Validate and canonicalize workspace ROOT."
  (when (file-remote-p root)
    (signal 'emacs-agent-workspace-error
            (list "Remote workspaces are not supported")))
  (unless (file-directory-p root)
    (signal 'emacs-agent-workspace-error
            (list (format "Workspace does not exist: %s" root))))
  (let ((canonical (file-name-as-directory
                    (file-truename (expand-file-name root)))))
    (when (equal canonical
                 (file-name-as-directory
                  (file-truename (expand-file-name "/"))))
      (signal 'emacs-agent-workspace-error
              (list "The filesystem root cannot be a workspace")))
    canonical))

(cl-defun emacs-agent-workspace-create
    (root &key workspace-id (access-mode 'autonomous)
          (save-policy 'immediate) writer-lease denied-paths allowed-paths
          state-directory)
  "Create and register a workspace rooted at ROOT.

WORKSPACE-ID defaults to an opaque identifier.  ACCESS-MODE is one of
`read-only', `review', or `autonomous'.  SAVE-POLICY is normally `immediate',
`manual', or `explicit-per-call'.  WRITER-LEASE, DENIED-PATHS, ALLOWED-PATHS,
and STATE-DIRECTORY initialize the corresponding policy state."
  (unless (memq access-mode '(read-only review autonomous))
    (signal 'wrong-type-argument
            (list '(member read-only review autonomous) access-mode)))
  (when (eq save-policy 'per-call)
    (setq save-policy 'explicit-per-call))
  (unless (memq save-policy '(immediate manual explicit-per-call))
    (signal 'wrong-type-argument
            (list '(member immediate manual explicit-per-call) save-policy)))
  (let* ((canonical (emacs-agent-workspace--canonical-root root))
         (id (or workspace-id (emacs-agent-workspace--random-id "ws")))
         (default-directory canonical)
         (workspace
          (emacs-agent-workspace--make
           :workspace-id id
           :root canonical
           :canonical-root canonical
           :project (project-current nil canonical)
           :server-epoch (emacs-agent-workspace--random-id "epoch")
           :access-mode access-mode
           :save-policy save-policy
           :writer-lease writer-lease
           :document-registry (make-hash-table :test #'equal)
           :changeset-registry (make-hash-table :test #'equal)
           :mutation-queue nil
           :mutation-active-p nil
           :denied-paths denied-paths
           :allowed-paths allowed-paths
           :state-directory state-directory
           :health-state 'healthy
           :paused-p nil
           :activity-ring nil
           :approval-registry (make-hash-table :test #'equal))))
    (puthash id workspace emacs-agent-workspace-registry)
    workspace))

(defun emacs-agent-workspace-bind (workspace)
  "Bind WORKSPACE as the current daemon workspace."
  (unless (emacs-agent-workspace-p workspace)
    (signal 'wrong-type-argument (list 'emacs-agent-workspace workspace)))
  (setq emacs-agent-current-workspace workspace))

(defun emacs-agent-workspace-current ()
  "Return the currently bound workspace, or signal if none is bound."
  (or emacs-agent-current-workspace
      (signal 'emacs-agent-workspace-error
              (list "No workspace is bound"))))

(defun emacs-agent-workspace-info (&optional workspace)
  "Return public information for WORKSPACE."
  (let ((workspace (or workspace (emacs-agent-workspace-current))))
    (list :workspace_id (emacs-agent-workspace-workspace-id workspace)
          :root (emacs-agent-workspace-root workspace)
          :access_mode (emacs-agent-workspace-access-mode workspace)
          :save_policy (emacs-agent-workspace-save-policy workspace)
          :writer_lease (emacs-agent-workspace-writer-lease workspace)
          :paused (and (emacs-agent-workspace-paused-p workspace) t)
          :health (emacs-agent-workspace-health-state workspace)
          :server_epoch (emacs-agent-workspace-server-epoch workspace))))

(defun emacs-agent-workspace-pause (&optional workspace)
  "Pause mutations in WORKSPACE."
  (interactive)
  (let ((workspace (or workspace (emacs-agent-workspace-current))))
    (setf (emacs-agent-workspace-paused-p workspace) t)
    (emacs-agent-workspace-record-activity
     workspace (list :tool "workspace_pause" :status "completed"))
    t))

(defun emacs-agent-workspace-resume (&optional workspace)
  "Resume mutations in WORKSPACE."
  (interactive)
  (let ((workspace (or workspace (emacs-agent-workspace-current))))
    (setf (emacs-agent-workspace-paused-p workspace) nil)
    (emacs-agent-workspace-record-activity
     workspace (list :tool "workspace_resume" :status "completed"))
    t))

(defun emacs-agent-workspace-mutations-allowed-p (&optional workspace)
  "Return non-nil when WORKSPACE permits mutations."
  (let ((workspace (or workspace (emacs-agent-workspace-current))))
    (and (not (emacs-agent-workspace-paused-p workspace))
         (not (eq (emacs-agent-workspace-access-mode workspace)
                  'read-only)))))

(defun emacs-agent-workspace--run-job (_workspace job)
  "Run mutation JOB for WORKSPACE and notify its callback."
  (pcase-let ((`(,function ,callback) job))
    (condition-case error-data
        (let ((result (funcall function)))
          (when callback (funcall callback result nil))
          (list t result))
      (error
       (when callback (funcall callback nil error-data))
       (list nil error-data)))))

(defun emacs-agent-workspace--drain-mutations (workspace)
  "Drain queued mutations for WORKSPACE."
  (unless (emacs-agent-workspace-mutation-active-p workspace)
    (setf (emacs-agent-workspace-mutation-active-p workspace) t)
    (unwind-protect
        (while (emacs-agent-workspace-mutation-queue workspace)
          (let ((job (pop (emacs-agent-workspace-mutation-queue workspace))))
            (emacs-agent-workspace--run-job workspace job)))
      (setf (emacs-agent-workspace-mutation-active-p workspace) nil))))

(defun emacs-agent-workspace-enqueue-mutation
    (workspace function &optional callback)
  "Serialize FUNCTION in WORKSPACE's mutation queue.

CALLBACK, when non-nil, receives RESULT and ERROR-DATA.  The initial job is
run immediately in Emacs's event loop and its result is returned.  A reentrant
job is queued and returns `queued'."
  (unless (emacs-agent-workspace-mutations-allowed-p workspace)
    (signal 'emacs-agent-workspace-paused
            (list (if (emacs-agent-workspace-paused-p workspace)
                      "Workspace mutations are paused"
                    "Workspace is read-only"))))
  (let ((job (list function callback)))
    (if (emacs-agent-workspace-mutation-active-p workspace)
        (progn
          (setf (emacs-agent-workspace-mutation-queue workspace)
                (nconc (emacs-agent-workspace-mutation-queue workspace)
                       (list job)))
          'queued)
      (setf (emacs-agent-workspace-mutation-active-p workspace) t)
      (unwind-protect
          (pcase-let ((`(,ok ,value)
                       (emacs-agent-workspace--run-job workspace job)))
            (while (emacs-agent-workspace-mutation-queue workspace)
              (emacs-agent-workspace--run-job
               workspace
               (pop (emacs-agent-workspace-mutation-queue workspace))))
            (if ok value (signal (car value) (cdr value))))
        (setf (emacs-agent-workspace-mutation-active-p workspace) nil)))))

(defun emacs-agent-workspace-record-activity (workspace event)
  "Record EVENT in WORKSPACE's bounded activity history."
  (let ((entry (copy-sequence event)))
    (unless (plist-member entry :timestamp)
      (setq entry (plist-put entry :timestamp (float-time))))
    (setf (emacs-agent-workspace-activity-ring workspace)
          (cl-subseq
           (cons entry (emacs-agent-workspace-activity-ring workspace))
           0 (min emacs-agent-activity-limit
                  (1+ (length
                       (emacs-agent-workspace-activity-ring workspace))))))
    entry))

(defun emacs-agent-workspace-recent-activity (&optional workspace)
  "Return recent activity for WORKSPACE, newest first."
  (copy-tree
   (emacs-agent-workspace-activity-ring
    (or workspace (emacs-agent-workspace-current)))))

(defun emacs-agent-workspace--canonical-approval-value (value)
  "Return a deterministic representation of approval VALUE."
  (cond
   ((hash-table-p value)
    (let (entries)
      (maphash
       (lambda (key item)
         (push
          (cons (format "%s" key)
                (emacs-agent-workspace--canonical-approval-value item))
          entries))
       value)
      (sort entries (lambda (left right)
                      (string< (car left) (car right))))))
   ((and (listp value) (keywordp (car value)))
    (let (entries)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (push
           (cons (symbol-name key)
                 (emacs-agent-workspace--canonical-approval-value item))
           entries)))
      (sort entries (lambda (left right)
                      (string< (car left) (car right))))))
   ((and (listp value)
         (cl-every
          (lambda (entry)
            (and (consp entry)
                 (or (symbolp (car entry))
                     (stringp (car entry)))))
          value))
    (sort
     (mapcar
      (lambda (entry)
        (cons (format "%s" (car entry))
              (emacs-agent-workspace--canonical-approval-value (cdr entry))))
      value)
     (lambda (left right) (string< (car left) (car right)))))
   ((vectorp value)
    (vconcat
     (mapcar #'emacs-agent-workspace--canonical-approval-value value)))
   ((listp value)
    (mapcar #'emacs-agent-workspace--canonical-approval-value value))
   (t value)))

(defun emacs-agent-workspace-approval-digest (operation arguments)
  "Return a stable digest for OPERATION and normalized ARGUMENTS."
  (secure-hash 'sha256
               (encode-coding-string
                (prin1-to-string
                 (list
                  operation
                  (emacs-agent-workspace--canonical-approval-value
                   arguments)))
                'utf-8)))

(defun emacs-agent-workspace-request-approval
    (workspace operation arguments credential)
  "Create a pending approval for OPERATION and ARGUMENTS in WORKSPACE.

The approval is bound to CREDENTIAL without exposing that credential in the
returned value or activity history."
  (let* ((id (emacs-agent-workspace--random-id "approval"))
         (now (float-time))
         (approval
          (emacs-agent-approval--make
           :id id :operation operation
           :digest (emacs-agent-workspace-approval-digest operation arguments)
           :credential credential :created-at now
           :expires-at (+ now emacs-agent-approval-lifetime)
           :status 'pending)))
    (puthash id approval
             (emacs-agent-workspace-approval-registry workspace))
    (emacs-agent-workspace-record-activity
     workspace
     (list :tool operation :status "approval_required"
           :approval_request_id id))
    (list :approval_request_id id
          :operation_digest
          (emacs-agent-workspace-approval-digest operation arguments)
          :expires_at (emacs-agent-approval-expires-at approval))))

(defun emacs-agent-workspace--approval (workspace id)
  "Return approval ID from WORKSPACE or signal."
  (or (gethash id (emacs-agent-workspace-approval-registry workspace))
      (signal 'emacs-agent-approval-error
              (list "Unknown approval request"))))

(defun emacs-agent-workspace-approve (workspace id)
  "Approve pending request ID in WORKSPACE."
  (let ((approval (emacs-agent-workspace--approval workspace id)))
    (unless (eq (emacs-agent-approval-status approval) 'pending)
      (signal 'emacs-agent-approval-error
              (list "Approval request is not pending")))
    (when (< (emacs-agent-approval-expires-at approval) (float-time))
      (setf (emacs-agent-approval-status approval) 'expired)
      (signal 'emacs-agent-approval-error
              (list "Approval request has expired")))
    (setf (emacs-agent-approval-status approval) 'approved)
    t))

(defun emacs-agent-workspace-reject (workspace id)
  "Reject pending request ID in WORKSPACE."
  (let ((approval (emacs-agent-workspace--approval workspace id)))
    (unless (memq (emacs-agent-approval-status approval) '(pending approved))
      (signal 'emacs-agent-approval-error
              (list "Approval request cannot be rejected")))
    (setf (emacs-agent-approval-status approval) 'rejected)
    t))

(defun emacs-agent-workspace-consume-approval
    (workspace id operation arguments credential)
  "Consume approval ID in WORKSPACE if it exactly authorizes this request.

The grant is one-use, unexpired, credential-bound, and tied to OPERATION and
ARGUMENTS."
  (let ((approval (emacs-agent-workspace--approval workspace id)))
    (unless (eq (emacs-agent-approval-status approval) 'approved)
      (signal 'emacs-agent-approval-error
              (list "Approval request has not been approved")))
    (when (< (emacs-agent-approval-expires-at approval) (float-time))
      (setf (emacs-agent-approval-status approval) 'expired)
      (signal 'emacs-agent-approval-error
              (list "Approval request has expired")))
    (unless (and (equal credential (emacs-agent-approval-credential approval))
                 (equal operation (emacs-agent-approval-operation approval))
                 (equal (emacs-agent-workspace-approval-digest
                         operation arguments)
                        (emacs-agent-approval-digest approval)))
      (signal 'emacs-agent-approval-error
              (list "Approval does not match this request")))
    (setf (emacs-agent-approval-status approval) 'consumed)
    t))

(provide 'emacs-agent-workspace)
;;; emacs-agent-workspace.el ends here
