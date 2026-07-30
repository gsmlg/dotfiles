;;; emacs-agent-runtime.el --- Editor runtime state -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Process-lifetime state, mutation serialization, activity history, and
;; review approvals for one Agent Editor MCP server.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function emacs-agent-changeset-final-revisions
                  "emacs-agent-changeset" (changeset))
(declare-function emacs-agent-changeset-get
                  "emacs-agent-changeset" (runtime changeset-id))
(declare-function emacs-agent-document-revision-for-target
                  "emacs-agent-document" (runtime target))
(declare-function emacs-agent-document-status
                  "emacs-agent-document" (runtime target))
(declare-function emacs-agent-project-resolve-target
                  "emacs-agent-project"
                  (runtime path &rest keys))
(declare-function emacs-agent-resolved-target-canonical-path
                  "emacs-agent-policy" (target))

(defgroup emacs-agent-editor nil
  "Buffer-first editing services for software agents."
  :group 'tools)

(defcustom emacs-agent-runtime-activity-limit 200
  "Maximum number of activity entries retained in memory."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-runtime-approval-lifetime 300
  "Number of seconds an approved request remains usable."
  :type 'integer
  :group 'emacs-agent-editor)

(define-error 'emacs-agent-runtime-error "Emacs Agent runtime error")
(define-error 'emacs-agent-runtime-not-started
  "Emacs Agent runtime is not started"
  'emacs-agent-runtime-error)
(define-error 'emacs-agent-runtime-already-started
  "Emacs Agent runtime is already started"
  'emacs-agent-runtime-error)
(define-error 'emacs-agent-runtime-paused
  "Editor runtime mutations are paused"
  'emacs-agent-runtime-error)
(define-error 'emacs-agent-runtime-approval-error
  "Approval is invalid"
  'emacs-agent-runtime-error)

(cl-defstruct (emacs-agent-runtime
               (:constructor emacs-agent-runtime--make))
  instance-id
  server-epoch
  started-at
  access-mode
  save-policy
  writer-lease
  document-registry
  project-registry
  project-root-index
  changeset-registry
  mutation-queue
  mutation-active-p
  state-directory
  health-state
  paused-p
  activity-ring
  approval-registry
  filesystem-policy
  allowed-roots
  denied-paths
  allowed-paths)

(cl-defstruct (emacs-agent-runtime-approval
               (:constructor emacs-agent-runtime-approval--make))
  id operation operation-digest credential created-at expires-at state summary
  revision-bindings arguments parent-id derived-id accepted-paths)

(defvar emacs-agent-current-runtime nil
  "Runtime currently bound to the editor service.")

(defun emacs-agent-runtime--random-id (prefix)
  "Return an opaque identifier beginning with PREFIX."
  (unless (and (file-readable-p "/dev/urandom")
               (executable-find "dd"))
    (signal 'emacs-agent-runtime-error
            (list "Secure OS entropy is unavailable")))
  (let ((bytes
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (unless
               (zerop
                (call-process
                 (executable-find "dd") nil (list (current-buffer) nil) nil
                 "if=/dev/urandom" "bs=32" "count=1"))
             (signal 'emacs-agent-runtime-error
                     (list "Unable to read secure OS entropy")))
           (unless (= (buffer-size) 32)
             (signal 'emacs-agent-runtime-error
                     (list "Secure OS entropy returned too few bytes")))
           (buffer-string))))
    (format "%s_%s" prefix (secure-hash 'sha256 bytes))))

(cl-defun emacs-agent-runtime-create
    (&key instance-id (access-mode 'autonomous) (save-policy 'immediate)
          writer-lease state-directory (filesystem-policy 'unrestricted)
          allowed-roots denied-paths allowed-paths)
  "Create an unbound editor runtime.

INSTANCE-ID defaults to an opaque identifier.  ACCESS-MODE is `read-only',
`review', or `autonomous'.  SAVE-POLICY is `immediate', `manual', or
`explicit-per-call'.  WRITER-LEASE and STATE-DIRECTORY initialize the
corresponding runtime state.  FILESYSTEM-POLICY is `unrestricted' or
`allowlist'; ALLOWED-ROOTS, DENIED-PATHS, and ALLOWED-PATHS initialize its
configured path rules."
  (unless (memq access-mode '(read-only review autonomous))
    (signal 'wrong-type-argument
            (list '(member read-only review autonomous) access-mode)))
  (unless (memq save-policy '(immediate manual explicit-per-call))
    (signal 'wrong-type-argument
            (list '(member immediate manual explicit-per-call) save-policy)))
  (unless (memq filesystem-policy '(unrestricted allowlist))
    (signal 'wrong-type-argument
            (list '(member unrestricted allowlist) filesystem-policy)))
  (emacs-agent-runtime--make
   :instance-id (or instance-id
                    (emacs-agent-runtime--random-id "editor"))
   :server-epoch (emacs-agent-runtime--random-id "epoch")
   :started-at (format-time-string "%FT%TZ" nil t)
   :access-mode access-mode
   :save-policy save-policy
   :writer-lease writer-lease
   :document-registry (make-hash-table :test #'equal)
   :project-registry (make-hash-table :test #'equal)
   :project-root-index (make-hash-table :test #'equal)
   :changeset-registry (make-hash-table :test #'equal)
   :mutation-queue nil
   :mutation-active-p nil
   :state-directory state-directory
   :health-state 'healthy
   :paused-p nil
   :activity-ring nil
   :approval-registry (make-hash-table :test #'equal)
   :filesystem-policy filesystem-policy
   :allowed-roots (copy-sequence allowed-roots)
   :denied-paths (copy-sequence denied-paths)
   :allowed-paths (copy-sequence allowed-paths)))

(defun emacs-agent-runtime-bind (runtime)
  "Bind RUNTIME as the current editor runtime."
  (unless (emacs-agent-runtime-p runtime)
    (signal 'wrong-type-argument (list 'emacs-agent-runtime runtime)))
  (when (and emacs-agent-current-runtime
             (not (eq emacs-agent-current-runtime runtime)))
    (signal 'emacs-agent-runtime-already-started
            (list "Another Agent Editor runtime is already active")))
  (setq emacs-agent-current-runtime runtime))

(defun emacs-agent-runtime-current ()
  "Return the active editor runtime, or signal when none is bound."
  (or emacs-agent-current-runtime
      (signal 'emacs-agent-runtime-not-started
              (list "Agent Editor MCP is not started"))))

(defun emacs-agent-runtime-clear (&optional runtime)
  "Discard mutable state belonging to RUNTIME without killing buffers."
  (let ((runtime (or runtime emacs-agent-current-runtime)))
    (when (emacs-agent-runtime-p runtime)
      (dolist (registry
               (list
                (emacs-agent-runtime-document-registry runtime)
                (emacs-agent-runtime-project-registry runtime)
                (emacs-agent-runtime-project-root-index runtime)
                (emacs-agent-runtime-changeset-registry runtime)
                (emacs-agent-runtime-approval-registry runtime)))
        (clrhash registry))
      (setf (emacs-agent-runtime-mutation-queue runtime) nil
            (emacs-agent-runtime-mutation-active-p runtime) nil
            (emacs-agent-runtime-activity-ring runtime) nil
            (emacs-agent-runtime-paused-p runtime) nil)
      (when (eq runtime emacs-agent-current-runtime)
        (setq emacs-agent-current-runtime nil))))
  t)

(defun emacs-agent-runtime-info (&optional runtime)
  "Return public state for RUNTIME."
  (let ((runtime (or runtime (emacs-agent-runtime-current))))
    (list
     :instance_id (emacs-agent-runtime-instance-id runtime)
     :access_mode (emacs-agent-runtime-access-mode runtime)
     :save_policy (emacs-agent-runtime-save-policy runtime)
     :paused (and (emacs-agent-runtime-paused-p runtime) t)
     :health (emacs-agent-runtime-health-state runtime)
     :project_count
     (hash-table-count (emacs-agent-runtime-project-registry runtime))
     :managed_document_count
     (hash-table-count (emacs-agent-runtime-document-registry runtime))
     :filesystem_policy
     (emacs-agent-runtime-filesystem-policy runtime))))

(defun emacs-agent-runtime-pause (&optional runtime)
  "Pause mutations in RUNTIME."
  (interactive)
  (let ((runtime (or runtime (emacs-agent-runtime-current))))
    (setf (emacs-agent-runtime-paused-p runtime) t)
    (emacs-agent-runtime-record-activity
     runtime (list :tool "editor_pause" :status "completed"))
    t))

(defun emacs-agent-runtime-resume (&optional runtime)
  "Resume mutations in RUNTIME."
  (interactive)
  (let ((runtime (or runtime (emacs-agent-runtime-current))))
    (setf (emacs-agent-runtime-paused-p runtime) nil)
    (emacs-agent-runtime-record-activity
     runtime (list :tool "editor_resume" :status "completed"))
    t))

(defun emacs-agent-runtime-mutations-allowed-p (&optional runtime)
  "Return non-nil when RUNTIME permits mutations."
  (let ((runtime (or runtime (emacs-agent-runtime-current))))
    (and (not (emacs-agent-runtime-paused-p runtime))
         (not (eq (emacs-agent-runtime-access-mode runtime)
                  'read-only)))))

(defun emacs-agent-runtime--run-job (_runtime job)
  "Run mutation JOB and notify its callback."
  (pcase-let ((`(,function ,callback) job))
    (condition-case error-data
        (let ((result (funcall function)))
          (when callback
            (funcall callback result nil))
          (list t result))
      (error
       (when callback
         (funcall callback nil error-data))
       (list nil error-data)))))

(defun emacs-agent-runtime-enqueue-mutation
    (runtime function &optional callback)
  "Serialize FUNCTION in RUNTIME's mutation queue.

CALLBACK, when non-nil, receives RESULT and ERROR-DATA.  A reentrant job is
queued and returns `queued'."
  (unless (emacs-agent-runtime-mutations-allowed-p runtime)
    (signal 'emacs-agent-runtime-paused
            (list
             (if (emacs-agent-runtime-paused-p runtime)
                 "Editor runtime mutations are paused"
               "Editor runtime is read-only"))))
  (let ((job (list function callback)))
    (if (emacs-agent-runtime-mutation-active-p runtime)
        (progn
          (setf (emacs-agent-runtime-mutation-queue runtime)
                (nconc
                 (emacs-agent-runtime-mutation-queue runtime)
                 (list job)))
          'queued)
      (setf (emacs-agent-runtime-mutation-active-p runtime) t)
      (unwind-protect
          (pcase-let
              ((`(,ok ,value)
                (emacs-agent-runtime--run-job runtime job)))
            (while (emacs-agent-runtime-mutation-queue runtime)
              (emacs-agent-runtime--run-job
               runtime
               (pop (emacs-agent-runtime-mutation-queue runtime))))
            (if ok
                value
              (signal (car value) (cdr value))))
        (setf (emacs-agent-runtime-mutation-active-p runtime) nil)))))

(defun emacs-agent-runtime-record-activity (runtime event)
  "Record EVENT in RUNTIME's bounded activity history."
  (let ((entry (copy-sequence event)))
    (unless (plist-member entry :timestamp)
      (setq entry (plist-put entry :timestamp (float-time))))
    (setf (emacs-agent-runtime-activity-ring runtime)
          (cl-subseq
           (cons entry (emacs-agent-runtime-activity-ring runtime))
           0
           (min emacs-agent-runtime-activity-limit
                (1+ (length
                     (emacs-agent-runtime-activity-ring runtime))))))
    entry))

(defun emacs-agent-runtime-recent-activity (&optional runtime)
  "Return recent activity for RUNTIME, newest first."
  (copy-tree
   (emacs-agent-runtime-activity-ring
    (or runtime (emacs-agent-runtime-current)))))

(defun emacs-agent-runtime--canonical-approval-value (value)
  "Return a deterministic representation of approval VALUE."
  (cond
   ((hash-table-p value)
    (let (entries)
      (maphash
       (lambda (key item)
         (push
          (cons
           (format "%s" key)
           (emacs-agent-runtime--canonical-approval-value item))
          entries))
       value)
      (sort entries
            (lambda (left right)
              (string< (car left) (car right))))))
   ((and (listp value) (keywordp (car value)))
    (let (entries)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (push
           (cons
            (symbol-name key)
            (emacs-agent-runtime--canonical-approval-value item))
           entries)))
      (sort entries
            (lambda (left right)
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
        (cons
         (format "%s" (car entry))
         (emacs-agent-runtime--canonical-approval-value (cdr entry))))
      value)
     (lambda (left right)
       (string< (car left) (car right)))))
   ((vectorp value)
    (vconcat
     (mapcar #'emacs-agent-runtime--canonical-approval-value value)))
   ((listp value)
    (mapcar #'emacs-agent-runtime--canonical-approval-value value))
   (t value)))

(defun emacs-agent-runtime-approval-digest (operation arguments)
  "Return a stable digest for OPERATION and normalized ARGUMENTS."
  (secure-hash
   'sha256
   (encode-coding-string
    (prin1-to-string
     (list
      operation
      (emacs-agent-runtime--canonical-approval-value arguments)))
    'utf-8)))

(defun emacs-agent-runtime--approval-field (object key)
  "Read KEY from approval argument OBJECT."
  (cond
   ((hash-table-p object)
    (or
     (gethash key object)
     (gethash (symbol-name key) object)
     (gethash (intern (concat ":" (symbol-name key))) object)))
   ((and (listp object) (keywordp (car object)))
    (plist-get object (intern (concat ":" (symbol-name key)))))
   ((listp object)
    (or
     (alist-get key object)
     (alist-get (symbol-name key) object nil nil #'string=)))))

(defun emacs-agent-runtime--approval-has-field-p (object key)
  "Return whether approval argument OBJECT has KEY."
  (cond
   ((hash-table-p object)
    (let ((missing (make-symbol "missing")))
      (or
       (not (eq missing (gethash key object missing)))
       (not (eq missing (gethash (symbol-name key) object missing)))
       (not
        (eq
         missing
         (gethash
          (intern (concat ":" (symbol-name key)))
          object missing))))))
   ((and (listp object) (keywordp (car object)))
    (plist-member object (intern (concat ":" (symbol-name key)))))
   ((listp object)
    (or
     (assq key object)
     (assoc (symbol-name key) object)))))

(defun emacs-agent-runtime--approval-revision-bindings
    (runtime arguments)
  "Return target and revision bindings for RUNTIME and ARGUMENTS."
  (let ((path (emacs-agent-runtime--approval-field arguments 'path))
        (project-id
         (emacs-agent-runtime--approval-field arguments 'project_id))
        (revision
         (emacs-agent-runtime--approval-field
          arguments 'expected_revision))
        (documents
         (emacs-agent-runtime--approval-field arguments 'documents))
        (changeset-id
         (emacs-agent-runtime--approval-field arguments 'changeset_id))
        bindings)
    (when (and (stringp path)
               (stringp revision)
               (fboundp 'emacs-agent-project-resolve-target))
      (push
       (cons
        (funcall
         #'emacs-agent-project-resolve-target
         runtime path :project-id
         (and (stringp project-id) project-id))
        revision)
       bindings))
    (dolist
        (document
         (cond
          ((vectorp documents)
           (append documents nil))
          ((listp documents)
           documents)))
      (let ((document-path
             (emacs-agent-runtime--approval-field document 'path))
            (document-project-id
             (emacs-agent-runtime--approval-field
              document 'project_id))
            (document-revision
             (emacs-agent-runtime--approval-field
              document 'expected_revision)))
        (when (and (stringp document-path)
                   (stringp document-revision)
                   (fboundp 'emacs-agent-project-resolve-target))
          (push
           (cons
            (funcall
             #'emacs-agent-project-resolve-target
             runtime document-path :project-id
             (and (stringp document-project-id)
                  document-project-id))
            document-revision)
           bindings))))
    (when (and (null bindings)
               (stringp changeset-id)
               (fboundp 'emacs-agent-changeset-get)
               (fboundp 'emacs-agent-changeset-final-revisions))
      (condition-case nil
          (setq
           bindings
           (mapcar
            (lambda (entry)
              (cons
               (funcall
                #'emacs-agent-project-resolve-target
                runtime (car entry))
               (cdr entry)))
            (funcall
             #'emacs-agent-changeset-final-revisions
             (funcall
              #'emacs-agent-changeset-get
              runtime changeset-id))))
        (error nil)))
    (nreverse bindings)))

(cl-defun emacs-agent-runtime--approval-target
    (runtime object &key (path-key 'path) (project-key 'project_id)
             for-create)
  "Return OBJECT's canonical approval target in RUNTIME.
PATH-KEY and PROJECT-KEY select its path fields.  FOR-CREATE permits a
missing leaf."
  (let ((path
         (emacs-agent-runtime--approval-field object path-key))
        (project-id
         (emacs-agent-runtime--approval-field object project-key)))
    (when (stringp path)
      (cond
       ((fboundp 'emacs-agent-project-resolve-target)
        (emacs-agent-resolved-target-canonical-path
         (funcall
          #'emacs-agent-project-resolve-target
          runtime path :project-id
          (and (stringp project-id) project-id)
          :for-create for-create)))
       ((stringp project-id)
        (signal
         'emacs-agent-runtime-approval-error
         (list "Project target resolution is unavailable")))
       (t path)))))

(defun emacs-agent-runtime--approval-summary
    (runtime operation arguments)
  "Return a display-safe summary for OPERATION and ARGUMENTS in RUNTIME."
  (let ((keys
         '(changeset_id expected_revision
                checkpoint force dry_run))
        summary)
    (dolist (key keys)
      (let ((value
             (emacs-agent-runtime--approval-field arguments key)))
        (when
            (and
             (emacs-agent-runtime--approval-has-field-p arguments key)
             (or
              (stringp value)
              (numberp value)
              (memq value '(t nil))))
          (setq
           summary
           (plist-put
            summary
            (intern (concat ":" (symbol-name key)))
            value)))))
    (when-let* ((path
                 (emacs-agent-runtime--approval-target runtime arguments)))
      (setq summary (plist-put summary :path path)))
    (when-let* ((new-path
                 (emacs-agent-runtime--approval-target
                  runtime arguments
                  :path-key 'new_path
                  :project-key 'new_project_id
                  :for-create t)))
      (setq summary (plist-put summary :new_path new-path)))
    (let ((documents
           (emacs-agent-runtime--approval-field arguments 'documents)))
      (when (or (listp documents) (vectorp documents))
        (setq summary
              (plist-put summary :document_count
                         (length documents)))
        (setq
         summary
         (plist-put
          summary :document_paths
          (delq
           nil
           (mapcar
            (lambda (document)
              (emacs-agent-runtime--approval-target runtime document))
            (append documents nil)))))))
    (plist-put
     summary :risk
     (cond
      ((equal operation "document_delete")
       "Deletes a document")
      ((equal operation "document_move")
       "Moves a document")
      ((equal operation "changeset_rollback")
       "Restores prior contents and may overwrite current files")
      ((string-match-p "format" (format "%s" operation))
       "Runs a configured formatter and changes buffer contents")
      (t
       "Performs a protected editor mutation")))))

(defun emacs-agent-runtime--copy-approval-value (value)
  "Return a recursive private copy of approval VALUE."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key item)
         (puthash
          key
          (emacs-agent-runtime--copy-approval-value item)
          copy))
       value)
      copy))
   ((vectorp value)
    (vconcat
     (mapcar #'emacs-agent-runtime--copy-approval-value value)))
   ((consp value)
    (cons
     (emacs-agent-runtime--copy-approval-value (car value))
     (emacs-agent-runtime--copy-approval-value (cdr value))))
   (t value)))

(defun emacs-agent-runtime-request-approval
    (runtime operation arguments credential)
  "Create a pending approval for OPERATION and ARGUMENTS in RUNTIME."
  (let* ((id (emacs-agent-runtime--random-id "approval"))
         (now (float-time))
         (approval
          (emacs-agent-runtime-approval--make
           :id id
           :operation operation
           :operation-digest
           (emacs-agent-runtime-approval-digest operation arguments)
           :credential credential
           :created-at now
           :expires-at (+ now emacs-agent-runtime-approval-lifetime)
           :state 'pending
           :summary
           (emacs-agent-runtime--approval-summary
            runtime operation arguments)
           :revision-bindings
           (emacs-agent-runtime--approval-revision-bindings
            runtime arguments)
           :arguments
           (emacs-agent-runtime--copy-approval-value arguments))))
    (puthash
     id approval
     (emacs-agent-runtime-approval-registry runtime))
    (emacs-agent-runtime-record-activity
     runtime
     (list
      :tool operation
      :status "approval_required"
      :approval_request_id id))
    (list
     :approval_request_id id
     :operation_digest
     (emacs-agent-runtime-approval-digest operation arguments)
     :expires_at (emacs-agent-runtime-approval-expires-at approval))))

(defun emacs-agent-runtime--approval (runtime id)
  "Return approval ID from RUNTIME or signal."
  (or
   (gethash id (emacs-agent-runtime-approval-registry runtime))
   (signal 'emacs-agent-runtime-approval-error
           (list "Unknown approval request"))))

(defun emacs-agent-runtime--approval-record-transition
    (runtime approval status &optional reason)
  "Audit APPROVAL transition to STATUS in RUNTIME, optionally for REASON."
  (emacs-agent-runtime-record-activity
   runtime
   (append
    (list
     :tool (emacs-agent-runtime-approval-operation approval)
     :status (symbol-name status)
     :approval_request_id (emacs-agent-runtime-approval-id approval))
    (when reason
      (list :reason reason)))))

(defun emacs-agent-runtime--approval-revisions-current-p
    (runtime approval)
  "Return non-nil when APPROVAL revision bindings still match RUNTIME."
  (or
   (null (emacs-agent-runtime-approval-revision-bindings approval))
   (not (and
         (fboundp 'emacs-agent-document-revision-for-target)
         (fboundp 'emacs-agent-document-status)))
   (cl-every
    (lambda (binding)
      (condition-case nil
          (if (cdr binding)
              (equal
               (cdr binding)
               (funcall
                #'emacs-agent-document-revision-for-target
                runtime (car binding)))
            (not
             (plist-get
              (funcall
               #'emacs-agent-document-status
               runtime (car binding))
              :exists_on_disk)))
        (error nil)))
    (emacs-agent-runtime-approval-revision-bindings approval))))

(defun emacs-agent-runtime--refresh-approval (runtime approval)
  "Refresh TTL and revision state for APPROVAL in RUNTIME."
  (when (memq (emacs-agent-runtime-approval-state approval)
              '(pending approved))
    (cond
     ((<= (emacs-agent-runtime-approval-expires-at approval)
          (float-time))
      (setf (emacs-agent-runtime-approval-state approval) 'expired)
      (emacs-agent-runtime--approval-record-transition
       runtime approval 'expired 'ttl))
     ((not
       (emacs-agent-runtime--approval-revisions-current-p
        runtime approval))
      (setf
       (emacs-agent-runtime-approval-state approval)
       'invalidated)
      (emacs-agent-runtime--approval-record-transition
       runtime approval 'invalidated 'revision_changed))))
  approval)

(defun emacs-agent-runtime--approval-partial-supported-p (approval)
  "Return whether APPROVAL supports safe per-document acceptance."
  (and
   (equal
    (emacs-agent-runtime-approval-operation approval)
    "editor_checkpoint")
   (> (or
       (plist-get
        (emacs-agent-runtime-approval-summary approval)
        :document_count)
       0)
      1)))

;;;###autoload
(defun emacs-agent-runtime-approval-status (runtime id)
  "Return a credential-free public status for approval ID in RUNTIME."
  (let* ((approval
          (emacs-agent-runtime--refresh-approval
           runtime
           (emacs-agent-runtime--approval runtime id)))
         (remaining
          (max
           0.0
           (- (emacs-agent-runtime-approval-expires-at approval)
              (float-time)))))
    (append
     (list
      :approval_request_id (emacs-agent-runtime-approval-id approval)
      :operation (emacs-agent-runtime-approval-operation approval)
      :operation_digest
      (emacs-agent-runtime-approval-operation-digest approval)
      :status (emacs-agent-runtime-approval-state approval)
      :created_at (emacs-agent-runtime-approval-created-at approval)
      :expires_at (emacs-agent-runtime-approval-expires-at approval)
      :ttl_remaining remaining
      :partial_accept_supported
      (and
       (memq
        (emacs-agent-runtime-approval-state approval)
        '(pending approved))
       (emacs-agent-runtime--approval-partial-supported-p approval))
      :partial_accept_granularity
      (and
       (emacs-agent-runtime--approval-partial-supported-p approval)
       'document)
      :parent_approval_request_id
      (emacs-agent-runtime-approval-parent-id approval)
      :derived_approval_request_id
      (emacs-agent-runtime-approval-derived-id approval)
      :accepted_paths
      (copy-sequence
       (or (emacs-agent-runtime-approval-accepted-paths approval) nil)))
     (copy-tree (emacs-agent-runtime-approval-summary approval)))))

;;;###autoload
(defun emacs-agent-runtime-approval-list (&optional runtime)
  "Return safe approval statuses for RUNTIME, newest first."
  (let ((runtime (or runtime (emacs-agent-runtime-current)))
        statuses)
    (maphash
     (lambda (id _approval)
       (push
        (emacs-agent-runtime-approval-status runtime id)
        statuses))
     (emacs-agent-runtime-approval-registry runtime))
    (sort
     statuses
     (lambda (left right)
       (> (plist-get left :created_at)
          (plist-get right :created_at))))))

(defun emacs-agent-runtime--approval-set-documents
    (arguments documents)
  "Return copied ARGUMENTS whose documents field is DOCUMENTS."
  (let ((copy
         (emacs-agent-runtime--copy-approval-value arguments)))
    (cond
     ((hash-table-p copy)
      (let ((missing (make-symbol "missing")))
        (cond
         ((not (eq missing (gethash 'documents copy missing)))
          (puthash 'documents documents copy))
         ((not (eq missing (gethash "documents" copy missing)))
          (puthash "documents" documents copy))
         (t
          (puthash :documents documents copy)))))
     ((and (listp copy) (keywordp (car copy)))
      (setq copy (plist-put copy :documents documents)))
     ((listp copy)
      (let ((key
             (if (assoc "documents" copy)
                 "documents"
               'documents)))
        (setq
         copy
         (cons
          (cons key documents)
          (if (stringp key)
              (assoc-delete-all key copy)
            (assq-delete-all key copy)))))))
    copy))

(defun emacs-agent-runtime--approval-selected-target
    (documents targets selected)
  "Resolve SELECTED against canonical TARGETS for DOCUMENTS.
An original direct absolute path remains a supported selection value."
  (or
   (and (member selected targets) selected)
   (let (matches)
     (cl-mapc
      (lambda (document target)
        (let ((path
               (emacs-agent-runtime--approval-field document 'path)))
          (when
              (and
               (stringp path)
               (file-name-absolute-p path)
               (equal path selected))
            (push target matches))))
      documents targets)
     (and (= (length matches) 1)
          (car matches)))))

;;;###autoload
(defun emacs-agent-runtime-approval-partial
    (runtime id selected-paths)
  "Partially accept approval ID for SELECTED-PATHS in RUNTIME.
SELECTED-PATHS normally contains canonical paths returned by approval status;
original direct absolute document paths remain accepted."
  (let* ((approval
          (emacs-agent-runtime--refresh-approval
           runtime
           (emacs-agent-runtime--approval runtime id)))
         (status (emacs-agent-runtime-approval-state approval))
         (arguments (emacs-agent-runtime-approval-arguments approval))
         (documents
          (append
           (emacs-agent-runtime--approval-field
            arguments 'documents)
           nil))
         (targets
          (mapcar
           (lambda (document)
             (emacs-agent-runtime--approval-target runtime document))
           documents))
         (requested-selection
          (cond
           ((vectorp selected-paths)
            (append selected-paths nil))
           ((listp selected-paths)
            selected-paths)))
         (selection
          (and
           requested-selection
           (mapcar
            (lambda (selected)
              (and
               (stringp selected)
               (emacs-agent-runtime--approval-selected-target
                documents targets selected)))
            requested-selection)))
         selected-documents)
    (unless
        (and
         (memq status '(pending approved))
         (emacs-agent-runtime--approval-partial-supported-p approval))
      (signal
       'emacs-agent-runtime-approval-error
       (list "Approval request does not support partial acceptance")))
    (unless
        (and
         selection
         (cl-every #'stringp selection)
         (= (length selection)
            (length (delete-dups (copy-sequence selection))))
         (cl-every #'stringp targets)
         (= (length targets)
            (length (delete-dups (copy-sequence targets))))
         (cl-every
          (lambda (target)
            (member target targets))
          selection)
         (< (length selection) (length targets)))
      (signal
       'emacs-agent-runtime-approval-error
       (list "Select a unique, non-empty proper subset of documents")))
    (cl-mapc
     (lambda (document target)
       (when (member target selection)
         (push document selected-documents)))
     documents targets)
    (setq selected-documents (nreverse selected-documents))
    (let* ((narrowed
            (emacs-agent-runtime--approval-set-documents
             arguments
             (if
                 (vectorp
                  (emacs-agent-runtime--approval-field
                   arguments 'documents))
                 (vconcat selected-documents)
               selected-documents)))
           (child-request
            (emacs-agent-runtime-request-approval
             runtime
             (emacs-agent-runtime-approval-operation approval)
             narrowed
             (emacs-agent-runtime-approval-credential approval)))
           (child-id
            (plist-get child-request :approval_request_id))
           (child
            (emacs-agent-runtime--approval runtime child-id)))
      (setf
       (emacs-agent-runtime-approval-parent-id child) id
       (emacs-agent-runtime-approval-expires-at child)
       (min
        (emacs-agent-runtime-approval-expires-at child)
        (emacs-agent-runtime-approval-expires-at approval)))
      (emacs-agent-runtime-approve runtime child-id)
      (setf
       (emacs-agent-runtime-approval-state approval) 'partially_approved
       (emacs-agent-runtime-approval-derived-id approval) child-id
       (emacs-agent-runtime-approval-accepted-paths approval)
       (copy-sequence selection))
      (emacs-agent-runtime-record-activity
       runtime
       (list
        :tool (emacs-agent-runtime-approval-operation approval)
        :status "partially_approved"
        :approval_request_id id
        :derived_approval_request_id child-id
        :paths (copy-sequence selection)))
      (append
       (emacs-agent-runtime-approval-status runtime id)
       (list
        :derived_approval
        (emacs-agent-runtime-approval-status runtime child-id))))))

;;;###autoload
(defun emacs-agent-runtime-approval-cancel (runtime id)
  "Cancel pending or approved approval ID in RUNTIME."
  (let ((approval
         (emacs-agent-runtime--refresh-approval
          runtime
          (emacs-agent-runtime--approval runtime id))))
    (unless
        (memq
         (emacs-agent-runtime-approval-state approval)
         '(pending approved))
      (signal
       'emacs-agent-runtime-approval-error
       (list "Approval request cannot be cancelled")))
    (setf (emacs-agent-runtime-approval-state approval) 'cancelled)
    (emacs-agent-runtime--approval-record-transition
     runtime approval 'cancelled)
    (emacs-agent-runtime-approval-status runtime id)))

(defun emacs-agent-runtime-approve (runtime id)
  "Approve pending request ID in RUNTIME."
  (let ((approval
         (emacs-agent-runtime--refresh-approval
          runtime
          (emacs-agent-runtime--approval runtime id))))
    (unless (eq (emacs-agent-runtime-approval-state approval) 'pending)
      (signal
       'emacs-agent-runtime-approval-error
       (list "Approval request is not pending")))
    (setf (emacs-agent-runtime-approval-state approval) 'approved)
    (emacs-agent-runtime--approval-record-transition
     runtime approval 'approved)
    t))

(defun emacs-agent-runtime-reject (runtime id)
  "Reject pending request ID in RUNTIME."
  (let ((approval
         (emacs-agent-runtime--refresh-approval
          runtime
          (emacs-agent-runtime--approval runtime id))))
    (unless
        (memq
         (emacs-agent-runtime-approval-state approval)
         '(pending approved))
      (signal
       'emacs-agent-runtime-approval-error
       (list "Approval request cannot be rejected")))
    (setf (emacs-agent-runtime-approval-state approval) 'rejected)
    (emacs-agent-runtime--approval-record-transition
     runtime approval 'rejected)
    t))

(defun emacs-agent-runtime-consume-approval
    (runtime id operation arguments credential)
  "Consume approval ID in RUNTIME if it exactly authorizes this request."
  (let ((approval
         (emacs-agent-runtime--refresh-approval
          runtime
          (emacs-agent-runtime--approval runtime id))))
    (unless (eq (emacs-agent-runtime-approval-state approval) 'approved)
      (signal
       'emacs-agent-runtime-approval-error
       (list "Approval request has not been approved")))
    (unless
        (and
         (equal credential
                (emacs-agent-runtime-approval-credential approval))
         (equal operation
                (emacs-agent-runtime-approval-operation approval))
         (equal
          (emacs-agent-runtime-approval-digest
           operation arguments)
          (emacs-agent-runtime-approval-operation-digest approval)))
      (signal
       'emacs-agent-runtime-approval-error
       (list "Approval does not match this request")))
    (setf (emacs-agent-runtime-approval-state approval) 'consumed)
    (emacs-agent-runtime--approval-record-transition
     runtime approval 'consumed)
    t))

(provide 'emacs-agent-runtime)
;;; emacs-agent-runtime.el ends here
