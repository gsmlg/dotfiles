;;; emacs-agent-editor.el --- Buffer-first HTTP MCP editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Gao

;; Author: Gao
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience

;;; Commentary:
;; Expose one Emacs workspace through a guarded, buffer-first HTTP MCP server.
;; The package is deliberately stopped when loaded.  Call
;; `emacs-agent-editor-start', or use the daemon integration in this repo.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'emacs-agent-request)
(require 'emacs-agent-session)
(require 'emacs-agent-protocol)
(require 'emacs-agent-http)
(require 'emacs-agent-policy)
(require 'emacs-agent-document)
(require 'emacs-agent-edit)
(require 'emacs-agent-workspace)
(require 'emacs-agent-search)
(require 'emacs-agent-changeset)
(require 'emacs-agent-journal)
(require 'emacs-agent-ui)

(defgroup emacs-agent-editor nil
  "Buffer-first editor capabilities exposed over HTTP MCP."
  :group 'tools
  :prefix "emacs-agent-editor-")

(defcustom emacs-agent-editor-host "127.0.0.1"
  "Address on which the MCP listener binds."
  :type 'string)

(defcustom emacs-agent-editor-port 0
  "TCP port on which the MCP listener binds.
A value of zero asks the operating system to choose an available port."
  :type 'natnum)

(defcustom emacs-agent-editor-endpoint "/mcp"
  "HTTP endpoint served by Agent Editor MCP."
  :type 'string)

(defcustom emacs-agent-editor-allowed-origins nil
  "Origins permitted to access the MCP endpoint.
An absent Origin header is permitted.  A present Origin is rejected unless it
is a member of this list."
  :type '(repeat string))

(defcustom emacs-agent-editor-state-directory
  (expand-file-name
   "emacs-agent-editor/"
   (or (getenv "XDG_STATE_HOME")
       (expand-file-name ".local/state/" "~")))
  "Directory used for private per-daemon runtime state."
  :type 'directory)

(defcustom emacs-agent-editor-access-mode 'autonomous
  "Default workspace access mode."
  :type '(choice (const read-only) (const review) (const autonomous)))

(defcustom emacs-agent-editor-save-policy 'immediate
  "Default workspace save policy."
  :type '(choice (const immediate) (const manual) (const explicit-per-call)))

(defcustom emacs-agent-editor-bearer-token nil
  "Explicit bearer token, or nil to generate a new token at server start."
  :type '(choice (const :tag "Generate token" nil) string))

(defvar emacs-agent-editor--http-server nil
  "Active Agent Editor HTTP server.")

(defvar emacs-agent-editor--workspace nil
  "Workspace bound to the active server.")

(defvar emacs-agent-editor--token nil
  "Bearer token accepted by the active server.")

(defvar emacs-agent-editor--connection-file nil
  "Connection metadata file for the active server.")

(defvar emacs-agent-editor--request-context nil
  "Dynamically bound request context for change-set attribution.")

(defvar emacs-agent-editor--diff-cursors (make-hash-table :test #'equal)
  "Opaque pagination cursors for diff results.")

(defun emacs-agent-editor--daemon-name ()
  "Return a filesystem-safe name for this Emacs instance."
  (replace-regexp-in-string
   "[^[:alnum:]_.-]" "_"
   (let ((name (daemonp)))
     (if (stringp name) name "interactive"))))

(defun emacs-agent-editor--random-token ()
  "Return a URL-safe bearer token using operating-system entropy."
  (condition-case error-data
      (funcall emacs-agent-session-id-function)
    (error
     (user-error
      "Secure entropy unavailable; customize `emacs-agent-editor-bearer-token': %s"
      (error-message-string error-data)))))

(defun emacs-agent-editor--instance-state-directory ()
  "Return the private state directory for the current Emacs instance."
  (expand-file-name
   (file-name-as-directory (emacs-agent-editor--daemon-name))
   emacs-agent-editor-state-directory))

(defun emacs-agent-editor--write-connection-file (workspace port token)
  "Publish private connection metadata for WORKSPACE, PORT, and TOKEN."
  (let* ((directory (emacs-agent-editor--instance-state-directory))
         (target (expand-file-name "connection.json" directory))
         (temporary nil)
         (metadata
          `((schema_version . 1)
            (daemon . ,(emacs-agent-editor--daemon-name))
            (pid . ,(emacs-pid))
            (workspace . ,(emacs-agent-workspace-root workspace))
            (endpoint . ,(format "http://%s:%d%s"
                                 emacs-agent-editor-host
                                 port
                                 emacs-agent-editor-endpoint))
            (token . ,token)
            (protocol_versions . ["2026-07-28" "2025-11-25"])
            (started_at . ,(format-time-string "%FT%TZ" nil t)))))
    (make-directory directory t)
    (set-file-modes directory #o700)
    (setq temporary (make-temp-file (expand-file-name ".connection-" directory)))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (insert (json-serialize metadata))
            (insert "\n"))
          (set-file-modes temporary #o600)
          (rename-file temporary target t)
          (setq temporary nil)
          (setq emacs-agent-editor--connection-file target))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun emacs-agent-editor--remove-connection-file ()
  "Remove published connection metadata, when present."
  (when (and emacs-agent-editor--connection-file
             (file-exists-p emacs-agent-editor--connection-file))
    (delete-file emacs-agent-editor--connection-file))
  (setq emacs-agent-editor--connection-file nil))

(defun emacs-agent-editor-running-p ()
  "Return non-nil when the Agent Editor HTTP server is active."
  (and emacs-agent-editor--http-server t))

(defun emacs-agent-editor-status ()
  "Display and return the current Agent Editor MCP status."
  (interactive)
  (let ((status
         (if (emacs-agent-editor-running-p)
             (format "running for %s (%s)"
                     (emacs-agent-workspace-root emacs-agent-editor--workspace)
                     emacs-agent-editor--connection-file)
           "stopped")))
    (when (called-interactively-p 'interactive)
      (message "Agent Editor MCP: %s" status))
    status))

(defun emacs-agent-editor--json-key (keyword)
  "Convert plist KEYWORD into a JSON alist key."
  (intern (string-remove-prefix ":" (symbol-name keyword))))

(defun emacs-agent-editor--json-value (value)
  "Convert internal VALUE into a `json-serialize' compatible value."
  (cond
   ((or (eq value t) (eq value :false) (stringp value)
        (numberp value) (vectorp value))
    value)
   ((null value) :false)
   ((and (symbolp value) (not (keywordp value)))
    (symbol-name value))
   ((and (listp value) (keywordp (car value)))
    (emacs-agent-editor--plist-to-alist value))
   ((and (listp value)
         (cl-every (lambda (entry)
                     (and (consp entry) (symbolp (car entry))))
                   value))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (emacs-agent-editor--json-value (cdr entry))))
            value))
   ((listp value)
    (vconcat (mapcar #'emacs-agent-editor--json-value value)))
   (t (format "%s" value))))

(defun emacs-agent-editor--plist-to-alist (plist)
  "Convert PLIST recursively to a JSON-compatible alist."
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (push (cons (emacs-agent-editor--json-key key)
                    (emacs-agent-editor--json-value value))
              result)))
    (nreverse result)))

(defun emacs-agent-editor--tool-error (code &rest details)
  "Signal a structured tool error with CODE and DETAILS."
  (signal 'emacs-agent-tool-error
          (list
           (append `((code . ,(if (symbolp code)
                                  (symbol-name code)
                                code)))
                   (emacs-agent-editor--plist-to-alist details)))))

(defun emacs-agent-editor--call (function)
  "Call FUNCTION and translate editor conditions into tool errors."
  (condition-case error-data
      (funcall function)
    (emacs-agent-error
     (let ((code (emacs-agent-error-code error-data))
           (details (emacs-agent-error-details error-data)))
       (apply #'emacs-agent-editor--tool-error code details)))
    (emacs-agent-workspace-paused
     (emacs-agent-editor--tool-error
      'workspace_paused :message (error-message-string error-data)))
    (emacs-agent-approval-error
     (emacs-agent-editor--tool-error
      'approval_required :message (error-message-string error-data)))
    (emacs-agent-rollback-conflict
     (apply
      #'emacs-agent-editor--tool-error
      'rollback_conflict
      :message (error-message-string error-data)
      (cddr error-data)))
    (emacs-agent-changeset-error
     (emacs-agent-editor--tool-error
      'changeset_error :message (error-message-string error-data)))
    (emacs-agent-search-error
     (emacs-agent-editor--tool-error
      'search_failed :message (error-message-string error-data)))
    (file-error
     (emacs-agent-editor--tool-error
      'filesystem_error :message (error-message-string error-data)))))

(defun emacs-agent-editor--checkpoint-p (workspace requested)
  "Return whether WORKSPACE should checkpoint a REQUESTED mutation."
  (pcase (emacs-agent-workspace-save-policy workspace)
    ('immediate t)
    ('manual (eq requested t))
    ('explicit-per-call (eq requested t))
    (_ (eq requested t))))

(defun emacs-agent-editor--argument (arguments key)
  "Read KEY from tool ARGUMENTS."
  (alist-get key arguments))

(defun emacs-agent-editor--approval-arguments (arguments)
  "Return normalized ARGUMENTS without its approval identifier."
  (assq-delete-all 'approval_request_id (copy-tree arguments)))

(defun emacs-agent-editor--authorize (workspace operation arguments)
  "Authorize protected OPERATION with ARGUMENTS in WORKSPACE."
  (when (eq (emacs-agent-workspace-access-mode workspace) 'review)
    (let* ((approval-id
            (emacs-agent-editor--argument arguments 'approval_request_id))
           (normalized (emacs-agent-editor--approval-arguments arguments)))
      (if approval-id
          (emacs-agent-workspace-consume-approval
           workspace approval-id operation normalized emacs-agent-editor--token)
        (let ((approval
               (emacs-agent-workspace-request-approval
                workspace operation normalized emacs-agent-editor--token)))
          (apply #'emacs-agent-editor--tool-error
                 'approval_required approval))))))

(defun emacs-agent-editor--record-edit
    (document before _after previous-revision new-revision)
  "Record a change to DOCUMENT from BEFORE at PREVIOUS-REVISION.
NEW-REVISION identifies the resulting content."
  (let* ((workspace (emacs-agent-document-workspace document))
         (path (emacs-agent-document-relative-path document))
         (checkpointed
          (not (buffer-modified-p (emacs-agent-document-buffer document))))
         (changeset
          (emacs-agent-changeset-record
           workspace
           :request-id
           (and emacs-agent-editor--request-context
                (emacs-agent-request-id emacs-agent-editor--request-context))
           :agent-identity
           (and emacs-agent-editor--request-context
                (emacs-agent-request-client-info
                 emacs-agent-editor--request-context))
           :operations (list (list :type 'edit :path path))
           :touched-documents (list path)
           :base-revisions (list (cons path previous-revision))
           :final-revisions (list (cons path new-revision))
           :before-snapshots
           (list (cons path (list :exists t :content before)))
           :checkpoint-state checkpointed)))
    (setf (emacs-agent-document-last-changeset-id document)
          (emacs-agent-changeset-changeset-id changeset))
    (emacs-agent-journal-write
     workspace
     (list :tool "document_apply_edits" :status "completed"
           :changeset_id (emacs-agent-changeset-changeset-id changeset)
           :paths (list path)))
    (emacs-agent-changeset-changeset-id changeset)))

(defun emacs-agent-editor--workspace-info (_arguments _context)
  "Implement `emacs_agent_workspace_info'."
  (let ((workspace (emacs-agent-workspace-current)))
    `((workspace_id . ,(emacs-agent-workspace-workspace-id workspace))
      (root . ,(emacs-agent-workspace-root workspace))
      (access_mode
       . ,(symbol-name (emacs-agent-workspace-access-mode workspace)))
      (save_policy
       . ,(symbol-name (emacs-agent-workspace-save-policy workspace)))
      (paused . ,(if (emacs-agent-workspace-paused-p workspace) t :false))
      (health
       . ,(symbol-name (emacs-agent-workspace-health-state workspace)))
      (protocol_versions . ["2026-07-28" "2025-11-25"])
      (capabilities
       . ["read" "edit" "create" "files" "search" "move" "delete"
          "checkpoint" "sync" "diff" "rollback"]))))

(defun emacs-agent-editor--document-read (arguments _context)
  "Implement `emacs_agent_document_read' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let ((result
            (emacs-agent-document-read
             (emacs-agent-workspace-current)
             (emacs-agent-editor--argument arguments 'path)
             (emacs-agent-editor--argument arguments 'start_line)
             (emacs-agent-editor--argument arguments 'end_line)
             (emacs-agent-editor--argument arguments 'max_chars)
             (emacs-agent-editor--argument arguments 'cursor))))
       (emacs-agent-editor--plist-to-alist result)))))

(defun emacs-agent-editor--document-apply-edits (arguments context)
  "Implement `emacs_agent_document_apply_edits' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (checkpoint
             (emacs-agent-editor--checkpoint-p
              workspace
              (emacs-agent-editor--argument arguments 'checkpoint)))
            (emacs-agent-editor--request-context context)
            (result
             (emacs-agent-workspace-enqueue-mutation
              workspace
              (lambda ()
                (emacs-agent-edit-apply
                 workspace
                 (emacs-agent-editor--argument arguments 'path)
                 (emacs-agent-editor--argument arguments 'expected_revision)
                 (emacs-agent-editor--argument arguments 'edits)
                 checkpoint)))))
       (emacs-agent-editor--plist-to-alist
        (cl-loop for (key value) on result by #'cddr
                 unless (memq key '(:before_content :after_content))
                 append (list key value)))))))

(defun emacs-agent-editor--record-lifecycle
    (workspace operation paths before base final checkpointed)
  "Record a lifecycle OPERATION in WORKSPACE."
  (let ((changeset
         (emacs-agent-changeset-record
          workspace
          :request-id
          (and emacs-agent-editor--request-context
               (emacs-agent-request-id emacs-agent-editor--request-context))
          :operations (list operation)
          :touched-documents paths
          :base-revisions base
          :final-revisions final
          :before-snapshots before
          :checkpoint-state checkpointed)))
    (emacs-agent-journal-write
     workspace
     (list :tool (plist-get operation :type)
           :status "completed"
           :changeset_id (emacs-agent-changeset-changeset-id changeset)
           :paths paths))
    (emacs-agent-changeset-changeset-id changeset)))

(defun emacs-agent-editor--document-create (arguments context)
  "Implement `emacs_agent_document_create' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (path (emacs-agent-editor--argument arguments 'path))
            (content (emacs-agent-editor--argument arguments 'content))
            (checkpoint
             (emacs-agent-editor--checkpoint-p
              workspace
              (emacs-agent-editor--argument arguments 'checkpoint)))
            (absolute (emacs-agent-policy-resolve workspace path t))
            (emacs-agent-editor--request-context context))
       (when (string-match-p (string 0) content)
         (emacs-agent-editor--tool-error
          'unsupported_document_type :path path :reason 'binary))
       (when (> (string-bytes content)
                emacs-agent-policy-maximum-document-bytes)
         (emacs-agent-editor--tool-error 'document_too_large :path path))
       (when (file-exists-p absolute)
         (emacs-agent-editor--tool-error
          'document_already_exists :path path))
       (emacs-agent-workspace-enqueue-mutation
        workspace
        (lambda ()
          (let* ((checked-absolute
                  (emacs-agent-policy-resolve workspace path t))
                 (_
                  (when (file-exists-p checked-absolute)
                    (emacs-agent-editor--tool-error
                     'document_already_exists :path path)))
                 (document (emacs-agent-document-open workspace path t))
                 (buffer (emacs-agent-document-buffer document)))
            (condition-case error-data
                (with-current-buffer buffer
                  (atomic-change-group
                    (erase-buffer)
                    (insert content)
                    (when checkpoint
                      (emacs-agent-policy-resolve workspace path t)
                      (save-buffer))))
              (error
               (when checkpoint
                 (setf (emacs-agent-document-degraded document) t
                       (emacs-agent-workspace-health-state workspace)
                       'degraded))
               (emacs-agent-editor--tool-error
                'save_failed :path path
                :message (error-message-string error-data)
                :reconciliation_required t
                :filesystem_rollback_guaranteed nil)))
            (let* ((revision (emacs-agent-document-revision document))
                   (changeset-id
                    (emacs-agent-editor--record-lifecycle
                     workspace
                     (list :type 'create :path path)
                     (list path)
                     (list (cons path (list :exists nil)))
                     nil
                     (list (cons path revision))
                     checkpoint)))
              `((path . ,path)
                (changeset_id . ,changeset-id)
                (new_revision . ,revision)
                (checkpointed
                 . ,(if checkpoint t :false)))))))))))

(defun emacs-agent-editor--workspace-files (arguments _context)
  "Implement `emacs_agent_workspace_files' for ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((result
             (emacs-agent-workspace-files
              (emacs-agent-workspace-current)
              :include-globs
              (emacs-agent-editor--argument arguments 'include_globs)
              :exclude-globs
              (emacs-agent-editor--argument arguments 'exclude_globs)
              :max-results
              (emacs-agent-editor--argument arguments 'max_results)
              :cursor (emacs-agent-editor--argument arguments 'cursor)))
            (items (plist-get result :results)))
       `((files . ,(vconcat items))
         (result_count . ,(plist-get result :result_count))
         ,@(when-let* ((cursor (plist-get result :next_cursor)))
             `((cursor . ,cursor))))))))

(defun emacs-agent-editor--workspace-search (arguments context)
  "Implement `emacs_agent_workspace_search' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            done result error-data process)
       (if (emacs-agent-editor--argument arguments 'cursor)
           (setq result
                 (emacs-agent-workspace-search
                  workspace
                  (emacs-agent-editor--argument arguments 'query)
                  :regexp
                  (eq (emacs-agent-editor--argument arguments 'regexp) t)
                  :include-globs
                  (emacs-agent-editor--argument arguments 'include_globs)
                  :exclude-globs
                  (emacs-agent-editor--argument arguments 'exclude_globs)
                  :max-results
                  (emacs-agent-editor--argument arguments 'max_results)
                  :cursor
                  (emacs-agent-editor--argument arguments 'cursor)))
         (setq process
               (emacs-agent-workspace-search
                workspace
                (emacs-agent-editor--argument arguments 'query)
                :regexp
                (eq (emacs-agent-editor--argument arguments 'regexp) t)
                :include-globs
                (emacs-agent-editor--argument arguments 'include_globs)
                :exclude-globs
                (emacs-agent-editor--argument arguments 'exclude_globs)
                :max-results
                (emacs-agent-editor--argument arguments 'max_results)
                :callback
                (lambda (value error)
                  (setq result value error-data error done t))))
         (when (processp process)
           (emacs-agent-request-on-cancel
            context (lambda () (emacs-agent-search-cancel process)))
           (while (and (not done) (process-live-p process))
             (accept-process-output process 0.05)))
         (when (eq (emacs-agent-request-state context) 'cancelled)
           (emacs-agent-editor--tool-error 'operation_cancelled))
         (when error-data (signal (car error-data) (cdr error-data))))
       (let ((items
              (mapcar #'emacs-agent-editor--plist-to-alist
                      (plist-get result :results))))
         `((results . ,(vconcat items))
           (result_count . ,(plist-get result :result_count))
           ,@(when-let* ((cursor (plist-get result :next_cursor)))
               `((cursor . ,cursor)))))))))

(defun emacs-agent-editor--require-lifecycle-checkpoint
    (workspace _arguments)
  "Require lifecycle checkpoint authorization in WORKSPACE."
  (unless (emacs-agent-editor--checkpoint-p workspace t)
    (emacs-agent-editor--tool-error 'checkpoint_required))
  t)

(defun emacs-agent-editor--document-move (arguments context)
  "Implement `emacs_agent_document_move' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (path (emacs-agent-editor--argument arguments 'path))
            (new-path (emacs-agent-editor--argument arguments 'new_path))
            (document (emacs-agent-document-open workspace path))
            (old-absolute (emacs-agent-document-canonical-path document))
            (new-absolute
             (emacs-agent-policy-resolve workspace new-path t))
            (registry (emacs-agent-workspace-document-registry workspace))
            (emacs-agent-editor--request-context context))
       (emacs-agent-editor--authorize workspace "document_move" arguments)
       (emacs-agent-editor--require-lifecycle-checkpoint workspace arguments)
       (emacs-agent-document-reconcile document)
       (when (file-exists-p new-absolute)
         (emacs-agent-editor--tool-error
          'document_already_exists :path new-path))
       (unless
           (equal (emacs-agent-document-revision document)
                  (emacs-agent-editor--argument arguments 'expected_revision))
         (emacs-agent-editor--tool-error
          'revision_conflict :path path :requires_reread t))
       (let* ((buffer (emacs-agent-document-buffer document))
              (before
               (with-current-buffer buffer
                 (save-restriction
                   (widen)
                   (buffer-substring-no-properties
                    (point-min) (point-max)))))
              (base (emacs-agent-document-revision document)))
         (emacs-agent-workspace-enqueue-mutation
          workspace
          (lambda ()
            (setq old-absolute
                  (emacs-agent-policy-resolve workspace path))
            (setq new-absolute
                  (emacs-agent-policy-resolve workspace new-path t))
            (when (file-exists-p new-absolute)
              (emacs-agent-editor--tool-error
               'document_already_exists :path new-path))
            (emacs-agent-document-reconcile document)
            (unless (equal base (emacs-agent-document-revision document))
              (emacs-agent-editor--tool-error
               'revision_conflict :path path :requires_reread t))
            (condition-case error-data
                (with-current-buffer buffer
                  (save-buffer))
              (error
               (setf (emacs-agent-document-degraded document) t
                     (emacs-agent-workspace-health-state workspace)
                     'degraded)
               (emacs-agent-editor--tool-error
                'save_failed :path path
                :message (error-message-string error-data)
                :reconciliation_required t
                :filesystem_rollback_guaranteed nil)))
            (setq old-absolute
                  (emacs-agent-policy-resolve workspace path))
            (setq new-absolute
                  (emacs-agent-policy-resolve workspace new-path t))
            (when (file-exists-p new-absolute)
              (emacs-agent-editor--tool-error
               'document_already_exists :path new-path))
            (rename-file old-absolute new-absolute)
            (with-current-buffer buffer
              (set-visited-file-name new-absolute t t)
              (set-buffer-modified-p nil))
            (remhash old-absolute registry)
            (setf (emacs-agent-document-relative-path document) new-path
                  (emacs-agent-document-canonical-path document) new-absolute
                  (emacs-agent-document-disk-fingerprint document)
                  (emacs-agent-document--disk-fingerprint new-absolute))
            (puthash new-absolute document registry)
            (let* ((revision (emacs-agent-document-revision document))
                   (changeset-id
                    (emacs-agent-editor--record-lifecycle
                     workspace
                     (list :type 'move :from path :to new-path)
                     (list path new-path)
                     (list (cons path (list :exists t :content before))
                           (cons new-path (list :exists nil)))
                     (list (cons path base))
                     (list (cons path nil) (cons new-path revision))
                     t)))
              `((path . ,path) (new_path . ,new-path)
                (changeset_id . ,changeset-id)
                (new_revision . ,revision)
                (checkpointed . t))))))))))

(defun emacs-agent-editor--document-delete (arguments context)
  "Implement `emacs_agent_document_delete' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (path (emacs-agent-editor--argument arguments 'path))
            (document (emacs-agent-document-open workspace path))
            (revision (emacs-agent-document-revision document))
            (emacs-agent-editor--request-context context))
       (emacs-agent-editor--authorize workspace "document_delete" arguments)
       (emacs-agent-editor--require-lifecycle-checkpoint workspace arguments)
       (emacs-agent-document-reconcile document)
       (setq revision (emacs-agent-document-revision document))
       (unless
           (equal revision
                  (emacs-agent-editor--argument arguments 'expected_revision))
         (emacs-agent-editor--tool-error
          'revision_conflict :path path :requires_reread t))
       (let* ((buffer (emacs-agent-document-buffer document))
              (absolute (emacs-agent-document-canonical-path document))
              (content
               (with-current-buffer buffer
                 (save-restriction
                   (widen)
                   (buffer-substring-no-properties
                    (point-min) (point-max))))))
         (emacs-agent-workspace-enqueue-mutation
          workspace
          (lambda ()
            (setq absolute
                  (emacs-agent-policy-resolve workspace path))
            (emacs-agent-document-reconcile document)
            (unless (equal revision
                           (emacs-agent-document-revision document))
              (emacs-agent-editor--tool-error
               'revision_conflict :path path :requires_reread t))
            (when (file-exists-p absolute) (delete-file absolute))
            (remhash absolute
                     (emacs-agent-workspace-document-registry workspace))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)
            (let ((changeset-id
                   (emacs-agent-editor--record-lifecycle
                    workspace
                    (list :type 'delete :path path)
                    (list path)
                    (list (cons path (list :exists t :content content)))
                    (list (cons path revision))
                    (list (cons path nil)) t)))
              `((path . ,path) (changeset_id . ,changeset-id)
                (deleted . t) (checkpointed . t))))))))))

(defun emacs-agent-editor--workspace-checkpoint (arguments context)
  "Implement `emacs_agent_workspace_checkpoint' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (documents
             (emacs-agent-editor--argument arguments 'documents))
            (emacs-agent-editor--request-context context))
       (emacs-agent-editor--authorize workspace "workspace_checkpoint" arguments)
       (let (resolved before-snapshots base-revisions)
         (dolist (entry documents)
           (let* ((path (alist-get 'path entry))
                  (document (emacs-agent-document-open workspace path))
                  (_ (emacs-agent-document-reconcile document))
                  (revision (emacs-agent-document-revision document)))
             (unless (equal revision (alist-get 'expected_revision entry))
               (emacs-agent-editor--tool-error
                'revision_conflict :path path :current_revision revision
                :requires_reread t))
             (push document resolved)
             (push (cons path revision) base-revisions)
             (push
              (cons
               path
               (list
                :exists t
                :content
                (with-current-buffer
                    (emacs-agent-document-buffer document)
                  (save-restriction
                    (widen)
                    (buffer-substring-no-properties
                     (point-min) (point-max))))))
              before-snapshots)))
         (setq resolved (nreverse resolved))
         (emacs-agent-workspace-enqueue-mutation
          workspace
          (lambda ()
            (let (results final-revisions saving-document)
              (condition-case error-data
                  (dolist (document resolved)
                    (setq saving-document document)
                    (emacs-agent-policy-resolve
                     workspace
                     (emacs-agent-document-relative-path document))
                    (emacs-agent-document-reconcile document)
                    (unless
                        (equal
                         (emacs-agent-document-revision document)
                         (cdr
                          (assoc
                           (emacs-agent-document-relative-path document)
                           base-revisions)))
                      (emacs-agent-editor--tool-error
                       'revision_conflict
                       :path
                       (emacs-agent-document-relative-path document)
                       :requires_reread t))
                    (with-current-buffer
                        (emacs-agent-document-buffer document)
                      (save-buffer))
                    (setf (emacs-agent-document-disk-fingerprint document)
                          (emacs-agent-document--disk-fingerprint
                           (emacs-agent-document-canonical-path document)))
                    (push
                     `((path
                        . ,(emacs-agent-document-relative-path document))
                       (revision
                        . ,(emacs-agent-document-revision document)))
                     results)
                    (push
                     (cons
                      (emacs-agent-document-relative-path document)
                      (emacs-agent-document-revision document))
                     final-revisions))
                (error
                 (setf (emacs-agent-workspace-health-state workspace)
                       'degraded)
                 (when saving-document
                   (setf (emacs-agent-document-degraded saving-document) t))
                 (emacs-agent-editor--tool-error
                  'save_failed
                  :message (error-message-string error-data)
                  :reconciliation_required t
                  :partial_completion (if results t :false)
                  :checkpointed
                  (vconcat (nreverse (copy-sequence results))))))
              (let ((changeset-id
                     (emacs-agent-editor--record-lifecycle
                      workspace
                      (list :type 'checkpoint)
                      (mapcar #'car base-revisions)
                      (nreverse before-snapshots)
                      (nreverse base-revisions)
                      (nreverse final-revisions)
                      t)))
                `((changeset_id . ,changeset-id)
                  (checkpointed . ,(vconcat (nreverse results)))))))))))))

(defun emacs-agent-editor--workspace-sync (arguments _context)
  "Implement `emacs_agent_workspace_sync' for ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (paths (emacs-agent-editor--argument arguments 'paths))
            (documents
             (if paths
                 (mapcar (lambda (path)
                           (emacs-agent-document-open workspace path))
                         paths)
               (let (items)
                 (maphash
                  (lambda (_path document) (push document items))
                  (emacs-agent-workspace-document-registry workspace))
                 items)))
            results)
       (emacs-agent-workspace-enqueue-mutation
        workspace
        (lambda ()
          (dolist (document documents)
            (condition-case error-data
                (progn
                  (emacs-agent-document-reconcile document)
                  (push
                   `((path . ,(emacs-agent-document-relative-path document))
                     (status . "synchronized")
                     (revision . ,(emacs-agent-document-revision document)))
                   results))
              (emacs-agent-error
               (push
                `((path . ,(emacs-agent-document-relative-path document))
                  (status . "conflicted")
                  (code
                   . ,(symbol-name (emacs-agent-error-code error-data))))
                results))))
          (when (cl-every
                 (lambda (entry)
                   (equal (alist-get 'status entry) "synchronized"))
                 results)
            (setf (emacs-agent-workspace-health-state workspace) 'healthy))
          `((documents . ,(vconcat (nreverse results))))))))))

(defun emacs-agent-editor--workspace-diff (arguments _context)
  "Implement `emacs_agent_workspace_diff' for ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (cursor (emacs-agent-editor--argument arguments 'cursor))
            (max-chars
             (or (emacs-agent-editor--argument arguments 'max_chars)
                 (* 256 1024)))
            state diff offset changeset-id)
       (if cursor
           (progn
             (setq state (gethash cursor emacs-agent-editor--diff-cursors))
             (unless (and state
                          (> (plist-get state :expires-at) (float-time)))
               (emacs-agent-editor--tool-error 'invalid_cursor))
             (setq diff (plist-get state :diff)
                   offset (plist-get state :offset)
                   changeset-id (plist-get state :changeset-id))
             (remhash cursor emacs-agent-editor--diff-cursors))
         (setq changeset-id
               (emacs-agent-editor--argument arguments 'changeset_id)
               diff (emacs-agent-changeset-diff workspace changeset-id)
               offset 0))
       (let* ((end (min (length diff) (+ offset max-chars)))
              (truncated (< end (length diff)))
              next)
         (when truncated
           (setq next (concat "diff_" (emacs-agent-editor--random-token)))
           (puthash
            next
            (list :diff diff :offset end :changeset-id changeset-id
                  :expires-at (+ (float-time) 300))
            emacs-agent-editor--diff-cursors))
         `((changeset_id . ,(or changeset-id "all"))
           (content . ,(substring diff offset end))
           (truncated . ,(if truncated t :false))
           ,@(when next `((cursor . ,next)))))))))

(defun emacs-agent-editor--changeset-rollback (arguments _context)
  "Implement `emacs_agent_changeset_rollback' for ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let ((workspace (emacs-agent-workspace-current))
           (changeset-id
            (emacs-agent-editor--argument arguments 'changeset_id)))
       (emacs-agent-editor--authorize workspace "changeset_rollback" arguments)
       (let ((changeset
              (emacs-agent-changeset-rollback workspace changeset-id)))
         `((changeset_id . ,changeset-id)
           (status
            . ,(symbol-name
                (emacs-agent-changeset-status changeset)))))))))

(defun emacs-agent-editor--object-schema (properties &optional required)
  "Return an object schema with PROPERTIES and REQUIRED names."
  `((type . "object")
    (properties . ,properties)
    (additionalProperties . :false)
    ,@(when required `((required . ,(vconcat required))))))

(defun emacs-agent-editor--register-tools ()
  "Register the complete version 0.1 MCP tool surface."
  (emacs-agent-tool-clear)
  (let* ((string '((type . "string")))
         (integer '((type . "integer")))
         (boolean '((type . "boolean")))
         (position
          (emacs-agent-editor--object-schema
           `((line . ,integer) (column . ,integer))
           '("line" "column")))
         (edit
          (emacs-agent-editor--object-schema
           `((start . ,position) (end . ,position)
             (new_text . ,string) (expected_text . ,string))
           '("start" "end" "new_text")))
         (document-guard
          (emacs-agent-editor--object-schema
           `((path . ,string) (expected_revision . ,string))
           '("path" "expected_revision")))
         (output '((type . "object"))))
    (emacs-agent-tool-register
     "emacs_agent_workspace_info"
     "Return the bound Emacs workspace, policy, and health."
     (emacs-agent-editor--object-schema nil) output
     #'emacs-agent-editor--workspace-info 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_read"
     "Read authoritative buffer content and its opaque revision."
     (emacs-agent-editor--object-schema
      `((path . ,string) (start_line . ,integer) (end_line . ,integer)
        (max_chars . ,integer) (cursor . ,string))
      '("path"))
     output #'emacs-agent-editor--document-read 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_apply_edits"
     "Apply guarded non-overlapping range edits as one undo unit."
     (emacs-agent-editor--object-schema
      `((path . ,string) (expected_revision . ,string)
        (edits . ((type . "array") (items . ,edit)))
        (checkpoint . ,boolean))
      '("path" "expected_revision" "edits"))
     output #'emacs-agent-editor--document-apply-edits 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_document_create"
     "Create a new visited text document inside the workspace."
     (emacs-agent-editor--object-schema
      `((path . ,string) (content . ,string) (checkpoint . ,boolean))
      '("path" "content"))
     output #'emacs-agent-editor--document-create 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_workspace_files"
     "List workspace files with filters and opaque pagination."
     (emacs-agent-editor--object-schema
      `((include_globs . ((type . "array") (items . ,string)))
        (exclude_globs . ((type . "array") (items . ,string)))
        (max_results . ,integer) (cursor . ,string)))
     output #'emacs-agent-editor--workspace-files 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_workspace_search"
     "Search workspace text with ripgrep or the Emacs fallback."
     (emacs-agent-editor--object-schema
      `((query . ,string) (regexp . ,boolean)
        (include_globs . ((type . "array") (items . ,string)))
        (exclude_globs . ((type . "array") (items . ,string)))
        (max_results . ,integer) (cursor . ,string))
      '("query"))
     output #'emacs-agent-editor--workspace-search 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_move"
     "Move a guarded document while preserving its visiting buffer."
     (emacs-agent-editor--object-schema
      `((path . ,string) (new_path . ,string)
        (expected_revision . ,string) (approval_request_id . ,string))
      '("path" "new_path" "expected_revision"))
     output #'emacs-agent-editor--document-move 'destructive)
    (emacs-agent-tool-register
     "emacs_agent_document_delete"
     "Delete a guarded document with rollback metadata."
     (emacs-agent-editor--object-schema
      `((path . ,string) (expected_revision . ,string)
        (approval_request_id . ,string))
      '("path" "expected_revision"))
     output #'emacs-agent-editor--document-delete 'destructive)
    (emacs-agent-tool-register
     "emacs_agent_workspace_checkpoint"
     "Save guarded buffers through their normal Emacs save hooks."
     (emacs-agent-editor--object-schema
      `((documents . ((type . "array") (items . ,document-guard)))
        (approval_request_id . ,string))
      '("documents"))
     output #'emacs-agent-editor--workspace-checkpoint 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_workspace_sync"
     "Reconcile managed buffers with external filesystem changes."
     (emacs-agent-editor--object-schema
      `((paths . ((type . "array") (items . ,string)))))
     output #'emacs-agent-editor--workspace-sync 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_workspace_diff"
     "Return a paginated unified diff for active or selected changes."
     (emacs-agent-editor--object-schema
      `((changeset_id . ,string) (max_chars . ,integer)
        (cursor . ,string)))
     output #'emacs-agent-editor--workspace-diff 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_changeset_rollback"
     "Rollback a change set only when all revision guards still match."
     (emacs-agent-editor--object-schema
      `((changeset_id . ,string) (approval_request_id . ,string))
      '("changeset_id"))
     output #'emacs-agent-editor--changeset-rollback 'destructive)))

;;;###autoload
(defun emacs-agent-editor-start (directory &optional port)
  "Start Agent Editor MCP for local workspace DIRECTORY.
PORT overrides `emacs-agent-editor-port' when non-nil."
  (interactive "DWorkspace: ")
  (when (emacs-agent-editor-running-p)
    (user-error "Agent Editor MCP is already running"))
  (unless (equal emacs-agent-editor-host "127.0.0.1")
    (user-error "Version 0.1 only supports the IPv4 loopback listener"))
  (let* ((token (or emacs-agent-editor-bearer-token
                    (emacs-agent-editor--random-token)))
         (state-directory (emacs-agent-editor--instance-state-directory))
         (workspace
          (emacs-agent-workspace-create
           directory
           :access-mode emacs-agent-editor-access-mode
           :save-policy emacs-agent-editor-save-policy
           :writer-lease token
           :state-directory state-directory))
         server)
    (condition-case error-data
        (progn
          (emacs-agent-workspace-bind workspace)
          (emacs-agent-editor--register-tools)
          (setq emacs-agent-edit-record-function
                #'emacs-agent-editor--record-edit
                emacs-agent-editor--token token
                emacs-agent-editor--workspace workspace)
          (emacs-agent-journal-open workspace)
          (setq server
                (emacs-agent-http-start
                 #'emacs-agent-protocol-handle-http-request
                 :host emacs-agent-editor-host
                 :port (or port emacs-agent-editor-port)
                 :endpoint emacs-agent-editor-endpoint
                 :token token
                 :allowed-origins emacs-agent-editor-allowed-origins)
                emacs-agent-editor--http-server server)
          (emacs-agent-editor--write-connection-file
           workspace (emacs-agent-http-server-port server) token)
          (emacs-agent-workspace-record-activity
           workspace
           (list :tool "server_start" :status "completed"))
          (when (called-interactively-p 'interactive)
            (message "Agent Editor MCP started: %s"
                     emacs-agent-editor--connection-file))
          server)
      (error
       (when server (emacs-agent-http-stop server))
       (ignore-errors (emacs-agent-journal-close workspace))
       (emacs-agent-editor--remove-connection-file)
       (setq emacs-agent-editor--http-server nil
             emacs-agent-editor--workspace nil
             emacs-agent-editor--token nil
             emacs-agent-edit-record-function nil)
       (signal (car error-data) (cdr error-data))))))

;;;###autoload
(defun emacs-agent-editor-stop ()
  "Stop Agent Editor MCP and remove its connection metadata."
  (interactive)
  (when emacs-agent-editor--workspace
    (ignore-errors
      (emacs-agent-workspace-record-activity
       emacs-agent-editor--workspace
       (list :tool "server_stop" :status "completed")))
    (ignore-errors
      (emacs-agent-journal-close emacs-agent-editor--workspace)))
  (when emacs-agent-editor--http-server
    (emacs-agent-http-stop emacs-agent-editor--http-server))
  (emacs-agent-session-clear)
  (emacs-agent-tool-clear)
  (emacs-agent-editor--remove-connection-file)
  (setq emacs-agent-current-workspace nil
        emacs-agent-editor--http-server nil
        emacs-agent-editor--workspace nil
        emacs-agent-editor--token nil
        emacs-agent-edit-record-function nil)
  t)

;;;###autoload
(defun emacs-agent-editor-pause ()
  "Pause agent mutations while leaving read tools available."
  (interactive)
  (emacs-agent-workspace-pause emacs-agent-editor--workspace))

;;;###autoload
(defun emacs-agent-editor-resume ()
  "Resume agent mutations."
  (interactive)
  (emacs-agent-workspace-resume emacs-agent-editor--workspace))

;;;###autoload
(defun emacs-agent-editor-revoke-writer ()
  "Pause mutations and rotate the active writer credential."
  (interactive)
  (unless (emacs-agent-editor-running-p)
    (user-error "Agent Editor MCP is not running"))
  (emacs-agent-editor-pause)
  (let ((token (emacs-agent-editor--random-token)))
    (setq emacs-agent-editor--token token)
    (setf (emacs-agent-http-server-token emacs-agent-editor--http-server)
          token
          (emacs-agent-workspace-writer-lease
           emacs-agent-editor--workspace)
          token)
    (emacs-agent-session-clear)
    (emacs-agent-editor--write-connection-file
     emacs-agent-editor--workspace
     (emacs-agent-http-server-port emacs-agent-editor--http-server)
     token))
  t)

(defalias 'emacs-agent-editor-show-activity #'emacs-agent-show-activity)
(defalias 'emacs-agent-editor-show-changes #'emacs-agent-show-changes)

(provide 'emacs-agent-editor)
;;; emacs-agent-editor.el ends here
