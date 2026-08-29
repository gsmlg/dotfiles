;;; emacs-agent-editor.el --- Buffer-first HTTP MCP editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Gao

;; Author: Gao
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: tools, convenience

;;; Commentary:
;; Expose one Emacs runtime through a guarded, buffer-first HTTP MCP server.
;; The package is deliberately stopped when loaded.  Call
;; `emacs-agent-editor-start', or use the daemon integration in this repo.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'server)
(require 'subr-x)
(require 'emacs-agent-request)
(require 'emacs-agent-session)
(require 'emacs-agent-protocol)
(require 'emacs-agent-http)
(require 'emacs-agent-policy)
(require 'emacs-agent-document)
(require 'emacs-agent-edit)
(require 'emacs-agent-transform)
(require 'emacs-agent-transaction)
(require 'emacs-agent-runtime)
(require 'emacs-agent-project)
(require 'emacs-agent-search)
(require 'emacs-agent-diagnostics)
(require 'emacs-agent-semantic)
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

(defcustom emacs-agent-editor-port 9876
  "TCP port on which the MCP listener binds.
A value of zero explicitly requests an ephemeral port."
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
   "emacs/agent-editor/"
   (or (getenv "XDG_STATE_HOME")
       (expand-file-name ".local/state/" "~")))
  "Directory used for the singleton Agent Editor connection metadata.

The formal interactive Emacs server publishes exactly one
`connection.json' in this directory.  Tests override the directory to an
isolated temporary location."
  :type 'directory)

(defcustom emacs-agent-editor-access-mode 'autonomous
  "Default runtime access mode."
  :type '(choice (const read-only) (const review) (const autonomous)))

(defcustom emacs-agent-editor-save-policy 'immediate
  "Default runtime save policy."
  :type '(choice (const immediate) (const manual) (const explicit-per-call)))

(defcustom emacs-agent-editor-token-authentication-enabled nil
  "Whether MCP requests require bearer-token authentication.
Authentication is disabled by default because the server is restricted to the
IPv4 loopback interface.  Enable this option when other local processes should
not be allowed to access the MCP endpoint."
  :type 'boolean)

(defcustom emacs-agent-editor-bearer-token nil
  "Bearer token used when token authentication is enabled.
A nil value generates a new token at server start.  This option is ignored
when `emacs-agent-editor-token-authentication-enabled' is nil."
  :type '(choice (const :tag "Generate token" nil) string))

(defvar emacs-agent-editor--http-server nil
  "Active Agent Editor HTTP server.")

(defvar emacs-agent-editor--runtime nil
  "Editor runtime bound to the active server.")

(defvar emacs-agent-editor--token nil
  "Bearer token accepted by the active server.")

(defvar emacs-agent-editor--connection-file nil
  "Connection metadata file for the active server.")

(defvar emacs-agent-editor--request-context nil
  "Dynamically bound request context for change-set attribution.")

(defvar emacs-agent-editor--operation-name "document_apply_edits"
  "Dynamically bound public operation name for single-document writes.")

(defvar emacs-agent-editor--diff-cursors (make-hash-table :test #'equal)
  "Opaque pagination cursors for diff results.")

(defconst emacs-agent-editor-position-semantics
  '((lineBase . 1)
    (columnBase . 0)
    (unit . "emacs_character")
    (range . "half_open")
    (tabWidth . 1)
    (editsRelativeTo . "expected_revision")
    (applicationOrder . "descending")
    (samePositionInserts . "rejected"))
  "Public position contract retained for compatibility.")

(defun emacs-agent-editor--daemon-name ()
  "Return a filesystem-safe name for this Emacs instance.

Prefer the live `server-name' when this process owns a server, otherwise the
daemon name, otherwise `interactive'."
  (replace-regexp-in-string
   "[^[:alnum:]_.-]" "_"
   (or (and server-mode server-name)
       (let ((name (daemonp)))
         (and (stringp name) name))
       "interactive")))

(defun emacs-agent-editor--random-token ()
  "Return a URL-safe bearer token using operating-system entropy."
  (condition-case error-data
      (funcall emacs-agent-session-id-function)
    (error
     (user-error
      "Secure entropy unavailable; customize `emacs-agent-editor-bearer-token': %s"
      (error-message-string error-data)))))

(defun emacs-agent-editor--instance-state-directory ()
  "Return the singleton state directory for the editor server.

Project and daemon identities are not encoded in this path; agents discover
registered projects through MCP `project_list'."
  (file-name-as-directory emacs-agent-editor-state-directory))

(defun emacs-agent-editor--write-connection-file (runtime port token)
  "Publish private connection metadata for RUNTIME, PORT, and TOKEN.
TOKEN is omitted from the metadata when authentication is disabled."
  (let* ((directory (emacs-agent-editor--instance-state-directory))
         (target (expand-file-name "connection.json" directory))
         (temporary nil)
         (server-name (emacs-agent-editor--daemon-name))
         (metadata
          (append
           `((schema_version . 2)
             (instance_id . ,(emacs-agent-runtime-instance-id runtime))
             (server_name . ,server-name)
             (daemon . ,server-name)
             (pid . ,(emacs-pid))
             (endpoint . ,(format "http://%s:%d%s"
                                  emacs-agent-editor-host
                                  port
                                  emacs-agent-editor-endpoint))
             (token_authentication . ,(if token t :false)))
           (when token `((token . ,token)))
           `((protocol_versions
              . ,(vconcat emacs-agent-protocol-versions))
             (filesystem_scope
              . ,(symbol-name
                  (emacs-agent-runtime-filesystem-policy runtime)))
             (started_at . ,(format-time-string "%FT%TZ" nil t))))))
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
             (format "runtime %s, %d projects (%s)"
                     (emacs-agent-runtime-instance-id
                      emacs-agent-editor--runtime)
                     (hash-table-count
                      (emacs-agent-runtime-project-registry
                       emacs-agent-editor--runtime))
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
        (numberp value))
    value)
   ((vectorp value)
    (vconcat
     (mapcar #'emacs-agent-editor--json-value (append value nil))))
   ((null value) :false)
   ((and (symbolp value) (not (keywordp value)))
    (symbol-name value))
   ((and (listp value) (keywordp (car value)))
    (emacs-agent-editor--plist-to-alist value))
   ((and (listp value)
         (cl-every (lambda (entry)
                     (and (consp entry)
                          (symbolp (car entry))
                          (not (keywordp (car entry)))))
                   value))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (if
                        (emacs-agent-editor--json-array-key-p
                         (car entry))
                        (emacs-agent-editor--json-array (cdr entry))
                      (emacs-agent-editor--json-value (cdr entry)))))
            value))
   ((listp value)
    (vconcat (mapcar #'emacs-agent-editor--json-value value)))
   (t (format "%s" value))))

(defconst emacs-agent-editor--json-array-plist-keys
  '(:accepted_paths :active_changesets :actions :changesets
    :checkpointed_paths :diagnostics :diagnostics_after
    :diagnostics_before :document_paths :documents :edits :files :matches
    :new_revisions :old_revisions :operations :paths :ranges
    :projects :references :related_information :restored_paths
    :revision_bindings :results :sources :symbols)
  "Public plist keys whose values are always JSON arrays.")

(defun emacs-agent-editor--json-array-key-p (key)
  "Return non-nil when public JSON field KEY is always an array."
  (memq
   (if (keywordp key)
       key
     (intern (concat ":" (symbol-name key))))
   emacs-agent-editor--json-array-plist-keys))

(defun emacs-agent-editor--json-array (items)
  "Convert collection ITEMS into a JSON-compatible array."
  (vconcat
   (mapcar #'emacs-agent-editor--json-value
           (append items nil))))

(defun emacs-agent-editor--plist-to-alist (plist)
  "Convert PLIST recursively to a JSON-compatible alist."
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (push (cons (emacs-agent-editor--json-key key)
                    (if (memq key
                              emacs-agent-editor--json-array-plist-keys)
                        (emacs-agent-editor--json-array value)
                      (emacs-agent-editor--json-value value)))
              result)))
    (nreverse result)))

(defun emacs-agent-editor--tool-error (code &rest details)
  "Signal a structured public tool error for internal CODE and DETAILS."
  (let* ((legacy
          (if (symbolp code) (symbol-name code) code))
         (public
          (or
           (cdr
            (assoc
             legacy
             '(("revision_conflict" . "REVISION_MISMATCH")
               ("expected_text_mismatch" . "EXPECTED_TEXT_MISMATCH")
               ("occurrence_count_mismatch" . "EXPECTED_TEXT_MISMATCH")
               ("ambiguous_text_match" . "MATCH_NOT_UNIQUE")
               ("invalid_patch" . "PATCH_INVALID")
               ("patch_path_mismatch" . "PATCH_INVALID")
               ("patch_conflict" . "PATCH_CONTEXT_MISMATCH")
               ("overlapping_edits" . "OVERLAPPING_EDITS")
               ("invalid_position" . "POSITION_OUT_OF_RANGE")
               ("external_change_conflict" . "EXTERNAL_CHANGE_CONFLICT")
               ("runtime_not_started" . "RUNTIME_NOT_STARTED")
               ("project_not_found" . "PROJECT_NOT_FOUND")
               ("project_path_required" . "PROJECT_PATH_REQUIRED")
               ("path_outside_project" . "PATH_OUTSIDE_PROJECT")
               ("path_not_allowed" . "PATH_NOT_ALLOWED")
               ("path_denied" . "PATH_DENIED")
               ("remote_path_unsupported" . "REMOTE_PATH_UNSUPPORTED")
               ("unsupported_document_type"
                . "UNSUPPORTED_DOCUMENT_TYPE")
               ("document_too_large" . "DOCUMENT_TOO_LARGE")
               ("runtime_paused" . "RUNTIME_PAUSED")
               ("approval_required" . "APPROVAL_REQUIRED")
               ("checkpoint_failed" . "CHECKPOINT_FAILED")
               ("save_failed" . "CHECKPOINT_FAILED")
               ("capability_unavailable" . "CAPABILITY_UNAVAILABLE")
               ("rollback_conflict" . "CHANGESET_NOT_ROLLBACKABLE"))))
           (upcase legacy)))
         (message
          (or (plist-get details :message)
              (replace-regexp-in-string "_" " " public)))
         (retryable
          (if (member
               public
               '("REVISION_MISMATCH" "EXTERNAL_CHANGE_CONFLICT"
                 "DIAGNOSTICS_TIMEOUT" "APPROVAL_EXPIRED"
                 "CHECKPOINT_FAILED" "OPERATION_TIMEOUT"))
              t :false))
         (detail-plist (copy-sequence details)))
    (when (plist-member detail-plist :message)
      (setq detail-plist
            (plist-put detail-plist :message nil))
      (setq detail-plist
            (cl-loop for (key value) on detail-plist by #'cddr
                     unless (eq key :message)
                     append (list key value))))
    (let ((detail-alist
           (emacs-agent-editor--plist-to-alist detail-plist)))
      (signal
       'emacs-agent-tool-error
       (list
        (append
         `((code . ,public)
           (legacy_code . ,legacy)
           (message . ,message)
           (retryable . ,retryable))
         detail-alist
         `((details . ,detail-alist))))))))

(defun emacs-agent-editor--call (function)
  "Call FUNCTION and translate editor conditions into tool errors."
  (condition-case error-data
      (funcall function)
    (emacs-agent-error
     (let ((code (emacs-agent-error-code error-data))
           (details (emacs-agent-error-details error-data)))
       (apply #'emacs-agent-editor--tool-error code details)))
    (emacs-agent-runtime-paused
     (emacs-agent-editor--tool-error
      'runtime_paused :message (error-message-string error-data)))
    (emacs-agent-runtime-approval-error
     (emacs-agent-editor--tool-error
      'approval_required :message (error-message-string error-data)))
    (emacs-agent-runtime-not-started
     (emacs-agent-editor--tool-error
      'runtime_not_started :message (error-message-string error-data)))
    (emacs-agent-runtime-error
     (emacs-agent-editor--tool-error
      'runtime_not_started :message (error-message-string error-data)))
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

(defun emacs-agent-editor--checkpoint-p (runtime requested)
  "Return whether RUNTIME should checkpoint a REQUESTED mutation."
  (pcase (emacs-agent-runtime-save-policy runtime)
    ('immediate t)
    ('manual (eq requested t))
    ('explicit-per-call (eq requested t))
    (_ (eq requested t))))

(defun emacs-agent-editor--argument (arguments key)
  "Read KEY from tool ARGUMENTS."
  (alist-get key arguments))

(defun emacs-agent-editor--optional-id (arguments key)
  "Return optional string ID at KEY in ARGUMENTS."
  (let ((value (emacs-agent-editor--argument arguments key)))
    (cond
     ((or (null value) (eq value :false)) nil)
     ((stringp value) value)
     (t
      (emacs-agent-signal 'invalid_argument :field key)))))

(defun emacs-agent-editor--resolve-target
    (arguments &optional for-create path-key project-key)
  "Resolve a document target from ARGUMENTS.
FOR-CREATE permits a missing leaf.  PATH-KEY defaults to `path' and
PROJECT-KEY defaults to `project_id'."
  (emacs-agent-project-resolve-target
   (emacs-agent-runtime-current)
   (emacs-agent-editor--argument arguments (or path-key 'path))
   :project-id
   (emacs-agent-editor--optional-id
    arguments (or project-key 'project_id))
   :for-create for-create))

(defun emacs-agent-editor--approval-arguments (arguments)
  "Return normalized ARGUMENTS without its approval identifier."
  (assq-delete-all 'approval_request_id (copy-tree arguments)))

(defun emacs-agent-editor--authorize (runtime operation arguments)
  "Authorize protected OPERATION with ARGUMENTS in RUNTIME."
  (when (eq (emacs-agent-runtime-access-mode runtime) 'review)
    (let* ((approval-id
            (emacs-agent-editor--argument arguments 'approval_request_id))
           (normalized (emacs-agent-editor--approval-arguments arguments)))
      (if approval-id
          (emacs-agent-runtime-consume-approval
           runtime approval-id operation normalized emacs-agent-editor--token)
        (let ((approval
               (emacs-agent-runtime-request-approval
                runtime operation normalized emacs-agent-editor--token)))
          (apply #'emacs-agent-editor--tool-error
                 'approval_required approval))))))

(defun emacs-agent-editor--record-edit
    (document before _after previous-revision new-revision)
  "Record a change to DOCUMENT from BEFORE at PREVIOUS-REVISION.
NEW-REVISION identifies the resulting content."
  (let* ((runtime (emacs-agent-document-runtime document))
         (path (emacs-agent-document-canonical-path document))
         (checkpointed
          (not (buffer-modified-p (emacs-agent-document-buffer document))))
         (changeset
          (emacs-agent-changeset-record
           runtime
           :request-id
           (and emacs-agent-editor--request-context
                (emacs-agent-request-id emacs-agent-editor--request-context))
           :agent-identity
           (and emacs-agent-editor--request-context
                (emacs-agent-request-client-info
                 emacs-agent-editor--request-context))
           :operations
           (list (list :type (intern emacs-agent-editor--operation-name)
                       :path path))
           :touched-documents (list path)
           :base-revisions (list (cons path previous-revision))
           :final-revisions (list (cons path new-revision))
           :before-snapshots
           (list (cons path (list :exists t :content before)))
           :checkpoint-state checkpointed)))
    (setf (emacs-agent-document-last-changeset-id document)
          (emacs-agent-changeset-changeset-id changeset))
    (emacs-agent-journal-write
     runtime
     (list :tool emacs-agent-editor--operation-name :status "completed"
           :changeset_id (emacs-agent-changeset-changeset-id changeset)
           :paths (list path)))
    (emacs-agent-changeset-changeset-id changeset)))

(defun emacs-agent-editor--observe-tool (name status duration payload)
  "Record bounded metadata for tool NAME, STATUS, DURATION, and PAYLOAD."
  (when (emacs-agent-runtime-p emacs-agent-editor--runtime)
    (let* ((changeset-id
            (and (listp payload)
                 (alist-get 'changeset_id payload)))
           (path
            (and (listp payload) (alist-get 'path payload)))
           (documents
            (and (listp payload) (alist-get 'documents payload)))
           (code
            (and (listp payload) (alist-get 'code payload)))
           (write-p
            (member
             name
             '("emacs_agent_document_apply_edits"
               "emacs_agent_document_replace"
               "emacs_agent_document_apply_patch"
               "emacs_agent_document_create"
               "emacs_agent_document_move"
               "emacs_agent_document_delete"
               "emacs_agent_editor_apply_edits"
               "emacs_agent_editor_checkpoint"
               "emacs_agent_changeset_rollback"
               "emacs_agent_symbol_rename"
               "emacs_agent_code_actions"
               "emacs_agent_format_document"
               "emacs_agent_format_range")))
           (file-count
            (if (not write-p)
                0
              (cond
             ((vectorp documents) (length documents))
             ((listp documents) (length documents))
             (path 1)
             (t 0))))
           (event
            (append
             (list
              :tool name :status status :duration duration
              :runtime_instance_id
              (emacs-agent-runtime-instance-id
               emacs-agent-editor--runtime)
              :modified_file_count file-count)
             (when changeset-id (list :changeset_id changeset-id))
             (when path (list :path path))
             (when code
               (list
                :code code
                :revision_mismatch_count
                (if (equal code "REVISION_MISMATCH") 1 0)
                :conflict_count
                (if (member
                     code
                     '("REVISION_MISMATCH" "EXTERNAL_CHANGE_CONFLICT"
                       "PATCH_CONTEXT_MISMATCH"))
                    1 0))))))
      (emacs-agent-runtime-record-activity
       emacs-agent-editor--runtime event)
      (emacs-agent-journal-write emacs-agent-editor--runtime event))))

(defun emacs-agent-editor--runtime-provider-buffers (runtime)
  "Return live document buffers belonging to RUNTIME.

The selected buffer is first only when it is one of those runtime buffers."
  (let ((selected (and (window-live-p (selected-window))
                       (window-buffer (selected-window))))
        buffers)
    (maphash
     (lambda (_path document)
       (let ((buffer (emacs-agent-document-buffer document)))
         (when (buffer-live-p buffer)
           (push buffer buffers))))
     (emacs-agent-runtime-document-registry runtime))
    (setq buffers (delete-dups buffers))
    (if (memq selected buffers)
        (cons selected (delq selected buffers))
      buffers)))

(defun emacs-agent-editor--editor-info (_arguments _context)
  "Implement `emacs_agent_editor_info'."
  (let* ((runtime (emacs-agent-runtime-current))
         (provider-buffers
          (emacs-agent-editor--runtime-provider-buffers runtime))
         (runtime-capabilities
          (emacs-agent-semantic-runtime-capabilities
           (or provider-buffers :none))))
    `((instance_id . ,(emacs-agent-runtime-instance-id runtime))
      (access_mode
       . ,(symbol-name (emacs-agent-runtime-access-mode runtime)))
      (save_policy
       . ,(symbol-name (emacs-agent-runtime-save-policy runtime)))
      (paused . ,(if (emacs-agent-runtime-paused-p runtime) t :false))
      (health
       . ,(symbol-name (emacs-agent-runtime-health-state runtime)))
      (project_count
       . ,(hash-table-count
           (emacs-agent-runtime-project-registry runtime)))
      (managed_document_count
       . ,(hash-table-count
           (emacs-agent-runtime-document-registry runtime)))
      (filesystem_policy
       . ,(symbol-name
           (emacs-agent-runtime-filesystem-policy runtime)))
      (protocol_versions . ,(vconcat emacs-agent-protocol-versions))
      (authentication
       . ((type . ,(if emacs-agent-editor--token "bearer" "none"))))
      (supported_tools
       . ,(vconcat
           (mapcar #'emacs-agent-tool-name
                   (emacs-agent-tool-list))))
      (runtime_capabilities
       . ,(emacs-agent-editor--json-value runtime-capabilities))
      (capabilities
       . ["read" "edit" "create" "files" "search" "move" "delete"
          "checkpoint" "sync" "diff" "rollback" "replace" "patch"
          "cross_document_transactions" "diagnostics" "changeset_query"
          "document_status" "symbols" "xref" "editor_context"
          "semantic_rename" "code_actions" "trusted_formatting"
          "format_range" "approval_status"])
      (position_semantics
       . ,emacs-agent-editor-position-semantics)
      (feature_capabilities
       . ((dryRun . t)
          (writeDiff . t)
          (diffPagination . t)
          (changesetPersistence . "daemon_memory")
          (bufferAwareSearch . t)
          (diagnostics . t)
          (currentContext . t)
          (semanticRename . "eglot")
          (codeActions . "eglot_safe_edits_only")
          (approvalStatus . t)
          (trustedFormatting
           . ,(if (functionp emacs-agent-semantic-format-function)
                  t :false))
          (semanticBackends . ["imenu" "xref"]))))))

(defun emacs-agent-editor--project-open (arguments _context)
  "Implement `emacs_agent_project_open' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-project-open
       (emacs-agent-runtime-current)
       (emacs-agent-editor--argument arguments 'root))))))

(defun emacs-agent-editor--project-list (_arguments _context)
  "Implement `emacs_agent_project_list'."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-project-list (emacs-agent-runtime-current))))))

(defun emacs-agent-editor--project-buffers (runtime project)
  "Return live managed buffers for PROJECT in RUNTIME."
  (let ((root (emacs-agent-project-canonical-root project))
        buffers)
    (maphash
     (lambda (path document)
       (let ((buffer (emacs-agent-document-buffer document)))
         (when (and (emacs-agent-policy--within-root-p path root)
                    (buffer-live-p buffer))
           (push buffer buffers))))
     (emacs-agent-runtime-document-registry runtime))
    (nreverse buffers)))

(defun emacs-agent-editor--project-info (arguments _context)
  "Implement `emacs_agent_project_info' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (project-id
             (emacs-agent-editor--argument arguments 'project_id))
            (project (emacs-agent-project-get runtime project-id))
            (metadata (emacs-agent-project-info runtime project-id))
            (buffers
             (emacs-agent-editor--project-buffers runtime project)))
       (emacs-agent-editor--plist-to-alist
        (append
         metadata
         (list
          :managed_document_count (length buffers)
          :capabilities
          (emacs-agent-semantic-runtime-capabilities
           (or buffers :none)))))))))

(defun emacs-agent-editor--project-close (arguments _context)
  "Implement `emacs_agent_project_close' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-project-close
       (emacs-agent-runtime-current)
       (emacs-agent-editor--argument arguments 'project_id))))))

(defun emacs-agent-editor--document-read (arguments _context)
  "Implement `emacs_agent_document_read' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target (emacs-agent-editor--resolve-target arguments))
            (result
             (emacs-agent-document-read
              runtime target
              (emacs-agent-editor--argument arguments 'start_line)
              (emacs-agent-editor--argument arguments 'end_line)
              (emacs-agent-editor--argument arguments 'max_chars)
              (emacs-agent-editor--argument arguments 'cursor))))
       (emacs-agent-editor--plist-to-alist result)))))

(defun emacs-agent-editor--document-apply-edits (arguments context)
  "Implement `emacs_agent_document_apply_edits' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target (emacs-agent-editor--resolve-target arguments))
            (path (emacs-agent-resolved-target-canonical-path target))
            (expected-revision
             (emacs-agent-editor--argument arguments 'expected_revision))
            (edits (emacs-agent-editor--argument arguments 'edits))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (checkpoint
             (emacs-agent-editor--checkpoint-p
              runtime
              (emacs-agent-editor--argument arguments 'checkpoint)))
            (emacs-agent-editor--request-context context)
            (emacs-agent-editor--operation-name
             "document_apply_edits")
            (result
             (if dry-run
                 (let* ((document
                         (emacs-agent-document-open runtime target))
                        (_ (emacs-agent-document-reconcile document))
                        (revision
                         (emacs-agent-document-revision document))
                        (buffer (emacs-agent-document-buffer document))
                        ranges before after)
                   (unless (equal revision expected-revision)
                     (emacs-agent-signal
                      'revision_conflict :path path
                      :expected_revision expected-revision
                      :current_revision revision :requires_reread t))
                   (with-current-buffer buffer
                     (save-restriction
                       (widen)
                       (setq ranges
                             (emacs-agent-edit--validate-ranges
                              document edits)
                             before
                             (buffer-substring-no-properties
                              (point-min) (point-max)))))
                   (setq after
                         (with-temp-buffer
                           (insert before)
                           (emacs-agent-edit--apply-ranges ranges)
                           (buffer-string)))
                   (append
                    (emacs-agent-document-output-fields target)
                    (list
                     :changeset_id nil
                     :previous_revision revision
                     :new_revision revision
                     :checkpointed nil :edit_count (length ranges)
                     :old_revision revision :applied nil
                     :modified (not (equal before after))
                     :diff
                     (emacs-agent-changeset--diff-text path before after)
                     :truncated nil :diff_truncated nil
                     :diagnostics_state "not_requested")))
               (emacs-agent-runtime-enqueue-mutation
                runtime
                (lambda ()
                  (emacs-agent-edit-apply
                   runtime target expected-revision edits checkpoint))))))
       (unless dry-run
         (setq result
               (plist-put
                result :old_revision
                (plist-get result :previous_revision)))
         (setq result (plist-put result :applied t))
         (setq result
               (plist-put
                result :diff
                (if-let* ((changeset-id
                           (plist-get result :changeset_id)))
                    (emacs-agent-changeset-diff
                     runtime changeset-id)
                  "")))
         (setq result (plist-put result :truncated nil))
         (setq result (plist-put result :diff_truncated nil)))
       (emacs-agent-editor--plist-to-alist
        (cl-loop for (key value) on result by #'cddr
                 unless (memq key '(:before_content :after_content))
                 append (list key value)))))))

(defun emacs-agent-editor--document-replace (arguments context)
  "Implement `emacs_agent_document_replace' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target (emacs-agent-editor--resolve-target arguments))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (checkpoint
             (emacs-agent-editor--checkpoint-p
              runtime
              (emacs-agent-editor--argument arguments 'checkpoint)))
            (emacs-agent-editor--request-context context)
            (emacs-agent-editor--operation-name "document_replace")
            (operation
             (lambda ()
               (emacs-agent-transform-replace
                runtime target
                (emacs-agent-editor--argument arguments 'expected_revision)
                (emacs-agent-editor--argument arguments 'old_text)
                (emacs-agent-editor--argument arguments 'new_text)
                :replace-all
                (eq (emacs-agent-editor--argument arguments 'replace_all) t)
                :expected-occurrences
                (emacs-agent-editor--argument
                 arguments 'expected_occurrences)
                :dry-run dry-run :checkpoint checkpoint)))
            (result
             (if dry-run
                 (funcall operation)
               (emacs-agent-runtime-enqueue-mutation
                runtime operation))))
       (emacs-agent-editor--plist-to-alist result)))))

(defun emacs-agent-editor--document-apply-patch (arguments context)
  "Implement `emacs_agent_document_apply_patch' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target (emacs-agent-editor--resolve-target arguments))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (checkpoint
             (emacs-agent-editor--checkpoint-p
              runtime
              (emacs-agent-editor--argument arguments 'checkpoint)))
            (emacs-agent-editor--request-context context)
            (emacs-agent-editor--operation-name "document_apply_patch")
            (operation
             (lambda ()
               (emacs-agent-transform-apply-patch
                runtime target
                (emacs-agent-editor--argument arguments 'expected_revision)
                (emacs-agent-editor--argument arguments 'patch)
                :fuzz
                (or (emacs-agent-editor--argument arguments 'fuzz) 0)
                :dry-run dry-run :checkpoint checkpoint)))
            (result
             (if dry-run
                 (funcall operation)
               (emacs-agent-runtime-enqueue-mutation
                runtime operation))))
       (emacs-agent-editor--plist-to-alist result)))))

(defun emacs-agent-editor--editor-apply-edits (arguments context)
  "Implement `emacs_agent_editor_apply_edits' with ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (plan
             (emacs-agent-transaction-plan
              runtime
              (emacs-agent-editor--argument arguments 'documents)))
            (result
             (emacs-agent-transaction-apply
              plan
              (eq (emacs-agent-editor--argument arguments 'dry_run) t)
              (emacs-agent-editor--checkpoint-p
               runtime
               (emacs-agent-editor--argument arguments 'checkpoint))
              context)))
       (emacs-agent-editor--plist-to-alist result)))))

(defun emacs-agent-editor--record-lifecycle
    (runtime operation paths before base final checkpointed)
  "Record a lifecycle OPERATION in RUNTIME."
  (let ((changeset
         (emacs-agent-changeset-record
          runtime
          :request-id
          (and emacs-agent-editor--request-context
               (emacs-agent-request-id emacs-agent-editor--request-context))
          :agent-identity
          (and emacs-agent-editor--request-context
               (emacs-agent-request-client-info
                emacs-agent-editor--request-context))
          :operations (list operation)
          :touched-documents paths
          :base-revisions base
          :final-revisions final
          :before-snapshots before
          :checkpoint-state checkpointed)))
    (emacs-agent-journal-write
     runtime
     (list :tool (plist-get operation :type)
           :status "completed"
           :changeset_id (emacs-agent-changeset-changeset-id changeset)
           :paths paths))
    (emacs-agent-changeset-changeset-id changeset)))

(defun emacs-agent-editor--document-create (arguments context)
  "Implement `emacs_agent_document_create' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target (emacs-agent-editor--resolve-target arguments t))
            (path (emacs-agent-resolved-target-canonical-path target))
            (content (emacs-agent-editor--argument arguments 'content))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (checkpoint
             (emacs-agent-editor--checkpoint-p
              runtime
              (emacs-agent-editor--argument arguments 'checkpoint)))
            (emacs-agent-editor--request-context context))
       (when (string-match-p (string 0) content)
         (emacs-agent-editor--tool-error
          'unsupported_document_type :path path :reason 'binary))
       (when (> (string-bytes content)
                emacs-agent-policy-maximum-document-bytes)
         (emacs-agent-editor--tool-error 'document_too_large :path path))
       (when (file-exists-p path)
         (emacs-agent-editor--tool-error
          'document_already_exists :path path))
       (if dry-run
           (emacs-agent-editor--plist-to-alist
            (append
             (emacs-agent-document-output-fields target)
             (list
              :old_revision nil
              :new_revision nil
              :changeset_id nil
              :applied nil
              :modified t
              :checkpointed nil
              :diff
              (emacs-agent-changeset--diff-text path "" content)
              :truncated nil
              :diff_truncated nil)))
         (emacs-agent-runtime-enqueue-mutation
          runtime
          (lambda ()
            (let* ((current-target
                    (emacs-agent-project-resolve-target
                     runtime
                     (emacs-agent-resolved-target-input-path target)
                     :project-id
                     (emacs-agent-resolved-target-project-id target)
                     :for-create t))
                   (current-path
                    (emacs-agent-resolved-target-canonical-path
                     current-target)))
              (unless (equal current-path path)
                (emacs-agent-editor--tool-error
                 'external_change_conflict :path path
                 :reason 'target_identity_changed))
              (when (file-exists-p current-path)
                (emacs-agent-editor--tool-error
                 'document_already_exists :path current-path))
              (let* ((document
                      (emacs-agent-document-open
                       runtime current-target t))
                     (buffer (emacs-agent-document-buffer document))
                     group)
                (condition-case error-data
                    (progn
                      (with-current-buffer buffer
                        (setq group (prepare-change-group))
                        (activate-change-group group)
                        (erase-buffer)
                        (insert content))
                      (when checkpoint
                        (emacs-agent-document-checkpoint document))
                      (accept-change-group group)
                      (setq group nil))
                  (emacs-agent-error
                   (when group
                     (let ((details
                            (emacs-agent-error-details error-data)))
                       (if (plist-get details :partial_completion)
                           (accept-change-group group)
                         (cancel-change-group group)))
                     (setq group nil))
                   (let ((code (emacs-agent-error-code error-data))
                         (details
                          (emacs-agent-error-details error-data)))
                     (when (or
                            (eq code 'save_failed)
                            (plist-get details
                                       :reconciliation_required))
                       (setf
                        (emacs-agent-document-degraded document) t
                        (emacs-agent-runtime-health-state runtime)
                        'degraded)))
                   (signal (car error-data) (cdr error-data)))
                  (error
                   (when group
                     (cancel-change-group group)
                     (setq group nil))
                   (when checkpoint
                     (setf (emacs-agent-document-degraded document) t
                           (emacs-agent-runtime-health-state runtime)
                           'degraded))
                   (emacs-agent-editor--tool-error
                    'save_failed :path path
                    :message (error-message-string error-data)
                    :reconciliation_required t
                    :filesystem_rollback_guaranteed nil)))
                (let* ((revision
                        (emacs-agent-document-revision document))
                       (changeset-id
                        (emacs-agent-editor--record-lifecycle
                         runtime
                         (list :type 'create :path path)
                         (list path)
                         (list (cons path (list :exists nil)))
                         nil
                         (list (cons path revision))
                         checkpoint)))
                  (emacs-agent-editor--plist-to-alist
                   (append
                    (emacs-agent-document-output-fields current-target)
                    (list
                     :old_revision nil
                     :changeset_id changeset-id
                     :new_revision revision
                     :applied t
                     :modified t
                     :checkpointed checkpoint
                     :diff
                     (emacs-agent-changeset-diff runtime changeset-id)
                     :truncated nil
                     :diff_truncated nil)))))))))))))

(defun emacs-agent-editor--project-files (arguments _context)
  "Implement `emacs_agent_project_files' for ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((result
             (emacs-agent-project-files
              (emacs-agent-runtime-current)
              (emacs-agent-editor--argument arguments 'project_id)
              :include-globs
              (emacs-agent-editor--argument arguments 'include_globs)
              :exclude-globs
              (emacs-agent-editor--argument arguments 'exclude_globs)
              :max-results
              (emacs-agent-editor--argument arguments 'max_results)
              :cursor (emacs-agent-editor--argument arguments 'cursor)))
            (items
             (mapcar
              #'emacs-agent-editor--plist-to-alist
              (plist-get result :results))))
       `((files . ,(vconcat items))
         (result_count . ,(plist-get result :result_count))
         ,@(when-let* ((cursor (plist-get result :next_cursor)))
             `((cursor . ,cursor))))))))

(defun emacs-agent-editor--project-search (arguments context)
  "Implement `emacs_agent_project_search' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (project-id
             (emacs-agent-editor--argument arguments 'project_id))
            done result error-data process)
       (if (emacs-agent-editor--argument arguments 'cursor)
           (setq result
                 (emacs-agent-project-search
                  runtime project-id
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
               (emacs-agent-project-search
                runtime project-id
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
    (runtime _arguments)
  "Require lifecycle checkpoint authorization in RUNTIME."
  (unless (emacs-agent-editor--checkpoint-p runtime t)
    (emacs-agent-editor--tool-error 'checkpoint_required))
  t)

(defun emacs-agent-editor--same-path-name-p (left right)
  "Return non-nil when absolute path names LEFT and RIGHT are equal."
  (and
   (stringp left)
   (stringp right)
   (equal (expand-file-name left) (expand-file-name right))))

(defun emacs-agent-editor--document-move-retarget-error
    (runtime document registry buffer old-path new-path retarget-error)
  "Compensate a move retarget failure in RUNTIME after filesystem rename.
DOCUMENT and BUFFER still represent the move from OLD-PATH to NEW-PATH.
REGISTRY is updated to match the safest surviving identity.  RETARGET-ERROR
is the error raised while changing BUFFER's visited file name."
  (let (rollback-error buffer-error)
    (unless
        (and
         (file-exists-p old-path)
         (not (file-exists-p new-path)))
      (condition-case error-data
          (when (file-exists-p new-path)
            (rename-file new-path old-path))
        ((error quit)
         (setq rollback-error error-data))))
    (let* ((old-exists (file-exists-p old-path))
           (new-exists (file-exists-p new-path))
           (buffer-path
            (with-current-buffer buffer buffer-file-name))
           (rollback-succeeded
            (and old-exists (not new-exists)))
           (surviving-path
            (cond
             (rollback-succeeded old-path)
             ((and new-exists (not old-exists)) new-path)
             ((emacs-agent-editor--same-path-name-p
               buffer-path old-path)
              old-path)
             ((emacs-agent-editor--same-path-name-p
               buffer-path new-path)
              new-path)
             (old-exists old-path)
             (t new-path))))
      (condition-case error-data
          (with-current-buffer buffer
            (unless
                (emacs-agent-editor--same-path-name-p
                 buffer-file-name surviving-path)
              (let ((after-set-visited-file-name-hook nil))
                (set-visited-file-name surviving-path t t)))
            (set-buffer-modified-p nil))
        ((error quit)
         (setq buffer-error error-data)))
      (let* ((buffer-aligned
              (with-current-buffer buffer
                (emacs-agent-editor--same-path-name-p
                 buffer-file-name surviving-path)))
             (reconciliation-required
              (not (and rollback-succeeded buffer-aligned)))
             (rollback-guaranteed
              (and rollback-succeeded buffer-aligned))
             (rollback-error-message
              (and rollback-error
                   (error-message-string rollback-error)))
             (buffer-error-message
              (and buffer-error
                   (error-message-string buffer-error))))
        (remhash old-path registry)
        (remhash new-path registry)
        (setf
         (emacs-agent-document-canonical-path document) surviving-path
         (emacs-agent-document-disk-fingerprint document)
         (emacs-agent-document--disk-fingerprint surviving-path)
         (emacs-agent-document-modified document) nil
         (emacs-agent-document-externally-modified document) nil
         (emacs-agent-document-degraded document)
         (and reconciliation-required t))
        (puthash surviving-path document registry)
        (when reconciliation-required
          (setf
           (emacs-agent-runtime-health-state runtime)
           'degraded))
        (let ((event
               (list
                :tool "document_move"
                :status "partial_failure"
                :path old-path
                :new_path new-path
                :surviving_path surviving-path
                :checkpointed t
                :partial_completion t
                :reconciliation_required
                (and reconciliation-required t)
                :filesystem_rollback_succeeded
                (and rollback-succeeded t)
                :filesystem_rollback_guaranteed
                (and rollback-guaranteed t)
                :buffer_identity_restored
                (and buffer-aligned t)
                :rollback_error rollback-error-message
                :buffer_retarget_error buffer-error-message)))
          (emacs-agent-runtime-record-activity runtime event)
          (emacs-agent-journal-write runtime event))
        (emacs-agent-editor--tool-error
         'filesystem_error
         :path old-path
         :new_path new-path
         :surviving_path surviving-path
         :message
         (format
          "%s; filesystem rollback %s"
          (error-message-string retarget-error)
          (if rollback-guaranteed "succeeded" "failed"))
         :checkpointed t
         :partial_completion t
         :reconciliation_required
         (if reconciliation-required t :false)
         :filesystem_rollback_succeeded
         (if rollback-succeeded t :false)
         :filesystem_rollback_guaranteed
         (if rollback-guaranteed t :false)
         :buffer_identity_restored
         (if buffer-aligned t :false)
         :rollback_error rollback-error-message
         :buffer_retarget_error buffer-error-message)))))

(defun emacs-agent-editor--document-move (arguments context)
  "Implement `emacs_agent_document_move' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (source (emacs-agent-editor--resolve-target arguments))
            (destination
             (emacs-agent-editor--resolve-target
              arguments t 'new_path 'new_project_id))
            (old-absolute
             (emacs-agent-resolved-target-canonical-path source))
            (new-absolute
             (emacs-agent-resolved-target-canonical-path destination))
            (document (emacs-agent-document-open runtime source))
            (registry (emacs-agent-runtime-document-registry runtime))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (emacs-agent-editor--request-context context))
       (when (equal old-absolute new-absolute)
         (emacs-agent-editor--tool-error
          'invalid_argument :field 'new_path
          :message "Source and destination resolve to the same document"))
       (unless dry-run
         (emacs-agent-editor--authorize runtime "document_move" arguments)
         (emacs-agent-editor--require-lifecycle-checkpoint
          runtime arguments))
       (emacs-agent-document-reconcile document)
       (when (file-exists-p new-absolute)
         (emacs-agent-editor--tool-error
          'document_already_exists :path new-absolute))
       (unless
           (equal (emacs-agent-document-revision document)
                  (emacs-agent-editor--argument arguments 'expected_revision))
         (emacs-agent-editor--tool-error
          'revision_conflict :path old-absolute :requires_reread t))
       (let* ((buffer (emacs-agent-document-buffer document))
              (before
               (with-current-buffer buffer
                 (save-restriction
                   (widen)
                   (buffer-substring-no-properties
                    (point-min) (point-max)))))
              (base (emacs-agent-document-revision document)))
         (if dry-run
             (emacs-agent-editor--plist-to-alist
              (append
               (emacs-agent-document-output-fields destination)
               (list
                :old_path old-absolute
                :old_project_id
                (emacs-agent-resolved-target-project-id source)
                :old_relative_path
                (emacs-agent-resolved-target-relative-path source)
                :new_path new-absolute
                :old_revision base
                :new_revision base
                :changeset_id nil
                :applied nil
                :modified t
                :checkpointed nil
                :diff
                (concat
                 (emacs-agent-changeset--diff-text
                  old-absolute before "")
                 (emacs-agent-changeset--diff-text
                  new-absolute "" before))
                :truncated nil
                :diff_truncated nil)))
           (emacs-agent-runtime-enqueue-mutation
            runtime
            (lambda ()
              (let* ((current-source
                      (emacs-agent-project-resolve-target
                       runtime
                       (emacs-agent-resolved-target-input-path source)
                       :project-id
                       (emacs-agent-resolved-target-project-id source)))
                     (current-destination
                      (emacs-agent-project-resolve-target
                       runtime
                       (emacs-agent-resolved-target-input-path destination)
                       :project-id
                       (emacs-agent-resolved-target-project-id destination)
                       :for-create t))
                     (current-old
                      (emacs-agent-resolved-target-canonical-path
                       current-source))
                     (current-new
                      (emacs-agent-resolved-target-canonical-path
                       current-destination)))
                (unless (and (equal current-old old-absolute)
                             (equal current-new new-absolute))
                  (emacs-agent-editor--tool-error
                   'external_change_conflict
                   :path old-absolute
                   :reason 'target_identity_changed))
                (when (file-exists-p current-new)
                  (emacs-agent-editor--tool-error
                   'document_already_exists :path current-new))
                (emacs-agent-document-reconcile document)
                (unless
                    (equal base
                           (emacs-agent-document-revision document))
                  (emacs-agent-editor--tool-error
                   'revision_conflict :path current-old
                   :requires_reread t))
                (condition-case error-data
                    (emacs-agent-document-checkpoint document)
                  (emacs-agent-error
                   (let ((code
                          (emacs-agent-error-code error-data))
                         (details
                          (emacs-agent-error-details error-data)))
                     (when (or
                            (eq code 'save_failed)
                            (plist-get details
                                       :reconciliation_required))
                       (setf
                        (emacs-agent-document-degraded document) t
                        (emacs-agent-runtime-health-state runtime)
                        'degraded)))
                   (signal (car error-data) (cdr error-data)))
                  (error
                   (setf
                    (emacs-agent-document-degraded document) t
                    (emacs-agent-runtime-health-state runtime)
                    'degraded)
                   (emacs-agent-editor--tool-error
                    'save_failed :path current-old
                    :message (error-message-string error-data)
                    :reconciliation_required t
                    :filesystem_rollback_guaranteed nil)))
                (let
                    ((refreshed
                      (condition-case error-data
                          (list
                           (emacs-agent-project-resolve-target
                            runtime
                            (emacs-agent-resolved-target-input-path
                             source)
                            :project-id
                            (emacs-agent-resolved-target-project-id
                             source))
                           (emacs-agent-project-resolve-target
                            runtime
                            (emacs-agent-resolved-target-input-path
                             destination)
                            :project-id
                            (emacs-agent-resolved-target-project-id
                             destination)
                            :for-create t))
                        ((emacs-agent-error file-error)
                         (emacs-agent-editor--tool-error
                          'external_change_conflict
                          :path current-old
                          :new_path current-new
                          :reason 'target_identity_changed
                          :checkpointed t
                          :partial_completion t
                          :reconciliation_required :false
                          :message
                          (format
                           "Move target changed during checkpoint: %s"
                           (error-message-string error-data)))))))
                  (setq
                   current-source (car refreshed)
                   current-destination (cadr refreshed))
                  (let ((refreshed-old
                         (emacs-agent-resolved-target-canonical-path
                          current-source))
                        (refreshed-new
                         (emacs-agent-resolved-target-canonical-path
                          current-destination)))
                    (unless
                        (and
                         (equal refreshed-old current-old)
                         (equal refreshed-new current-new))
                      (emacs-agent-editor--tool-error
                       'external_change_conflict
                       :path current-old
                       :new_path current-new
                       :reason 'target_identity_changed
                       :checkpointed t
                       :partial_completion t
                       :reconciliation_required :false))
                    (setq
                     current-old refreshed-old
                     current-new refreshed-new)))
                (when (file-exists-p current-new)
                  (emacs-agent-editor--tool-error
                   'document_already_exists :path current-new))
                (condition-case error-data
                    (rename-file current-old current-new)
                  ((error quit)
                   (setf
                    (emacs-agent-document-degraded document) t
                    (emacs-agent-runtime-health-state runtime)
                    'degraded)
                   (let ((event
                          (list
                           :tool "document_move"
                           :status "partial_failure"
                           :path current-old
                           :new_path current-new
                           :checkpointed t
                           :reconciliation_required t)))
                     (emacs-agent-runtime-record-activity runtime event)
                     (emacs-agent-journal-write runtime event))
                   (emacs-agent-editor--tool-error
                    'filesystem_error
                    :path current-old
                    :new_path current-new
                    :message (error-message-string error-data)
                    :checkpointed t
                    :partial_completion t
                    :reconciliation_required t
                    :filesystem_rollback_guaranteed :false)))
                (condition-case error-data
                    (with-current-buffer buffer
                      (set-visited-file-name current-new t t)
                      (set-buffer-modified-p nil))
                  ((error quit)
                   (emacs-agent-editor--document-move-retarget-error
                    runtime document registry buffer
                    current-old current-new error-data)))
                (remhash current-old registry)
                (setf
                 (emacs-agent-document-canonical-path document) current-new
                 (emacs-agent-document-disk-fingerprint document)
                 (emacs-agent-document--disk-fingerprint current-new))
                (puthash current-new document registry)
                (let* ((revision
                        (emacs-agent-document-revision document))
                       (changeset-id
                        (emacs-agent-editor--record-lifecycle
                         runtime
                         (list
                          :type 'move
                          :from current-old
                          :to current-new)
                         (list current-old current-new)
                         (list
                          (cons
                           current-old
                           (list :exists t :content before))
                          (cons current-new (list :exists nil)))
                         (list (cons current-old base))
                         (list
                          (cons current-old nil)
                          (cons current-new revision))
                         t)))
                  (emacs-agent-editor--plist-to-alist
                   (append
                    (emacs-agent-document-output-fields
                     current-destination)
                    (list
                     :old_path current-old
                     :old_project_id
                     (emacs-agent-resolved-target-project-id
                      current-source)
                     :old_relative_path
                     (emacs-agent-resolved-target-relative-path
                      current-source)
                     :new_path current-new
                     :old_revision base
                     :changeset_id changeset-id
                     :new_revision revision
                     :applied t
                     :modified t
                     :checkpointed t
                     :diff
                     (emacs-agent-changeset-diff
                      runtime changeset-id)
                     :truncated nil
                     :diff_truncated nil)))))))))))))

(defun emacs-agent-editor--document-delete (arguments context)
  "Implement `emacs_agent_document_delete' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target (emacs-agent-editor--resolve-target arguments))
            (path (emacs-agent-resolved-target-canonical-path target))
            (document (emacs-agent-document-open runtime target))
            (revision (emacs-agent-document-revision document))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (emacs-agent-editor--request-context context))
       (unless dry-run
         (emacs-agent-editor--authorize runtime "document_delete" arguments)
         (emacs-agent-editor--require-lifecycle-checkpoint
          runtime arguments))
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
         (if dry-run
             (emacs-agent-editor--plist-to-alist
              (append
               (emacs-agent-document-output-fields target)
               (list
                :old_revision revision
                :new_revision nil
                :changeset_id nil
                :deleted nil
                :applied nil
                :modified t
                :checkpointed nil
                :diff
                (emacs-agent-changeset--diff-text path content "")
                :truncated nil
                :diff_truncated nil)))
           (emacs-agent-runtime-enqueue-mutation
            runtime
            (lambda ()
              (let* ((current-target
                      (emacs-agent-project-resolve-target
                       runtime
                       (emacs-agent-resolved-target-input-path target)
                       :project-id
                       (emacs-agent-resolved-target-project-id target)))
                     (current-path
                      (emacs-agent-resolved-target-canonical-path
                       current-target)))
                (unless (equal current-path absolute)
                  (emacs-agent-editor--tool-error
                   'external_change_conflict :path path
                   :reason 'target_identity_changed))
                (emacs-agent-document-reconcile document)
                (unless
                    (equal
                     revision
                     (emacs-agent-document-revision document))
                  (emacs-agent-editor--tool-error
                   'revision_conflict :path path :requires_reread t))
                (when (file-exists-p current-path)
                  (delete-file current-path))
                (remhash
                 current-path
                 (emacs-agent-runtime-document-registry runtime))
                (with-current-buffer buffer
                  (set-buffer-modified-p nil))
                (kill-buffer buffer)
                (let ((changeset-id
                       (emacs-agent-editor--record-lifecycle
                        runtime
                        (list :type 'delete :path current-path)
                        (list current-path)
                        (list
                         (cons
                          current-path
                          (list :exists t :content content)))
                        (list (cons current-path revision))
                        (list (cons current-path nil))
                        t)))
                  (emacs-agent-editor--plist-to-alist
                   (append
                    (emacs-agent-document-output-fields
                     current-target)
                    (list
                     :changeset_id changeset-id
                     :old_revision revision
                     :new_revision nil
                     :deleted t
                     :applied t
                     :modified t
                     :checkpointed t
                     :diff
                     (emacs-agent-changeset-diff
                      runtime changeset-id)
                     :truncated nil
                     :diff_truncated nil)))))))))))))

(defun emacs-agent-editor--editor-checkpoint (arguments context)
  "Implement `emacs_agent_editor_checkpoint' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (entries
             (append
              (emacs-agent-editor--argument arguments 'documents)
              nil))
            (emacs-agent-editor--request-context context))
       (emacs-agent-editor--authorize runtime "editor_checkpoint" arguments)
       (let ((seen (make-hash-table :test #'equal))
             resolved before-snapshots base-revisions)
         (dolist (entry entries)
           (let* ((target
                   (emacs-agent-project-resolve-target
                    runtime
                    (alist-get 'path entry)
                    :project-id
                    (emacs-agent-editor--optional-id entry 'project_id)))
                  (path
                   (emacs-agent-resolved-target-canonical-path target))
                  (document (emacs-agent-document-open runtime target))
                  (_ (emacs-agent-document-reconcile document))
                  (revision (emacs-agent-document-revision document)))
             (when (gethash path seen)
               (emacs-agent-editor--tool-error
                'duplicate_document :path path))
             (puthash path t seen)
             (unless (equal revision (alist-get 'expected_revision entry))
               (emacs-agent-editor--tool-error
                'revision_conflict :path path :current_revision revision
                :requires_reread t))
             (push
              (list
               :target target
               :document document
               :path path
               :revision revision)
              resolved)
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
         (emacs-agent-runtime-enqueue-mutation
          runtime
          (lambda ()
            (let (results final-revisions saving-entry any-modified)
              (condition-case error-data
                  (dolist (resolved-entry resolved)
                    (setq saving-entry resolved-entry)
                    (let* ((target
                            (plist-get resolved-entry :target))
                           (document
                            (plist-get resolved-entry :document))
                           (path
                            (plist-get resolved-entry :path))
                           (base
                            (plist-get resolved-entry :revision))
                           (before
                            (plist-get
                             (cdr (assoc path before-snapshots))
                             :content))
                           (current-target
                            (emacs-agent-project-resolve-target
                             runtime
                             (emacs-agent-resolved-target-input-path target)
                             :project-id
                             (emacs-agent-resolved-target-project-id
                              target))))
                      (unless
                          (equal
                           path
                           (emacs-agent-resolved-target-canonical-path
                            current-target))
                        (emacs-agent-editor--tool-error
                         'external_change_conflict :path path
                         :reason 'target_identity_changed))
                      (emacs-agent-document-reconcile document)
                      (unless
                          (equal
                           (emacs-agent-document-revision document)
                           base)
                        (emacs-agent-editor--tool-error
                         'revision_conflict :path path
                         :requires_reread t))
                      (emacs-agent-document-checkpoint document)
                      (let* ((revision
                              (emacs-agent-document-revision document))
                             (after
                              (emacs-agent-document--buffer-content
                               (emacs-agent-document-buffer document)))
                             (modified (not (equal before after))))
                        (when modified
                          (setq any-modified t))
                        (push
                         (append
                          (emacs-agent-document-output-fields target)
                          (list
                           :old_revision base
                           :new_revision revision
                           :revision revision
                           :applied t
                           :checkpointed t
                           :modified (and modified t)
                           :diff
                           (emacs-agent-changeset--diff-text
                            path before after)
                           :truncated nil
                           :diff_truncated nil))
                         results)
                        (push
                         (cons path revision)
                         final-revisions))))
                (emacs-agent-error
                 (let* ((code
                         (emacs-agent-error-code error-data))
                        (details
                         (copy-sequence
                          (emacs-agent-error-details error-data)))
                        (partial
                         (or
                          results
                          (plist-get details :partial_completion)))
                        (degraded
                         (or
                          results
                          (eq code 'save_failed)
                          (plist-get
                           details :reconciliation_required))))
                   (when degraded
                     (setf
                      (emacs-agent-runtime-health-state runtime)
                      'degraded)
                     (when saving-entry
                       (setf
                        (emacs-agent-document-degraded
                         (plist-get saving-entry :document))
                        t)))
                   (setq
                    details
                    (plist-put
                     details :partial_completion
                     (and partial t)))
                   (setq
                    details
                    (plist-put
                     details :checkpointed
                     (vconcat
                      (mapcar
                       #'emacs-agent-editor--plist-to-alist
                       (nreverse
                        (copy-sequence results))))))
                   (apply
                    #'emacs-agent-editor--tool-error
                    code details)))
                (error
                 (setf
                  (emacs-agent-runtime-health-state runtime)
                  'degraded)
                 (when saving-entry
                   (setf
                    (emacs-agent-document-degraded
                     (plist-get saving-entry :document))
                    t))
                 (emacs-agent-editor--tool-error
                  'save_failed
                  :message (error-message-string error-data)
                  :reconciliation_required t
                  :partial_completion (and results t)
                  :checkpointed
                  (vconcat
                   (mapcar
                    #'emacs-agent-editor--plist-to-alist
                    (nreverse (copy-sequence results)))))))
              (let ((changeset-id
                     (emacs-agent-editor--record-lifecycle
                      runtime
                      (list :type 'checkpoint)
                      (mapcar #'car base-revisions)
                      (nreverse before-snapshots)
                      (nreverse base-revisions)
                      (nreverse final-revisions)
                      t)))
                (let* ((documents
                        (vconcat
                         (mapcar
                          #'emacs-agent-editor--plist-to-alist
                          (nreverse results))))
                       (single (and (= (length documents) 1)
                                    (aref documents 0))))
                  `((old_revision
                     . ,(if single
                            (alist-get 'old_revision single)
                          :false))
                    (new_revision
                     . ,(if single
                            (alist-get 'new_revision single)
                          :false))
                    (changeset_id . ,changeset-id)
                    (applied . t) (checkpointed . t)
                    (modified . ,(if any-modified t :false))
                    (diff
                     . ,(emacs-agent-changeset-diff
                         runtime changeset-id))
                    (truncated . :false)
                    (diff_truncated . :false)
                    (documents . ,documents))))))))))))

(defun emacs-agent-editor--editor-sync (arguments _context)
  "Implement `emacs_agent_editor_sync' for ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (requested
             (append
              (emacs-agent-editor--argument arguments 'documents)
              nil))
            (documents
             (if requested
                 (mapcar
                  (lambda (entry)
                    (let ((target
                           (emacs-agent-project-resolve-target
                            runtime
                            (alist-get 'path entry)
                            :project-id
                            (emacs-agent-editor--optional-id
                             entry 'project_id))))
                      (cons
                       target
                       (emacs-agent-document-open runtime target))))
                  requested)
               (let (items)
                 (maphash
                  (lambda (path document)
                    (push
                     (cons
                      (emacs-agent-project-resolve-target
                       runtime path)
                      document)
                     items))
                  (emacs-agent-runtime-document-registry runtime))
                 (nreverse items))))
            results)
       (emacs-agent-runtime-enqueue-mutation
        runtime
        (lambda ()
          (dolist (entry documents)
            (let ((target (car entry))
                  (document (cdr entry)))
            (condition-case error-data
                (progn
                  (emacs-agent-document-reconcile document)
                  (push
                   (emacs-agent-editor--plist-to-alist
                    (append
                     (emacs-agent-document-output-fields target)
                     (list
                      :status "synchronized"
                      :revision
                      (emacs-agent-document-revision document))))
                   results))
              (emacs-agent-error
               (push
                (emacs-agent-editor--plist-to-alist
                 (append
                  (emacs-agent-document-output-fields target)
                  (list
                   :status "conflicted"
                   :code
                   (symbol-name
                    (emacs-agent-error-code error-data)))))
                results)))))
          (when (cl-every
                 (lambda (entry)
                   (equal (alist-get 'status entry) "synchronized"))
                 results)
            (setf (emacs-agent-runtime-health-state runtime) 'healthy))
          `((documents . ,(vconcat (nreverse results))))))))))

(defun emacs-agent-editor--document-status (arguments _context)
  "Implement `emacs_agent_document_status' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
     (emacs-agent-document-status
       (emacs-agent-runtime-current)
       (emacs-agent-editor--resolve-target arguments))))))

(defun emacs-agent-editor--editor-modified-documents
    (_arguments _context)
  "Implement `emacs_agent_editor_modified_documents'."
  (emacs-agent-editor--call
   (lambda ()
     `((documents
        . ,(vconcat
            (mapcar
             #'emacs-agent-editor--plist-to-alist
             (emacs-agent-document-modified-documents
              (emacs-agent-runtime-current)))))))))

(defun emacs-agent-editor--changeset-list (arguments _context)
  "Implement `emacs_agent_changeset_list' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (path (emacs-agent-editor--argument arguments 'path))
            (target
             (and
              path
              (emacs-agent-editor--resolve-target arguments t))))
       (emacs-agent-editor--plist-to-alist
        (emacs-agent-changeset-query
         runtime
         :path
         (and
          target
          (emacs-agent-resolved-target-canonical-path target))
       :statuses
       (mapcar
        #'intern
        (append
         (emacs-agent-editor--argument arguments 'status) nil))
       :limit
       (or (emacs-agent-editor--argument arguments 'limit) 50)
         :cursor
         (emacs-agent-editor--argument arguments 'cursor)))))))

(defun emacs-agent-editor--changeset-get (arguments _context)
  "Implement `emacs_agent_changeset_get' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-changeset-detail
       (emacs-agent-runtime-current)
       (emacs-agent-editor--argument arguments 'changeset_id)
       :max-chars
       (or (emacs-agent-editor--argument arguments 'max_chars)
           (* 256 1024))
       :cursor (emacs-agent-editor--argument arguments 'cursor))))))

(defun emacs-agent-editor--document-diagnostics (arguments _context)
  "Implement `emacs_agent_document_diagnostics' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
     (emacs-agent-document-diagnostics
       (emacs-agent-runtime-current)
       (emacs-agent-editor--resolve-target arguments)
       :expected-revision
       (emacs-agent-editor--argument arguments 'expected_revision)
       :sources
       (append
        (emacs-agent-editor--argument arguments 'sources) nil)
       :wait-ms
       (or (emacs-agent-editor--argument arguments 'wait_ms) 3000))))))

(defun emacs-agent-editor--project-diagnostics (arguments _context)
  "Implement `emacs_agent_project_diagnostics' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
     (emacs-agent-project-diagnostics
       (emacs-agent-runtime-current)
       (emacs-agent-editor--argument arguments 'project_id)
       :paths
       (append (emacs-agent-editor--argument arguments 'paths) nil)
       :include-globs
       (append
        (emacs-agent-editor--argument arguments 'include_globs) nil)
       :exclude-globs
       (append
        (emacs-agent-editor--argument arguments 'exclude_globs) nil)
       :severities
       (append
        (emacs-agent-editor--argument arguments 'severities) nil)
       :sources
       (append
        (emacs-agent-editor--argument arguments 'sources) nil)
       :wait-ms
       (or (emacs-agent-editor--argument arguments 'wait_ms) 3000)
       :limit
       (or (emacs-agent-editor--argument arguments 'limit) 100)
       :cursor (emacs-agent-editor--argument arguments 'cursor))))))

(defun emacs-agent-editor--document-symbols (arguments _context)
  "Implement `emacs_agent_document_symbols' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target (emacs-agent-editor--resolve-target arguments)))
       (emacs-agent-editor--json-value
        (append
         (emacs-agent-editor--plist-to-alist
          (emacs-agent-document-output-fields target))
         `((symbols
            . ,(vconcat
                (emacs-agent-semantic-document-symbols
                 runtime target))))))))))

(defun emacs-agent-editor--project-symbols (arguments context)
  "Implement `emacs_agent_project_symbols' with ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let ((project-id
            (emacs-agent-editor--argument arguments 'project_id)))
       (emacs-agent-editor--json-value
        (append
         `((project_id . ,project-id))
         (emacs-agent-project-symbols
          (emacs-agent-runtime-current)
          project-id
          (emacs-agent-editor--argument arguments 'path)
          (emacs-agent-editor--argument arguments 'query)
          (emacs-agent-editor--argument arguments 'kind)
          (emacs-agent-editor--argument arguments 'path_prefix)
          (emacs-agent-editor--argument arguments 'limit)
          context)))))))

(defun emacs-agent-editor--symbol-definition (arguments context)
  "Implement `emacs_agent_symbol_definition' with ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--json-value
      `((definitions
         . ,(vconcat
             (emacs-agent-semantic-definition
              (emacs-agent-runtime-current)
              (emacs-agent-editor--resolve-target arguments)
              (emacs-agent-editor--argument arguments 'position)
              (emacs-agent-editor--argument arguments 'symbol)
              context))))))))

(defun emacs-agent-editor--symbol-references (arguments context)
  "Implement `emacs_agent_symbol_references' with ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--json-value
      (emacs-agent-semantic-references
       (emacs-agent-runtime-current)
       (emacs-agent-editor--resolve-target arguments)
       (emacs-agent-editor--argument arguments 'position)
       (emacs-agent-editor--argument arguments 'symbol)
       context)))))

(defun emacs-agent-editor--editor-context-get (arguments _context)
  "Implement `emacs_agent_editor_context_get' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--json-value
      (emacs-agent-semantic-editor-context
       (emacs-agent-runtime-current)
       nil
       (emacs-agent-editor--optional-id arguments 'project_id))))))

(defun emacs-agent-editor--format-document (arguments context)
  "Implement guarded `emacs_agent_format_document' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target (emacs-agent-editor--resolve-target arguments))
            (revision
             (emacs-agent-editor--argument arguments 'expected_revision))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t)))
       (if dry-run
           (let* ((preview
                   (emacs-agent-semantic-format-preview
                    runtime target revision))
                  (changed (alist-get 'changed preview)))
             (emacs-agent-editor--json-value
              (append
               preview
               `((old_revision . ,revision)
                 (new_revision . ,revision)
                 (changeset_id . :false)
                 (applied . :false)
                 (checkpointed . :false)
                 (modified . ,changed)
                 (truncated . :false)
                 (diff_truncated . :false)))))
         (let* ((checkpoint
                 (emacs-agent-editor--checkpoint-p
                  runtime
                  (emacs-agent-editor--argument arguments 'checkpoint)))
                (emacs-agent-editor--request-context context)
                (emacs-agent-editor--operation-name "format_document")
                (_
                 (emacs-agent-editor--authorize
                  runtime "format_document" arguments))
                (result
                 (emacs-agent-runtime-enqueue-mutation
                  runtime
                  (lambda ()
                    (emacs-agent-semantic-format-apply
                     runtime target revision checkpoint)))))
           (emacs-agent-editor--plist-to-alist
            (append
             result
             (list
              :old_revision
              (or (plist-get result :previous_revision) revision)
              :applied t
              :modified (plist-get result :changed)
              :truncated nil
              :diff_truncated nil)))))))))

(defun emacs-agent-editor--symbol-rename (arguments context)
  "Preview or apply `emacs_agent_symbol_rename' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target
             (and
              (eq
               (emacs-agent-editor--argument arguments 'dry_run)
               t)
              (emacs-agent-editor--resolve-target arguments)))
            (preview-id
             (emacs-agent-editor--argument arguments 'preview_id))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t)))
       (if dry-run
           (emacs-agent-editor--json-value
            (emacs-agent-semantic-rename-preview
             runtime target
             (emacs-agent-editor--argument arguments 'position)
             (emacs-agent-editor--argument arguments 'new_name)
             (emacs-agent-editor--argument arguments 'expected_revision)))
         (unless (stringp preview-id)
           (emacs-agent-editor--tool-error
            'invalid_argument
            :field 'preview_id
            :message "A prior dry-run preview_id is required"))
         (emacs-agent-editor--plist-to-alist
          (emacs-agent-semantic-rename-apply
           runtime preview-id
           (emacs-agent-editor--checkpoint-p
            runtime
            (emacs-agent-editor--argument arguments 'checkpoint))
           context)))))))

(defun emacs-agent-editor--code-actions (arguments context)
  "List or safely apply `emacs_agent_code_actions' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (action-id
             (emacs-agent-editor--argument arguments 'action_id)))
       (if action-id
           (emacs-agent-editor--plist-to-alist
            (emacs-agent-semantic-code-action-apply
             runtime action-id
             (emacs-agent-editor--checkpoint-p
              runtime
              (emacs-agent-editor--argument arguments 'checkpoint))
             context))
         (emacs-agent-editor--json-value
          (emacs-agent-semantic-code-actions
           runtime
           (emacs-agent-editor--resolve-target arguments)
           (emacs-agent-editor--argument arguments 'range)
           (emacs-agent-editor--argument arguments 'expected_revision)
           (emacs-agent-editor--argument arguments 'kind))))))))

(defun emacs-agent-editor--format-range (arguments context)
  "Preview or apply `emacs_agent_format_range' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (target
             (and
              (eq
               (emacs-agent-editor--argument arguments 'dry_run)
               t)
              (emacs-agent-editor--resolve-target arguments)))
            (preview-id
             (emacs-agent-editor--argument arguments 'preview_id))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t)))
       (if dry-run
           (emacs-agent-editor--json-value
            (emacs-agent-semantic-format-range-preview
             runtime target
             (emacs-agent-editor--argument arguments 'range)
             (emacs-agent-editor--argument arguments 'expected_revision)))
         (unless (stringp preview-id)
           (emacs-agent-editor--tool-error
            'invalid_argument
            :field 'preview_id
            :message "A prior dry-run preview_id is required"))
         (emacs-agent-editor--authorize
          runtime "format_range" arguments)
         (emacs-agent-editor--plist-to-alist
          (emacs-agent-semantic-format-range-apply
           runtime preview-id
           (emacs-agent-editor--checkpoint-p
            runtime
            (emacs-agent-editor--argument arguments 'checkpoint))
           context)))))))

(defun emacs-agent-editor--approval-status (arguments _context)
  "Implement `emacs_agent_approval_status' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-runtime-approval-status
       (emacs-agent-runtime-current)
       (emacs-agent-editor--argument arguments 'approval_request_id))))))

(defun emacs-agent-editor--approval-cancel (arguments _context)
  "Implement `emacs_agent_approval_cancel' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-runtime-approval-cancel
       (emacs-agent-runtime-current)
       (emacs-agent-editor--argument arguments 'approval_request_id))))))

(defun emacs-agent-editor--editor-diff (arguments _context)
  "Implement `emacs_agent_editor_diff' for ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((runtime (emacs-agent-runtime-current))
            (cursor (emacs-agent-editor--argument arguments 'cursor))
            (max-chars
             (or (emacs-agent-editor--argument arguments 'max_chars)
                 (* 256 1024)))
            state diff offset changeset-id)
       (if cursor
           (progn
             (setq state (gethash cursor emacs-agent-editor--diff-cursors))
             (unless (and state
                          (equal
                           (plist-get state :runtime-instance-id)
                           (emacs-agent-runtime-instance-id runtime))
                          (> (plist-get state :expires-at) (float-time)))
               (remhash cursor emacs-agent-editor--diff-cursors)
               (emacs-agent-editor--tool-error 'invalid_cursor))
             (setq diff (plist-get state :diff)
                   offset (plist-get state :offset)
                   changeset-id (plist-get state :changeset-id))
             (remhash cursor emacs-agent-editor--diff-cursors))
         (setq changeset-id
               (emacs-agent-editor--argument arguments 'changeset_id)
               diff (emacs-agent-changeset-diff runtime changeset-id)
               offset 0))
       (let* ((end (min (length diff) (+ offset max-chars)))
              (truncated (< end (length diff)))
              next)
         (when truncated
           (setq next (concat "diff_" (emacs-agent-editor--random-token)))
           (puthash
            next
            (list
                  :runtime-instance-id
                  (emacs-agent-runtime-instance-id runtime)
                  :diff diff :offset end :changeset-id changeset-id
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
     (let ((runtime (emacs-agent-runtime-current))
           (dry-run
            (eq (emacs-agent-editor--argument arguments 'dry_run) t))
           (changeset-id
            (emacs-agent-editor--argument arguments 'changeset_id)))
       (unless dry-run
         (emacs-agent-editor--authorize
          runtime "changeset_rollback" arguments))
       (let* ((target (emacs-agent-changeset-get runtime changeset-id))
              (rollback
               (emacs-agent-changeset--rollback-status runtime target))
              (_
               (unless (plist-get rollback :available)
                 (emacs-agent-editor--tool-error
                  'rollback_conflict
                  :message "Change set is not currently rollback-compatible"
                  :reason (plist-get rollback :reason))))
              (old-revisions
               (emacs-agent-changeset-final-revisions target))
              (diff
               (mapconcat
                (lambda (entry)
                  (emacs-agent-changeset--diff-text
                   (car entry)
                   (emacs-agent-changeset--current-content
                    runtime (car entry))
                   (emacs-agent-changeset--snapshot-content (cdr entry))))
                (emacs-agent-changeset-before-snapshots target)
                ""))
              (changeset
               (unless dry-run
                 (emacs-agent-changeset-rollback runtime changeset-id)))
              (new-revisions
               (if dry-run
                   (mapcar
                    (lambda (entry)
                      `((path . ,(car entry)) (revision . :false)))
                    (emacs-agent-changeset-before-snapshots target))
                 (mapcar
                  (lambda (path)
                    `((path . ,path)
                      (revision
                       . ,(emacs-agent-changeset--revision runtime path))))
                  (emacs-agent-changeset-touched-documents changeset))))
              (documents
               (vconcat
                (mapcar
                 (lambda (entry)
                   (let* ((path (alist-get 'path entry))
                          (resolved
                           (emacs-agent-project-resolve-target
                            runtime path :for-create t))
                          (new-revision (alist-get 'revision entry))
                          (old-revision (cdr (assoc path old-revisions))))
                     (emacs-agent-editor--plist-to-alist
                      (append
                       (emacs-agent-document-output-fields resolved)
                       (list
                        :old_revision old-revision
                        :new_revision new-revision
                        :applied (if dry-run :false t)
                        :checkpointed
                        (if (and
                             (not dry-run)
                             (eq
                              (emacs-agent-runtime-save-policy runtime)
                              'immediate))
                            t :false)
                        :modified t :diff ""
                        :truncated :false
                        :diff_truncated :false)))))
                 new-revisions)))
              (single (and (= (length documents) 1)
                           (aref documents 0))))
         `((changeset_id . ,changeset-id)
           (old_revision
            . ,(if single (alist-get 'old_revision single) :false))
           (new_revision
            . ,(if single (alist-get 'new_revision single) :false))
           (old_revisions
            . ,(vconcat
                (mapcar
                 (lambda (entry)
                   `((path . ,(car entry)) (revision . ,(cdr entry))))
                 old-revisions)))
           (new_revisions . ,(vconcat new-revisions))
           (applied . ,(if dry-run :false t))
           (modified . t)
           (checkpointed
            . ,(if (and
                     (not dry-run)
                     (eq (emacs-agent-runtime-save-policy runtime)
                         'immediate))
                   t :false))
           (diff . ,diff)
           (truncated . :false)
           (diff_truncated . :false)
           (documents . ,documents)
           (status
            . ,(if dry-run
                   "preview"
                 (symbol-name
                  (emacs-agent-changeset-status changeset))))))))))

(defun emacs-agent-editor--object-schema (properties &optional required)
  "Return an object schema with PROPERTIES and REQUIRED names."
  `((type . "object")
    (properties . ,properties)
    (additionalProperties . :false)
    ,@(when required `((required . ,(vconcat required))))))

(defun emacs-agent-editor--register-tools ()
  "Register the exact Agent Editor MCP v0.3 tool surface."
  (emacs-agent-tool-clear)
  (let* ((string '((type . "string")))
         (integer '((type . "integer")))
         (boolean '((type . "boolean")))
         (position
          (emacs-agent-editor--object-schema
           `((line
              . ((type . "integer") (minimum . 1)
                 (description
                  . "One-based logical line in the authoritative buffer.")))
             (column
              . ((type . "integer") (minimum . 0)
                 (description
                  . "Zero-based Emacs character offset; tabs count as one."))))
           '("line" "column")))
         (range
          (emacs-agent-editor--object-schema
           `((start . ,position) (end . ,position))
           '("start" "end")))
         (edit
          (emacs-agent-editor--object-schema
           `((start . ,position) (end . ,position)
             (new_text . ,string) (expected_text . ,string))
           '("start" "end" "new_text")))
         (replace-edit
          (emacs-agent-editor--object-schema
           `((old_text . ,string) (new_text . ,string)
             (replace_all . ,boolean)
             (expected_occurrences . ,integer))
           '("old_text" "new_text")))
         (transaction-document
          (emacs-agent-editor--object-schema
           `((path . ,string) (project_id . ,string)
             (expected_revision . ,string)
             (edits . ((type . "array") (items . ,replace-edit)))
             (patch . ,string))
           '("path" "expected_revision")))
         (document-guard
          (emacs-agent-editor--object-schema
           `((path . ,string) (project_id . ,string)
             (expected_revision . ,string))
           '("path" "expected_revision")))
         (falseable-string '((type . ["string" "boolean"])))
         (falseable-id '((type . ["string" "integer" "boolean"])))
         (target-properties
          `((path . ,string)
            (project_id . ,falseable-string)
            (relative_path . ,falseable-string)))
         (target-required
          '("path" "project_id" "relative_path"))
         (target-object
          (emacs-agent-editor--object-schema
           target-properties target-required))
         (target-array
          `((type . "array") (items . ,target-object)))
         (agent-identity
          (emacs-agent-editor--object-schema
           `((name . ,string) (version . ,string))
           '("name" "version")))
         (falseable-agent-identity
          `((type . ["object" "boolean"])
            (properties . ,(alist-get 'properties agent-identity))
            (additionalProperties . :false)
            (required . ,(alist-get 'required agent-identity))))
         (string-array `((type . "array") (items . ,string)))
         (object-array
          '((type . "array") (items . ((type . "object")))))
         (revision-item
          (emacs-agent-editor--object-schema
           `((path . ,string) (revision . ,falseable-string))
           '("path" "revision")))
         (revision-array
          `((type . "array") (items . ,revision-item)))
         (diagnostic-item
          (emacs-agent-editor--object-schema
           (append
            target-properties
            `((source . ,string) (severity . ,string)
             (code . ,falseable-string) (message . ,string)
             (range . ((type . ["object" "boolean"])))
             (revision . ,string) (stale . ,boolean)
             (related_information . ,object-array)
              (action_id . ,falseable-string)))
           '("source" "severity" "message" "range")))
         (diagnostic-array
          `((type . "array") (items . ,diagnostic-item)))
         (project-diagnostic-item
          (emacs-agent-editor--object-schema
           (alist-get 'properties diagnostic-item)
           (append
            target-required
            '("source" "severity" "message" "range"))))
         (project-diagnostic-array
          `((type . "array") (items . ,project-diagnostic-item)))
         (diagnostic-document
          (emacs-agent-editor--object-schema
           (append
            target-properties
            `((document_revision . ,string)
              (diagnostics_revision . ,string)
              (providers . ,string-array)
              (pending . ,boolean) (stale . ,boolean)
              (diagnostics . ,diagnostic-array)))
           (append
            target-required
            '("document_revision" "diagnostics_revision"
              "providers" "pending" "stale" "diagnostics"))))
         (diagnostic-document-array
          `((type . "array") (items . ,diagnostic-document)))
         (write-properties
          (append
           target-properties
           `((old_path . ,string)
            (old_project_id . ,falseable-string)
            (old_relative_path . ,falseable-string)
            (new_path . ,string)
            (old_revision . ,falseable-string)
            (new_revision . ,falseable-string)
            (previous_revision . ,falseable-string)
            (old_revisions . ,revision-array)
            (new_revisions . ,revision-array)
            (changeset_id . ,falseable-string)
            (applied . ,boolean)
            (checkpointed . ,boolean)
            (modified
             . ((type . "boolean")
                (description
                 . "Whether authoritative content differs because of this operation.")))
            (documents . ,object-array)
            (diff . ,string)
            (truncated . ,boolean)
            (diff_truncated . ,boolean)
            (revision . ,falseable-string)
            (edit_count . ,integer)
            (diff_summary
             . ((type . "object")
                (properties
                 . ((before_chars . ((type . "integer")))
                    (after_chars . ((type . "integer")))))
                (additionalProperties . :false)
                (required . ["before_chars" "after_chars"])))
            (diagnostics_state . ,string)
            (deleted . ,boolean)
            (status . ,string)
            (changed . ,boolean)
            (preview_id . ,falseable-string)
            (operation . ,string)
            (checkpoint_error . ((type . ["object" "boolean"]))))))
         (write-required
          '("old_revision" "new_revision" "changeset_id" "applied"
            "checkpointed" "modified" "diff" "truncated"))
         (write-document-required
          (append
           target-required
           '("old_revision" "new_revision" "applied"
             "checkpointed" "modified" "diff" "truncated")))
         (document-write-output
          (emacs-agent-editor--object-schema
           write-properties
           (append target-required write-required)))
         (write-document-item
          (emacs-agent-editor--object-schema
           write-properties write-document-required))
         (write-document-array
          `((type . "array") (items . ,write-document-item)))
         (transform-write-output
          (emacs-agent-editor--object-schema
           (append
            write-properties
            `((ranges . ((type . "array") (items . ,range)))
              (match_count . ((type . ["integer" "boolean"])))))
           (append target-required write-required '("ranges"))))
         (editor-write-output
          (emacs-agent-editor--object-schema
           (append
            `((documents . ,write-document-array))
            write-properties)
           (append write-required '("documents"))))
         (document-read-output
          (emacs-agent-editor--object-schema
           (append
            target-properties
            `((revision . ,string)
              (modified . ,boolean) (checkpointed . ,boolean)
              (coding_system . ,string) (eol_style . ,string)
              (start_line . ,integer) (end_line . ,integer)
              (total_lines . ,integer) (truncated . ,boolean)
              (cursor . ,falseable-string) (content . ,string)))
           (append
            target-required
            '("revision" "modified" "checkpointed"
              "coding_system" "eol_style" "start_line" "end_line"
              "total_lines" "truncated" "content"))))
         (files-output
          (emacs-agent-editor--object-schema
           `((files . ,target-array) (result_count . ,integer)
             (cursor . ,falseable-string))
           '("files" "result_count")))
         (search-result-item
          (emacs-agent-editor--object-schema
           (append
            target-properties
            `((line . ,integer)
              (column . ,integer) (match . ,string)
              (context . ,string) (preview . ,string)
              (source . ,string)
              (modified . ,boolean)
              (revision . ,falseable-string)))
           (append
            target-required
            '("line" "column" "match" "context"
              "source" "modified"))))
         (search-output
          (emacs-agent-editor--object-schema
           `((results
              . ((type . "array")
                 (items . ,search-result-item)))
             (result_count . ,integer)
             (cursor . ,falseable-string))
           '("results" "result_count")))
         (managed-document-item
          (emacs-agent-editor--object-schema
           (append
            target-properties
            `((status . ,string)
              (code . ,string)
              (visited . ,boolean)
              (exists_on_disk . ,boolean)
              (modified . ,boolean)
              (checkpointed . ,boolean)
              (disk_changed . ,boolean)
              (conflicted . ,boolean)
              (revision . ,falseable-string)
              (coding_system . ,falseable-string)
              (eol_style . ,falseable-string)
              (major_mode . ,falseable-string)
              (read_only . ,boolean)
              (active_changesets . ,string-array)))
           target-required))
         (managed-document-array
          `((type . "array") (items . ,managed-document-item)))
         (documents-output
          (emacs-agent-editor--object-schema
           `((documents . ,managed-document-array))
           '("documents")))
         (editor-diff-output
          (emacs-agent-editor--object-schema
           `((changeset_id . ,string) (content . ,string)
             (truncated . ,boolean) (cursor . ,falseable-string))
           '("changeset_id" "content" "truncated")))
         (document-status-output
          (emacs-agent-editor--object-schema
           (alist-get 'properties managed-document-item)
           (append
            target-required
            '("visited" "exists_on_disk" "modified"
              "checkpointed" "disk_changed" "conflicted" "revision"
              "coding_system" "eol_style" "major_mode" "read_only"
              "active_changesets"))))
         (changeset-list-output
          (emacs-agent-editor--object-schema
           `((changesets
              . ((type . "array")
                 (items
                  . ,(emacs-agent-editor--object-schema
                      `((changeset_id . ,string)
                        (created_at . ((type . "number")))
                        (status . ,string) (paths . ,string-array)
                        (operations . ,object-array)
                        (old_revisions . ,revision-array)
                        (new_revisions . ,revision-array)
                        (checkpointed . ,boolean)
                        (rollback_available . ,boolean)
                        (rollback_unavailable_reason . ,falseable-string)
                        (request_id . ,falseable-id)
                        (agent_identity . ,falseable-agent-identity))
                      '("changeset_id" "created_at" "status" "paths"
                        "operations" "old_revisions" "new_revisions"
                        "checkpointed" "rollback_available")))))
             (result_count . ,integer)
             (truncated . ,boolean) (cursor . ,falseable-string))
           '("changesets" "result_count" "truncated")))
         (changeset-detail-output
          (emacs-agent-editor--object-schema
           `((changeset_id . ,string) (created_at . ((type . "number")))
             (status . ,string) (paths . ,string-array)
             (operations . ,object-array)
             (old_revisions . ,revision-array)
             (new_revisions . ,revision-array)
             (checkpointed . ,boolean)
             (rollback_available . ,boolean)
             (rollback_unavailable_reason . ,falseable-string)
             (request_id . ,falseable-id)
             (agent_identity . ,falseable-agent-identity)
             (diff . ,string) (diff_truncated . ,boolean)
             (diff_cursor . ,falseable-string)
             (diagnostics_before . ,diagnostic-array)
             (diagnostics_after . ,diagnostic-array))
           '("changeset_id" "created_at" "status" "paths"
             "operations" "old_revisions" "new_revisions"
             "checkpointed" "rollback_available" "diff"
             "diff_truncated" "diagnostics_before"
             "diagnostics_after")))
         (document-diagnostics-output
          (emacs-agent-editor--object-schema
           (append
            target-properties
            `((document_revision . ,string)
              (diagnostics_revision . ,string)
              (providers . ,string-array)
              (pending . ,boolean) (stale . ,boolean)
              (diagnostics . ,diagnostic-array)))
           (append
            target-required
            '("document_revision" "diagnostics_revision"
              "providers" "pending" "stale" "diagnostics"))))
         (project-diagnostics-output
          (emacs-agent-editor--object-schema
           `((project_id . ,string)
             (document_count . ,integer) (diagnostic_count . ,integer)
             (pending . ,boolean) (stale . ,boolean)
             (next_cursor . ,falseable-string)
             (summary . ((type . "object")))
             (documents . ,diagnostic-document-array)
             (diagnostics . ,project-diagnostic-array))
           '("project_id" "document_count" "diagnostic_count"
             "pending" "stale" "summary" "documents" "diagnostics")))
         (editor-info-output
          (emacs-agent-editor--object-schema
           `((instance_id . ,string)
             (access_mode . ,string) (save_policy . ,string)
             (paused . ,boolean) (health . ,string)
             (project_count . ,integer)
             (managed_document_count . ,integer)
             (filesystem_policy . ,string)
             (protocol_versions . ,string-array)
             (authentication . ((type . "object")))
             (supported_tools . ,string-array)
             (runtime_capabilities . ((type . "object")))
             (capabilities . ,string-array)
             (position_semantics . ((type . "object")))
             (feature_capabilities . ((type . "object"))))
           '("instance_id" "access_mode" "save_policy"
             "paused" "health" "project_count"
             "managed_document_count" "filesystem_policy"
             "protocol_versions" "authentication" "supported_tools"
             "runtime_capabilities" "capabilities"
             "position_semantics" "feature_capabilities")))
         (project-properties
          `((project_id . ,string)
            (root . ,string)
            (name . ,string)
            (type . ,string)
            (native_project . ,boolean)
            (opened . ,boolean)))
         (project-required
          '("project_id" "root" "name" "type"
            "native_project" "opened"))
         (project-output
          (emacs-agent-editor--object-schema
           project-properties project-required))
         (project-array
          `((type . "array") (items . ,project-output)))
         (project-list-output
          (emacs-agent-editor--object-schema
           `((projects . ,project-array)
             (project_count . ,integer))
           '("projects" "project_count")))
         (project-info-output
          (emacs-agent-editor--object-schema
           (append
            project-properties
            `((managed_document_count . ,integer)
              (capabilities . ((type . "object")))))
           (append
            project-required
            '("managed_document_count" "capabilities"))))
         (project-close-output
          (emacs-agent-editor--object-schema
           `((project_id . ,string)
             (closed . ,boolean)
             (managed_document_count . ,integer))
           '("project_id" "closed" "managed_document_count")))
         (document-symbols-output
          (emacs-agent-editor--object-schema
           (append
            target-properties
            `((symbols . ,object-array)))
           (append target-required '("symbols"))))
         (symbol-location
          (emacs-agent-editor--object-schema
           (append
            target-properties
            `((range . ,range)
              (preview . ,string)
              (summary . ,string)
              (kind . ,string)
              (relation . ,falseable-string)
              (source . ,string)
              (revision . ,string)))
           (append
            target-required
            '("range" "preview" "summary" "kind"
              "relation" "source" "revision"))))
         (symbol-location-array
          `((type . "array") (items . ,symbol-location)))
         (project-symbols-output
          (emacs-agent-editor--object-schema
           `((project_id . ,string)
             (symbols . ,symbol-location-array)
             (possibly_incomplete . ,boolean)
             (source . ,string))
           '("project_id" "symbols" "possibly_incomplete" "source")))
         (definitions-output
          (emacs-agent-editor--object-schema
           `((definitions . ,symbol-location-array))
           '("definitions")))
         (references-output
          (emacs-agent-editor--object-schema
           `((references . ,symbol-location-array)
             (possibly_incomplete . ,boolean)
             (source . ,string))
           '("references" "possibly_incomplete" "source")))
         (context-buffer
          (emacs-agent-editor--object-schema
           `((name . ,string)
             (path . ,falseable-string)
             (project_id . ,falseable-string)
             (relative_path . ,falseable-string)
             (modified . ,boolean))
           '("name" "path" "project_id"
             "relative_path" "modified")))
         (context-output
          (emacs-agent-editor--object-schema
           `((instance_id . ,string)
             (redacted . ,boolean)
             (redaction_reason . ,falseable-string)
             (buffer . ,context-buffer)
             (point . ,position)
             (active_region . ((type . ["object" "boolean"])))
             (visible_range . ((type . ["object" "boolean"])))
             (major_mode . ,string))
           '("instance_id" "redacted")))
         (format-document-output
          (emacs-agent-editor--object-schema
           write-properties
           (append
            target-required write-required '("changed"))))
         (code-action-item
          (emacs-agent-editor--object-schema
           `((action_id . ,string)
             (title . ,string)
             (kind . ,falseable-string)
             (classification . ,string)
             (requires_approval . ,boolean)
             (preferred . ,boolean)
             (disabled . ,boolean)
             (command . ,falseable-string)
             (documents . ,write-document-array))
           '("action_id" "title" "kind" "classification"
             "requires_approval" "preferred" "disabled"
             "command" "documents")))
         (code-action-array
          `((type . "array") (items . ,code-action-item)))
         (approval-output
          (emacs-agent-editor--object-schema
           `((approval_request_id . ,string)
             (operation . ,string)
             (operation_digest . ,string)
             (status . ,string)
             (created_at . ((type . "number")))
             (expires_at . ((type . "number")))
             (ttl_remaining . ((type . "number")))
             (partial_accept_supported . ,boolean)
             (partial_accept_granularity . ,falseable-string)
             (parent_approval_request_id . ,falseable-string)
             (derived_approval_request_id . ,falseable-string)
             (accepted_paths . ,string-array)
             (path . ,falseable-string)
             (new_path . ,falseable-string)
             (changeset_id . ,falseable-string)
             (expected_revision . ,falseable-string)
             (checkpoint . ,boolean)
             (force . ,boolean)
             (dry_run . ,boolean)
             (document_count . ,integer)
             (document_paths . ,string-array)
             (risk . ,string))
           '("approval_request_id" "operation" "operation_digest"
             "status" "created_at" "expires_at" "ttl_remaining"
             "partial_accept_supported" "partial_accept_granularity"
             "parent_approval_request_id"
             "derived_approval_request_id" "accepted_paths" "risk")))
         (semantic-write-output
          (emacs-agent-editor--object-schema
           (append
            `((documents . ,write-document-array)
              (actions . ,code-action-array))
            write-properties)
           nil)))
    (emacs-agent-tool-register
     "emacs_agent_editor_info"
     "Return the bound Emacs runtime, policy, and health."
     (emacs-agent-editor--object-schema nil) editor-info-output
     #'emacs-agent-editor--editor-info 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_project_open"
     "Register an absolute local project root without changing the Emacs UI."
     (emacs-agent-editor--object-schema
      `((root . ,string))
      '("root"))
     project-output #'emacs-agent-editor--project-open 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_project_list"
     "List all project contexts registered in this editor runtime."
     (emacs-agent-editor--object-schema nil)
     project-list-output #'emacs-agent-editor--project-list 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_project_info"
     "Return metadata and safe capabilities for one registered project."
     (emacs-agent-editor--object-schema
      `((project_id . ,string))
      '("project_id"))
     project-info-output #'emacs-agent-editor--project-info 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_project_close"
     "Unregister project context without killing its visiting buffers."
     (emacs-agent-editor--object-schema
      `((project_id . ,string))
      '("project_id"))
     project-close-output #'emacs-agent-editor--project-close 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_read"
     "Read authoritative buffer content and its opaque revision."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (start_line . ,integer) (end_line . ,integer)
        (max_chars . ,integer) (cursor . ,string))
      '("path"))
     document-read-output #'emacs-agent-editor--document-read 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_apply_edits"
     "Apply guarded half-open ranges (line 1-based, column 0-based Emacs characters) against one revision, validated together and executed in descending order as one undo unit; overlaps and same-position inserts are rejected."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (expected_revision . ,string)
        (edits . ((type . "array") (items . ,edit)))
        (dry_run . ,boolean) (checkpoint . ,boolean))
      '("path" "expected_revision" "edits"))
     document-write-output
     #'emacs-agent-editor--document-apply-edits 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_document_create"
     "Create a new visited text document inside the runtime."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (content . ,string)
        (dry_run . ,boolean) (checkpoint . ,boolean))
      '("path" "content"))
     document-write-output #'emacs-agent-editor--document-create 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_project_files"
     "List runtime files with filters and opaque pagination."
     (emacs-agent-editor--object-schema
      `((project_id . ,string)
        (include_globs . ((type . "array") (items . ,string)))
        (exclude_globs . ((type . "array") (items . ,string)))
        (max_results . ,integer) (cursor . ,string))
      '("project_id"))
     files-output #'emacs-agent-editor--project-files 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_project_search"
     "Search runtime text with ripgrep or the Emacs fallback."
     (emacs-agent-editor--object-schema
      `((project_id . ,string)
        (query . ,string) (regexp . ,boolean)
        (include_globs . ((type . "array") (items . ,string)))
        (exclude_globs . ((type . "array") (items . ,string)))
        (max_results . ,integer) (cursor . ,string))
      '("project_id" "query"))
     search-output #'emacs-agent-editor--project-search 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_move"
     "Move a guarded document while preserving its visiting buffer."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (new_path . ,string) (new_project_id . ,string)
        (expected_revision . ,string) (dry_run . ,boolean)
        (approval_request_id . ,string))
      '("path" "new_path" "expected_revision"))
     document-write-output #'emacs-agent-editor--document-move 'destructive)
    (emacs-agent-tool-register
     "emacs_agent_document_delete"
     "Delete a guarded document with rollback metadata."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (expected_revision . ,string)
        (dry_run . ,boolean) (approval_request_id . ,string))
      '("path" "expected_revision"))
     document-write-output #'emacs-agent-editor--document-delete 'destructive)
    (emacs-agent-tool-register
     "emacs_agent_editor_checkpoint"
     "Save guarded buffers through their normal Emacs save hooks."
     (emacs-agent-editor--object-schema
      `((documents . ((type . "array") (items . ,document-guard)))
        (approval_request_id . ,string))
      '("documents"))
     editor-write-output #'emacs-agent-editor--editor-checkpoint 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_editor_sync"
     "Reconcile managed buffers with external filesystem changes."
     (emacs-agent-editor--object-schema
      `((documents
         . ((type . "array")
            (items
             . ,(emacs-agent-editor--object-schema
                 `((path . ,string)
                   (project_id . ,string))
                 '("path")))))))
     documents-output #'emacs-agent-editor--editor-sync 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_editor_diff"
     "Return a paginated unified diff for active or selected changes."
     (emacs-agent-editor--object-schema
      `((changeset_id . ,string) (max_chars . ,integer)
        (cursor . ,string)))
     editor-diff-output #'emacs-agent-editor--editor-diff 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_changeset_rollback"
     "Rollback a change set only when all revision guards still match."
     (emacs-agent-editor--object-schema
      `((changeset_id . ,string) (dry_run . ,boolean)
        (approval_request_id . ,string))
      '("changeset_id"))
     editor-write-output #'emacs-agent-editor--changeset-rollback 'destructive)
    (emacs-agent-tool-register
     "emacs_agent_document_replace"
     "Replace exact authoritative text, with dry-run and revision guards."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (expected_revision . ,string)
        (old_text . ,string) (new_text . ,string)
        (replace_all . ,boolean) (expected_occurrences . ,integer)
        (dry_run . ,boolean) (checkpoint . ,boolean))
      '("path" "expected_revision" "old_text" "new_text"))
     transform-write-output #'emacs-agent-editor--document-replace 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_document_apply_patch"
     "Validate and apply one strict single-file unified patch."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (expected_revision . ,string)
        (patch . ,string) (fuzz . ,integer)
        (dry_run . ,boolean) (checkpoint . ,boolean))
      '("path" "expected_revision" "patch"))
     transform-write-output
     #'emacs-agent-editor--document-apply-patch 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_editor_apply_edits"
     "Validate and atomically apply exact edits across multiple buffers."
     (emacs-agent-editor--object-schema
      `((documents
         . ((type . "array")
            (items . ,transaction-document)))
        (atomic
         . ((type . "boolean") (enum . [t])
            (description . "Transactions are always atomic; false is invalid.")))
        (dry_run . ,boolean)
        (checkpoint . ,boolean))
      '("documents"))
     editor-write-output #'emacs-agent-editor--editor-apply-edits 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_document_status"
     "Return document visit, disk, conflict, encoding, and revision state."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string))
      '("path"))
     document-status-output #'emacs-agent-editor--document-status 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_editor_modified_documents"
     "List dirty, externally changed, conflicted, or deleted buffers."
     (emacs-agent-editor--object-schema nil)
     documents-output
     #'emacs-agent-editor--editor-modified-documents 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_changeset_list"
     "List filterable change-set summaries with immutable pagination."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (status . ((type . "array") (items . ,string)))
        (limit . ,integer) (cursor . ,string)))
     changeset-list-output #'emacs-agent-editor--changeset-list 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_changeset_get"
     "Return change-set metadata and a paginated frozen diff."
     (emacs-agent-editor--object-schema
      `((changeset_id . ,string) (max_chars . ,integer)
        (cursor . ,string))
      '("changeset_id"))
     changeset-detail-output #'emacs-agent-editor--changeset-get 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_diagnostics"
     "Collect revision-bound safe parser and enabled editor diagnostics."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (expected_revision . ,string)
        (sources . ((type . "array") (items . ,string)))
        (wait_ms . ,integer))
      '("path"))
     document-diagnostics-output
     #'emacs-agent-editor--document-diagnostics 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_project_diagnostics"
     "Aggregate revision-bound diagnostics across runtime documents."
     (emacs-agent-editor--object-schema
      `((project_id . ,string)
        (paths . ((type . "array") (items . ,string)))
        (include_globs . ((type . "array") (items . ,string)))
        (exclude_globs . ((type . "array") (items . ,string)))
        (severities . ((type . "array") (items . ,string)))
        (sources . ((type . "array") (items . ,string)))
        (wait_ms . ,integer) (limit . ,integer) (cursor . ,string))
      '("project_id"))
     project-diagnostics-output
     #'emacs-agent-editor--project-diagnostics 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_symbols"
     "Return the native imenu symbol tree for one document."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string))
      '("path"))
     document-symbols-output #'emacs-agent-editor--document-symbols 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_project_symbols"
     "Search symbols through the active native xref backend."
     (emacs-agent-editor--object-schema
      `((project_id . ,string)
        (path . ,string) (query . ,string) (kind . ,string)
        (path_prefix . ,string) (limit . ,integer))
      '("project_id" "path" "query"))
     project-symbols-output #'emacs-agent-editor--project-symbols 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_symbol_definition"
     "Resolve definitions through the active native xref backend."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (position . ,position) (symbol . ,string))
      '("path" "position"))
     definitions-output #'emacs-agent-editor--symbol-definition 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_symbol_references"
     "Resolve references through the active native xref backend."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (position . ,position) (symbol . ,string))
      '("path" "position"))
     references-output #'emacs-agent-editor--symbol-references 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_editor_context_get"
     "Return safe metadata for the current Emacs editing context."
     (emacs-agent-editor--object-schema
      `((project_id . ,string)))
     context-output #'emacs-agent-editor--editor-context-get 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_format_document"
     "Preview or apply a server-configured trusted document formatter."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (expected_revision . ,string)
        (dry_run . ,boolean) (checkpoint . ,boolean)
        (approval_request_id . ,string))
      '("path" "expected_revision" "dry_run"))
     format-document-output #'emacs-agent-editor--format-document 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_symbol_rename"
     "Preview an Eglot semantic rename, then atomically apply only its frozen preview_id."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (position . ,position)
        (new_name . ,string) (expected_revision . ,string)
        (preview_id . ,string) (dry_run . ,boolean)
        (checkpoint . ,boolean)))
     semantic-write-output #'emacs-agent-editor--symbol-rename 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_code_actions"
     "List Eglot code actions or atomically apply a pure runtime-edit action; commands are never executed."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (range . ,range)
        (expected_revision . ,string) (kind . ,string)
        (action_id . ,string) (checkpoint . ,boolean)))
     semantic-write-output #'emacs-agent-editor--code-actions 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_format_range"
     "Preview Eglot range formatting, then atomically apply only its frozen preview_id."
     (emacs-agent-editor--object-schema
      `((path . ,string) (project_id . ,string)
        (range . ,range)
        (expected_revision . ,string) (preview_id . ,string)
        (dry_run . ,boolean) (checkpoint . ,boolean)
        (approval_request_id . ,string)))
     semantic-write-output #'emacs-agent-editor--format-range 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_approval_status"
     "Return credential-free TTL and revision-bound approval status."
     (emacs-agent-editor--object-schema
      `((approval_request_id . ,string))
      '("approval_request_id"))
     approval-output #'emacs-agent-editor--approval-status 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_approval_cancel"
     "Cancel a pending or approved request and audit the transition."
     (emacs-agent-editor--object-schema
      `((approval_request_id . ,string))
      '("approval_request_id"))
     approval-output #'emacs-agent-editor--approval-cancel 'mutating)))

;;;###autoload
(defun emacs-agent-editor-start (&optional port)
  "Start the project-optional Agent Editor MCP runtime.
PORT overrides `emacs-agent-editor-port' when non-nil.

Repeated calls are idempotent: an already running runtime is returned without
recreating the listener or registry."
  (interactive)
  (if (emacs-agent-editor-running-p)
      (progn
        (when (called-interactively-p 'interactive)
          (message "Agent Editor MCP already running: %s"
                   emacs-agent-editor--connection-file))
        emacs-agent-editor--http-server)
    (unless (equal emacs-agent-editor-host "127.0.0.1")
      (user-error "Agent Editor MCP only supports the IPv4 loopback listener"))
    (when (and port (not (integerp port)))
      (signal 'wrong-type-argument (list 'integerp port)))
    (when (or (< (or port emacs-agent-editor-port) 0)
              (> (or port emacs-agent-editor-port) 65535))
      (user-error "Agent Editor MCP port must be between 0 and 65535"))
    (let* ((token
            (when emacs-agent-editor-token-authentication-enabled
              (or emacs-agent-editor-bearer-token
                  (emacs-agent-editor--random-token))))
           (state-directory (emacs-agent-editor--instance-state-directory))
           (runtime
            (emacs-agent-runtime-create
             :access-mode emacs-agent-editor-access-mode
             :save-policy emacs-agent-editor-save-policy
             :writer-lease token
             :state-directory state-directory
             :filesystem-policy emacs-agent-policy-filesystem-scope
             :allowed-roots emacs-agent-policy-allowed-roots
             :denied-paths emacs-agent-policy-denied-paths))
           server)
      (condition-case error-data
          (progn
            (emacs-agent-runtime-bind runtime)
            (emacs-agent-editor--register-tools)
            (setq emacs-agent-edit-record-function
                  #'emacs-agent-editor--record-edit
                  emacs-agent-protocol-tool-observer
                  #'emacs-agent-editor--observe-tool
                  emacs-agent-editor--token token
                  emacs-agent-editor--runtime runtime)
            (emacs-agent-journal-open runtime)
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
             runtime (emacs-agent-http-server-port server) token)
            (emacs-agent-runtime-record-activity
             runtime
             (list :tool "server_start" :status "completed"))
            (when (called-interactively-p 'interactive)
              (message "Agent Editor MCP started: %s"
                       emacs-agent-editor--connection-file))
            server)
        (error
         (when server
           (ignore-errors (emacs-agent-http-stop server)))
         (ignore-errors (emacs-agent-journal-close runtime))
         (ignore-errors (emacs-agent-semantic-clear runtime))
         (ignore-errors (emacs-agent-search-clear runtime))
         (ignore-errors (emacs-agent-session-clear))
         (ignore-errors (emacs-agent-tool-clear))
         (ignore-errors (clrhash emacs-agent-editor--diff-cursors))
         (ignore-errors (clrhash emacs-agent-search-cursors))
         (ignore-errors (clrhash emacs-agent-diagnostics-cursors))
         (ignore-errors (clrhash emacs-agent-document-cursors))
         (ignore-errors (clrhash emacs-agent-changeset-cursors))
         (ignore-errors (emacs-agent-runtime-clear runtime))
         (ignore-errors (emacs-agent-editor--remove-connection-file))
         (when (eq emacs-agent-current-runtime runtime)
           (setq emacs-agent-current-runtime nil))
         (setq emacs-agent-editor--http-server nil
               emacs-agent-editor--runtime nil
               emacs-agent-editor--connection-file nil
               emacs-agent-editor--token nil
               emacs-agent-edit-record-function nil
               emacs-agent-protocol-tool-observer nil)
         (signal (car error-data) (cdr error-data)))))))

;;;###autoload
(defun emacs-agent-editor-stop ()
  "Stop Agent Editor MCP and remove its connection metadata."
  (interactive)
  (let ((runtime emacs-agent-editor--runtime)
        (server emacs-agent-editor--http-server))
    (unwind-protect
        (progn
          (when runtime
            (ignore-errors
              (emacs-agent-runtime-record-activity
               runtime
               (list :tool "server_stop" :status "completed")))
            (ignore-errors
              (emacs-agent-journal-close runtime))
            (ignore-errors
              (emacs-agent-semantic-clear runtime))
            (ignore-errors
              (emacs-agent-search-clear runtime)))
          (when server
            (emacs-agent-http-stop server)))
      (ignore-errors (emacs-agent-session-clear))
      (ignore-errors (emacs-agent-tool-clear))
      (ignore-errors (emacs-agent-editor--remove-connection-file))
      (ignore-errors (clrhash emacs-agent-editor--diff-cursors))
      (ignore-errors (clrhash emacs-agent-search-cursors))
      (ignore-errors (clrhash emacs-agent-diagnostics-cursors))
      (ignore-errors (clrhash emacs-agent-document-cursors))
      (ignore-errors (clrhash emacs-agent-changeset-cursors))
      (when runtime
        (ignore-errors (emacs-agent-runtime-clear runtime)))
      (when (eq emacs-agent-current-runtime runtime)
        (setq emacs-agent-current-runtime nil))
      (setq emacs-agent-editor--http-server nil
            emacs-agent-editor--runtime nil
            emacs-agent-editor--connection-file nil
            emacs-agent-editor--token nil
            emacs-agent-edit-record-function nil
            emacs-agent-protocol-tool-observer nil)))
  t)

;;;###autoload
(defun emacs-agent-editor-pause ()
  "Pause agent mutations while leaving read tools available."
  (interactive)
  (emacs-agent-runtime-pause emacs-agent-editor--runtime))

;;;###autoload
(defun emacs-agent-editor-resume ()
  "Resume agent mutations."
  (interactive)
  (emacs-agent-runtime-resume emacs-agent-editor--runtime))

;;;###autoload
(defun emacs-agent-editor-revoke-writer ()
  "Pause mutations and revoke the active writer credential.
When token authentication is enabled, also rotate the bearer token."
  (interactive)
  (unless (emacs-agent-editor-running-p)
    (user-error "Agent Editor MCP is not running"))
  (emacs-agent-editor-pause)
  (let ((token
         (when emacs-agent-editor--token
           (emacs-agent-editor--random-token))))
    (setq emacs-agent-editor--token token)
    (setf (emacs-agent-http-server-token emacs-agent-editor--http-server)
          token
          (emacs-agent-runtime-writer-lease
           emacs-agent-editor--runtime)
          token)
    (emacs-agent-session-clear)
    (emacs-agent-editor--write-connection-file
     emacs-agent-editor--runtime
     (emacs-agent-http-server-port emacs-agent-editor--http-server)
     token))
  t)

(defalias 'emacs-agent-editor-show-activity #'emacs-agent-show-activity)
(defalias 'emacs-agent-editor-show-changes #'emacs-agent-show-changes)

(provide 'emacs-agent-editor)
;;; emacs-agent-editor.el ends here
