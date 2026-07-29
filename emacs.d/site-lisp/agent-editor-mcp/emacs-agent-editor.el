;;; emacs-agent-editor.el --- Buffer-first HTTP MCP editor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Gao

;; Author: Gao
;; Version: 0.2.0
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
(require 'emacs-agent-transform)
(require 'emacs-agent-transaction)
(require 'emacs-agent-workspace)
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

(defvar emacs-agent-editor--workspace nil
  "Workspace bound to the active server.")

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
  "Publish private connection metadata for WORKSPACE, PORT, and TOKEN.
TOKEN is omitted from the metadata when authentication is disabled."
  (let* ((directory (emacs-agent-editor--instance-state-directory))
         (target (expand-file-name "connection.json" directory))
         (temporary nil)
         (metadata
          (append
           `((schema_version . 1)
             (daemon . ,(emacs-agent-editor--daemon-name))
             (pid . ,(emacs-pid))
             (workspace . ,(emacs-agent-workspace-root workspace))
             (endpoint . ,(format "http://%s:%d%s"
                                  emacs-agent-editor-host
                                  port
                                  emacs-agent-editor-endpoint))
             (token_authentication . ,(if token t :false)))
           (when token `((token . ,token)))
           `((protocol_versions . ["2026-07-28" "2025-11-25"])
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
    :diagnostics_before :document_paths :documents :edits :matches
    :new_revisions :old_revisions :operations :paths :ranges
    :references :related_information :restored_paths :revision_bindings
    :sources :symbols)
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
               ("path_outside_root" . "WORKSPACE_BOUNDARY_VIOLATION")
               ("path_denied" . "WORKSPACE_BOUNDARY_VIOLATION")
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
                 "CHECKPOINT_FAILED"))
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
     workspace
     (list :tool emacs-agent-editor--operation-name :status "completed"
           :changeset_id (emacs-agent-changeset-changeset-id changeset)
           :paths (list path)))
    (emacs-agent-changeset-changeset-id changeset)))

(defun emacs-agent-editor--observe-tool (name status duration payload)
  "Record bounded metadata for tool NAME, STATUS, DURATION, and PAYLOAD."
  (when (emacs-agent-workspace-p emacs-agent-editor--workspace)
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
               "emacs_agent_workspace_apply_edits"
               "emacs_agent_workspace_checkpoint"
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
              :workspace_id
              (emacs-agent-workspace-workspace-id
               emacs-agent-editor--workspace)
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
      (emacs-agent-workspace-record-activity
       emacs-agent-editor--workspace event)
      (emacs-agent-journal-write emacs-agent-editor--workspace event))))

(defun emacs-agent-editor--workspace-provider-buffers (workspace)
  "Return live document buffers belonging to WORKSPACE.

The selected buffer is first only when it is one of those workspace buffers."
  (let ((selected (and (window-live-p (selected-window))
                       (window-buffer (selected-window))))
        buffers)
    (maphash
     (lambda (_path document)
       (let ((buffer (emacs-agent-document-buffer document)))
         (when (buffer-live-p buffer)
           (push buffer buffers))))
     (emacs-agent-workspace-document-registry workspace))
    (setq buffers (delete-dups buffers))
    (if (memq selected buffers)
        (cons selected (delq selected buffers))
      buffers)))

(defun emacs-agent-editor--workspace-info (_arguments _context)
  "Implement `emacs_agent_workspace_info'."
  (let* ((workspace (emacs-agent-workspace-current))
         (provider-buffers
          (emacs-agent-editor--workspace-provider-buffers workspace))
         (runtime-capabilities
          (emacs-agent-semantic-runtime-capabilities
           (or provider-buffers :none))))
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
          "workspace_transactions" "diagnostics" "changeset_query"
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
            (path (emacs-agent-editor--argument arguments 'path))
            (expected-revision
             (emacs-agent-editor--argument arguments 'expected_revision))
            (edits (emacs-agent-editor--argument arguments 'edits))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (checkpoint
             (emacs-agent-editor--checkpoint-p
              workspace
              (emacs-agent-editor--argument arguments 'checkpoint)))
            (emacs-agent-editor--request-context context)
            (emacs-agent-editor--operation-name
             "document_apply_edits")
            (result
             (if dry-run
                 (let* ((document
                         (emacs-agent-document-open workspace path))
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
                   (list
                    :path path :changeset_id nil
                    :previous_revision revision
                    :new_revision revision
                    :checkpointed nil :edit_count (length ranges)
                    :old_revision revision :applied nil
                    :modified (not (equal before after))
                    :diff
                    (emacs-agent-changeset--diff-text path before after)
                    :truncated nil :diff_truncated nil
                    :diagnostics_state "not_requested"))
               (emacs-agent-workspace-enqueue-mutation
                workspace
                (lambda ()
                  (emacs-agent-edit-apply
                   workspace path expected-revision edits checkpoint))))))
       (unless dry-run
         (setq result
               (append
                result
                (list
                 :old_revision (plist-get result :previous_revision)
                 :applied t
                 :modified t
                 :diff
                 (emacs-agent-changeset-diff
                  workspace (plist-get result :changeset_id))
                 :truncated nil
                 :diff_truncated nil))))
       (emacs-agent-editor--plist-to-alist
        (cl-loop for (key value) on result by #'cddr
                 unless (memq key '(:before_content :after_content))
                 append (list key value)))))))

(defun emacs-agent-editor--document-replace (arguments context)
  "Implement `emacs_agent_document_replace' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (checkpoint
             (emacs-agent-editor--checkpoint-p
              workspace
              (emacs-agent-editor--argument arguments 'checkpoint)))
            (emacs-agent-editor--request-context context)
            (emacs-agent-editor--operation-name "document_replace")
            (operation
             (lambda ()
               (emacs-agent-transform-replace
                workspace
                (emacs-agent-editor--argument arguments 'path)
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
               (emacs-agent-workspace-enqueue-mutation
                workspace operation))))
       (emacs-agent-editor--plist-to-alist result)))))

(defun emacs-agent-editor--document-apply-patch (arguments context)
  "Implement `emacs_agent_document_apply_patch' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (checkpoint
             (emacs-agent-editor--checkpoint-p
              workspace
              (emacs-agent-editor--argument arguments 'checkpoint)))
            (emacs-agent-editor--request-context context)
            (emacs-agent-editor--operation-name "document_apply_patch")
            (operation
             (lambda ()
               (emacs-agent-transform-apply-patch
                workspace
                (emacs-agent-editor--argument arguments 'path)
                (emacs-agent-editor--argument arguments 'expected_revision)
                (emacs-agent-editor--argument arguments 'patch)
                :fuzz
                (or (emacs-agent-editor--argument arguments 'fuzz) 0)
                :dry-run dry-run :checkpoint checkpoint)))
            (result
             (if dry-run
                 (funcall operation)
               (emacs-agent-workspace-enqueue-mutation
                workspace operation))))
       (emacs-agent-editor--plist-to-alist result)))))

(defun emacs-agent-editor--workspace-apply-edits (arguments context)
  "Implement `emacs_agent_workspace_apply_edits' with ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (plan
             (emacs-agent-transaction-plan
              workspace
              (emacs-agent-editor--argument arguments 'documents)))
            (result
             (emacs-agent-transaction-apply
              plan
              (eq (emacs-agent-editor--argument arguments 'dry_run) t)
              (emacs-agent-editor--checkpoint-p
               workspace
               (emacs-agent-editor--argument arguments 'checkpoint))
              context)))
       (emacs-agent-editor--plist-to-alist result)))))

(defun emacs-agent-editor--record-lifecycle
    (workspace operation paths before base final checkpointed)
  "Record a lifecycle OPERATION in WORKSPACE."
  (let ((changeset
         (emacs-agent-changeset-record
          workspace
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
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
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
       (if dry-run
           `((path . ,path)
             (old_revision . :false)
             (new_revision . :false)
             (changeset_id . :false)
             (applied . :false)
             (modified . t)
             (checkpointed . :false)
             (diff
              . ,(emacs-agent-changeset--diff-text path "" content))
             (truncated . :false)
             (diff_truncated . :false))
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
            (when checkpoint
              (setf
               (emacs-agent-document-disk-fingerprint document)
               (emacs-agent-document--disk-fingerprint checked-absolute)))
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
                (old_revision . :false)
                (changeset_id . ,changeset-id)
                (new_revision . ,revision)
                (applied . t)
                (modified . t)
                (checkpointed
                 . ,(if checkpoint t :false))
                (diff
                 . ,(emacs-agent-changeset-diff workspace changeset-id))
                (truncated . :false)
                (diff_truncated . :false)))))))))))

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
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (emacs-agent-editor--request-context context))
       (unless dry-run
         (emacs-agent-editor--authorize workspace "document_move" arguments)
         (emacs-agent-editor--require-lifecycle-checkpoint
          workspace arguments))
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
         (if dry-run
             `((path . ,path) (new_path . ,new-path)
               (old_revision . ,base)
               (new_revision . ,base)
               (changeset_id . :false)
               (applied . :false)
               (modified . t)
               (checkpointed . :false)
               (diff
                . ,(concat
                    (emacs-agent-changeset--diff-text
                     path before "")
                    (emacs-agent-changeset--diff-text
                     new-path "" before)))
               (truncated . :false)
               (diff_truncated . :false))
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
                (old_revision . ,base)
                (changeset_id . ,changeset-id)
                (new_revision . ,revision)
                (applied . t) (modified . t)
                (checkpointed . t)
                (diff
                 . ,(emacs-agent-changeset-diff workspace changeset-id))
                (truncated . :false)
                (diff_truncated . :false)))))))))))

(defun emacs-agent-editor--document-delete (arguments context)
  "Implement `emacs_agent_document_delete' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (path (emacs-agent-editor--argument arguments 'path))
            (document (emacs-agent-document-open workspace path))
            (revision (emacs-agent-document-revision document))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t))
            (emacs-agent-editor--request-context context))
       (unless dry-run
         (emacs-agent-editor--authorize workspace "document_delete" arguments)
         (emacs-agent-editor--require-lifecycle-checkpoint
          workspace arguments))
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
             `((path . ,path)
               (old_revision . ,revision)
               (new_revision . :false)
               (changeset_id . :false)
               (deleted . :false)
               (applied . :false)
               (modified . t)
               (checkpointed . :false)
               (diff
                . ,(emacs-agent-changeset--diff-text
                    path content ""))
               (truncated . :false)
               (diff_truncated . :false))
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
                (old_revision . ,revision) (new_revision . :false)
                (deleted . t) (applied . t) (modified . t)
                (checkpointed . t)
                (diff
                 . ,(emacs-agent-changeset-diff workspace changeset-id))
                (truncated . :false)
                (diff_truncated . :false)))))))))))

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
                       (old_revision
                        . ,(cdr
                            (assoc
                             (emacs-agent-document-relative-path document)
                             base-revisions)))
                       (new_revision
                        . ,(emacs-agent-document-revision document))
                       (revision
                        . ,(emacs-agent-document-revision document))
                       (applied . t) (checkpointed . t)
                       (modified . :false) (diff . "")
                       (truncated . :false)
                       (diff_truncated . :false))
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
                (let* ((documents (vconcat (nreverse results)))
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
                    (modified . :false) (diff . "")
                    (truncated . :false)
                    (diff_truncated . :false)
                    (documents . ,documents))))))))))))

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

(defun emacs-agent-editor--document-status (arguments _context)
  "Implement `emacs_agent_document_status' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-document-status
       (emacs-agent-workspace-current)
       (emacs-agent-editor--argument arguments 'path))))))

(defun emacs-agent-editor--workspace-modified-documents
    (_arguments _context)
  "Implement `emacs_agent_workspace_modified_documents'."
  (emacs-agent-editor--call
   (lambda ()
     `((documents
        . ,(vconcat
            (mapcar
             #'emacs-agent-editor--plist-to-alist
             (emacs-agent-workspace-modified-documents
              (emacs-agent-workspace-current)))))))))

(defun emacs-agent-editor--changeset-list (arguments _context)
  "Implement `emacs_agent_changeset_list' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-changeset-query
       (emacs-agent-workspace-current)
       :path (emacs-agent-editor--argument arguments 'path)
       :statuses
       (mapcar
        #'intern
        (append
         (emacs-agent-editor--argument arguments 'status) nil))
       :limit
       (or (emacs-agent-editor--argument arguments 'limit) 50)
       :cursor (emacs-agent-editor--argument arguments 'cursor))))))

(defun emacs-agent-editor--changeset-get (arguments _context)
  "Implement `emacs_agent_changeset_get' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-changeset-detail
       (emacs-agent-workspace-current)
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
       (emacs-agent-workspace-current)
       (emacs-agent-editor--argument arguments 'path)
       :expected-revision
       (emacs-agent-editor--argument arguments 'expected_revision)
       :sources
       (append
        (emacs-agent-editor--argument arguments 'sources) nil)
       :wait-ms
       (or (emacs-agent-editor--argument arguments 'wait_ms) 3000))))))

(defun emacs-agent-editor--workspace-diagnostics (arguments _context)
  "Implement `emacs_agent_workspace_diagnostics' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-workspace-diagnostics
       (emacs-agent-workspace-current)
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
     `((symbols
        . ,(vconcat
            (emacs-agent-semantic-document-symbols
             (emacs-agent-workspace-current)
             (emacs-agent-editor--argument arguments 'path))))))))

(defun emacs-agent-editor--workspace-symbols (arguments _context)
  "Implement `emacs_agent_workspace_symbols' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-semantic-workspace-symbols
      (emacs-agent-workspace-current)
      (emacs-agent-editor--argument arguments 'path)
      (emacs-agent-editor--argument arguments 'query)
      (emacs-agent-editor--argument arguments 'kind)
      (emacs-agent-editor--argument arguments 'path_prefix)
      (emacs-agent-editor--argument arguments 'limit)))))

(defun emacs-agent-editor--symbol-definition (arguments _context)
  "Implement `emacs_agent_symbol_definition' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     `((definitions
        . ,(vconcat
            (emacs-agent-semantic-definition
             (emacs-agent-workspace-current)
             (emacs-agent-editor--argument arguments 'path)
             (emacs-agent-editor--argument arguments 'position)
             (emacs-agent-editor--argument arguments 'symbol))))))))

(defun emacs-agent-editor--symbol-references (arguments _context)
  "Implement `emacs_agent_symbol_references' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-semantic-references
      (emacs-agent-workspace-current)
      (emacs-agent-editor--argument arguments 'path)
      (emacs-agent-editor--argument arguments 'position)
      (emacs-agent-editor--argument arguments 'symbol)))))

(defun emacs-agent-editor--editor-context-get (_arguments _context)
  "Implement `emacs_agent_editor_context_get'."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-semantic-editor-context
      (emacs-agent-workspace-current)))))

(defun emacs-agent-editor--format-document (arguments context)
  "Implement guarded `emacs_agent_format_document' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (path (emacs-agent-editor--argument arguments 'path))
            (revision
             (emacs-agent-editor--argument arguments 'expected_revision))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t)))
       (if dry-run
           (let* ((preview
                   (emacs-agent-semantic-format-preview
                    workspace path revision))
                  (changed (alist-get 'changed preview)))
             (append
              preview
              `((old_revision . ,revision)
                (new_revision . ,revision)
                (changeset_id . :false)
                (applied . :false)
                (checkpointed . :false)
                (modified . ,changed)
                (truncated . :false)
                (diff_truncated . :false))))
         (let* ((checkpoint
                 (emacs-agent-editor--checkpoint-p
                  workspace
                  (emacs-agent-editor--argument arguments 'checkpoint)))
                (emacs-agent-editor--request-context context)
                (emacs-agent-editor--operation-name "format_document")
                (_
                 (emacs-agent-editor--authorize
                  workspace "format_document" arguments))
                (result
                 (emacs-agent-workspace-enqueue-mutation
                  workspace
                  (lambda ()
                    (emacs-agent-semantic-format-apply
                     workspace path revision checkpoint)))))
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
     (let* ((workspace (emacs-agent-workspace-current))
            (preview-id
             (emacs-agent-editor--argument arguments 'preview_id))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t)))
       (if dry-run
           (emacs-agent-editor--json-value
            (emacs-agent-semantic-rename-preview
             workspace
             (emacs-agent-editor--argument arguments 'path)
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
           workspace preview-id
           (emacs-agent-editor--checkpoint-p
            workspace
            (emacs-agent-editor--argument arguments 'checkpoint))
           context)))))))

(defun emacs-agent-editor--code-actions (arguments context)
  "List or safely apply `emacs_agent_code_actions' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (action-id
             (emacs-agent-editor--argument arguments 'action_id)))
       (if action-id
           (emacs-agent-editor--plist-to-alist
            (emacs-agent-semantic-code-action-apply
             workspace action-id
             (emacs-agent-editor--checkpoint-p
              workspace
              (emacs-agent-editor--argument arguments 'checkpoint))
             context))
         (emacs-agent-editor--json-value
          (emacs-agent-semantic-code-actions
           workspace
           (emacs-agent-editor--argument arguments 'path)
           (emacs-agent-editor--argument arguments 'range)
           (emacs-agent-editor--argument arguments 'expected_revision)
           (emacs-agent-editor--argument arguments 'kind))))))))

(defun emacs-agent-editor--format-range (arguments context)
  "Preview or apply `emacs_agent_format_range' for ARGUMENTS and CONTEXT."
  (emacs-agent-editor--call
   (lambda ()
     (let* ((workspace (emacs-agent-workspace-current))
            (preview-id
             (emacs-agent-editor--argument arguments 'preview_id))
            (dry-run
             (eq (emacs-agent-editor--argument arguments 'dry_run) t)))
       (if dry-run
           (emacs-agent-editor--json-value
            (emacs-agent-semantic-format-range-preview
             workspace
             (emacs-agent-editor--argument arguments 'path)
             (emacs-agent-editor--argument arguments 'range)
             (emacs-agent-editor--argument arguments 'expected_revision)))
         (unless (stringp preview-id)
           (emacs-agent-editor--tool-error
            'invalid_argument
            :field 'preview_id
            :message "A prior dry-run preview_id is required"))
         (emacs-agent-editor--authorize
          workspace "format_range" arguments)
         (emacs-agent-editor--plist-to-alist
          (emacs-agent-semantic-format-range-apply
           workspace preview-id
           (emacs-agent-editor--checkpoint-p
            workspace
            (emacs-agent-editor--argument arguments 'checkpoint))
           context)))))))

(defun emacs-agent-editor--approval-status (arguments _context)
  "Implement `emacs_agent_approval_status' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-workspace-approval-status
       (emacs-agent-workspace-current)
       (emacs-agent-editor--argument arguments 'approval_request_id))))))

(defun emacs-agent-editor--approval-cancel (arguments _context)
  "Implement `emacs_agent_approval_cancel' with ARGUMENTS."
  (emacs-agent-editor--call
   (lambda ()
     (emacs-agent-editor--plist-to-alist
      (emacs-agent-workspace-approval-cancel
       (emacs-agent-workspace-current)
       (emacs-agent-editor--argument arguments 'approval_request_id))))))

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
           (dry-run
            (eq (emacs-agent-editor--argument arguments 'dry_run) t))
           (changeset-id
            (emacs-agent-editor--argument arguments 'changeset_id)))
       (unless dry-run
         (emacs-agent-editor--authorize
          workspace "changeset_rollback" arguments))
       (let* ((target (emacs-agent-changeset-get workspace changeset-id))
              (rollback
               (emacs-agent-changeset--rollback-status workspace target))
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
                    workspace (car entry))
                   (emacs-agent-changeset--snapshot-content (cdr entry))))
                (emacs-agent-changeset-before-snapshots target)
                ""))
              (changeset
               (unless dry-run
                 (emacs-agent-changeset-rollback workspace changeset-id)))
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
                       . ,(emacs-agent-changeset--revision workspace path))))
                  (emacs-agent-changeset-touched-documents changeset))))
              (documents
               (vconcat
                (mapcar
                 (lambda (entry)
                   (let* ((path (alist-get 'path entry))
                          (new-revision (alist-get 'revision entry))
                          (old-revision (cdr (assoc path old-revisions))))
                     `((path . ,path)
                       (old_revision . ,old-revision)
                       (new_revision . ,new-revision)
                       (applied . ,(if dry-run :false t))
                       (checkpointed
                        . ,(if (and
                                (not dry-run)
                                (eq
                                 (emacs-agent-workspace-save-policy workspace)
                                 'immediate))
                               t :false))
                       (modified . t) (diff . "")
                       (truncated . :false)
                       (diff_truncated . :false))))
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
                     (eq (emacs-agent-workspace-save-policy workspace)
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
  "Register the complete version 0.2 MCP tool surface."
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
           `((path . ,string) (expected_revision . ,string)
             (edits . ((type . "array") (items . ,replace-edit)))
             (patch . ,string))
           '("path" "expected_revision")))
         (document-guard
          (emacs-agent-editor--object-schema
           `((path . ,string) (expected_revision . ,string))
           '("path" "expected_revision")))
         (falseable-string '((type . ["string" "boolean"])))
         (falseable-id '((type . ["string" "integer" "boolean"])))
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
           `((path . ,string) (source . ,string) (severity . ,string)
             (code . ,falseable-string) (message . ,string)
             (range . ((type . ["object" "boolean"])))
             (revision . ,string) (stale . ,boolean)
             (related_information . ,object-array)
             (action_id . ,falseable-string))
           '("source" "severity" "message" "range")))
         (diagnostic-array
          `((type . "array") (items . ,diagnostic-item)))
         (diagnostic-document
          (emacs-agent-editor--object-schema
           `((path . ,string) (document_revision . ,string)
             (diagnostics_revision . ,string)
             (providers . ,string-array)
             (pending . ,boolean) (stale . ,boolean)
             (diagnostics . ,diagnostic-array))
           '("path" "document_revision" "diagnostics_revision"
             "providers" "pending" "stale" "diagnostics")))
         (diagnostic-document-array
          `((type . "array") (items . ,diagnostic-document)))
         (write-properties
          `((path . ,string)
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
            (diagnostics_state . ,string)
            (deleted . ,boolean)
            (status . ,string)
            (checkpoint_error . ((type . ["object" "boolean"])))))
         (write-required
          '("old_revision" "new_revision" "changeset_id" "applied"
            "checkpointed" "modified" "diff" "truncated"))
         (write-document-required
          '("path" "old_revision" "new_revision" "applied"
            "checkpointed" "modified" "diff" "truncated"))
         (document-write-output
          (emacs-agent-editor--object-schema
           write-properties (cons "path" write-required)))
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
           (append (cons "path" write-required) '("ranges"))))
         (workspace-write-output
          (emacs-agent-editor--object-schema
           (append
            `((documents . ,write-document-array))
            write-properties)
           (append write-required '("documents"))))
         (document-read-output
          (emacs-agent-editor--object-schema
           `((path . ,string) (revision . ,string)
             (modified . ,boolean) (checkpointed . ,boolean)
             (coding_system . ,string) (eol_style . ,string)
             (start_line . ,integer) (end_line . ,integer)
             (total_lines . ,integer) (truncated . ,boolean)
             (cursor . ,falseable-string) (content . ,string))
           '("path" "revision" "modified" "checkpointed"
             "coding_system" "eol_style" "start_line" "end_line"
             "total_lines" "truncated" "content")))
         (files-output
          (emacs-agent-editor--object-schema
           `((files . ,string-array) (result_count . ,integer)
             (cursor . ,falseable-string))
           '("files" "result_count")))
         (search-output
          (emacs-agent-editor--object-schema
           `((results
              . ((type . "array")
                 (items
                  . ,(emacs-agent-editor--object-schema
                      `((path . ,string) (line . ,integer)
                        (column . ,integer) (match . ,string)
                        (context . ,string) (source . ,string)
                        (modified . ,boolean) (revision . ,string))
                      '("path" "line" "column" "match" "context"
                        "source" "modified" "revision")))))
             (result_count . ,integer)
             (cursor . ,falseable-string))
           '("results" "result_count")))
         (documents-output
          (emacs-agent-editor--object-schema
           `((documents . ,object-array))
           '("documents")))
         (workspace-diff-output
          (emacs-agent-editor--object-schema
           `((changeset_id . ,string) (content . ,string)
             (truncated . ,boolean) (cursor . ,falseable-string))
           '("changeset_id" "content" "truncated")))
         (document-status-output
          (emacs-agent-editor--object-schema
           `((path . ,string) (visited . ,boolean)
             (exists_on_disk . ,boolean) (modified . ,boolean)
             (checkpointed . ,boolean) (disk_changed . ,boolean)
             (conflicted . ,boolean) (revision . ,string)
             (coding_system . ,string) (eol_style . ,string)
             (major_mode . ,string) (read_only . ,boolean)
             (active_changesets . ,string-array))
           '("path" "visited" "exists_on_disk" "modified"
             "checkpointed" "disk_changed" "conflicted" "revision"
             "coding_system" "eol_style" "major_mode" "read_only"
             "active_changesets")))
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
           `((path . ,string) (document_revision . ,string)
             (diagnostics_revision . ,string)
             (providers . ,string-array)
             (pending . ,boolean) (stale . ,boolean)
             (diagnostics . ,diagnostic-array))
           '("path" "document_revision" "diagnostics_revision"
             "providers" "pending" "stale" "diagnostics")))
         (workspace-diagnostics-output
          (emacs-agent-editor--object-schema
           `((document_count . ,integer) (diagnostic_count . ,integer)
             (pending . ,boolean) (stale . ,boolean)
             (next_cursor . ,falseable-string)
             (summary . ((type . "object")))
             (documents . ,diagnostic-document-array)
             (diagnostics . ,diagnostic-array))
           '("document_count" "diagnostic_count" "pending" "stale"
             "summary" "documents" "diagnostics")))
         (workspace-info-output
          (emacs-agent-editor--object-schema
           `((workspace_id . ,string) (root . ,string)
             (access_mode . ,string) (save_policy . ,string)
             (paused . ,boolean) (health . ,string)
             (protocol_versions . ,string-array)
             (authentication . ((type . "object")))
             (supported_tools . ,string-array)
             (runtime_capabilities . ((type . "object")))
             (capabilities . ,string-array)
             (position_semantics . ((type . "object")))
             (feature_capabilities . ((type . "object"))))
           '("workspace_id" "root" "access_mode" "save_policy"
             "paused" "health" "protocol_versions" "authentication"
             "supported_tools" "runtime_capabilities" "capabilities"
             "position_semantics" "feature_capabilities")))
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
     "Apply guarded half-open ranges (line 1-based, column 0-based Emacs characters) against one revision, validated together and executed in descending order as one undo unit; overlaps and same-position inserts are rejected."
     (emacs-agent-editor--object-schema
      `((path . ,string) (expected_revision . ,string)
        (edits . ((type . "array") (items . ,edit)))
        (dry_run . ,boolean) (checkpoint . ,boolean))
      '("path" "expected_revision" "edits"))
     output #'emacs-agent-editor--document-apply-edits 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_document_create"
     "Create a new visited text document inside the workspace."
     (emacs-agent-editor--object-schema
      `((path . ,string) (content . ,string)
        (dry_run . ,boolean) (checkpoint . ,boolean))
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
        (expected_revision . ,string) (dry_run . ,boolean)
        (approval_request_id . ,string))
      '("path" "new_path" "expected_revision"))
     output #'emacs-agent-editor--document-move 'destructive)
    (emacs-agent-tool-register
     "emacs_agent_document_delete"
     "Delete a guarded document with rollback metadata."
     (emacs-agent-editor--object-schema
      `((path . ,string) (expected_revision . ,string)
        (dry_run . ,boolean) (approval_request_id . ,string))
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
      `((changeset_id . ,string) (dry_run . ,boolean)
        (approval_request_id . ,string))
      '("changeset_id"))
     output #'emacs-agent-editor--changeset-rollback 'destructive)
    (emacs-agent-tool-register
     "emacs_agent_document_replace"
     "Replace exact authoritative text, with dry-run and revision guards."
     (emacs-agent-editor--object-schema
      `((path . ,string) (expected_revision . ,string)
        (old_text . ,string) (new_text . ,string)
        (replace_all . ,boolean) (expected_occurrences . ,integer)
        (dry_run . ,boolean) (checkpoint . ,boolean))
      '("path" "expected_revision" "old_text" "new_text"))
     output #'emacs-agent-editor--document-replace 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_document_apply_patch"
     "Validate and apply one strict single-file unified patch."
     (emacs-agent-editor--object-schema
      `((path . ,string) (expected_revision . ,string)
        (patch . ,string) (fuzz . ,integer)
        (dry_run . ,boolean) (checkpoint . ,boolean))
      '("path" "expected_revision" "patch"))
     output #'emacs-agent-editor--document-apply-patch 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_workspace_apply_edits"
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
     output #'emacs-agent-editor--workspace-apply-edits 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_document_status"
     "Return document visit, disk, conflict, encoding, and revision state."
     (emacs-agent-editor--object-schema `((path . ,string)) '("path"))
     output #'emacs-agent-editor--document-status 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_workspace_modified_documents"
     "List dirty, externally changed, conflicted, or deleted buffers."
     (emacs-agent-editor--object-schema nil)
     output #'emacs-agent-editor--workspace-modified-documents 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_changeset_list"
     "List filterable change-set summaries with immutable pagination."
     (emacs-agent-editor--object-schema
      `((path . ,string)
        (status . ((type . "array") (items . ,string)))
        (limit . ,integer) (cursor . ,string)))
     output #'emacs-agent-editor--changeset-list 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_changeset_get"
     "Return change-set metadata and a paginated frozen diff."
     (emacs-agent-editor--object-schema
      `((changeset_id . ,string) (max_chars . ,integer)
        (cursor . ,string))
      '("changeset_id"))
     output #'emacs-agent-editor--changeset-get 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_diagnostics"
     "Collect revision-bound safe parser and enabled editor diagnostics."
     (emacs-agent-editor--object-schema
      `((path . ,string) (expected_revision . ,string)
        (sources . ((type . "array") (items . ,string)))
        (wait_ms . ,integer))
      '("path"))
     output #'emacs-agent-editor--document-diagnostics 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_workspace_diagnostics"
     "Aggregate revision-bound diagnostics across workspace documents."
     (emacs-agent-editor--object-schema
      `((paths . ((type . "array") (items . ,string)))
        (include_globs . ((type . "array") (items . ,string)))
        (exclude_globs . ((type . "array") (items . ,string)))
        (severities . ((type . "array") (items . ,string)))
        (sources . ((type . "array") (items . ,string)))
        (wait_ms . ,integer) (limit . ,integer) (cursor . ,string)))
     output #'emacs-agent-editor--workspace-diagnostics 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_document_symbols"
     "Return the native imenu symbol tree for one document."
     (emacs-agent-editor--object-schema `((path . ,string)) '("path"))
     output #'emacs-agent-editor--document-symbols 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_workspace_symbols"
     "Search symbols through the active native xref backend."
     (emacs-agent-editor--object-schema
      `((path . ,string) (query . ,string) (kind . ,string)
        (path_prefix . ,string) (limit . ,integer))
      '("path" "query"))
     output #'emacs-agent-editor--workspace-symbols 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_symbol_definition"
     "Resolve definitions through the active native xref backend."
     (emacs-agent-editor--object-schema
      `((path . ,string) (position . ,position) (symbol . ,string))
      '("path" "position"))
     output #'emacs-agent-editor--symbol-definition 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_symbol_references"
     "Resolve references through the active native xref backend."
     (emacs-agent-editor--object-schema
      `((path . ,string) (position . ,position) (symbol . ,string))
      '("path" "position"))
     output #'emacs-agent-editor--symbol-references 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_editor_context_get"
     "Return safe metadata for the current Emacs editing context."
     (emacs-agent-editor--object-schema nil)
     output #'emacs-agent-editor--editor-context-get 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_format_document"
     "Preview or apply a server-configured trusted document formatter."
     (emacs-agent-editor--object-schema
      `((path . ,string) (expected_revision . ,string)
        (dry_run . ,boolean) (checkpoint . ,boolean)
        (approval_request_id . ,string))
      '("path" "expected_revision" "dry_run"))
     output #'emacs-agent-editor--format-document 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_symbol_rename"
     "Preview an Eglot semantic rename, then atomically apply only its frozen preview_id."
     (emacs-agent-editor--object-schema
      `((path . ,string) (position . ,position)
        (new_name . ,string) (expected_revision . ,string)
        (preview_id . ,string) (dry_run . ,boolean)
        (checkpoint . ,boolean)))
     output #'emacs-agent-editor--symbol-rename 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_code_actions"
     "List Eglot code actions or atomically apply a pure workspace-edit action; commands are never executed."
     (emacs-agent-editor--object-schema
      `((path . ,string) (range . ,range)
        (expected_revision . ,string) (kind . ,string)
        (action_id . ,string) (checkpoint . ,boolean)))
     output #'emacs-agent-editor--code-actions 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_format_range"
     "Preview Eglot range formatting, then atomically apply only its frozen preview_id."
     (emacs-agent-editor--object-schema
      `((path . ,string) (range . ,range)
        (expected_revision . ,string) (preview_id . ,string)
        (dry_run . ,boolean) (checkpoint . ,boolean)
        (approval_request_id . ,string)))
     output #'emacs-agent-editor--format-range 'mutating)
    (emacs-agent-tool-register
     "emacs_agent_approval_status"
     "Return credential-free TTL and revision-bound approval status."
     (emacs-agent-editor--object-schema
      `((approval_request_id . ,string))
      '("approval_request_id"))
     output #'emacs-agent-editor--approval-status 'read-only)
    (emacs-agent-tool-register
     "emacs_agent_approval_cancel"
     "Cancel a pending or approved request and audit the transition."
     (emacs-agent-editor--object-schema
      `((approval_request_id . ,string))
      '("approval_request_id"))
     output #'emacs-agent-editor--approval-cancel 'mutating)
    (dolist
        (entry
         `(("emacs_agent_document_read" . ,document-read-output)
           ("emacs_agent_workspace_info" . ,workspace-info-output)
           ("emacs_agent_document_apply_edits" . ,document-write-output)
           ("emacs_agent_document_create" . ,document-write-output)
           ("emacs_agent_workspace_files" . ,files-output)
           ("emacs_agent_workspace_search" . ,search-output)
           ("emacs_agent_document_move" . ,document-write-output)
           ("emacs_agent_document_delete" . ,document-write-output)
           ("emacs_agent_workspace_checkpoint" . ,workspace-write-output)
           ("emacs_agent_workspace_sync" . ,documents-output)
           ("emacs_agent_workspace_diff" . ,workspace-diff-output)
           ("emacs_agent_changeset_rollback" . ,workspace-write-output)
           ("emacs_agent_document_replace" . ,transform-write-output)
           ("emacs_agent_document_apply_patch" . ,transform-write-output)
           ("emacs_agent_workspace_apply_edits" . ,workspace-write-output)
           ("emacs_agent_document_status" . ,document-status-output)
           ("emacs_agent_workspace_modified_documents" . ,documents-output)
           ("emacs_agent_changeset_list" . ,changeset-list-output)
           ("emacs_agent_changeset_get" . ,changeset-detail-output)
           ("emacs_agent_document_diagnostics"
            . ,document-diagnostics-output)
           ("emacs_agent_workspace_diagnostics"
            . ,workspace-diagnostics-output)))
      (setf
       (emacs-agent-tool-output-schema
        (emacs-agent-tool-get (car entry)))
       (cdr entry)))))

;;;###autoload
(defun emacs-agent-editor-start (directory &optional port)
  "Start Agent Editor MCP for local workspace DIRECTORY.
PORT overrides `emacs-agent-editor-port' when non-nil."
  (interactive "DWorkspace: ")
  (when (emacs-agent-editor-running-p)
    (user-error "Agent Editor MCP is already running"))
  (unless (equal emacs-agent-editor-host "127.0.0.1")
    (user-error "Version 0.2 only supports the IPv4 loopback listener"))
  (let* ((token
          (when emacs-agent-editor-token-authentication-enabled
            (or emacs-agent-editor-bearer-token
                (emacs-agent-editor--random-token))))
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
                emacs-agent-protocol-tool-observer
                #'emacs-agent-editor--observe-tool
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
             emacs-agent-edit-record-function nil
             emacs-agent-protocol-tool-observer nil)
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
        emacs-agent-edit-record-function nil
        emacs-agent-protocol-tool-observer nil)
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
