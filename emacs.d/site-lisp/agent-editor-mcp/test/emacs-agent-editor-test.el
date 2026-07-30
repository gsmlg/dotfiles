;;; emacs-agent-editor-test.el --- Editor entrypoint integration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Lifecycle, v0.3 tool registry, schema, and composed handler tests.

;;; Code:

(require 'ert)
(require 'seq)
(require 'emacs-agent-editor)

(defconst emacs-agent-editor-test--tool-names
  '("emacs_agent_approval_cancel"
    "emacs_agent_approval_status"
    "emacs_agent_changeset_get"
    "emacs_agent_changeset_list"
    "emacs_agent_changeset_rollback"
    "emacs_agent_code_actions"
    "emacs_agent_document_apply_edits"
    "emacs_agent_document_apply_patch"
    "emacs_agent_document_create"
    "emacs_agent_document_delete"
    "emacs_agent_document_diagnostics"
    "emacs_agent_document_move"
    "emacs_agent_document_read"
    "emacs_agent_document_replace"
    "emacs_agent_document_status"
    "emacs_agent_document_symbols"
    "emacs_agent_editor_apply_edits"
    "emacs_agent_editor_checkpoint"
    "emacs_agent_editor_context_get"
    "emacs_agent_editor_diff"
    "emacs_agent_editor_info"
    "emacs_agent_editor_modified_documents"
    "emacs_agent_editor_sync"
    "emacs_agent_format_document"
    "emacs_agent_format_range"
    "emacs_agent_project_close"
    "emacs_agent_project_diagnostics"
    "emacs_agent_project_files"
    "emacs_agent_project_info"
    "emacs_agent_project_list"
    "emacs_agent_project_open"
    "emacs_agent_project_search"
    "emacs_agent_project_symbols"
    "emacs_agent_symbol_definition"
    "emacs_agent_symbol_references"
    "emacs_agent_symbol_rename")
  "Exact Agent Editor MCP v0.3 public tool registry.")

(defun emacs-agent-editor-test--cleanup-buffers (directory)
  "Kill test buffers visiting files beneath DIRECTORY."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer))
                  (inside
                   (condition-case nil
                       (file-in-directory-p
                        (expand-file-name file) directory)
                     (file-error nil))))
        (when inside
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(defmacro emacs-agent-editor-test--with-server (&rest body)
  "Run BODY with a project-free temporary Agent Editor server.

The lexical variable `root' names a temporary directory available to BODY.
No project is registered automatically."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "emacs-agent-editor-" t))
          (state (make-temp-file "emacs-agent-editor-state-" t))
          (emacs-agent-editor-state-directory state)
          (emacs-agent-editor-port 0)
          (emacs-agent-editor-token-authentication-enabled t)
          (emacs-agent-editor-bearer-token "test-bearer-token")
          (emacs-agent-editor-access-mode 'autonomous)
          (emacs-agent-editor-save-policy 'immediate)
          (emacs-agent-policy-filesystem-scope 'unrestricted)
          (emacs-agent-policy-allowed-roots nil)
          (emacs-agent-policy-denied-paths nil))
     (unwind-protect
         (progn
           (emacs-agent-editor-start)
           ,@body)
       (ignore-errors (emacs-agent-editor-stop))
       (emacs-agent-editor-test--cleanup-buffers root)
       (delete-directory root t)
       (delete-directory state t))))

(defun emacs-agent-editor-test--modern-request (name arguments)
  "Construct a modern MCP tools/call request for NAME and ARGUMENTS."
  (let ((meta
         '((io\.modelcontextprotocol/protocolVersion . "2026-07-28")
           (io\.modelcontextprotocol/clientInfo
            . ((name . "ert") (version . "1")))
           (io\.modelcontextprotocol/clientCapabilities . ()))))
    (emacs-agent-http-request-create
     :method "POST" :target "/mcp" :version "HTTP/1.1"
     :headers
     `(("mcp-protocol-version" . "2026-07-28")
       ("mcp-method" . "tools/call")
       ("mcp-name" . ,name))
     :body
     (emacs-agent-jsonrpc-encode
      `((jsonrpc . "2.0")
        (id . 41)
        (method . "tools/call")
        (params
         . ((name . ,name)
            (arguments . ,arguments)
            (_meta . ,meta))))))))

(defun emacs-agent-editor-test--response-json (response)
  "Decode protocol RESPONSE as an alist."
  (json-parse-string
   (decode-coding-string
   (emacs-agent-protocol-response-body response) 'utf-8)
   :object-type 'alist :array-type 'list :false-object :false))

(defun emacs-agent-editor-test--protocol-tool-result (name arguments)
  "Call tool NAME with ARGUMENTS and return validated structured content."
  (let* ((response
          (emacs-agent-protocol-handle-http-request
           (emacs-agent-editor-test--modern-request name arguments)))
         (json (emacs-agent-editor-test--response-json response))
         (rpc-result (alist-get 'result json)))
    (should (= (emacs-agent-protocol-response-status response) 200))
    (should (eq (alist-get 'isError rpc-result) :false))
    (or
     (alist-get 'structuredContent rpc-result)
     (ert-fail (format "Tool returned no structured content: %S" json)))))

(defun emacs-agent-editor-test--request-context ()
  "Create a deterministic context for invoking handlers directly."
  (emacs-agent-request-create
   :id 42 :protocol-version "2026-07-28"
   :client-info '((name . "ert") (version . "1"))))

(defun emacs-agent-editor-test--fail-file-retarget ()
  "Signal an injected post-rename buffer-retarget failure."
  (error "Injected after-set-visited-file-name failure"))

(defvar-local emacs-agent-editor-test--destination-parent nil
  "Destination parent replaced by the move save-hook regression.")

(defvar-local emacs-agent-editor-test--outside-parent nil
  "Outside directory used by the move save-hook regression.")

(defvar-local emacs-agent-editor-test--source-parent nil
  "Source parent replaced by the pre-write checkpoint regression.")

(defvar-local emacs-agent-editor-test--moved-source-parent nil
  "Renamed source parent used by the pre-write checkpoint regression.")

(defvar-local emacs-agent-editor-test--after-save-source-parent nil
  "Source parent replaced by the post-write checkpoint regression.")

(defvar-local emacs-agent-editor-test--after-save-moved-parent nil
  "Renamed source parent used by the post-write checkpoint regression.")

(defvar-local emacs-agent-editor-test--after-save-outside-parent nil
  "Outside directory used by the post-write checkpoint regression.")

(defvar-local emacs-agent-editor-test--after-save-write-string nil
  "Whether the post-write checkpoint regression writes a string.")

(defun emacs-agent-editor-test--replace-destination-parent ()
  "Replace the configured destination parent with an escaping symlink."
  (delete-directory emacs-agent-editor-test--destination-parent)
  (make-symbolic-link
   emacs-agent-editor-test--outside-parent
   emacs-agent-editor-test--destination-parent))

(defun emacs-agent-editor-test--replace-source-parent-before-save ()
  "Replace the configured source parent with an escaping symlink."
  (remove-hook
   'before-save-hook
   #'emacs-agent-editor-test--replace-source-parent-before-save t)
  (rename-file
   emacs-agent-editor-test--source-parent
   emacs-agent-editor-test--moved-source-parent)
  (make-symbolic-link
   emacs-agent-editor-test--outside-parent
   emacs-agent-editor-test--source-parent))

(defun emacs-agent-editor-test--write-sidecar-after-parent-swap ()
  "Swap the source parent and attempt one guarded sidecar write."
  (remove-hook
   'after-save-hook
   #'emacs-agent-editor-test--write-sidecar-after-parent-swap t)
  (rename-file
   emacs-agent-editor-test--after-save-source-parent
   emacs-agent-editor-test--after-save-moved-parent)
  (make-symbolic-link
   emacs-agent-editor-test--after-save-outside-parent
   emacs-agent-editor-test--after-save-source-parent)
  (let ((sidecar
         (expand-file-name
          "sidecar.txt"
          emacs-agent-editor-test--after-save-source-parent)))
    (if emacs-agent-editor-test--after-save-write-string
        (write-region "sidecar content\n" nil sidecar nil 'silent)
      (write-region (point-min) (point-max) sidecar nil 'silent))))

(defun emacs-agent-editor-test--dirty-buffer-after-save ()
  "Make the current buffer dirty once after a successful save."
  (remove-hook
   'after-save-hook
   #'emacs-agent-editor-test--dirty-buffer-after-save t)
  (goto-char (point-max))
  (insert "hook dirtied buffer\n"))

(defun emacs-agent-editor-test--rewrite-disk-after-save ()
  "Replace the visited file on disk once without changing its buffer."
  (remove-hook
   'after-save-hook
   #'emacs-agent-editor-test--rewrite-disk-after-save t)
  (let ((path buffer-file-name))
    (with-temp-buffer
      (insert "external rewrite\n")
      (write-region nil nil path nil 'silent))))

(defun emacs-agent-editor-test--format-before-save ()
  "Insert deterministic formatter text once before saving."
  (remove-hook
   'before-save-hook
   #'emacs-agent-editor-test--format-before-save t)
  (goto-char (point-max))
  (insert "formatted by save hook\n"))

(defun emacs-agent-editor-test--signal-after-save ()
  "Signal one structured error after a successful save."
  (remove-hook
   'after-save-hook
   #'emacs-agent-editor-test--signal-after-save t)
  (emacs-agent-signal 'checkpoint_failed :reason 'injected_after_save))

(defun emacs-agent-editor-test--tool (name)
  "Return registered tool NAME, failing the current test when absent."
  (or (emacs-agent-tool-get name)
      (ert-fail (format "Tool is not registered: %s" name))))

(defun emacs-agent-editor-test--input-schema (name)
  "Return input schema for registered tool NAME."
  (emacs-agent-tool-input-schema
   (emacs-agent-editor-test--tool name)))

(defun emacs-agent-editor-test--output-schema (name)
  "Return output schema for registered tool NAME."
  (emacs-agent-tool-output-schema
   (emacs-agent-editor-test--tool name)))

(defun emacs-agent-editor-test--properties (schema)
  "Return property definitions from object SCHEMA."
  (alist-get 'properties schema))

(defun emacs-agent-editor-test--property (schema property)
  "Return PROPERTY schema from object SCHEMA."
  (alist-get property
             (emacs-agent-editor-test--properties schema)))

(defun emacs-agent-editor-test--required-p (schema property)
  "Return non-nil when PROPERTY is required by object SCHEMA."
  (seq-contains-p
   (alist-get 'required schema)
   (symbol-name property)
   #'equal))

(defun emacs-agent-editor-test--array-item-schema
    (schema property)
  "Return the item schema for array PROPERTY in object SCHEMA."
  (alist-get
   'items
   (emacs-agent-editor-test--property schema property)))

(defun emacs-agent-editor-test--validate-output (name output)
  "Validate OUTPUT against registered tool NAME's output schema."
  (emacs-agent-schema-validate
   output
   (emacs-agent-editor-test--output-schema name)))

(defun emacs-agent-editor-test--assert-target-schema (schema)
  "Assert that SCHEMA carries the canonical document target fields."
  (dolist (property '(path project_id relative_path))
    (should
     (emacs-agent-editor-test--property schema property))
    (should
     (emacs-agent-editor-test--required-p schema property))))

(defun emacs-agent-editor-test--metadata ()
  "Read the active server's connection metadata."
  (json-parse-string
   (with-temp-buffer
     (insert-file-contents emacs-agent-editor--connection-file)
     (buffer-string))
   :object-type 'alist :array-type 'list :false-object :false))

(defun emacs-agent-editor-test--write-file (path content)
  "Write CONTENT to test file PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil path nil 'silent)))

(ert-deftest emacs-agent-editor-starts-project-free-with-v2-connection ()
  (emacs-agent-editor-test--with-server
    (should (emacs-agent-editor-running-p))
    (should (emacs-agent-runtime-p emacs-agent-editor--runtime))
    (should
     (= 0
        (hash-table-count
         (emacs-agent-runtime-project-registry
          emacs-agent-editor--runtime))))
    (let ((info (emacs-agent-editor--editor-info nil nil))
          (metadata (emacs-agent-editor-test--metadata)))
      (should (= (alist-get 'project_count info) 0))
      (should (= (alist-get 'managed_document_count info) 0))
      (should-not (assq 'root info))
      (should-not (assq 'workspace_id info))
      (emacs-agent-editor-test--validate-output
       "emacs_agent_editor_info" info)
      (should (= (alist-get 'schema_version metadata) 2))
      (should
       (equal
        (alist-get 'instance_id metadata)
        (emacs-agent-runtime-instance-id
         emacs-agent-editor--runtime)))
      (should-not (assq 'workspace metadata))
      (should
       (equal
        (alist-get 'filesystem_scope metadata)
        "unrestricted"))
      (should
       (equal
        (alist-get 'protocol_versions metadata)
        '("2026-07-28" "2025-11-25")))
      (should (eq (alist-get 'token_authentication metadata) t))
      (should
       (equal (alist-get 'token metadata) "test-bearer-token")))
    (should
     (string-match-p
      "runtime editor_.*0 projects"
      (emacs-agent-editor-status)))
    (should
     (= #o600
        (logand
         (file-modes emacs-agent-editor--connection-file)
         #o777)))
    (should
     (= #o700
        (logand
         (file-modes
          (file-name-directory
           emacs-agent-editor--connection-file))
         #o777)))))

(ert-deftest emacs-agent-editor-token-authentication-is-disabled-by-default ()
  (let* ((state
          (make-temp-file "emacs-agent-editor-no-auth-state-" t))
         (emacs-agent-editor-state-directory state)
         (emacs-agent-editor-port 0)
         (emacs-agent-editor-token-authentication-enabled nil)
         (emacs-agent-editor-bearer-token "ignored-token")
         (emacs-agent-policy-filesystem-scope 'unrestricted))
    (unwind-protect
        (progn
          (emacs-agent-editor-start)
          (should-not emacs-agent-editor--token)
          (should-not
           (emacs-agent-http-server-token
            emacs-agent-editor--http-server))
          (let ((metadata
                 (emacs-agent-editor-test--metadata))
                (info
                 (emacs-agent-editor--editor-info nil nil)))
            (should
             (eq
              (alist-get 'token_authentication metadata)
              :false))
            (should-not (assq 'token metadata))
            (should
             (equal
              (alist-get
               'type
               (alist-get 'authentication info))
              "none"))))
      (ignore-errors (emacs-agent-editor-stop))
      (delete-directory state t))))

(ert-deftest emacs-agent-editor-registers-exact-v03-tool-contract ()
  (emacs-agent-editor-test--with-server
    (let ((tools (emacs-agent-tool-list)))
      (should (= (length tools) 36))
      (should
       (equal
        (mapcar #'emacs-agent-tool-name tools)
        emacs-agent-editor-test--tool-names))
      (dolist (tool tools)
        (should
         (emacs-agent-tool-input-schema tool))
        (should
         (emacs-agent-tool-output-schema tool))
        (should
         (equal
          (alist-get
           'type
           (emacs-agent-tool-input-schema tool))
          "object"))
        (should
         (equal
          (alist-get
           'type
           (emacs-agent-tool-output-schema tool))
          "object"))
        (should-not
         (equal
          (emacs-agent-tool-output-schema tool)
          '((type . "object"))))
        (should-not
         (string-prefix-p
          (concat "emacs_agent_" "workspace_")
          (emacs-agent-tool-name tool)))))))

(ert-deftest emacs-agent-editor-input-schemas-require-explicit-context ()
  (emacs-agent-editor-test--with-server
    (dolist
        (name
         '("emacs_agent_document_read"
           "emacs_agent_document_status"
           "emacs_agent_document_apply_edits"
           "emacs_agent_document_replace"
           "emacs_agent_document_apply_patch"
           "emacs_agent_document_create"
           "emacs_agent_document_move"
           "emacs_agent_document_delete"
           "emacs_agent_document_diagnostics"
           "emacs_agent_document_symbols"
           "emacs_agent_changeset_list"
           "emacs_agent_symbol_definition"
           "emacs_agent_symbol_references"
           "emacs_agent_symbol_rename"
           "emacs_agent_code_actions"
           "emacs_agent_format_document"
           "emacs_agent_format_range"))
      (should
       (emacs-agent-editor-test--property
        (emacs-agent-editor-test--input-schema name)
        'project_id)))
    (should
     (emacs-agent-editor-test--property
      (emacs-agent-editor-test--input-schema
       "emacs_agent_editor_context_get")
      'project_id))
    (dolist
        (name
         '("emacs_agent_project_info"
           "emacs_agent_project_close"
           "emacs_agent_project_files"
           "emacs_agent_project_search"
           "emacs_agent_project_diagnostics"
           "emacs_agent_project_symbols"))
      (let ((schema
             (emacs-agent-editor-test--input-schema name)))
        (should
         (emacs-agent-editor-test--property
          schema 'project_id))
        (should
         (emacs-agent-editor-test--required-p
          schema 'project_id))))
    (should
     (emacs-agent-editor-test--required-p
      (emacs-agent-editor-test--input-schema
       "emacs_agent_project_open")
      'root))
    (should-not
     (emacs-agent-editor-test--property
      (emacs-agent-editor-test--input-schema
       "emacs_agent_project_list")
      'project_id))
    (let ((move
           (emacs-agent-editor-test--input-schema
            "emacs_agent_document_move")))
      (should
       (emacs-agent-editor-test--property move 'project_id))
      (should
       (emacs-agent-editor-test--property move 'new_project_id)))
    (dolist
        (name
         '("emacs_agent_editor_apply_edits"
           "emacs_agent_editor_checkpoint"))
      (let* ((schema
              (emacs-agent-editor-test--input-schema name))
             (document
              (emacs-agent-editor-test--array-item-schema
               schema 'documents)))
        (should
         (emacs-agent-editor-test--property
          document 'project_id))))
    (let* ((schema
            (emacs-agent-editor-test--input-schema
             "emacs_agent_editor_sync"))
           (document
            (emacs-agent-editor-test--array-item-schema
             schema 'documents)))
      (should
       (emacs-agent-editor-test--property
        schema 'documents))
      (should-not
       (emacs-agent-editor-test--property
        schema 'paths))
      (should
       (emacs-agent-editor-test--property
        document 'project_id))
      (should
       (emacs-agent-editor-test--required-p
        document 'path)))))

(ert-deftest emacs-agent-editor-output-schemas-carry-canonical-targets ()
  (emacs-agent-editor-test--with-server
    (dolist
        (name
         '("emacs_agent_document_read"
           "emacs_agent_document_status"
           "emacs_agent_document_apply_edits"
           "emacs_agent_document_replace"
           "emacs_agent_document_apply_patch"
           "emacs_agent_document_create"
           "emacs_agent_document_move"
           "emacs_agent_document_delete"
           "emacs_agent_document_diagnostics"
           "emacs_agent_document_symbols"
           "emacs_agent_format_document"))
      (emacs-agent-editor-test--assert-target-schema
       (emacs-agent-editor-test--output-schema name)))
    (dolist
        (entry
         '(("emacs_agent_editor_apply_edits" . documents)
           ("emacs_agent_editor_checkpoint" . documents)
           ("emacs_agent_editor_sync" . documents)
           ("emacs_agent_editor_modified_documents" . documents)
           ("emacs_agent_changeset_rollback" . documents)
           ("emacs_agent_project_files" . files)
           ("emacs_agent_project_search" . results)
           ("emacs_agent_project_diagnostics" . documents)
           ("emacs_agent_project_symbols" . symbols)
           ("emacs_agent_symbol_definition" . definitions)
           ("emacs_agent_symbol_references" . references)
           ("emacs_agent_symbol_rename" . documents)
           ("emacs_agent_code_actions" . documents)
           ("emacs_agent_format_range" . documents)))
      (emacs-agent-editor-test--assert-target-schema
       (emacs-agent-editor-test--array-item-schema
        (emacs-agent-editor-test--output-schema (car entry))
        (cdr entry))))
    (let* ((schema
            (emacs-agent-editor-test--output-schema
             "emacs_agent_project_diagnostics"))
           (diagnostic
            (emacs-agent-editor-test--array-item-schema
             schema 'diagnostics)))
      (emacs-agent-editor-test--assert-target-schema diagnostic))
    (let* ((schema
            (emacs-agent-editor-test--output-schema
             "emacs_agent_editor_context_get"))
           (buffer
            (emacs-agent-editor-test--property schema 'buffer)))
      (emacs-agent-editor-test--assert-target-schema buffer))))

(ert-deftest emacs-agent-editor-project-and-document-handler-outputs-validate ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-editor-test--request-context))
           (project-path
            (expand-file-name "project.el" root))
           (direct-path
            (expand-file-name "direct.el" root))
           (moved-path
            (expand-file-name "moved.el" root)))
      (emacs-agent-editor-test--write-file
       project-path "(defun project-function () t)\n")
      (let* ((opened
              (emacs-agent-editor--project-open
               `((root . ,root)) context))
             (project-id
              (alist-get 'project_id opened)))
        (emacs-agent-editor-test--validate-output
         "emacs_agent_project_open" opened)
        (let ((listed
               (emacs-agent-editor--project-list nil context))
              (info
               (emacs-agent-editor--project-info
                `((project_id . ,project-id)) context))
              (files
               (emacs-agent-editor--project-files
                `((project_id . ,project-id)) context)))
          (emacs-agent-editor-test--validate-output
           "emacs_agent_project_list" listed)
          (emacs-agent-editor-test--validate-output
           "emacs_agent_project_info" info)
          (emacs-agent-editor-test--validate-output
           "emacs_agent_project_files" files))
        (let ((project-read
               (emacs-agent-editor--document-read
                `((project_id . ,project-id)
                  (path . "project.el"))
                context)))
          (emacs-agent-editor-test--validate-output
           "emacs_agent_document_read" project-read)
          (should
           (equal
            (alist-get 'path project-read)
            (file-truename project-path)))
          (should
           (equal
            (alist-get 'project_id project-read)
            project-id))
          (should
           (equal
            (alist-get 'relative_path project-read)
            "project.el")))
        (let* ((created
                (emacs-agent-editor--document-create
                 `((path . ,direct-path)
                   (content
                    . "(defun direct-function () 1)\n"))
                 context))
               (read
                (emacs-agent-editor--document-read
                 `((path . ,direct-path)) context))
               (status
                (emacs-agent-editor--document-status
                 `((path . ,direct-path)) context))
               (revision (alist-get 'revision read))
               (edited
                (emacs-agent-editor--document-apply-edits
                 `((path . ,direct-path)
                   (expected_revision . ,revision)
                   (edits
                    . (((start
                         . ((line . 1) (column . 26)))
                        (end
                         . ((line . 1) (column . 27)))
                        (new_text . "2")))))
                 context)))
          (dolist
              (entry
               `(("emacs_agent_document_create" . ,created)
                 ("emacs_agent_document_read" . ,read)
                 ("emacs_agent_document_status" . ,status)
                 ("emacs_agent_document_apply_edits" . ,edited)))
            (emacs-agent-editor-test--validate-output
             (car entry) (cdr entry)))
          (dolist (key '(project_id relative_path))
            (should (eq (alist-get key created) :false))
            (should (eq (alist-get key read) :false))
            (should (eq (alist-get key status) :false))
            (should (eq (alist-get key edited) :false)))
          (let* ((fresh
                  (emacs-agent-editor--document-read
                   `((path . ,direct-path)) context))
                 (buffer
                  (find-buffer-visiting direct-path)))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert ";; human edit\n"))
            (let ((modified
                   (emacs-agent-editor--editor-modified-documents
                    nil context)))
              (emacs-agent-editor-test--validate-output
               "emacs_agent_editor_modified_documents"
               modified))
            (setq fresh
                  (emacs-agent-editor--document-read
                   `((path . ,direct-path)) context))
            (let ((checkpoint
                   (emacs-agent-editor--editor-checkpoint
                    `((documents
                       . (((path . ,direct-path)
                           (expected_revision
                            . ,(alist-get 'revision fresh))))))
                    context)))
              (emacs-agent-editor-test--validate-output
               "emacs_agent_editor_checkpoint" checkpoint))
            (let ((sync
                   (emacs-agent-editor--editor-sync
                    `((documents . (((path . ,direct-path)))))
                    context)))
              (emacs-agent-editor-test--validate-output
               "emacs_agent_editor_sync" sync)))
          (let* ((current
                  (emacs-agent-editor--document-read
                   `((path . ,direct-path)) context))
                 (diagnostics
                  (emacs-agent-editor--document-diagnostics
                   `((path . ,direct-path)
                     (expected_revision
                      . ,(alist-get 'revision current))
                     (sources . ["parser"]))
                   context))
                 (symbols
                  (emacs-agent-editor--document-symbols
                   `((path . ,direct-path)) context))
                 (emacs-agent-semantic-format-function
                  (lambda (content _mode) content))
                 (format
                  (emacs-agent-editor--format-document
                   `((path . ,direct-path)
                     (expected_revision
                      . ,(alist-get 'revision current))
                     (dry_run . t))
                   context)))
            (emacs-agent-editor-test--validate-output
             "emacs_agent_document_diagnostics" diagnostics)
            (emacs-agent-editor-test--validate-output
             "emacs_agent_document_symbols" symbols)
            (emacs-agent-editor-test--validate-output
             "emacs_agent_format_document" format))
          (let* ((current
                  (emacs-agent-editor--document-read
                   `((path . ,direct-path)) context))
                 (moved
                  (emacs-agent-editor--document-move
                   `((path . ,direct-path)
                     (new_path . ,moved-path)
                     (expected_revision
                      . ,(alist-get 'revision current)))
                   context))
                 (deleted
                  (emacs-agent-editor--document-delete
                   `((path . ,moved-path)
                     (expected_revision
                      . ,(alist-get 'new_revision moved)))
                   context)))
            (emacs-agent-editor-test--validate-output
             "emacs_agent_document_move" moved)
            (emacs-agent-editor-test--validate-output
             "emacs_agent_document_delete" deleted)
            (let* ((changesets
                    (emacs-agent-editor--changeset-list nil context))
                   (first
                    (aref
                     (alist-get 'changesets changesets)
                     0))
                   (changeset-id
                    (alist-get 'changeset_id first))
                   (detail
                    (emacs-agent-editor--changeset-get
                     `((changeset_id . ,changeset-id))
                     context))
                   (diff
                    (emacs-agent-editor--editor-diff
                     `((changeset_id . ,changeset-id))
                     context)))
              (emacs-agent-editor-test--validate-output
               "emacs_agent_changeset_list" changesets)
              (emacs-agent-editor-test--validate-output
               "emacs_agent_changeset_get" detail)
              (emacs-agent-editor-test--validate-output
               "emacs_agent_editor_diff" diff))))
        (let* ((approval
                (emacs-agent-runtime-request-approval
                 emacs-agent-editor--runtime
                 "document_delete"
                 `((path . ,project-path))
                 emacs-agent-editor--token))
               (approval-id
                (plist-get approval :approval_request_id))
               (approval-status
                (emacs-agent-editor--approval-status
                 `((approval_request_id . ,approval-id))
                 context))
               (approval-cancel
                (emacs-agent-editor--approval-cancel
                 `((approval_request_id . ,approval-id))
                 context)))
          (emacs-agent-editor-test--validate-output
           "emacs_agent_approval_status" approval-status)
          (emacs-agent-editor-test--validate-output
           "emacs_agent_approval_cancel" approval-cancel))
        (let* ((buffer
                (find-buffer-visiting project-path))
               (context-output
                (with-current-buffer buffer
                  (emacs-agent-editor--editor-context-get
                   `((project_id . ,project-id)) context))))
          (emacs-agent-editor-test--validate-output
           "emacs_agent_editor_context_get" context-output))
        (let* ((buffer
                (find-buffer-visiting project-path))
               (closed
                (emacs-agent-editor--project-close
                 `((project_id . ,project-id)) context)))
          (emacs-agent-editor-test--validate-output
           "emacs_agent_project_close" closed)
          (should (buffer-live-p buffer))
          (should
           (equal
            (alist-get
             'path
             (emacs-agent-editor--document-read
              `((path . ,project-path)) context))
            (file-truename project-path))))))))

(ert-deftest emacs-agent-editor-noop-format-checkpoints-dirty-buffer ()
  (emacs-agent-editor-test--with-server
    (let* ((path (expand-file-name "identity-format.txt" root))
           (context (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,path) (content . "already formatted\n"))
             context))
           (revision
            (alist-get
             'revision
             (emacs-agent-editor--document-read
              `((path . ,path)) context)))
           (emacs-agent-semantic-format-function
            (lambda (content _mode) content)))
      (with-current-buffer (find-buffer-visiting path)
        (goto-char (point-max))
        (insert "dirty\n")
        (setq revision
              (emacs-agent-document-revision
               (gethash
                (file-truename path)
                (emacs-agent-runtime-document-registry
                 emacs-agent-editor--runtime)))))
      (let ((structured
             (emacs-agent-editor-test--protocol-tool-result
              "emacs_agent_format_document"
              `((path . ,path)
                (expected_revision . ,revision)
                (dry_run . :false)))))
        (should (eq (alist-get 'changed structured) :false))
        (should (eq (alist-get 'changeset_id structured) :false))
        (should (eq (alist-get 'checkpointed structured) t))
        (should (eq (alist-get 'applied structured) t)))
      (should-not
       (buffer-modified-p (find-buffer-visiting path)))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
        "already formatted\ndirty\n")))))

(ert-deftest emacs-agent-editor-noop-format-manual-keeps-dirty-buffer ()
  (emacs-agent-editor-test--with-server
    (let* ((path (expand-file-name "identity-format.txt" root))
           (context (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,path) (content . "already formatted\n"))
             context))
           (document
            (gethash
             (file-truename path)
             (emacs-agent-runtime-document-registry
              emacs-agent-editor--runtime)))
           (emacs-agent-semantic-format-function
            (lambda (content _mode) content)))
      (setf
       (emacs-agent-runtime-save-policy emacs-agent-editor--runtime)
       'manual)
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "dirty\n"))
      (let* ((revision (emacs-agent-document-revision document))
             (structured
              (emacs-agent-editor-test--protocol-tool-result
               "emacs_agent_format_document"
               `((path . ,path)
                 (expected_revision . ,revision)
                 (dry_run . :false)
                 (checkpoint . :false)))))
        (should (eq (alist-get 'changed structured) :false))
        (should (eq (alist-get 'changeset_id structured) :false))
        (should (eq (alist-get 'checkpointed structured) :false))
        (should (eq (alist-get 'applied structured) t)))
      (should
       (buffer-modified-p (emacs-agent-document-buffer document)))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
        "already formatted\n")))))

(ert-deftest emacs-agent-editor-noop-format-records-before-save-change ()
  (emacs-agent-editor-test--with-server
    (let* ((path (expand-file-name "hook-format.txt" root))
           (context (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,path) (content . "already formatted\n"))
             context))
           (document
            (gethash
             (file-truename path)
             (emacs-agent-runtime-document-registry
              emacs-agent-editor--runtime)))
           (buffer (emacs-agent-document-buffer document))
           (emacs-agent-semantic-format-function
            (lambda (content _mode) content)))
      (with-current-buffer buffer
        (set-buffer-modified-p t)
        (add-hook
         'before-save-hook
         #'emacs-agent-editor-test--format-before-save nil t))
      (let* ((revision (emacs-agent-document-revision document))
             (structured
              (emacs-agent-editor-test--protocol-tool-result
               "emacs_agent_format_document"
               `((path . ,path)
                 (expected_revision . ,revision)
                 (dry_run . :false)))))
        (should (eq (alist-get 'changed structured) t))
        (should (eq (alist-get 'modified structured) t))
        (should (stringp (alist-get 'changeset_id structured)))
        (should (eq (alist-get 'checkpointed structured) t))
        (should
         (string-match-p
          "formatted by save hook"
          (alist-get 'diff structured))))
      (should-not (buffer-modified-p buffer))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
        "already formatted\nformatted by save hook\n")))))

(ert-deftest emacs-agent-editor-checkpoint-reports-before-save-change ()
  (emacs-agent-editor-test--with-server
    (let* ((path (expand-file-name "hook-checkpoint.txt" root))
           (context (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,path) (content . "before\n"))
             context))
           (document
            (gethash
             (file-truename path)
             (emacs-agent-runtime-document-registry
              emacs-agent-editor--runtime)))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (set-buffer-modified-p t)
        (add-hook
         'before-save-hook
         #'emacs-agent-editor-test--format-before-save nil t))
      (let* ((revision (emacs-agent-document-revision document))
             (result
              (emacs-agent-editor--editor-checkpoint
               `((documents
                  . (((path . ,path)
                      (expected_revision . ,revision)))))
               context))
             (documents (alist-get 'documents result))
             (entry (aref documents 0)))
        (should (eq (alist-get 'applied result) t))
        (should (eq (alist-get 'checkpointed result) t))
        (should (eq (alist-get 'modified result) t))
        (should (stringp (alist-get 'changeset_id result)))
        (should
         (string-match-p
          "formatted by save hook"
          (alist-get 'diff result)))
        (should (eq (alist-get 'modified entry) t))
        (should
         (string-match-p
          "formatted by save hook"
          (alist-get 'diff entry)))
        (should-not
         (equal
          (alist-get 'old_revision entry)
          (alist-get 'new_revision entry))))
      (should-not (buffer-modified-p buffer))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
        "before\nformatted by save hook\n")))))

(ert-deftest emacs-agent-editor-noop-edit-checkpoints-dirty-buffer ()
  (emacs-agent-editor-test--with-server
    (let* ((path (expand-file-name "noop-edit.txt" root))
           (context (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,path) (content . "before\n"))
             context))
           (document
            (gethash
             (file-truename path)
             (emacs-agent-runtime-document-registry
              emacs-agent-editor--runtime))))
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "dirty\n"))
      (let* ((revision (emacs-agent-document-revision document))
             (result
              (emacs-agent-editor-test--protocol-tool-result
               "emacs_agent_document_apply_edits"
               `((path . ,path)
                 (expected_revision . ,revision)
                 (edits
                  . [((start . ((line . 1) (column . 0)))
                      (end . ((line . 1) (column . 0)))
                      (new_text . ""))])
                 (checkpoint . t)))))
        (should (eq (alist-get 'modified result) :false))
        (should (eq (alist-get 'changeset_id result) :false))
        (should (eq (alist-get 'checkpointed result) t))
        (should (eq (alist-get 'applied result) t))
        (should (equal (alist-get 'diff result) "")))
      (should-not
       (buffer-modified-p (emacs-agent-document-buffer document)))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
        "before\ndirty\n")))))

(ert-deftest emacs-agent-editor-noop-edit-manual-keeps-dirty-buffer ()
  (emacs-agent-editor-test--with-server
    (let* ((path (expand-file-name "noop-edit.txt" root))
           (context (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,path) (content . "before\n"))
             context))
           (document
            (gethash
             (file-truename path)
             (emacs-agent-runtime-document-registry
              emacs-agent-editor--runtime))))
      (setf
       (emacs-agent-runtime-save-policy emacs-agent-editor--runtime)
       'manual)
      (with-current-buffer (emacs-agent-document-buffer document)
        (goto-char (point-max))
        (insert "dirty\n"))
      (let* ((revision (emacs-agent-document-revision document))
             (result
              (emacs-agent-editor-test--protocol-tool-result
               "emacs_agent_document_apply_edits"
               `((path . ,path)
                 (expected_revision . ,revision)
                 (edits
                  . [((start . ((line . 1) (column . 0)))
                      (end . ((line . 1) (column . 0)))
                      (new_text . ""))])
                 (checkpoint . :false)))))
        (should (eq (alist-get 'modified result) :false))
        (should (eq (alist-get 'changeset_id result) :false))
        (should (eq (alist-get 'checkpointed result) :false))
        (should (eq (alist-get 'applied result) t))
        (should (equal (alist-get 'diff result) "")))
      (should
       (buffer-modified-p (emacs-agent-document-buffer document)))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
        "before\n")))))

(defun emacs-agent-editor-test--assert-post-write-sidecar-failure
    (write-string)
  "Assert safe post-write failure behavior.
When WRITE-STRING is non-nil, exercise the string form of `write-region'."
  (emacs-agent-editor-test--with-server
    (let ((outside
           (make-temp-file "emacs-agent-after-save-outside-" t)))
      (unwind-protect
          (let* ((source-parent
                  (expand-file-name "source" root))
                 (moved-parent
                  (expand-file-name "saved-source" root))
                 (path (expand-file-name "file.txt" source-parent))
                 (saved-path
                  (expand-file-name "file.txt" moved-parent))
                 (outside-path
                  (expand-file-name "file.txt" outside))
                 (outside-sidecar
                  (expand-file-name "sidecar.txt" outside))
                 (context
                  (emacs-agent-editor-test--request-context))
                 (_ (make-directory source-parent))
                 (_outside
                  (with-temp-file outside-path
                    (insert "outside sentinel\n")))
                 (_created
                  (emacs-agent-editor--document-create
                   `((path . ,path) (content . "before\n"))
                   context))
                 (document
                  (gethash
                   (file-truename path)
                   (emacs-agent-runtime-document-registry
                    emacs-agent-editor--runtime)))
                 (buffer (emacs-agent-document-buffer document))
                 (revision
                  (emacs-agent-document-revision document))
                 error-data)
            (with-current-buffer buffer
              (setq-local
               emacs-agent-editor-test--after-save-source-parent
               source-parent)
              (setq-local
               emacs-agent-editor-test--after-save-moved-parent
               moved-parent)
              (setq-local
               emacs-agent-editor-test--after-save-outside-parent
               outside)
              (setq-local
               emacs-agent-editor-test--after-save-write-string
               write-string)
              (add-hook
               'after-save-hook
               #'emacs-agent-editor-test--write-sidecar-after-parent-swap
               nil t))
            (setq error-data
                  (should-error
                   (emacs-agent-editor--document-apply-edits
                    `((path . ,path)
                      (expected_revision . ,revision)
                      (edits
                       . (((start
                            . ((line . 1) (column . 0)))
                           (end
                            . ((line . 1) (column . 6)))
                           (new_text . "after")
                           (expected_text . "before")))))
                    context)
                   :type 'emacs-agent-tool-error))
            (let ((error-object (cadr error-data)))
              (should
               (equal
                (alist-get 'code error-object)
                "CHECKPOINT_FAILED"))
              (should
               (eq
                (alist-get 'partial_completion error-object)
                t))
              (should
               (eq
                (alist-get 'reconciliation_required error-object)
                t)))
            (should (file-exists-p saved-path))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents saved-path)
                (buffer-string))
              "after\n"))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents outside-path)
                (buffer-string))
              "outside sentinel\n"))
            (if write-string
                (should
                 (equal
                  (with-temp-buffer
                    (insert-file-contents outside-sidecar)
                    (buffer-string))
                  "sidecar content\n"))
              (should-not (file-exists-p outside-sidecar)))
            (with-current-buffer buffer
              (should (buffer-modified-p))
              (should (equal (buffer-string) "after\n")))
            (should (emacs-agent-document-degraded document))
            (should
             (eq
              (emacs-agent-runtime-health-state
               emacs-agent-editor--runtime)
              'degraded))
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer))
        (delete-directory outside t)))))

(ert-deftest emacs-agent-editor-edit-keeps-post-write-numeric-failure ()
  (emacs-agent-editor-test--assert-post-write-sidecar-failure nil))

(ert-deftest emacs-agent-editor-edit-keeps-post-write-string-failure ()
  (emacs-agent-editor-test--assert-post-write-sidecar-failure t))

(ert-deftest emacs-agent-editor-position-contract-is-explicit ()
  (emacs-agent-editor-test--with-server
    (let ((semantics
           (alist-get
            'position_semantics
            (emacs-agent-editor--editor-info nil nil))))
      (should (= (alist-get 'lineBase semantics) 1))
      (should (= (alist-get 'columnBase semantics) 0))
      (should
       (equal
        (alist-get 'unit semantics)
        "emacs_character"))
      (should
       (equal (alist-get 'range semantics) "half_open"))
      (should (= (alist-get 'tabWidth semantics) 1))
      (should
       (equal
        (alist-get 'editsRelativeTo semantics)
        "expected_revision"))
      (should
       (equal
        (alist-get 'applicationOrder semantics)
        "descending")))))

(ert-deftest emacs-agent-editor-runtime-not-started-is-a-stable-tool-error ()
  (ignore-errors (emacs-agent-editor-stop))
  (let* ((path
          (expand-file-name
           "missing.txt"
           temporary-file-directory))
         (error-data
          (should-error
           (emacs-agent-editor--document-read
            `((path . ,path)) nil))))
    (should (eq (car error-data) 'emacs-agent-tool-error))
    (should
     (equal
      (alist-get 'code (cadr error-data))
      "RUNTIME_NOT_STARTED"))))

(ert-deftest emacs-agent-editor-stop-cleans-up-after-listener-error ()
  (emacs-agent-editor-test--with-server
    (let* ((path
            (expand-file-name "kept.txt" root))
           (context
            (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,path) (content . "kept\n"))
             context))
           (buffer (find-buffer-visiting path))
           (runtime emacs-agent-editor--runtime)
           (connection emacs-agent-editor--connection-file)
           (original-stop
            (symbol-function 'emacs-agent-http-stop)))
      (emacs-agent-editor--project-open
       `((root . ,root)) context)
      (emacs-agent-session-create
       "2025-11-25" '((name . "ert")))
      (cl-letf
          (((symbol-function 'emacs-agent-http-stop)
            (lambda (server)
              (funcall original-stop server)
              (error "Injected listener stop failure"))))
        (should-error
         (emacs-agent-editor-stop)
         :type 'error))
      (should-not emacs-agent-editor--http-server)
      (should-not emacs-agent-editor--runtime)
      (should-not emacs-agent-current-runtime)
      (should-not emacs-agent-editor--token)
      (should (= (hash-table-count emacs-agent--sessions) 0))
      (should-not (emacs-agent-tool-list))
      (should-not (file-exists-p connection))
      (should
       (= 0
          (hash-table-count
           (emacs-agent-runtime-project-registry runtime))))
      (should
       (= 0
          (hash-table-count
           (emacs-agent-runtime-document-registry runtime))))
      (should (buffer-live-p buffer)))))

(ert-deftest emacs-agent-editor-move-rename-failure-is-auditable ()
  (emacs-agent-editor-test--with-server
    (let* ((source
            (expand-file-name "rename-source.txt" root))
           (destination
            (expand-file-name "rename-destination.txt" root))
           (context
            (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,source) (content . "before\n"))
             context))
           (buffer (find-buffer-visiting source))
           (canonical-source (file-truename source))
           (canonical-destination
            (expand-file-name
             (file-name-nondirectory destination)
             (file-truename (file-name-directory destination))))
           (backup-directory-alist nil)
           (rename-file-function
            (symbol-function 'rename-file)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "checkpointed before rename\n"))
      (let* ((read
              (emacs-agent-editor--document-read
               `((path . ,source)) context))
             (revision (alist-get 'revision read))
             error-data)
        (cl-letf
            (((symbol-function 'rename-file)
              (lambda (old-name new-name
                       &optional ok-if-already-exists)
                (if
                    (and
                     (equal
                      (expand-file-name old-name)
                      canonical-source)
                     (equal
                      (expand-file-name new-name)
                      canonical-destination))
                    (signal 'file-error
                            '("injected rename failure"))
                  (funcall
                   rename-file-function old-name new-name
                   ok-if-already-exists)))))
          (setq error-data
                (should-error
                 (emacs-agent-editor--document-move
                  `((path . ,source)
                    (new_path . ,destination)
                    (expected_revision . ,revision))
                  context)
                 :type 'emacs-agent-tool-error)))
        (let ((error-object (cadr error-data)))
          (should
           (equal
            (alist-get 'code error-object)
            "FILESYSTEM_ERROR"))
          (should
           (eq
            (alist-get 'reconciliation_required error-object)
            t))
          (should
           (eq
            (alist-get 'partial_completion error-object)
            t))
          (should
           (eq
            (alist-get 'checkpointed error-object)
            t)))
        (should
         (file-exists-p source))
        (should
         (file-exists-p (concat source "~")))
        (should-not
         (file-exists-p destination))
        (should
         (equal
          (file-truename (buffer-file-name buffer))
          (file-truename source)))
        (should-not
         (buffer-modified-p buffer))
        (let ((document
               (gethash
                (file-truename source)
                (emacs-agent-runtime-document-registry
                 emacs-agent-editor--runtime))))
          (should (emacs-agent-document-degraded document)))
        (should
         (eq
          (emacs-agent-runtime-health-state
           emacs-agent-editor--runtime)
          'degraded))))))

(ert-deftest emacs-agent-editor-move-retarget-failure-rolls-back ()
  (emacs-agent-editor-test--with-server
    (let* ((source
            (expand-file-name "retarget-source.txt" root))
           (destination
            (expand-file-name "retarget-destination.txt" root))
           (context
            (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,source) (content . "before\n"))
             context))
           (buffer (find-buffer-visiting source))
           (revision
            (alist-get
             'revision
             (emacs-agent-editor--document-read
              `((path . ,source)) context)))
           error-data)
      (with-current-buffer buffer
        (add-hook
         'after-set-visited-file-name-hook
         #'emacs-agent-editor-test--fail-file-retarget nil t))
      (setq error-data
            (should-error
             (emacs-agent-editor--document-move
              `((path . ,source)
                (new_path . ,destination)
                (expected_revision . ,revision))
              context)
             :type 'emacs-agent-tool-error))
      (let ((error-object (cadr error-data)))
        (should (equal (alist-get 'code error-object) "FILESYSTEM_ERROR"))
        (should (eq (alist-get 'partial_completion error-object) t))
        (should
         (eq (alist-get 'reconciliation_required error-object) :false))
        (should
         (eq
          (alist-get 'filesystem_rollback_succeeded error-object)
          t))
        (should
         (eq
          (alist-get 'filesystem_rollback_guaranteed error-object)
          t)))
      (should (file-exists-p source))
      (should-not (file-exists-p destination))
      (should
       (equal
        (file-truename (buffer-file-name buffer))
        (file-truename source)))
      (let* ((registry
              (emacs-agent-runtime-document-registry
               emacs-agent-editor--runtime))
             (document (gethash (file-truename source) registry)))
        (should document)
        (should-not (gethash (file-truename destination) registry))
        (should
         (equal
          (emacs-agent-document-canonical-path document)
          (file-truename source)))
        (should-not (emacs-agent-document-degraded document)))
      (should
       (eq
        (emacs-agent-runtime-health-state
         emacs-agent-editor--runtime)
        'healthy)))))

(ert-deftest emacs-agent-editor-move-retarget-rollback-failure-realigns ()
  (emacs-agent-editor-test--with-server
    (let* ((source
            (expand-file-name "retarget-source.txt" root))
           (destination
            (expand-file-name "retarget-destination.txt" root))
           (context
            (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,source) (content . "before\n"))
             context))
           (buffer (find-buffer-visiting source))
           (revision
            (alist-get
             'revision
             (emacs-agent-editor--document-read
              `((path . ,source)) context)))
           (original-rename (symbol-function 'rename-file))
           error-data)
      (with-current-buffer buffer
        (add-hook
         'after-set-visited-file-name-hook
         #'emacs-agent-editor-test--fail-file-retarget nil t))
      (cl-letf
          (((symbol-function 'rename-file)
            (lambda (from to &optional ok-if-already-exists)
              (if (and
                   (equal from (file-truename destination))
                   (equal to (file-truename source)))
                  (signal 'file-error
                          '("Injected rollback rename failure"))
                (funcall
                 original-rename from to ok-if-already-exists)))))
        (setq error-data
              (should-error
               (emacs-agent-editor--document-move
                `((path . ,source)
                  (new_path . ,destination)
                  (expected_revision . ,revision))
                context)
               :type 'emacs-agent-tool-error)))
      (let ((error-object (cadr error-data)))
        (should (equal (alist-get 'code error-object) "FILESYSTEM_ERROR"))
        (should (eq (alist-get 'partial_completion error-object) t))
        (should
         (eq (alist-get 'reconciliation_required error-object) t))
        (should
         (eq
          (alist-get 'filesystem_rollback_succeeded error-object)
          :false))
        (should
         (eq
          (alist-get 'filesystem_rollback_guaranteed error-object)
          :false))
        (should
         (equal
          (alist-get 'surviving_path error-object)
          (file-truename destination))))
      (should-not (file-exists-p source))
      (should (file-exists-p destination))
      (should
       (equal
        (file-truename (buffer-file-name buffer))
        (file-truename destination)))
      (let* ((registry
              (emacs-agent-runtime-document-registry
               emacs-agent-editor--runtime))
             (document
              (gethash (file-truename destination) registry)))
        (should document)
        (should-not (gethash (file-truename source) registry))
        (should
         (equal
          (emacs-agent-document-canonical-path document)
          (file-truename destination)))
        (should (emacs-agent-document-degraded document)))
      (should
       (eq
        (emacs-agent-runtime-health-state
         emacs-agent-editor--runtime)
        'degraded)))))

(ert-deftest emacs-agent-editor-move-reresolves-after-save-hook ()
  (emacs-agent-editor-test--with-server
    (let ((outside (make-temp-file "emacs-agent-move-outside-" t)))
      (unwind-protect
          (let* ((source
                  (expand-file-name "move-source.txt" root))
                 (destination-parent
                  (expand-file-name "destination" root))
                 (destination
                  (expand-file-name "move-destination.txt"
                                    destination-parent))
                 (outside-destination
                  (expand-file-name "move-destination.txt" outside))
                 (context
                  (emacs-agent-editor-test--request-context))
                 (project
                  (emacs-agent-editor--project-open
                   `((root . ,root)) context))
                 (project-id (alist-get 'project_id project))
                 (_ (make-directory destination-parent))
                 (_created
                  (emacs-agent-editor--document-create
                   `((path . "move-source.txt")
                     (project_id . ,project-id)
                     (content . "before\n"))
                   context))
                 (buffer (find-buffer-visiting source))
                 error-data)
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "saved before move\n")
              (setq-local
               emacs-agent-editor-test--destination-parent
               destination-parent)
              (setq-local
               emacs-agent-editor-test--outside-parent
               outside)
              (add-hook
               'after-save-hook
               #'emacs-agent-editor-test--replace-destination-parent
               nil t))
            (let ((revision
                   (alist-get
                    'revision
                    (emacs-agent-editor--document-read
                     `((path . "move-source.txt")
                       (project_id . ,project-id))
                     context))))
              (setq error-data
                    (should-error
                     (emacs-agent-editor--document-move
                      `((path . "move-source.txt")
                        (project_id . ,project-id)
                        (new_path . "destination/move-destination.txt")
                        (new_project_id . ,project-id)
                        (expected_revision . ,revision))
                      context)
                     :type 'emacs-agent-tool-error)))
            (should
             (equal
              (alist-get 'code (cadr error-data))
              "EXTERNAL_CHANGE_CONFLICT"))
            (should (eq (alist-get 'checkpointed (cadr error-data)) t))
            (should
             (eq (alist-get 'partial_completion (cadr error-data)) t))
            (should
             (eq
              (alist-get 'reconciliation_required (cadr error-data))
              :false))
            (should (file-exists-p source))
            (should-not (file-exists-p destination))
            (should-not (file-exists-p outside-destination))
            (should
             (equal
              (file-truename (buffer-file-name buffer))
              (file-truename source)))
            (let ((registry
                   (emacs-agent-runtime-document-registry
                    emacs-agent-editor--runtime)))
              (should (gethash (file-truename source) registry))
              (should-not
               (gethash (file-truename outside-destination) registry))))
        (delete-directory outside t)))))

(ert-deftest emacs-agent-editor-move-aborts-when-save-hook-dirties-buffer ()
  (emacs-agent-editor-test--with-server
    (let* ((source (expand-file-name "hook-source.txt" root))
           (destination (expand-file-name "hook-destination.txt" root))
           (context (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,source) (content . "before\n"))
             context))
           (buffer (find-buffer-visiting source))
           error-data)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "saved before hook\n")
        (add-hook
         'after-save-hook
         #'emacs-agent-editor-test--dirty-buffer-after-save nil t))
      (let ((revision
             (emacs-agent-document-revision
              (gethash
               (file-truename source)
               (emacs-agent-runtime-document-registry
                emacs-agent-editor--runtime)))))
        (setq error-data
              (should-error
               (emacs-agent-editor--document-move
                `((path . ,source)
                  (new_path . ,destination)
                  (expected_revision . ,revision))
                context)
               :type 'emacs-agent-tool-error)))
      (let ((error-object (cadr error-data)))
        (should
         (equal (alist-get 'code error-object) "CHECKPOINT_FAILED"))
        (should (eq (alist-get 'checkpointed error-object) :false))
        (should (eq (alist-get 'partial_completion error-object) t))
        (should
         (eq
          (alist-get 'reconciliation_required error-object)
          :false)))
      (should (file-exists-p source))
      (should-not (file-exists-p destination))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents source)
          (buffer-string))
        "before\nsaved before hook\n"))
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should
         (equal
          (buffer-string)
          "before\nsaved before hook\nhook dirtied buffer\n"))
        (should
         (equal
          (file-truename buffer-file-name)
          (file-truename source))))
      (let ((document
             (gethash
              (file-truename source)
              (emacs-agent-runtime-document-registry
               emacs-agent-editor--runtime))))
        (should document)
        (should-not (emacs-agent-document-degraded document))
        (should
         (equal
          (emacs-agent-document-disk-fingerprint document)
          (emacs-agent-document--disk-fingerprint source))))
      (should
       (eq
        (emacs-agent-runtime-health-state
         emacs-agent-editor--runtime)
        'healthy)))))

(ert-deftest emacs-agent-editor-move-preserves-buffer-on-disk-only-rewrite ()
  (emacs-agent-editor-test--with-server
    (let* ((source (expand-file-name "disk-hook-source.txt" root))
           (destination
            (expand-file-name "disk-hook-destination.txt" root))
           (context (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,source) (content . "before\n"))
             context))
           (buffer (find-buffer-visiting source))
           error-data)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "saved content\n")
        (add-hook
         'after-save-hook
         #'emacs-agent-editor-test--rewrite-disk-after-save nil t))
      (let ((revision
             (emacs-agent-document-revision
              (gethash
               (file-truename source)
               (emacs-agent-runtime-document-registry
                emacs-agent-editor--runtime)))))
        (setq error-data
              (should-error
               (emacs-agent-editor--document-move
                `((path . ,source)
                  (new_path . ,destination)
                  (expected_revision . ,revision))
                context)
               :type 'emacs-agent-tool-error)))
      (let ((error-object (cadr error-data)))
        (should
         (equal (alist-get 'code error-object) "CHECKPOINT_FAILED"))
        (should (eq (alist-get 'checkpointed error-object) :false))
        (should (eq (alist-get 'partial_completion error-object) t))
        (should
         (eq
          (alist-get 'reconciliation_required error-object)
          t)))
      (should (file-exists-p source))
      (should-not (file-exists-p destination))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents source)
          (buffer-string))
        "external rewrite\n"))
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should
         (equal
          (buffer-string)
          "before\nsaved content\n"))
        (should
         (equal
          (file-truename buffer-file-name)
          (file-truename source))))
      (let ((document
             (gethash
              (file-truename source)
              (emacs-agent-runtime-document-registry
               emacs-agent-editor--runtime))))
        (should document)
        (should (emacs-agent-document-degraded document))
        (should
         (emacs-agent-document-externally-modified document)))
      (should
       (eq
        (emacs-agent-runtime-health-state
         emacs-agent-editor--runtime)
        'degraded)))))

(ert-deftest emacs-agent-editor-move-blocks-before-save-symlink-escape ()
  (emacs-agent-editor-test--with-server
    (let* ((outside
            (make-temp-file "emacs-agent-source-outside-" t))
           (source-parent (expand-file-name "source-parent" root))
           (moved-source-parent
            (expand-file-name "source-parent-moved" root))
           (source (expand-file-name "source.txt" source-parent))
           (moved-source
            (expand-file-name "source.txt" moved-source-parent))
           (outside-source (expand-file-name "source.txt" outside))
           (destination (expand-file-name "destination.txt" root))
           (context (emacs-agent-editor-test--request-context))
           buffer canonical-source)
      (unwind-protect
          (progn
            (make-directory source-parent)
            (write-region "outside untouched\n" nil outside-source)
            (let ((created
                   (emacs-agent-editor--document-create
                    `((path . ,source) (content . "before\n"))
                    context)))
              (setq canonical-source (alist-get 'path created)))
            (setq buffer (find-buffer-visiting source))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "authoritative dirty\n")
              (setq-local
               emacs-agent-editor-test--source-parent source-parent)
              (setq-local
               emacs-agent-editor-test--moved-source-parent
               moved-source-parent)
              (setq-local
               emacs-agent-editor-test--outside-parent outside)
              (add-hook
               'before-save-hook
               #'emacs-agent-editor-test--replace-source-parent-before-save
               nil t))
            (let* ((document
                    (gethash
                     canonical-source
                     (emacs-agent-runtime-document-registry
                      emacs-agent-editor--runtime)))
                   (revision
                    (emacs-agent-document-revision document))
                   (error-data
                    (should-error
                     (emacs-agent-editor--document-move
                      `((path . ,source)
                        (new_path . ,destination)
                        (expected_revision . ,revision))
                      context)
                     :type 'emacs-agent-tool-error))
                   (error-object (cadr error-data)))
              (should
               (equal
                (alist-get 'code error-object)
                "CHECKPOINT_FAILED"))
              (should
               (eq
                (alist-get 'partial_completion error-object)
                :false))
              (should
               (eq
                (alist-get 'reconciliation_required error-object)
                t))
              (should (emacs-agent-document-degraded document)))
            (should-not (file-exists-p destination))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents outside-source)
                (buffer-string))
              "outside untouched\n"))
            (should
             (equal
              (with-temp-buffer
                (insert-file-contents moved-source)
                (buffer-string))
              "before\n"))
            (with-current-buffer buffer
              (should (buffer-modified-p))
              (should
               (equal
                (buffer-string)
                "before\nauthoritative dirty\n")))
            (should
             (eq
              (emacs-agent-runtime-health-state
               emacs-agent-editor--runtime)
              'degraded)))
        (when (file-symlink-p source-parent)
          (delete-file source-parent))
        (when (file-directory-p moved-source-parent)
          (rename-file moved-source-parent source-parent))
        (when (file-directory-p outside)
          (delete-directory outside t))))))

(ert-deftest emacs-agent-editor-move-verifies-after-save-agent-error ()
  (emacs-agent-editor-test--with-server
    (let* ((source (expand-file-name "agent-error-source.txt" root))
           (destination
            (expand-file-name "agent-error-destination.txt" root))
           (context (emacs-agent-editor-test--request-context))
           (_created
            (emacs-agent-editor--document-create
             `((path . ,source) (content . "before\n"))
             context))
           (document
            (gethash
             (file-truename source)
             (emacs-agent-runtime-document-registry
              emacs-agent-editor--runtime)))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "saved before signal\n")
        (add-hook
         'after-save-hook
         #'emacs-agent-editor-test--signal-after-save nil t))
      (let* ((revision (emacs-agent-document-revision document))
             (error-data
              (should-error
               (emacs-agent-editor--document-move
                `((path . ,source)
                  (new_path . ,destination)
                  (expected_revision . ,revision))
                context)
               :type 'emacs-agent-tool-error))
             (error-object (cadr error-data)))
        (should
         (equal (alist-get 'code error-object) "CHECKPOINT_FAILED"))
        (should (eq (alist-get 'partial_completion error-object) t))
        (should
         (eq
          (alist-get 'reconciliation_required error-object)
          :false)))
      (should (file-exists-p source))
      (should-not (file-exists-p destination))
      (should-not (buffer-modified-p buffer))
      (should
       (equal
        (emacs-agent-document--buffer-content buffer)
        "before\nsaved before signal\n"))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents source)
          (buffer-string))
        "before\nsaved before signal\n"))
      (should (emacs-agent-document-degraded document))
      (should
       (eq
        (emacs-agent-runtime-health-state
         emacs-agent-editor--runtime)
        'degraded)))))

(ert-deftest emacs-agent-editor-validates-explicit-port ()
  (ignore-errors (emacs-agent-editor-stop))
  (should-error
   (emacs-agent-editor-start "9876")
   :type 'wrong-type-argument)
  (should-error
   (emacs-agent-editor-start -1)
   :type 'user-error)
  (should-error
   (emacs-agent-editor-start 65536)
   :type 'user-error)
  (should-not emacs-agent-current-runtime))

(provide 'emacs-agent-editor-test)
;;; emacs-agent-editor-test.el ends here
