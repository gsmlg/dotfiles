;;; emacs-agent-editor-test.el --- Entrypoint integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Lifecycle and composed tool-surface tests.

;;; Code:

(require 'ert)
(require 'emacs-agent-editor)

(defmacro emacs-agent-editor-test--with-server (&rest body)
  "Run BODY with a temporary Agent Editor server."
  (declare (indent 0) (debug t))
  `(let ((root (make-temp-file "emacs-agent-editor-" t))
         (state (make-temp-file "emacs-agent-editor-state-" t))
         (emacs-agent-editor-state-directory nil)
         (emacs-agent-editor-token-authentication-enabled t)
         (emacs-agent-editor-bearer-token "test-bearer-token")
         (emacs-agent-editor-access-mode 'autonomous)
         (emacs-agent-editor-save-policy 'immediate))
     (setq emacs-agent-editor-state-directory state)
     (unwind-protect
         (progn
           (emacs-agent-editor-start root)
           ,@body)
       (ignore-errors (emacs-agent-editor-stop))
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
   :object-type 'alist :array-type 'list))

(defun emacs-agent-editor-test--write-result-missing (operation result)
  "Return missing common write fields for OPERATION and RESULT."
  (let (missing)
    (dolist (key '(old_revision new_revision changeset_id applied checkpointed
                                modified diff truncated))
      (unless (assq key result)
        (push (cons operation key) missing)))
    (unless (stringp (alist-get 'diff result))
      (push (cons operation 'diff_string) missing))
    (nreverse missing)))

(ert-deftest emacs-agent-editor-start-publishes-private-connection ()
  (emacs-agent-editor-test--with-server
    (should (emacs-agent-editor-running-p))
    (let ((names (mapcar #'emacs-agent-tool-name
                         (emacs-agent-tool-list))))
      ;; The original v0.1 surface remains available as the PRD adds tools.
      (dolist
          (name
           '("emacs_agent_workspace_info"
             "emacs_agent_document_read"
             "emacs_agent_document_apply_edits"
             "emacs_agent_document_create"
             "emacs_agent_workspace_files"
             "emacs_agent_workspace_search"
             "emacs_agent_document_move"
             "emacs_agent_document_delete"
             "emacs_agent_workspace_checkpoint"
             "emacs_agent_workspace_sync"
             "emacs_agent_workspace_diff"
             "emacs_agent_changeset_rollback"))
        (should (member name names))))
    (should (file-exists-p emacs-agent-editor--connection-file))
    (should (= (logand (file-modes emacs-agent-editor--connection-file)
                       #o777)
               #o600))
    (let ((metadata
           (json-parse-string
            (with-temp-buffer
              (insert-file-contents emacs-agent-editor--connection-file)
              (buffer-string))
            :object-type 'alist :array-type 'list
            :false-object :false)))
      (should (eq (alist-get 'token_authentication metadata) t))
      (should (equal (alist-get 'token metadata) "test-bearer-token")))
    (should
     (string-match-p
      "\"tools\":\\["
      (decode-coding-string
       (emacs-agent-jsonrpc-encode
       (emacs-agent-protocol-tool-list-result t))
       'utf-8)))))

(ert-deftest emacs-agent-editor-token-authentication-is-disabled-by-default ()
  (let ((root (make-temp-file "emacs-agent-editor-no-auth-" t))
        (state (make-temp-file "emacs-agent-editor-no-auth-state-" t))
        (emacs-agent-editor-state-directory nil)
        (emacs-agent-editor-token-authentication-enabled nil)
        (emacs-agent-editor-bearer-token "ignored-token"))
    (setq emacs-agent-editor-state-directory state)
    (unwind-protect
        (progn
          (emacs-agent-editor-start root)
          (should-not emacs-agent-editor--token)
          (should-not
           (emacs-agent-http-server-token
            emacs-agent-editor--http-server))
          (let ((metadata
                 (json-parse-string
                  (with-temp-buffer
                    (insert-file-contents
                     emacs-agent-editor--connection-file)
                    (buffer-string))
                  :object-type 'alist :array-type 'list
                  :false-object :false)))
            (should
             (eq (alist-get 'token_authentication metadata) :false))
            (should-not (assq 'token metadata)))
          (should
           (equal
            (alist-get
             'type
             (alist-get
              'authentication
              (emacs-agent-editor--workspace-info nil nil)))
            "none")))
      (ignore-errors (emacs-agent-editor-stop))
      (delete-directory root t)
      (delete-directory state t))))

(ert-deftest emacs-agent-editor-registers-prd-tool-contract ()
  (emacs-agent-editor-test--with-server
    (let ((names (mapcar #'emacs-agent-tool-name
                         (emacs-agent-tool-list)))
          missing)
      (dolist
          (name
           '("emacs_agent_document_replace"
             "emacs_agent_document_apply_patch"
             "emacs_agent_workspace_apply_edits"
             "emacs_agent_changeset_list"
             "emacs_agent_changeset_get"
             "emacs_agent_workspace_modified_documents"
             "emacs_agent_document_status"
             "emacs_agent_document_diagnostics"
             "emacs_agent_workspace_diagnostics"
             "emacs_agent_document_symbols"
             "emacs_agent_workspace_symbols"
             "emacs_agent_symbol_definition"
             "emacs_agent_symbol_references"
             "emacs_agent_symbol_rename"
             "emacs_agent_code_actions"
             "emacs_agent_format_document"
             "emacs_agent_format_range"
             "emacs_agent_editor_context_get"
             "emacs_agent_approval_status"
             "emacs_agent_approval_cancel"))
        (unless (member name names)
          (push name missing)))
      (should (null (nreverse missing))))))

(ert-deftest emacs-agent-editor-position-contract-is-explicit ()
  (emacs-agent-editor-test--with-server
    (let ((semantics
           (alist-get
            'position_semantics
            (emacs-agent-editor--workspace-info nil nil))))
      (should (= (alist-get 'lineBase semantics) 1))
      (should (= (alist-get 'columnBase semantics) 0))
      (should (equal (alist-get 'unit semantics) "emacs_character"))
      (should (equal (alist-get 'range semantics) "half_open"))
      (should (= (alist-get 'tabWidth semantics) 1))
      (should
       (equal (alist-get 'editsRelativeTo semantics)
              "expected_revision"))
      (should
       (equal (alist-get 'applicationOrder semantics) "descending")))))

(ert-deftest emacs-agent-editor-modern-mcp-replace-and-error-contract ()
  (emacs-agent-editor-test--with-server
    (let* ((path "mcp.txt")
           (absolute (expand-file-name path root)))
      (write-region "old text\n" nil absolute)
      (let* ((revision
              (alist-get
               'revision
               (emacs-agent-editor--document-read
                `((path . ,path)) nil)))
             (response
              (emacs-agent-protocol-handle-http-request
               (emacs-agent-editor-test--modern-request
                "emacs_agent_document_replace"
                `((path . ,path)
                  (expected_revision . ,revision)
                  (old_text . "old")
                  (new_text . "new")))))
             (json (emacs-agent-editor-test--response-json response))
             (result (alist-get 'result json))
             (structured (alist-get 'structuredContent result)))
        (should (= (emacs-agent-protocol-response-status response) 200))
        (should (eq (alist-get 'isError result) :false))
        (should (equal (alist-get 'applied structured) t))
        (should (string-match-p "^-old text" (alist-get 'diff structured)))
        (should
         (equal
          (with-current-buffer (find-buffer-visiting absolute)
            (buffer-string))
          "new text\n"))
        (let* ((error-response
                (emacs-agent-protocol-handle-http-request
                 (emacs-agent-editor-test--modern-request
                  "emacs_agent_document_replace"
                  `((path . ,path)
                    (expected_revision . ,revision)
                    (old_text . "new")
                    (new_text . "again")))))
               (error-json
                (emacs-agent-editor-test--response-json error-response))
               (error-result (alist-get 'result error-json))
               (error-object
                (alist-get
                 'error
                 (alist-get 'structuredContent error-result))))
          (should (alist-get 'isError error-result))
          (should
           (equal (alist-get 'code error-object) "REVISION_MISMATCH"))
          (should (stringp (alist-get 'message error-object)))
          (should (eq (alist-get 'retryable error-object) t))
          (should (assq 'details error-object)))))))

(ert-deftest emacs-agent-editor-write-results-have-common-fields ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 42 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (created
            (emacs-agent-editor--document-create
             '((path . "contract.txt") (content . "old\n")) context)))
      (let* ((revision (alist-get 'new_revision created))
             (edited
              (emacs-agent-editor--document-apply-edits
               `((path . "contract.txt")
                 (expected_revision . ,revision)
                 (edits
                  . (((start . ((line . 1) (column . 0)))
                      (end . ((line . 1) (column . 3)))
                      (new_text . "new")))))
               context)))
        (let* ((moved
                (emacs-agent-editor--document-move
                 `((path . "contract.txt")
                   (new_path . "moved.txt")
                   (expected_revision . ,(alist-get 'new_revision edited)))
                 context)))
          (let* ((deleted
                  (emacs-agent-editor--document-delete
                   `((path . "moved.txt")
                     (expected_revision
                     . ,(alist-get 'new_revision moved)))
                   context))
                 (delete-id (alist-get 'changeset_id deleted))
                 (rollback
                  (emacs-agent-editor--changeset-rollback
                   `((changeset_id . ,delete-id)) context))
                 (results
                  `((document_create . ,created)
                    (document_apply_edits . ,edited)
                    (document_move . ,moved)
                    (document_delete . ,deleted)
                    (changeset_rollback . ,rollback)))
                 missing)
            (dolist (entry results)
              (setq missing
                    (nconc
                     missing
                     (emacs-agent-editor-test--write-result-missing
                      (car entry) (cdr entry)))))
            (should (null missing))))))))

(ert-deftest emacs-agent-editor-serves-modern-discovery-over-http ()
  (emacs-agent-editor-test--with-server
    (let* ((body
            (encode-coding-string
             (json-serialize
              '((jsonrpc . "2.0")
                (id . 1)
                (method . "server/discover")
                (params
                 . ((_meta
                     . ((io\.modelcontextprotocol/protocolVersion
                         . "2026-07-28")
                        (io\.modelcontextprotocol/clientInfo
                         . ((name . "ert") (version . "1")))
                        (io\.modelcontextprotocol/clientCapabilities
                         . ())))))))
             'utf-8 t))
           (response "")
           (client
            (make-network-process
             :name "emacs-agent-editor-test-client"
             :host "127.0.0.1"
             :service
             (emacs-agent-http-server-port
              emacs-agent-editor--http-server)
             :coding 'binary
             :noquery t
             :filter
             (lambda (_process chunk)
               (setq response (concat response chunk))))))
      (process-send-string
       client
       (concat
        "POST /mcp HTTP/1.1\r\n"
        "Host: 127.0.0.1\r\n"
        "Authorization: Bearer test-bearer-token\r\n"
        "Content-Type: application/json\r\n"
        "Accept: application/json, text/event-stream\r\n"
        "MCP-Protocol-Version: 2026-07-28\r\n"
        "Mcp-Method: server/discover\r\n"
        (format "Content-Length: %d\r\n\r\n" (length body))
        body))
      (process-send-eof client)
      (let ((deadline (+ (float-time) 2)))
        (while (and (< (float-time) deadline)
                    (not (string-match-p "supportedVersions" response)))
          (accept-process-output nil 0.05)))
      (should (string-prefix-p "HTTP/1.1 200" response))
      (should (string-match-p "\"supportedVersions\":\\[\"2026-07-28\"\\]"
                              response)))))

(ert-deftest emacs-agent-editor-create-edit-and-rollback ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 1 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (created
            (emacs-agent-editor--document-create
             '((path . "sample.txt") (content . "hello\n")) context))
           (read
            (emacs-agent-editor--document-read
             '((path . "sample.txt")) context))
           (revision (alist-get 'revision read))
           (edited
            (emacs-agent-editor--document-apply-edits
             `((path . "sample.txt")
               (expected_revision . ,revision)
               (edits
                . (((start . ((line . 1) (column . 0)))
                    (end . ((line . 1) (column . 5)))
                    (new_text . "goodbye")))))
             context))
           (changeset-id (alist-get 'changeset_id edited)))
      (should (stringp (alist-get 'changeset_id created)))
      (should (equal
               (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name
                   "sample.txt"
                   (emacs-agent-workspace-root
                    emacs-agent-editor--workspace)))
                 (buffer-string))
               "goodbye\n"))
      (emacs-agent-editor--changeset-rollback
       `((changeset_id . ,changeset-id)) context)
      (should
       (equal
        (with-current-buffer
            (find-buffer-visiting
             (expand-file-name
              "sample.txt"
              (emacs-agent-workspace-root
               emacs-agent-editor--workspace)))
          (buffer-substring-no-properties (point-min) (point-max)))
        "hello\n")))))

(ert-deftest emacs-agent-editor-move-delete-and-rollback ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 2 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (_created
            (emacs-agent-editor--document-create
             '((path . "old.txt") (content . "content\n")) context))
           (revision
            (alist-get
             'revision
             (emacs-agent-editor--document-read
              '((path . "old.txt")) context)))
           (moved
            (emacs-agent-editor--document-move
             `((path . "old.txt") (new_path . "new.txt")
               (expected_revision . ,revision))
             context))
           (move-id (alist-get 'changeset_id moved))
           (root (emacs-agent-workspace-root
                  emacs-agent-editor--workspace)))
      (should-not (file-exists-p (expand-file-name "old.txt" root)))
      (should (file-exists-p (expand-file-name "new.txt" root)))
      (emacs-agent-editor--changeset-rollback
       `((changeset_id . ,move-id)) context)
      (should (file-exists-p (expand-file-name "old.txt" root)))
      (should-not (file-exists-p (expand-file-name "new.txt" root)))
      (let* ((restored-revision
              (alist-get
               'revision
               (emacs-agent-editor--document-read
                '((path . "old.txt")) context)))
             (deleted
              (emacs-agent-editor--document-delete
               `((path . "old.txt")
                 (expected_revision . ,restored-revision))
               context))
             (delete-id (alist-get 'changeset_id deleted)))
        (should-not (file-exists-p (expand-file-name "old.txt" root)))
        (emacs-agent-editor--changeset-rollback
         `((changeset_id . ,delete-id)) context)
        (should (file-exists-p (expand-file-name "old.txt" root)))))))

(ert-deftest emacs-agent-editor-manual-create-checkpoints-with-changeset ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 3 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (workspace emacs-agent-editor--workspace)
           (root (emacs-agent-workspace-root workspace)))
      (setf (emacs-agent-workspace-save-policy workspace) 'manual)
      (emacs-agent-editor--document-create
       '((path . "manual.txt") (content . "buffer only\n")) context)
      (should-not (file-exists-p (expand-file-name "manual.txt" root)))
      (let* ((revision
              (alist-get
               'revision
               (emacs-agent-editor--document-read
                '((path . "manual.txt")) context)))
             (result
              (emacs-agent-editor--workspace-checkpoint
               `((documents
                  . (((path . "manual.txt")
                      (expected_revision . ,revision)))))
               context)))
        (should (stringp (alist-get 'changeset_id result)))
        (should (file-exists-p (expand-file-name "manual.txt" root)))))))

(ert-deftest emacs-agent-editor-move-reconciles-external-change ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 4 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (_created
            (emacs-agent-editor--document-create
             '((path . "old.txt") (content . "original\n")) context))
           (revision
            (alist-get
             'revision
             (emacs-agent-editor--document-read
              '((path . "old.txt")) context)))
           (root (emacs-agent-workspace-root
                  emacs-agent-editor--workspace)))
      (write-region "external\n" nil (expand-file-name "old.txt" root))
      (should-error
       (emacs-agent-editor--document-move
        `((path . "old.txt") (new_path . "new.txt")
          (expected_revision . ,revision))
        context)
       :type 'emacs-agent-tool-error)
      (should (file-exists-p (expand-file-name "old.txt" root)))
      (should-not (file-exists-p (expand-file-name "new.txt" root))))))

(ert-deftest emacs-agent-editor-rejected-binary-edit-does-not-degrade ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 5 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (_created
            (emacs-agent-editor--document-create
             '((path . "safe.txt") (content . "safe\n")) context))
           (document
            (emacs-agent-document-open
             emacs-agent-editor--workspace "safe.txt"))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-editor--document-apply-edits
        `((path . "safe.txt")
          (expected_revision . ,revision)
          (edits
           . (((start . ((line . 1) (column . 0)))
               (end . ((line . 1) (column . 0)))
               (new_text . ,(string 0))))))
        context)
       :type 'emacs-agent-tool-error)
      (should-not (emacs-agent-document-degraded document))
      (should
       (eq (emacs-agent-workspace-health-state
            emacs-agent-editor--workspace)
           'healthy)))))

(ert-deftest emacs-agent-editor-move-save-failure-degrades-document ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 6 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (_created
            (emacs-agent-editor--document-create
             '((path . "old.txt") (content . "safe\n")) context))
           (document
            (emacs-agent-document-open
             emacs-agent-editor--workspace "old.txt"))
           (revision (emacs-agent-document-revision document)))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _) (error "test save failure"))))
        (should-error
         (emacs-agent-editor--document-move
          `((path . "old.txt") (new_path . "new.txt")
            (expected_revision . ,revision))
          context)
         :type 'emacs-agent-tool-error))
      (should (emacs-agent-document-degraded document))
      (should
       (eq (emacs-agent-workspace-health-state
            emacs-agent-editor--workspace)
           'degraded)))))

(ert-deftest emacs-agent-editor-legacy-writes-support-dry-run-preview ()
  (emacs-agent-editor-test--with-server
    (let* ((context
            (emacs-agent-request-create
             :id 11 :protocol-version "2026-07-28"
             :client-info '((name . "ert"))))
           (root (emacs-agent-workspace-root
                  emacs-agent-editor--workspace))
           (_created
            (emacs-agent-editor--document-create
             '((path . "source.txt") (content . "before\n")) context))
           (revision
            (alist-get
             'revision
             (emacs-agent-editor--document-read
              '((path . "source.txt")) context)))
           (edit-arguments
            `((path . "source.txt")
              (expected_revision . ,revision)
              (edits
               . (((start . ((line . 1) (column . 0)))
                   (end . ((line . 1) (column . 6)))
                   (new_text . "after")
                   (expected_text . "before"))))))
           (edit-preview
            (emacs-agent-editor--document-apply-edits
             (append edit-arguments '((dry_run . t))) context)))
      (should (eq (alist-get 'applied edit-preview) :false))
      (should (string-match-p "^+after" (alist-get 'diff edit-preview)))
      (should
       (equal
        (with-current-buffer
            (find-buffer-visiting (expand-file-name "source.txt" root))
          (buffer-string))
        "before\n"))
      (emacs-agent-editor--document-create
       '((path . "preview.txt") (content . "new\n") (dry_run . t))
       context)
      (should-not (file-exists-p (expand-file-name "preview.txt" root)))
      (should-not
       (find-buffer-visiting (expand-file-name "preview.txt" root)))
      (let ((move-preview
             (emacs-agent-editor--document-move
              `((path . "source.txt") (new_path . "moved.txt")
                (expected_revision . ,revision) (dry_run . t))
              context))
            (delete-preview
             (emacs-agent-editor--document-delete
              `((path . "source.txt")
                (expected_revision . ,revision) (dry_run . t))
              context)))
        (should (eq (alist-get 'applied move-preview) :false))
        (should (eq (alist-get 'applied delete-preview) :false))
        (should (file-exists-p (expand-file-name "source.txt" root)))
        (should-not (file-exists-p (expand-file-name "moved.txt" root))))
      (let* ((edited
              (emacs-agent-editor--document-apply-edits
               edit-arguments context))
             (changeset-id (alist-get 'changeset_id edited))
             (rollback-preview
              (emacs-agent-editor--changeset-rollback
               `((changeset_id . ,changeset-id) (dry_run . t))
               context)))
        (should (eq (alist-get 'applied rollback-preview) :false))
        (should
         (equal
          (with-current-buffer
              (find-buffer-visiting (expand-file-name "source.txt" root))
            (buffer-string))
          "after\n"))))))

(provide 'emacs-agent-editor-test)
;;; emacs-agent-editor-test.el ends here
