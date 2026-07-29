;;; emacs-agent-wire-contract-test.el --- MCP wire-shape contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests parse real MCP JSON responses with arrays represented as
;; vectors.  This deliberately distinguishes JSON arrays from objects after
;; recursive editor result conversion.

;;; Code:

(require 'ert)
(require 'emacs-agent-editor-test)

(defun emacs-agent-wire-test--response-json (response)
  "Decode protocol RESPONSE while preserving JSON arrays as vectors."
  (json-parse-string
   (decode-coding-string
    (emacs-agent-protocol-response-body response) 'utf-8)
   :object-type 'alist :array-type 'array
   :null-object :null :false-object :false))

(defun emacs-agent-wire-test--tcp-post (object &optional session)
  "POST JSON-RPC OBJECT over a real TCP connection.

When SESSION is non-nil, send the legacy protocol and session headers.  Return
a plist containing the HTTP status, response headers, and decoded JSON body."
  (let* ((body (emacs-agent-jsonrpc-encode object))
         (response "")
         done
         (client
          (make-network-process
           :name "emacs-agent-wire-tcp-client"
           :host "127.0.0.1"
           :service
           (emacs-agent-http-server-port
            emacs-agent-editor--http-server)
           :coding 'binary
           :noquery t
           :filter
           (lambda (_process chunk)
             (setq response (concat response chunk)))
           :sentinel
           (lambda (_process _event)
             (setq done t)))))
    (unwind-protect
        (progn
          (process-send-string
           client
           (concat
            "POST /mcp HTTP/1.1\r\n"
            "Host: 127.0.0.1\r\n"
            "Authorization: Bearer test-bearer-token\r\n"
            "Content-Type: application/json\r\n"
            "Accept: application/json, text/event-stream\r\n"
            (when session
              (concat
               "MCP-Protocol-Version: 2025-11-25\r\n"
               "Mcp-Session-Id: " session "\r\n"))
            (format "Content-Length: %d\r\n\r\n" (string-bytes body))
            body))
          (process-send-eof client)
          (let ((deadline (+ (float-time) 3)))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should done)
          (should (string-match "\r\n\r\n" response))
          (let* ((header-end (match-end 0))
                 (header-lines
                  (split-string
                   (substring response 0 (- header-end 4)) "\r\n"))
                 (status-line (pop header-lines))
                 headers)
            (dolist (line header-lines)
              (when (string-match
                     "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
                (push
                 (cons (downcase (match-string 1 line))
                       (match-string 2 line))
                 headers)))
            (should (string-match
                     "\\`HTTP/1\\.1 \\([0-9]+\\)" status-line))
            (let ((payload (substring response header-end)))
              (list
               :status (string-to-number (match-string 1 status-line))
               :headers (nreverse headers)
               :json
               (unless (string-empty-p payload)
                 (json-parse-string
                  (decode-coding-string payload 'utf-8)
                  :object-type 'alist :array-type 'array
                  :null-object :null :false-object :false))))))
      (when (process-live-p client)
        (delete-process client)))))

(defun emacs-agent-wire-test--legacy-tcp-call
    (session id name arguments)
  "Call legacy tool NAME with ARGUMENTS over TCP SESSION using ID."
  (emacs-agent-wire-test--tcp-post
   `((jsonrpc . "2.0") (id . ,id) (method . "tools/call")
     (params . ((name . ,name) (arguments . ,arguments))))
   session))

(defun emacs-agent-wire-test--call (name arguments)
  "Call registered MCP tool NAME with wire ARGUMENTS.
Return its decoded structured result and require a successful tool call."
  (let* ((response
          (emacs-agent-protocol-handle-http-request
           (emacs-agent-editor-test--modern-request name arguments)))
         (json (emacs-agent-wire-test--response-json response))
         (result (alist-get 'result json)))
    (ert-info ((format "MCP response for %s: %S" name json))
      (should (= (emacs-agent-protocol-response-status response) 200))
      (should (eq (alist-get 'isError result) :false)))
    (alist-get 'structuredContent result)))

(defun emacs-agent-wire-test--revision (path)
  "Return the current wire revision for PATH."
  (alist-get
   'revision
   (emacs-agent-editor--document-read `((path . ,path)) nil)))

(defun emacs-agent-wire-test--assert-array (object key)
  "Return KEY from OBJECT after requiring a JSON array."
  (let ((value (alist-get key object)))
    (should (vectorp value))
    value))

(ert-deftest emacs-agent-wire-empty-alist-collections-are-arrays ()
  (let ((converted
         (emacs-agent-editor--json-value
          '((actions . nil)
            (references . nil)
            (symbols . nil)))))
    (dolist (key '(actions references symbols))
      (should (equal (alist-get key converted) [])))))

(defun emacs-agent-wire-test--output-schema-errors
    (tool-name result required array-properties)
  "Return validation errors for TOOL-NAME's advertised output schema.
REQUIRED names must be advertised as required.  ARRAY-PROPERTIES must be
declared as arrays in the advertised schema."
  (let* ((tool (emacs-agent-tool-get tool-name))
         (schema (emacs-agent-tool-output-schema tool))
         (schema-required (alist-get 'required schema))
         (properties (alist-get 'properties schema))
         errors)
    (unless (equal (alist-get 'type schema) "object")
      (push (list tool-name 'type) errors))
    (unless (vectorp schema-required)
      (push (list tool-name 'required_array) errors))
    (dolist (name required)
      (unless (member name (append schema-required nil))
        (push (list tool-name 'required name) errors)))
    (dolist (name array-properties)
      (let ((property (cdr (assq (intern name) properties))))
        (unless (and property
                     (equal (alist-get 'type property) "array"))
          (push (list tool-name 'array_property name) errors))))
    (condition-case condition
        (unless (equal (emacs-agent-schema-validate result schema) result)
          (push (list tool-name 'live_result_changed) errors))
      (emacs-agent-schema-error
       (push (list tool-name 'live_result_invalid (cadr condition))
             errors)))
    (nreverse errors)))

(ert-deftest emacs-agent-wire-replace-and-patch-ranges-are-arrays ()
  (emacs-agent-editor-test--with-server
    (let* ((path "transform.txt")
           (absolute (expand-file-name path root)))
      (write-region "one\ntwo\n" nil absolute)
      (let* ((revision (emacs-agent-wire-test--revision path))
             (replace
              (emacs-agent-wire-test--call
               "emacs_agent_document_replace"
               `((path . ,path)
                 (expected_revision . ,revision)
                 (old_text . "two")
                 (new_text . "second")
                 (dry_run . t))))
             (replace-ranges
              (emacs-agent-wire-test--assert-array replace 'ranges))
             (patch
              (emacs-agent-wire-test--call
               "emacs_agent_document_apply_patch"
               `((path . ,path)
                 (expected_revision . ,revision)
                 (patch
                  . ,(concat
                      "--- a/transform.txt\n"
                      "+++ b/transform.txt\n"
                      "@@ -1,2 +1,2 @@\n"
                      " one\n"
                      "-two\n"
                      "+second\n"))
                 (dry_run . t))))
             (patch-ranges
              (emacs-agent-wire-test--assert-array patch 'ranges)))
        (should (= (length replace-ranges) 1))
        (should (= (length patch-ranges) 1))
        (dolist (range
                 (append replace-ranges patch-ranges nil))
          (should (listp range))
          (should (listp (alist-get 'start range)))
          (should (listp (alist-get 'end range))))))))

(ert-deftest emacs-agent-wire-workspace-apply-documents-are-arrays ()
  (emacs-agent-editor-test--with-server
    (write-region "old a\n" nil (expand-file-name "a.txt" root))
    (write-region "old b\n" nil (expand-file-name "b.txt" root))
    (let* ((revision-a (emacs-agent-wire-test--revision "a.txt"))
           (revision-b (emacs-agent-wire-test--revision "b.txt"))
           (result
            (emacs-agent-wire-test--call
             "emacs_agent_workspace_apply_edits"
             `((documents
                . [((path . "a.txt")
                    (expected_revision . ,revision-a)
                    (edits
                     . [((old_text . "old a")
                         (new_text . "new a")
                         (expected_occurrences . 1))]))
                   ((path . "b.txt")
                    (expected_revision . ,revision-b)
                    (edits
                     . [((old_text . "old b")
                         (new_text . "new b")
                         (expected_occurrences . 1))]))])
               (atomic . t)
               (dry_run . t))))
           (documents
            (emacs-agent-wire-test--assert-array result 'documents)))
      (should (= (length documents) 2))
      (dolist (document (append documents nil))
        (should (stringp (alist-get 'path document)))
        (should (stringp (alist-get 'diff document))))
      (should
       (equal
        (with-current-buffer
            (find-buffer-visiting (expand-file-name "a.txt" root))
          (buffer-string))
        "old a\n")))))

(ert-deftest emacs-agent-wire-workspace-apply-rejects-nonatomic-mode ()
  (emacs-agent-editor-test--with-server
    (write-region "old\n" nil (expand-file-name "atomic.txt" root))
    (let* ((revision (emacs-agent-wire-test--revision "atomic.txt"))
           (response
            (emacs-agent-protocol-handle-http-request
             (emacs-agent-editor-test--modern-request
              "emacs_agent_workspace_apply_edits"
              `((documents
                 . [((path . "atomic.txt")
                     (expected_revision . ,revision)
                     (edits
                      . [((old_text . "old")
                          (new_text . "new"))]))])
                (atomic . :false)
                (dry_run . t)))))
           (json (emacs-agent-wire-test--response-json response)))
      (should (= (emacs-agent-protocol-response-status response) 400))
      (should
       (equal
        (alist-get 'message (alist-get 'error json))
        "Invalid tool arguments")))))

(ert-deftest emacs-agent-wire-changeset-list-and-get-nested-arrays ()
  (emacs-agent-editor-test--with-server
    (let* ((created
            (emacs-agent-wire-test--call
             "emacs_agent_document_create"
             '((path . "created.txt")
               (content . "created\n")
               (checkpoint . t))))
           (changeset-id (alist-get 'changeset_id created))
           (listed
            (emacs-agent-wire-test--call
             "emacs_agent_changeset_list"
             '((status . ["checkpointed"]) (limit . 10))))
           (changesets
            (emacs-agent-wire-test--assert-array listed 'changesets))
           (summary (aref changesets 0))
           (detail
            (emacs-agent-wire-test--call
             "emacs_agent_changeset_get"
             `((changeset_id . ,changeset-id)))))
      (should (equal (alist-get 'changeset_id summary) changeset-id))
      (dolist (key '(paths operations old_revisions new_revisions))
        (emacs-agent-wire-test--assert-array summary key)
        (emacs-agent-wire-test--assert-array detail key))
      (emacs-agent-wire-test--assert-array detail 'diagnostics_before)
      (emacs-agent-wire-test--assert-array detail 'diagnostics_after))))

(ert-deftest emacs-agent-wire-tcp-client-info-changeset-identity ()
  (emacs-agent-editor-test--with-server
    (write-region "old-a\n" nil (expand-file-name "tcp-a.txt" root))
    (write-region "old-b\n" nil (expand-file-name "tcp-b.txt" root))
    (let* ((initialize
            (emacs-agent-wire-test--tcp-post
             '((jsonrpc . "2.0") (id . 1) (method . "initialize")
               (params
                . ((protocolVersion . "2025-11-25")
                   (capabilities . ())
                   (clientInfo
                    . ((name . "emacs-agent-recheck")
                       (version . "1"))))))))
           (session
            (cdr (assoc "mcp-session-id"
                        (plist-get initialize :headers)))))
      (should (= (plist-get initialize :status) 200))
      (should (stringp session))
      (should-not (alist-get 'error (plist-get initialize :json)))
      (let ((initialized
             (emacs-agent-wire-test--tcp-post
              '((jsonrpc . "2.0")
                (method . "notifications/initialized")
                (params . ()))
              session)))
        (should (= (plist-get initialized :status) 202)))
      (let* ((read-a
              (emacs-agent-wire-test--legacy-tcp-call
               session 2 "emacs_agent_document_read"
               '((path . "tcp-a.txt"))))
             (read-b
              (emacs-agent-wire-test--legacy-tcp-call
               session 3 "emacs_agent_document_read"
               '((path . "tcp-b.txt"))))
             (read-a-result
              (alist-get 'structuredContent
                         (alist-get 'result (plist-get read-a :json))))
             (read-b-result
              (alist-get 'structuredContent
                         (alist-get 'result (plist-get read-b :json))))
             (transaction
              (emacs-agent-wire-test--legacy-tcp-call
               session 4 "emacs_agent_workspace_apply_edits"
               `((documents
                  . [((path . "tcp-a.txt")
                      (expected_revision
                       . ,(alist-get 'revision read-a-result))
                      (edits
                       . [((old_text . "old-a")
                           (new_text . "new-a"))]))
                     ((path . "tcp-b.txt")
                      (expected_revision
                       . ,(alist-get 'revision read-b-result))
                      (edits
                       . [((old_text . "old-b")
                           (new_text . "new-b"))]))])
                 (atomic . t)
                 (dry_run . :false))))
             (transaction-result
              (alist-get 'structuredContent
                         (alist-get 'result
                                    (plist-get transaction :json))))
             (changeset-id
              (alist-get 'changeset_id transaction-result))
             (detail
              (emacs-agent-wire-test--legacy-tcp-call
               session 5 "emacs_agent_changeset_get"
               `((changeset_id . ,changeset-id))))
             (detail-json (plist-get detail :json))
             (detail-result
              (alist-get 'structuredContent
                         (alist-get 'result detail-json)))
             (listing
              (emacs-agent-wire-test--legacy-tcp-call
               session 6 "emacs_agent_changeset_list" '((limit . 10))))
             (listing-json (plist-get listing :json))
             (listing-result
              (alist-get 'structuredContent
                         (alist-get 'result listing-json)))
             (changesets
              (alist-get 'changesets listing-result))
             (listed
              (seq-find
               (lambda (item)
                 (equal (alist-get 'changeset_id item) changeset-id))
               changesets)))
        (dolist (response
                 (list read-a read-b transaction detail listing))
          (should (= (plist-get response :status) 200))
          (should-not (alist-get 'error (plist-get response :json))))
        (should (stringp changeset-id))
        (dolist (identity
                 (list (alist-get 'agent_identity detail-result)
                       (alist-get 'agent_identity listed)))
          (should
           (equal identity
                  '((name . "emacs-agent-recheck")
                    (version . "1")))))))))

(ert-deftest emacs-agent-wire-workspace-diagnostics-preserve-arrays ()
  (emacs-agent-editor-test--with-server
    (write-region "{}\n" nil (expand-file-name "good.json" root))
    (write-region "{broken\n" nil (expand-file-name "bad.json" root))
    (let* ((result
            (emacs-agent-wire-test--call
             "emacs_agent_workspace_diagnostics"
             '((paths . ["good.json" "bad.json"])
               (limit . 10))))
           (documents
            (emacs-agent-wire-test--assert-array result 'documents))
           (diagnostics
            (emacs-agent-wire-test--assert-array result 'diagnostics)))
      (should (= (length documents) 2))
      (should (= (length diagnostics) 1))
      (dolist (document (append documents nil))
        (let ((providers
               (emacs-agent-wire-test--assert-array document 'providers))
              (items
               (emacs-agent-wire-test--assert-array
                document 'diagnostics)))
          (should (equal providers ["parser"]))
          (when (equal (alist-get 'path document) "good.json")
            (should (= (length items) 0)))))
      (should
       (equal
        (alist-get 'severity (aref diagnostics 0))
        "error")))))

(ert-deftest emacs-agent-wire-diagnostics-accept-explicit-filters ()
  (emacs-agent-editor-test--with-server
    (write-region "{broken\n" nil (expand-file-name "bad.json" root))
    (let* ((document
            (emacs-agent-wire-test--call
             "emacs_agent_document_diagnostics"
             '((path . "bad.json")
               (sources . ["parser"]))))
           (workspace
            (emacs-agent-wire-test--call
             "emacs_agent_workspace_diagnostics"
             '((paths . ["bad.json"])
               (sources . ["parser"])
               (severities . ["error"])))))
      (should
       (= (length
           (emacs-agent-wire-test--assert-array
            document 'diagnostics))
          1))
      (should
       (= (length
           (emacs-agent-wire-test--assert-array
            workspace 'diagnostics))
          1)))))

(ert-deftest emacs-agent-wire-workspace-info-separates-runtime-capabilities ()
  (emacs-agent-editor-test--with-server
    (let* ((result
            (emacs-agent-wire-test--call
             "emacs_agent_workspace_info" nil))
           (supported
            (emacs-agent-wire-test--assert-array
             result 'supported_tools))
           (runtime (alist-get 'runtime_capabilities result))
           (availability
            (emacs-agent-wire-test--assert-array
             runtime 'tool_availability)))
      (should (= (length supported) 32))
      (should (> (length availability) 0))
      (should (listp (alist-get 'providers runtime))))))

(ert-deftest emacs-agent-wire-workspace-info-ignores-unrelated-buffer ()
  (emacs-agent-editor-test--with-server
    (write-region "workspace\n" nil (expand-file-name "workspace.txt" root))
    (emacs-agent-wire-test--revision "workspace.txt")
    (let ((unrelated (generate-new-buffer " *agent-unrelated*"))
          (window (selected-window))
          (original (window-buffer (selected-window))))
      (unwind-protect
          (progn
            (with-current-buffer unrelated
              (setq-local imenu-generic-expression
                          '((nil "^\\(outside\\)$" 1))))
            (set-window-buffer window unrelated)
            (let* ((result
                    (emacs-agent-wire-test--call
                     "emacs_agent_workspace_info" nil))
                   (runtime (alist-get 'runtime_capabilities result))
                   (providers (alist-get 'providers runtime))
                   (imenu (alist-get 'imenu providers)))
              (should (eq (alist-get 'available imenu) :false))))
        (set-window-buffer window original)
        (kill-buffer unrelated)))))

(ert-deftest emacs-agent-wire-checkpoint-and-rollback-use-write-contract ()
  (emacs-agent-editor-test--with-server
    (let* ((created
            (emacs-agent-wire-test--call
             "emacs_agent_document_create"
             '((path . "checkpoint.txt")
               (content . "before\n"))))
           (absolute (expand-file-name "checkpoint.txt" root))
           (buffer (find-buffer-visiting absolute)))
      (should (stringp (alist-get 'changeset_id created)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "after\n"))
      (let* ((revision
              (emacs-agent-wire-test--revision "checkpoint.txt"))
             (checkpoint
              (emacs-agent-wire-test--call
               "emacs_agent_workspace_checkpoint"
               `((documents
                  . [((path . "checkpoint.txt")
                      (expected_revision . ,revision))]))))
             (checkpoint-documents
              (emacs-agent-wire-test--assert-array
               checkpoint 'documents))
             (rollback
              (emacs-agent-wire-test--call
               "emacs_agent_changeset_rollback"
               `((changeset_id
                  . ,(alist-get 'changeset_id checkpoint)))))
             (rollback-documents
              (emacs-agent-wire-test--assert-array
               rollback 'documents)))
        (dolist (result (list checkpoint rollback))
          (dolist (key '(old_revision new_revision changeset_id applied
                                     checkpointed modified diff truncated))
            (should (assq key result))))
        (should (= (length checkpoint-documents) 1))
        (should (= (length rollback-documents) 1))))))

(ert-deftest emacs-agent-wire-p0-p1-output-schemas-validate-live-results ()
  (emacs-agent-editor-test--with-server
    (write-region "old\n" nil (expand-file-name "schema.txt" root))
    (write-region "{broken\n" nil (expand-file-name "schema.json" root))
    (let* ((revision (emacs-agent-wire-test--revision "schema.txt"))
           (apply-edits
            (emacs-agent-wire-test--call
             "emacs_agent_document_apply_edits"
             `((path . "schema.txt")
               (expected_revision . ,revision)
               (edits
                . [((start . ((line . 1) (column . 0)))
                    (end . ((line . 1) (column . 3)))
                    (new_text . "new"))])
               (dry_run . t))))
           (replace
            (emacs-agent-wire-test--call
             "emacs_agent_document_replace"
             `((path . "schema.txt")
               (expected_revision . ,revision)
               (old_text . "old")
               (new_text . "new")
               (dry_run . t))))
           (patch
            (emacs-agent-wire-test--call
             "emacs_agent_document_apply_patch"
             `((path . "schema.txt")
               (expected_revision . ,revision)
               (patch
                . ,(concat
                    "--- a/schema.txt\n"
                    "+++ b/schema.txt\n"
                    "@@ -1 +1 @@\n"
                    "-old\n"
                    "+new\n"))
               (dry_run . t))))
           (workspace-edit
            (emacs-agent-wire-test--call
             "emacs_agent_workspace_apply_edits"
             `((documents
                . [((path . "schema.txt")
                    (expected_revision . ,revision)
                    (edits
                     . [((old_text . "old")
                         (new_text . "new")
                         (expected_occurrences . 1))]))])
               (dry_run . t))))
           (created
            (emacs-agent-wire-test--call
             "emacs_agent_document_create"
             '((path . "schema-created.txt")
               (content . "created\n")
               (checkpoint . t))))
           (changeset-id (alist-get 'changeset_id created))
           (changeset-list
            (emacs-agent-wire-test--call
             "emacs_agent_changeset_list"
             '((limit . 10))))
           (changeset-get
            (emacs-agent-wire-test--call
             "emacs_agent_changeset_get"
             `((changeset_id . ,changeset-id))))
           (document-diagnostics
            (emacs-agent-wire-test--call
             "emacs_agent_document_diagnostics"
             '((path . "schema.json"))))
           (diagnostics
            (emacs-agent-wire-test--call
             "emacs_agent_workspace_diagnostics"
             '((paths . ["schema.json"])))))
      (let ((specs
             `(("emacs_agent_document_apply_edits"
                ,apply-edits
                ("path" "old_revision" "new_revision" "changeset_id"
                 "applied" "checkpointed" "modified" "diff" "truncated")
                ())
               ("emacs_agent_document_replace"
                ,replace
                ("path" "old_revision" "new_revision" "applied"
                 "diff" "ranges")
                ("ranges"))
               ("emacs_agent_document_apply_patch"
                ,patch
                ("path" "old_revision" "new_revision" "applied"
                 "diff" "ranges")
                ("ranges"))
               ("emacs_agent_workspace_apply_edits"
                ,workspace-edit
                ("applied" "modified" "diff" "documents")
                ("documents"))
               ("emacs_agent_changeset_list"
                ,changeset-list
                ("changesets" "result_count" "truncated")
                ("changesets"))
               ("emacs_agent_changeset_get"
                ,changeset-get
                ("changeset_id" "paths" "operations" "old_revisions"
                 "new_revisions" "diff")
                ("paths" "operations" "old_revisions" "new_revisions"
                 "diagnostics_before" "diagnostics_after"))
               ("emacs_agent_document_diagnostics"
                ,document-diagnostics
                ("path" "document_revision" "diagnostics_revision"
                 "providers" "diagnostics")
                ("providers" "diagnostics"))
               ("emacs_agent_workspace_diagnostics"
                ,diagnostics
                ("document_count" "diagnostic_count" "summary"
                 "documents" "diagnostics")
                ("documents" "diagnostics"))))
            errors)
        (dolist (spec specs)
          (setq errors
                (nconc
                 errors
                 (emacs-agent-wire-test--output-schema-errors
                  (nth 0 spec) (nth 1 spec)
                  (nth 2 spec) (nth 3 spec)))))
        (should (null errors))))))

(provide 'emacs-agent-wire-contract-test)
;;; emacs-agent-wire-contract-test.el ends here
