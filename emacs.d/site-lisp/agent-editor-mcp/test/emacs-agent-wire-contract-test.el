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

(defun emacs-agent-wire-test--modern-list-request ()
  "Construct a modern stateless `tools/list' request."
  (emacs-agent-http-request-create
   :method "POST" :target "/mcp" :version "HTTP/1.1"
   :headers
   '(("mcp-protocol-version" . "2026-07-28")
     ("mcp-method" . "tools/list"))
   :body
   (emacs-agent-jsonrpc-encode
    '((jsonrpc . "2.0")
      (id . 91)
      (method . "tools/list")
      (params
       . ((_meta
           . ((io\.modelcontextprotocol/protocolVersion
               . "2026-07-28")
              (io\.modelcontextprotocol/clientInfo
               . ((name . "ert") (version . "1")))
              (io\.modelcontextprotocol/clientCapabilities . ())))))))))

(defun emacs-agent-wire-test--normalize-profile-result (result)
  "Remove profile-specific metadata from protocol RESULT."
  (let ((normalized (copy-tree result)))
    (dolist (key '(resultType ttlMs cacheScope _meta))
      (setq normalized (assq-delete-all key normalized)))
    normalized))

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

(defun emacs-agent-wire-test--open-legacy-session ()
  "Open and initialize a real legacy TCP session."
  (let* ((initialize
          (emacs-agent-wire-test--tcp-post
           '((jsonrpc . "2.0")
             (id . 89)
             (method . "initialize")
             (params
              . ((protocolVersion . "2025-11-25")
                 (capabilities . ())
                 (clientInfo
                  . ((name . "wire-contract")
                     (version . "1"))))))))
         (session
          (cdr
           (assoc
            "mcp-session-id"
            (plist-get initialize :headers)))))
    (should (= (plist-get initialize :status) 200))
    (should (stringp session))
    (should
     (=
      (plist-get
       (emacs-agent-wire-test--tcp-post
        '((jsonrpc . "2.0")
          (method . "notifications/initialized")
          (params . ()))
        session)
       :status)
      202))
    session))

(defun emacs-agent-wire-test--call-across-profiles
    (session id name arguments)
  "Call NAME with ARGUMENTS through both profiles.
Use real legacy TCP SESSION and request ID, compare the normalized result to
the modern stateless profile, and return that normalized result."
  (let* ((legacy-response
          (emacs-agent-wire-test--legacy-tcp-call
           session id name arguments))
         (modern-response
          (emacs-agent-protocol-handle-http-request
           (emacs-agent-editor-test--modern-request name arguments)))
         (legacy-result
          (alist-get
           'result
           (plist-get legacy-response :json)))
         (modern-result
          (alist-get
           'result
           (emacs-agent-wire-test--response-json modern-response)))
         (normalized
          (emacs-agent-wire-test--normalize-profile-result
           modern-result)))
    (should (= (plist-get legacy-response :status) 200))
    (should
     (= (emacs-agent-protocol-response-status modern-response) 200))
    (should
     (equal
      (emacs-agent-wire-test--normalize-profile-result legacy-result)
      normalized))
    (should (eq (alist-get 'isError normalized) :false))
    normalized))

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

(ert-deftest emacs-agent-wire-tool-registry-matches-across-profiles ()
  (emacs-agent-editor-test--with-server
    (let ((session (emacs-agent-wire-test--open-legacy-session)))
      (let* ((legacy-response
              (emacs-agent-wire-test--tcp-post
               '((jsonrpc . "2.0")
                 (id . 90)
                 (method . "tools/list")
                 (params . ()))
               session))
             (modern-response
              (emacs-agent-protocol-handle-http-request
               (emacs-agent-wire-test--modern-list-request)))
             (legacy-result
              (alist-get
               'result
               (plist-get legacy-response :json)))
             (modern-result
              (alist-get
               'result
               (emacs-agent-wire-test--response-json
                modern-response)))
             (normalized-modern
              (emacs-agent-wire-test--normalize-profile-result
               modern-result))
             (tools (alist-get 'tools legacy-result))
             (names
              (mapcar
               (lambda (descriptor)
                 (alist-get 'name descriptor))
               (append tools nil)))
             (forbidden-prefix
              (concat "emacs_agent_" "workspace_")))
        (should (= (plist-get legacy-response :status) 200))
        (should
          (= (emacs-agent-protocol-response-status modern-response) 200))
        (should
         (equal
          (emacs-agent-wire-test--normalize-profile-result legacy-result)
          normalized-modern))
        (should (vectorp tools))
        (should (= (length tools) 36))
        (should (equal names emacs-agent-editor-test--tool-names))
        (should (= (length (delete-dups (copy-sequence names))) 36))
        (dolist (descriptor (append tools nil))
          (should (stringp (alist-get 'name descriptor)))
          (should (listp (alist-get 'inputSchema descriptor)))
          (should (listp (alist-get 'outputSchema descriptor)))
          (should-not
           (string-prefix-p
            forbidden-prefix
            (alist-get 'name descriptor))))))))

(ert-deftest emacs-agent-wire-error-envelope-matches-across-profiles ()
  (emacs-agent-editor-test--with-server
    (let* ((session (emacs-agent-wire-test--open-legacy-session))
           (name "emacs_agent_document_read")
           (arguments '((path . "relative.txt")))
           (legacy-response
            (emacs-agent-wire-test--legacy-tcp-call
             session 92 name arguments))
           (modern-response
            (emacs-agent-protocol-handle-http-request
             (emacs-agent-editor-test--modern-request name arguments)))
           (legacy-result
            (alist-get
             'result
             (plist-get legacy-response :json)))
           (modern-result
            (alist-get
             'result
             (emacs-agent-wire-test--response-json
              modern-response)))
           (normalized
            (emacs-agent-wire-test--normalize-profile-result
             modern-result))
           (error-data
            (alist-get
             'error
             (alist-get 'structuredContent normalized))))
      (should (= (plist-get legacy-response :status) 200))
      (should
       (= (emacs-agent-protocol-response-status modern-response) 200))
      (should
       (equal
        (emacs-agent-wire-test--normalize-profile-result legacy-result)
        normalized))
      (should (eq (alist-get 'isError normalized) t))
      (should (vectorp (alist-get 'content normalized)))
      (should (= (length (alist-get 'content normalized)) 1))
      (should (= (length error-data) 6))
      (dolist (key '(code legacy_code message retryable path details))
        (should (assq key error-data)))
      (should
       (equal (alist-get 'code error-data) "PROJECT_PATH_REQUIRED"))
      (should
       (equal
        (alist-get 'legacy_code error-data)
        "project_path_required"))
      (should (stringp (alist-get 'message error-data)))
      (should (eq (alist-get 'retryable error-data) :false))
      (should (equal (alist-get 'path error-data) "relative.txt"))
      (should
       (equal
        (alist-get 'details error-data)
        '((path . "relative.txt"))))
      (should (equal (alist-get 'resultType modern-result) "complete"))
      (should (listp (alist-get '_meta modern-result)))
      (should-not (assq 'resultType legacy-result))
      (should-not (assq '_meta legacy-result)))))

(ert-deftest emacs-agent-wire-legacy-live-fields-match-modern-profile ()
  (emacs-agent-editor-test--with-server
    (let* ((session (emacs-agent-wire-test--open-legacy-session))
           (project-root (expand-file-name "project" root))
           (path (expand-file-name "file.txt" project-root))
           (_ (make-directory project-root))
           (_file
            (with-temp-file path
              (insert "shared content\n")))
           (registered
            (emacs-agent-project-open
             emacs-agent-editor--runtime project-root))
           (project-id (plist-get registered :project_id))
           (opened
            (alist-get
             'structuredContent
             (emacs-agent-wire-test--call-across-profiles
              session 93 "emacs_agent_project_open"
              `((root . ,project-root)))))
           (document
            (alist-get
             'structuredContent
             (emacs-agent-wire-test--call-across-profiles
              session 94 "emacs_agent_document_read"
              `((project_id . ,project-id)
                (path . "file.txt")))))
           (listed
            (alist-get
             'structuredContent
             (emacs-agent-wire-test--call-across-profiles
              session 95 "emacs_agent_project_list" nil)))
           (info
            (alist-get
             'structuredContent
             (emacs-agent-wire-test--call-across-profiles
              session 96 "emacs_agent_project_info"
              `((project_id . ,project-id)))))
           (editor-info
            (alist-get
             'structuredContent
             (emacs-agent-wire-test--call-across-profiles
              session 97 "emacs_agent_editor_info" nil)))
           (projects (alist-get 'projects listed))
           (listed-project (aref projects 0))
           (canonical-root (file-truename project-root)))
      (should (equal (alist-get 'project_id opened) project-id))
      (should (file-equal-p (alist-get 'root opened) canonical-root))
      (should
       (equal
        (alist-get 'name opened)
        (file-name-nondirectory
         (directory-file-name canonical-root))))
      (should (equal (alist-get 'type opened) "directory"))
      (should (eq (alist-get 'native_project opened) :false))
      (should (eq (alist-get 'opened opened) :false))
      (should (= (alist-get 'project_count listed) 1))
      (should (vectorp projects))
      (should (= (length projects) 1))
      (should (equal (alist-get 'project_id listed-project) project-id))
      (should
       (file-equal-p
        (alist-get 'root listed-project)
        canonical-root))
      (should (equal (alist-get 'project_id info) project-id))
      (should (file-equal-p (alist-get 'root info) canonical-root))
      (should (= (alist-get 'managed_document_count info) 1))
      (should (equal (alist-get 'path document) (file-truename path)))
      (should (equal (alist-get 'project_id document) project-id))
      (should (equal (alist-get 'relative_path document) "file.txt"))
      (should (equal (alist-get 'content document) "shared content\n"))
      (should
       (string-prefix-p "rev:" (alist-get 'revision document)))
      (should (= (alist-get 'project_count editor-info) 1))
      (should (= (alist-get 'managed_document_count editor-info) 1))
      (should
       (= (length (alist-get 'supported_tools editor-info)) 36))
      (should
       (equal
        (alist-get 'protocol_versions editor-info)
        ["2026-07-28" "2025-11-25"])))))

(defun emacs-agent-wire-test--output-schema-errors
    (tool-name result required array-properties)
  "Return validation errors for TOOL-NAME's advertised output schema.
RESULT is the handler result to validate against that schema.
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
    (let ((path (expand-file-name "transform.txt" root)))
      (write-region "one\ntwo\n" nil path)
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
                  . ,(format
                      (concat
                       "--- %s\n"
                       "+++ %s\n"
                       "@@ -1,2 +1,2 @@\n"
                      " one\n"
                      "-two\n"
                      "+second\n")
                      (file-truename path)
                      (file-truename path)))
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

(ert-deftest emacs-agent-wire-editor-apply-documents-are-arrays ()
  (emacs-agent-editor-test--with-server
    (let ((path-a (expand-file-name "a.txt" root))
          (path-b (expand-file-name "b.txt" root)))
      (write-region "old a\n" nil path-a)
      (write-region "old b\n" nil path-b)
      (let* ((revision-a (emacs-agent-wire-test--revision path-a))
           (revision-b (emacs-agent-wire-test--revision path-b))
           (result
            (emacs-agent-wire-test--call
             "emacs_agent_editor_apply_edits"
             `((documents
                . [((path . ,path-a)
                    (expected_revision . ,revision-a)
                    (edits
                     . [((old_text . "old a")
                         (new_text . "new a")
                         (expected_occurrences . 1))]))
                   ((path . ,path-b)
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
              (find-buffer-visiting path-a)
            (buffer-string))
          "old a\n"))))))

(ert-deftest emacs-agent-wire-editor-apply-rejects-nonatomic-mode ()
  (emacs-agent-editor-test--with-server
    (let ((path (expand-file-name "atomic.txt" root)))
      (write-region "old\n" nil path)
      (let* ((revision (emacs-agent-wire-test--revision path))
           (response
            (emacs-agent-protocol-handle-http-request
             (emacs-agent-editor-test--modern-request
              "emacs_agent_editor_apply_edits"
              `((documents
                 . [((path . ,path)
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
          "Invalid tool arguments"))))))

(ert-deftest emacs-agent-wire-changeset-list-and-get-nested-arrays ()
  (emacs-agent-editor-test--with-server
    (let* ((created
            (emacs-agent-wire-test--call
             "emacs_agent_document_create"
             `((path . ,(expand-file-name "created.txt" root))
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
               `((path . ,(expand-file-name "tcp-a.txt" root)))))
             (read-b
              (emacs-agent-wire-test--legacy-tcp-call
               session 3 "emacs_agent_document_read"
               `((path . ,(expand-file-name "tcp-b.txt" root)))))
             (read-a-result
              (alist-get 'structuredContent
                         (alist-get 'result (plist-get read-a :json))))
             (read-b-result
              (alist-get 'structuredContent
                         (alist-get 'result (plist-get read-b :json))))
             (transaction
              (emacs-agent-wire-test--legacy-tcp-call
               session 4 "emacs_agent_editor_apply_edits"
               `((documents
                  . [((path . ,(expand-file-name "tcp-a.txt" root))
                      (expected_revision
                       . ,(alist-get 'revision read-a-result))
                      (edits
                       . [((old_text . "old-a")
                           (new_text . "new-a"))]))
                     ((path . ,(expand-file-name "tcp-b.txt" root))
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

(ert-deftest emacs-agent-wire-project-diagnostics-preserve-arrays ()
  (emacs-agent-editor-test--with-server
    (write-region "{}\n" nil (expand-file-name "good.json" root))
    (write-region "{broken\n" nil (expand-file-name "bad.json" root))
    (let* ((project
            (emacs-agent-wire-test--call
             "emacs_agent_project_open"
             `((root . ,root))))
           (project-id (alist-get 'project_id project))
           (result
            (emacs-agent-wire-test--call
             "emacs_agent_project_diagnostics"
             `((project_id . ,project-id)
               (paths . ["good.json" "bad.json"])
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
          (when (equal
                 (alist-get 'relative_path document)
                 "good.json")
            (should (= (length items) 0)))))
      (should
       (equal
        (alist-get 'severity (aref diagnostics 0))
        "error")))))

(ert-deftest emacs-agent-wire-diagnostics-accept-explicit-filters ()
  (emacs-agent-editor-test--with-server
    (write-region "{broken\n" nil (expand-file-name "bad.json" root))
    (let* ((project
            (emacs-agent-wire-test--call
             "emacs_agent_project_open"
             `((root . ,root))))
           (project-id (alist-get 'project_id project))
           (document
            (emacs-agent-wire-test--call
             "emacs_agent_document_diagnostics"
             `((path . ,(expand-file-name "bad.json" root))
               (sources . ["parser"]))))
           (project-result
            (emacs-agent-wire-test--call
             "emacs_agent_project_diagnostics"
             `((project_id . ,project-id)
               (paths . ["bad.json"])
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
            project-result 'diagnostics))
          1)))))

(ert-deftest emacs-agent-wire-editor-info-separates-runtime-capabilities ()
  (emacs-agent-editor-test--with-server
    (let* ((result
            (emacs-agent-wire-test--call
             "emacs_agent_editor_info" nil))
           (supported
            (emacs-agent-wire-test--assert-array
             result 'supported_tools))
           (runtime (alist-get 'runtime_capabilities result))
           (availability
            (emacs-agent-wire-test--assert-array
             runtime 'tool_availability)))
      (should (= (length supported) 36))
      (should (> (length availability) 0))
      (should (listp (alist-get 'providers runtime))))))

(ert-deftest emacs-agent-wire-editor-info-ignores-unrelated-buffer ()
  (emacs-agent-editor-test--with-server
    (let ((path (expand-file-name "editor.txt" root)))
      (write-region "editor\n" nil path)
      (emacs-agent-wire-test--revision path))
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
                     "emacs_agent_editor_info" nil))
                   (runtime (alist-get 'runtime_capabilities result))
                   (providers (alist-get 'providers runtime))
                   (imenu (alist-get 'imenu providers)))
              (should (eq (alist-get 'available imenu) :false))))
        (set-window-buffer window original)
        (kill-buffer unrelated)))))

(ert-deftest emacs-agent-wire-checkpoint-and-rollback-use-write-contract ()
  (emacs-agent-editor-test--with-server
    (let* ((absolute (expand-file-name "checkpoint.txt" root))
           (created
            (emacs-agent-wire-test--call
             "emacs_agent_document_create"
             `((path . ,absolute)
               (content . "before\n"))))
           (buffer (find-buffer-visiting absolute)))
      (should (stringp (alist-get 'changeset_id created)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "after\n"))
      (let* ((revision
              (emacs-agent-wire-test--revision absolute))
             (checkpoint
              (emacs-agent-wire-test--call
               "emacs_agent_editor_checkpoint"
               `((documents
                  . [((path . ,absolute)
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
    (let ((text-path (expand-file-name "schema.txt" root))
          (json-path (expand-file-name "schema.json" root))
          (created-path
           (expand-file-name "schema-created.txt" root)))
      (write-region "old\n" nil text-path)
      (write-region "{broken\n" nil json-path)
      (let* ((project
              (emacs-agent-wire-test--call
               "emacs_agent_project_open"
               `((root . ,root))))
             (project-id (alist-get 'project_id project))
             (revision (emacs-agent-wire-test--revision text-path))
           (apply-edits
            (emacs-agent-wire-test--call
             "emacs_agent_document_apply_edits"
             `((path . ,text-path)
               (expected_revision . ,revision)
               (edits
                . [((start . ((line . 1) (column . 0)))
                    (end . ((line . 1) (column . 3)))
                    (new_text . "new"))])
               (dry_run . t))))
           (replace
            (emacs-agent-wire-test--call
             "emacs_agent_document_replace"
             `((path . ,text-path)
               (expected_revision . ,revision)
               (old_text . "old")
               (new_text . "new")
               (dry_run . t))))
           (patch
            (emacs-agent-wire-test--call
             "emacs_agent_document_apply_patch"
             `((path . ,text-path)
               (expected_revision . ,revision)
               (patch
                . ,(format
                    (concat
                     "--- %s\n"
                     "+++ %s\n"
                     "@@ -1 +1 @@\n"
                     "-old\n"
                     "+new\n")
                    (file-truename text-path)
                    (file-truename text-path)))
               (dry_run . t))))
           (editor-edit
            (emacs-agent-wire-test--call
             "emacs_agent_editor_apply_edits"
             `((documents
                . [((path . ,text-path)
                    (expected_revision . ,revision)
                    (edits
                     . [((old_text . "old")
                         (new_text . "new")
                         (expected_occurrences . 1))]))])
               (dry_run . t))))
           (created
            (emacs-agent-wire-test--call
             "emacs_agent_document_create"
             `((path . ,created-path)
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
             `((path . ,json-path))))
           (diagnostics
            (emacs-agent-wire-test--call
             "emacs_agent_project_diagnostics"
             `((project_id . ,project-id)
               (paths . ["schema.json"])))))
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
               ("emacs_agent_editor_apply_edits"
                ,editor-edit
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
               ("emacs_agent_project_diagnostics"
                ,diagnostics
                ("project_id" "document_count" "diagnostic_count" "summary"
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
          (should (null errors)))))))

(provide 'emacs-agent-wire-contract-test)
;;; emacs-agent-wire-contract-test.el ends here
