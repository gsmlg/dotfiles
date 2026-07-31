;;; emacs-agent-transport-protocol-test.el --- Transport/protocol tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for HTTP framing, MCP profiles, sessions, and request handling.

;;; Code:

(require 'ert)
(require 'emacs-agent-protocol)
(require 'emacs-agent-request)
(require 'emacs-agent-session)

(defun emacs-agent-test--modern-request (method params &optional name)
  "Construct a modern HTTP request for METHOD, PARAMS, and optional NAME."
  (emacs-agent-http-request-create
   :method "POST" :target "/mcp" :version "HTTP/1.1"
   :headers
   `(("mcp-protocol-version" . "2026-07-28")
     ("mcp-method" . ,method)
     ,@(when name `(("mcp-name" . ,name))))
   :body
   (emacs-agent-jsonrpc-encode
    `((jsonrpc . "2.0") (id . 1) (method . ,method)
      (params
       . ,(append
           params
           '((_meta
              . ((io\.modelcontextprotocol/protocolVersion
                  . "2026-07-28")
                 (io\.modelcontextprotocol/clientInfo
                  . ((name . "ert") (version . "1")))
                 (io\.modelcontextprotocol/clientCapabilities . ()))))))))))

(defun emacs-agent-test--legacy-request
    (method params &optional session id protocol-version)
  "Construct a compatibility request for METHOD, PARAMS, SESSION, and ID.
Use PROTOCOL-VERSION or `2025-11-25' for non-initialize requests."
  (emacs-agent-http-request-create
   :method "POST" :target "/mcp" :version "HTTP/1.1"
   :headers
   `(,@(unless (equal method "initialize")
         `(("mcp-protocol-version"
            . ,(or protocol-version "2025-11-25"))))
     ,@(when session `(("mcp-session-id" . ,session))))
   :body
   (emacs-agent-jsonrpc-encode
    `((jsonrpc . "2.0")
      ,@(when id `((id . ,id)))
      (method . ,method)
      (params . ,params)))))

(ert-deftest emacs-agent-protocol-empty-tool-list-encodes-array ()
  (emacs-agent-tool-clear)
  (let* ((response
          (emacs-agent-protocol-handle-http-request
           (emacs-agent-test--modern-request "tools/list" nil)))
         (decoded
          (json-parse-string
           (decode-coding-string
            (emacs-agent-protocol-response-body response) 'utf-8)
           :object-type 'alist :array-type 'array)))
    (should (= (emacs-agent-protocol-response-status response) 200))
    (should (vectorp (alist-get 'tools (alist-get 'result decoded))))
    (should (= (length (alist-get 'tools (alist-get 'result decoded))) 0))
    (should (alist-get
             'io\.modelcontextprotocol/serverInfo
             (alist-get '_meta (alist-get 'result decoded))))))

(ert-deftest emacs-agent-protocol-instructions-describe-v03-targets ()
  (emacs-agent-session-clear)
  (unwind-protect
      (let* ((modern-response
              (emacs-agent-protocol-handle-http-request
               (emacs-agent-test--modern-request "server/discover" nil)))
             (modern
              (json-parse-string
               (decode-coding-string
                (emacs-agent-protocol-response-body modern-response)
                'utf-8)
               :object-type 'alist))
             (legacy-response
              (emacs-agent-protocol-handle-http-request
               (emacs-agent-test--legacy-request
                "initialize"
                '((protocolVersion . "2025-11-25")
                  (capabilities . ())
                  (clientInfo . ((name . "ert") (version . "1"))))
                nil 1)))
             (legacy
              (json-parse-string
               (decode-coding-string
                (emacs-agent-protocol-response-body legacy-response)
                'utf-8)
               :object-type 'alist))
             (instructions
              (list
               (alist-get 'instructions (alist-get 'result modern))
               (alist-get 'instructions (alist-get 'result legacy)))))
        (dolist (instruction instructions)
          (should (stringp instruction))
          (should (string-match-p "direct local files" instruction))
          (should
           (string-match-p "explicitly registered projects" instruction))
          (should-not (string-match-p "\\_<workspace\\_>" instruction))))
    (emacs-agent-session-clear)))

(ert-deftest emacs-agent-protocol-tool-list-and-call-encode-arrays ()
  (unwind-protect
      (progn
        (emacs-agent-tool-clear)
        (emacs-agent-tool-register
         "emacs_agent_echo" "Echo an input string."
         '((type . "object")
           (properties . ((value . ((type . "string")))))
           (required . ["value"]))
         '((type . "object")
           (properties . ((value . ((type . "string")))))
           (required . ["value"]))
         (lambda (arguments _context) arguments))
        (let* ((list-response
                (emacs-agent-protocol-handle-http-request
                 (emacs-agent-test--modern-request "tools/list" nil)))
               (list-json
                (json-parse-string
                 (decode-coding-string
                  (emacs-agent-protocol-response-body list-response) 'utf-8)
                 :object-type 'alist :array-type 'array))
               (tools (alist-get 'tools (alist-get 'result list-json))))
          (should (vectorp tools))
          (should (= (length tools) 1)))
        (let* ((call-response
                (emacs-agent-protocol-handle-http-request
                 (emacs-agent-test--modern-request
                  "tools/call"
                  '((name . "emacs_agent_echo")
                    (arguments . ((value . "hello"))))
                  "emacs_agent_echo")))
               (call-json
                (json-parse-string
                 (decode-coding-string
                  (emacs-agent-protocol-response-body call-response) 'utf-8)
                 :object-type 'alist :array-type 'array))
               (result (alist-get 'result call-json)))
          (should (= (emacs-agent-protocol-response-status call-response) 200))
          (should (vectorp (alist-get 'content result)))
          (should (equal
                   (alist-get 'value (alist-get 'structuredContent result))
                   "hello"))))
    (emacs-agent-tool-clear)))

(ert-deftest emacs-agent-protocol-distinguishes-output-schema-errors ()
  (unwind-protect
      (progn
        (emacs-agent-tool-clear)
        (emacs-agent-tool-register
         "emacs_agent_bad_output" "Return an invalid test output."
         '((type . "object")
           (properties . ((value . ((type . "string")))))
           (required . ["value"])
           (additionalProperties . :false))
         '((type . "object")
           (properties . ((result . ((type . "string")))))
           (required . ["result"])
           (additionalProperties . :false))
         (lambda (_arguments _context)
           '((result . 42))))
        (let* ((input-response
                (emacs-agent-protocol-handle-http-request
                 (emacs-agent-test--modern-request
                  "tools/call"
                  '((name . "emacs_agent_bad_output")
                    (arguments . ((value . 42))))
                  "emacs_agent_bad_output")))
               (input-json
                (json-parse-string
                 (decode-coding-string
                  (emacs-agent-protocol-response-body input-response)
                  'utf-8)
                 :object-type 'alist))
               (input-error (alist-get 'error input-json))
               (output-response
                (emacs-agent-protocol-handle-http-request
                 (emacs-agent-test--modern-request
                  "tools/call"
                  '((name . "emacs_agent_bad_output")
                    (arguments . ((value . "valid"))))
                  "emacs_agent_bad_output")))
               (output-json
                (json-parse-string
                 (decode-coding-string
                  (emacs-agent-protocol-response-body output-response)
                  'utf-8)
                 :object-type 'alist))
               (output-error (alist-get 'error output-json))
               (output-data (alist-get 'data output-error)))
          (should (= (alist-get 'code input-error)
                     emacs-agent-jsonrpc-invalid-params))
          (should (equal (alist-get 'message input-error)
                         "Invalid tool arguments"))
          (should (= (alist-get 'code output-error)
                     emacs-agent-jsonrpc-internal-error))
          (should (equal (alist-get 'message output-error)
                         "Tool output contract violation"))
          (should (equal (alist-get 'code output-data)
                         "OUTPUT_SCHEMA_VIOLATION"))
          (should (equal (alist-get 'tool output-data)
                         "emacs_agent_bad_output"))
          (should (equal (alist-get 'schema_path output-data)
                         "$.result"))))
    (emacs-agent-tool-clear)))

(ert-deftest emacs-agent-http-framing-rejects-trailing-request ()
  (let* ((server
          (emacs-agent-http-server--create
           :endpoint "/mcp" :token "secret" :allowed-origins nil))
         (body "{}")
         (frame
          (format
           (concat "POST /mcp HTTP/1.1\r\n"
                   "Authorization: Bearer secret\r\n"
                   "Content-Type: application/json\r\n"
                   "Content-Length: %d\r\n\r\n%sX")
           (string-bytes body) body)))
    (should-error
     (emacs-agent-http--try-frame server frame nil)
     :type 'emacs-agent-http-error)))

(ert-deftest emacs-agent-http-auth-error-does-not-echo-token ()
  (let* ((server
          (emacs-agent-http-server--create
           :endpoint "/mcp" :token "expected-secret"
           :allowed-origins nil))
         (supplied-secret "attacker-supplied-secret")
         (body "{}")
         (frame
          (format
           (concat "POST /mcp HTTP/1.1\r\n"
                   "Authorization: Bearer %s\r\n"
                   "Content-Type: application/json\r\n"
                   "Content-Length: %d\r\n\r\n%s")
           supplied-secret (string-bytes body) body))
         captured)
    (condition-case condition
        (emacs-agent-http--try-frame server frame nil)
      (emacs-agent-http-error (setq captured condition)))
    (should captured)
    (let ((public-error (prin1-to-string captured)))
      (should-not (string-match-p "expected-secret" public-error))
      (should-not
       (string-match-p (regexp-quote supplied-secret) public-error)))))

(ert-deftest emacs-agent-http-allows-missing-authorization-when-disabled ()
  (let* ((server
          (emacs-agent-http-server--create
           :endpoint "/mcp" :token nil :allowed-origins nil))
         (request
          (emacs-agent-http-request-create
           :method "GET" :target "/mcp" :version "HTTP/1.1"
           :headers nil :body "")))
    (should (eq (emacs-agent-http--validate-request server request)
                request))))

(ert-deftest emacs-agent-http-rejects-invalid-utf8 ()
  (should (emacs-agent-http--valid-utf8-p
           (encode-coding-string "λ" 'utf-8 t)))
  (should-not (emacs-agent-http--valid-utf8-p
               (unibyte-string #xC0 #xAF)))
  (should-not (emacs-agent-http--valid-utf8-p
               (unibyte-string #xED #xA0 #x80)))
  (should-not (emacs-agent-http--valid-utf8-p
               (unibyte-string #xF4 #x90 #x80 #x80))))

(ert-deftest emacs-agent-request-ids-are-scoped ()
  (let ((legacy
         (emacs-agent-request-create
          :id 7 :protocol-version "2025-11-25" :session-id "a"))
        (modern
         (emacs-agent-request-create
          :id 7 :protocol-version "2026-07-28")))
    (unwind-protect
        (progn
          (emacs-agent-request-register legacy)
          (emacs-agent-request-register modern)
          (should
           (eq legacy
               (emacs-agent-request-find 7 "2025-11-25" "a")))
          (emacs-agent-request-cancel-id 7 "2025-11-25" "a")
          (should (eq (emacs-agent-request-state legacy) 'cancelled))
          (should (eq (emacs-agent-request-state modern) 'pending)))
      (emacs-agent-request-cancel legacy)
      (emacs-agent-request-cancel modern))))

(ert-deftest emacs-agent-protocol-modern-cancellation-cancels-request ()
  (let ((pending
         (emacs-agent-request-create
          :id 9 :protocol-version "2026-07-28")))
    (unwind-protect
        (progn
          (emacs-agent-request-register pending)
          (let* ((request
                  (emacs-agent-http-request-create
                   :method "POST" :target "/mcp" :version "HTTP/1.1"
                   :headers
                   '(("mcp-protocol-version" . "2026-07-28")
                     ("mcp-method" . "notifications/cancelled"))
                   :body
                   (emacs-agent-jsonrpc-encode
                    '((jsonrpc . "2.0")
                      (method . "notifications/cancelled")
                      (params
                       . ((requestId . 9)
                          (_meta
                           . ((io\.modelcontextprotocol/protocolVersion
                               . "2026-07-28")
                              (io\.modelcontextprotocol/clientInfo
                               . ((name . "ert") (version . "1")))
                              (io\.modelcontextprotocol/clientCapabilities
                               . ())))))))))
                 (response
                  (emacs-agent-protocol-handle-http-request request)))
            (should (= (emacs-agent-protocol-response-status response) 202))
            (should (eq (emacs-agent-request-state pending) 'pending))
            (let ((deadline (+ (float-time) 1)))
              (while (and
                      (eq (emacs-agent-request-state pending) 'pending)
                      (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (eq (emacs-agent-request-state pending) 'cancelled))))
      (emacs-agent-request-cancel pending))))

(ert-deftest emacs-agent-protocol-legacy-initialize-session-and-list ()
  (emacs-agent-tool-clear)
  (emacs-agent-session-clear)
  (let* ((initialize
          (emacs-agent-protocol-handle-http-request
           (emacs-agent-test--legacy-request
            "initialize"
            '((protocolVersion . "2025-11-25")
              (capabilities . ((roots . nil)))
              (clientInfo . ((name . "ert") (version . "1"))))
            nil 1)))
         (session
          (cdr (assoc "Mcp-Session-Id"
                      (emacs-agent-protocol-response-headers initialize)))))
    (should (= (emacs-agent-protocol-response-status initialize) 200))
    (should (stringp session))
    (should
     (= (emacs-agent-protocol-response-status
         (emacs-agent-protocol-handle-http-request
          (emacs-agent-test--legacy-request
           "notifications/initialized" nil session nil)))
        202))
    (let* ((response
            (emacs-agent-protocol-handle-http-request
             (emacs-agent-test--legacy-request
              "tools/list" nil session 2)))
           (decoded
            (json-parse-string
             (decode-coding-string
              (emacs-agent-protocol-response-body response) 'utf-8)
             :object-type 'alist :array-type 'array)))
      (should (= (emacs-agent-protocol-response-status response) 200))
      (should (vectorp (alist-get 'tools (alist-get 'result decoded)))))))

(ert-deftest emacs-agent-protocol-rejects-session-version-mismatch ()
  (emacs-agent-session-clear)
  (unwind-protect
      (let* ((version "2025-06-18")
             (initialize
              (emacs-agent-protocol-handle-http-request
               (emacs-agent-test--legacy-request
                "initialize"
                `((protocolVersion . ,version)
                  (capabilities . ())
                  (clientInfo . ((name . "ert") (version . "1"))))
                nil 1)))
             (session
              (cdr
               (assoc
                "Mcp-Session-Id"
                (emacs-agent-protocol-response-headers initialize)))))
        (should (= (emacs-agent-protocol-response-status initialize) 200))
        (should
         (=
          (emacs-agent-protocol-response-status
           (emacs-agent-protocol-handle-http-request
            (emacs-agent-test--legacy-request
             "notifications/initialized" nil session nil version)))
          202))
        (let* ((response
                (emacs-agent-protocol-handle-http-request
                 (emacs-agent-test--legacy-request
                  "tools/list" nil session 2 "2025-11-25")))
               (decoded
                (json-parse-string
                 (decode-coding-string
                  (emacs-agent-protocol-response-body response) 'utf-8)
                 :object-type 'alist))
               (error (alist-get 'error decoded)))
          (should (= (alist-get 'code error)
                     emacs-agent-jsonrpc-invalid-request))
          (should
           (equal
            (alist-get 'message error)
            "MCP protocol version does not match session"))))
    (emacs-agent-session-clear)))

(provide 'emacs-agent-transport-protocol-test)
;;; emacs-agent-transport-protocol-test.el ends here
