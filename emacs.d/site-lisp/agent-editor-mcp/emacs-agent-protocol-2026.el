;;; emacs-agent-protocol-2026.el --- MCP 2026-07-28 adapter  -*- lexical-binding: t; -*-

;;; Commentary:

;; Stateless per-request MCP adapter.

;;; Code:

(require 'emacs-agent-protocol)

(defconst emacs-agent-protocol-2026-version "2026-07-28")
(defconst emacs-agent-protocol-2026-version-meta
  'io\.modelcontextprotocol/protocolVersion)
(defconst emacs-agent-protocol-2026-client-info-meta
  'io\.modelcontextprotocol/clientInfo)
(defconst emacs-agent-protocol-2026-client-capabilities-meta
  'io\.modelcontextprotocol/clientCapabilities)

(defun emacs-agent-protocol-2026--expected-name (method params)
  "Return the standard request name for METHOD and PARAMS."
  (pcase method
    ("tools/call" (alist-get 'name params))
    ("resources/read" (alist-get 'uri params))
    ("prompts/get" (alist-get 'name params))
    (_ nil)))

(defun emacs-agent-protocol-2026--validate-headers
    (http-request rpc-request)
  "Validate HTTP-REQUEST standard headers against RPC-REQUEST."
  (let* ((method (alist-get 'method rpc-request))
         (params (or (alist-get 'params rpc-request) '()))
         (header-method
          (emacs-agent-http-header http-request "mcp-method"))
         (header-name (emacs-agent-http-header http-request "mcp-name"))
         (expected-name
          (emacs-agent-protocol-2026--expected-name method params)))
    (unless (and (equal header-method method)
                 (if expected-name
                     (equal header-name expected-name)
                   (null header-name)))
      (signal 'emacs-agent-jsonrpc-error
              (list emacs-agent-jsonrpc-invalid-request
                    "MCP standard headers do not match request"
                    `((expectedMethod . ,method)
                      (expectedName . ,expected-name)))))))

(defun emacs-agent-protocol-2026--meta (rpc-request)
  "Validate and return modern metadata from RPC-REQUEST."
  (let* ((params (or (alist-get 'params rpc-request) '()))
         (meta (alist-get '_meta params))
         (version
          (alist-get emacs-agent-protocol-2026-version-meta meta))
         (client-info
          (alist-get emacs-agent-protocol-2026-client-info-meta meta))
         (client-capabilities
          (alist-get
           emacs-agent-protocol-2026-client-capabilities-meta meta)))
    (unless (and (listp meta)
                 (equal version emacs-agent-protocol-2026-version)
                 (assq emacs-agent-protocol-2026-client-info-meta meta)
                 (listp client-info)
                 (stringp (alist-get 'name client-info))
                 (stringp (alist-get 'version client-info))
                 (assq
                  emacs-agent-protocol-2026-client-capabilities-meta meta)
                 (listp client-capabilities))
      (signal 'emacs-agent-jsonrpc-error
              (list emacs-agent-jsonrpc-invalid-request
                    "Missing or unsupported per-request MCP metadata"
                    `((supportedVersions
                       . [,emacs-agent-protocol-2026-version])))))
    meta))

(defun emacs-agent-protocol-2026--request-context
    (http-request rpc-request meta operation arguments)
  "Construct a neutral context.
Use HTTP-REQUEST, RPC-REQUEST, META, OPERATION, and ARGUMENTS."
  (let* ((connection (emacs-agent-http-request-connection http-request))
         (context
          (emacs-agent-request-create
           :id (alist-get 'id rpc-request)
           :protocol-version emacs-agent-protocol-2026-version
           :client-info
           (alist-get emacs-agent-protocol-2026-client-info-meta meta)
           :operation operation :arguments arguments
           :cancellation-token connection
           :progress-context meta)))
    (when (processp connection)
      (process-put connection 'emacs-agent-request context))
    context))

(defun emacs-agent-protocol-2026--response-meta ()
  "Return metadata attached to each modern response."
  `((io\.modelcontextprotocol/serverInfo
     . ,(emacs-agent-protocol-server-info))))

(defun emacs-agent-protocol-2026--result (id result)
  "Construct modern result envelope for ID and RESULT."
  (emacs-agent-jsonrpc-result
   id
   (append result
           `((_meta . ,(emacs-agent-protocol-2026--response-meta))))))

(defun emacs-agent-protocol-2026-handle (http-request rpc-request)
  "Handle stateless RPC-REQUEST received as HTTP-REQUEST."
  (let* ((id (alist-get 'id rpc-request))
         (method (alist-get 'method rpc-request))
         (params (or (alist-get 'params rpc-request) '())))
    (condition-case condition
        (progn
          (emacs-agent-protocol-2026--validate-headers
           http-request rpc-request)
          (let ((meta (emacs-agent-protocol-2026--meta rpc-request)))
            (if (emacs-agent-jsonrpc-notification-p rpc-request)
                (progn
                  (when (equal method "notifications/cancelled")
                    (emacs-agent-request-cancel-id
                     (alist-get 'requestId params)
                     emacs-agent-protocol-2026-version))
                  (emacs-agent-protocol-response-create
                   :status 202 :body nil))
              (pcase method
              ("server/discover"
               (emacs-agent-protocol--json-response
                200
                (emacs-agent-protocol-2026--result
                 id
                 `((resultType . "complete")
                   (supportedVersions
                    . [,emacs-agent-protocol-2026-version])
                   (capabilities
                    . ,(emacs-agent-protocol-capabilities))
                   (instructions
                    . "Use Emacs Agent Editor tools for direct local files and optional explicitly registered projects.")
                   (ttlMs . 60000)
                   (cacheScope . "private")))))
              ("tools/list"
               (emacs-agent-protocol--json-response
                200
                (emacs-agent-protocol-2026--result
                 id (emacs-agent-protocol-tool-list-result t))))
              ("tools/call"
               (let* ((name (alist-get 'name params))
                      (arguments (or (alist-get 'arguments params) '()))
                      (context
                       (emacs-agent-protocol-2026--request-context
                        http-request rpc-request meta name arguments)))
                 (emacs-agent-request-register context)
                 (unwind-protect
                     (emacs-agent-protocol--json-response
                      200
                      (emacs-agent-protocol-2026--result
                       id
                       (append
                        '((resultType . "complete"))
                        (emacs-agent-protocol-call-tool
                         name arguments context))))
                   (unless
                       (eq (emacs-agent-request-state context) 'cancelled)
                     (emacs-agent-request-finish context 'completed)))))
              (_
               (emacs-agent-protocol--json-response
                404
                (emacs-agent-jsonrpc-error-result
                 id emacs-agent-jsonrpc-method-not-found
                 "Method not found")))))))
      (emacs-agent-jsonrpc-error
       (emacs-agent-protocol--json-response
        400 (emacs-agent-jsonrpc-condition-result id condition))))))

(provide 'emacs-agent-protocol-2026)
;;; emacs-agent-protocol-2026.el ends here
