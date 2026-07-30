;;; emacs-agent-protocol-2025.el --- MCP 2025-11-25 adapter  -*- lexical-binding: t; -*-

;;; Commentary:

;; Stateful compatibility metadata for legacy Streamable HTTP clients.

;;; Code:

(require 'emacs-agent-protocol)
(require 'emacs-agent-session)

(defconst emacs-agent-protocol-2025-version "2025-11-25")

(defun emacs-agent-protocol-2025--session (http-request rpc-request)
  "Resolve the compatibility session for HTTP-REQUEST and RPC-REQUEST."
  (unless (equal (alist-get 'method rpc-request) "initialize")
    (let* ((id (emacs-agent-http-header http-request "mcp-session-id"))
           (session (emacs-agent-session-get id)))
      (unless session
        (signal 'emacs-agent-jsonrpc-error
                (list emacs-agent-jsonrpc-invalid-request
                      "Missing or invalid MCP session" nil)))
      session)))

(defun emacs-agent-protocol-2025--request-context
    (http-request rpc-request session operation arguments)
  "Construct a neutral context.
Use HTTP-REQUEST, RPC-REQUEST, SESSION, OPERATION, and ARGUMENTS."
  (let* ((connection (emacs-agent-http-request-connection http-request))
         (context
          (emacs-agent-request-create
           :id (alist-get 'id rpc-request)
           :protocol-version emacs-agent-protocol-2025-version
           :client-info (emacs-agent-session-client-info session)
           :operation operation :arguments arguments
           :session-id (emacs-agent-session-id session)
           :cancellation-token connection
           :progress-context
           (alist-get '_meta (alist-get 'params rpc-request)))))
    (when (processp connection)
      (process-put connection 'emacs-agent-request context))
    context))

(defun emacs-agent-protocol-2025-handle (http-request rpc-request)
  "Handle legacy RPC-REQUEST received as HTTP-REQUEST."
  (let* ((id (alist-get 'id rpc-request))
         (method (alist-get 'method rpc-request))
         (params (or (alist-get 'params rpc-request) '()))
         (session (emacs-agent-protocol-2025--session
                   http-request rpc-request)))
    (condition-case condition
        (pcase method
          ("initialize"
           (let* ((requested (alist-get 'protocolVersion params))
                  (client-info (alist-get 'clientInfo params)))
             (unless (and (equal requested
                                 emacs-agent-protocol-2025-version)
                          (assq 'capabilities params)
                          (listp (alist-get 'capabilities params))
                          (assq 'clientInfo params)
                          (listp client-info)
                          (stringp (alist-get 'name client-info))
                          (stringp (alist-get 'version client-info)))
               (signal 'emacs-agent-jsonrpc-error
                       (list emacs-agent-jsonrpc-invalid-params
                             "Invalid initialize parameters" nil)))
             (setq session
                   (emacs-agent-session-create requested client-info))
             (emacs-agent-protocol--json-response
              200
              (emacs-agent-jsonrpc-result
               id
               `((protocolVersion . ,emacs-agent-protocol-2025-version)
                 (capabilities . ,(emacs-agent-protocol-capabilities))
                 (serverInfo . ,(emacs-agent-protocol-server-info))
                 (instructions
                  . "Use Emacs Agent Editor tools for direct local files and optional explicitly registered projects.")))
              `(("Mcp-Session-Id" . ,(emacs-agent-session-id session))))))
          ("notifications/initialized"
           (setf (emacs-agent-session-initialized session) t)
           (emacs-agent-protocol-response-create :status 202 :body nil))
          ("notifications/cancelled"
           (emacs-agent-request-cancel-id
            (alist-get 'requestId params)
            emacs-agent-protocol-2025-version
            (emacs-agent-session-id session))
           (emacs-agent-protocol-response-create :status 202 :body nil))
          ((guard (emacs-agent-jsonrpc-notification-p rpc-request))
           ;; Notifications never receive JSON-RPC responses.
           (emacs-agent-protocol-response-create :status 202 :body nil))
          ("tools/list"
           (unless (emacs-agent-session-initialized session)
             (signal 'emacs-agent-jsonrpc-error
                     (list emacs-agent-jsonrpc-invalid-request
                           "MCP session is not initialized" nil)))
           (emacs-agent-protocol--json-response
            200 (emacs-agent-jsonrpc-result
                 id (emacs-agent-protocol-tool-list-result))))
          ("tools/call"
           (unless (emacs-agent-session-initialized session)
             (signal 'emacs-agent-jsonrpc-error
                     (list emacs-agent-jsonrpc-invalid-request
                           "MCP session is not initialized" nil)))
           (let* ((name (alist-get 'name params))
                  (arguments (or (alist-get 'arguments params) '()))
                  (context
                   (emacs-agent-protocol-2025--request-context
                    http-request rpc-request session name arguments)))
             (emacs-agent-request-register context)
             (unwind-protect
                 (emacs-agent-protocol--json-response
                  200
                  (emacs-agent-jsonrpc-result
                   id (emacs-agent-protocol-call-tool
                       name arguments context)))
               (unless (eq (emacs-agent-request-state context) 'cancelled)
                 (emacs-agent-request-finish context 'completed)))))
          (_
           (emacs-agent-protocol--json-response
            200 (emacs-agent-jsonrpc-error-result
                 id emacs-agent-jsonrpc-method-not-found
                 "Method not found"))))
      (emacs-agent-jsonrpc-error
       (emacs-agent-protocol--json-response
        200 (emacs-agent-jsonrpc-condition-result id condition))))))

(provide 'emacs-agent-protocol-2025)
;;; emacs-agent-protocol-2025.el ends here
