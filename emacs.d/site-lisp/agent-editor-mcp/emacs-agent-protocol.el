;;; emacs-agent-protocol.el --- MCP protocol dispatch  -*- lexical-binding: t; -*-

;;; Commentary:

;; Version-neutral tool execution and HTTP profile selection.

;;; Code:

(require 'cl-lib)
(require 'emacs-agent-http)
(require 'emacs-agent-jsonrpc)
(require 'emacs-agent-request)
(require 'emacs-agent-schema)

(declare-function emacs-agent-protocol-2025-handle
                  "emacs-agent-protocol-2025")
(declare-function emacs-agent-protocol-2026-handle
                  "emacs-agent-protocol-2026")

(defconst emacs-agent-protocol-versions
  '("2026-07-28" "2025-11-25" "2025-06-18"))
(defconst emacs-agent-server-name "emacs-agent-editor")
(defconst emacs-agent-server-version "0.3.0")

(defvar emacs-agent-protocol-tool-observer nil
  "Optional callback for bounded tool execution metadata.
The function receives NAME, STATUS, DURATION, and a result or error PAYLOAD.")

(cl-defstruct (emacs-agent-protocol-response
               (:constructor emacs-agent-protocol-response-create))
  status headers body)

(defun emacs-agent-protocol-server-info ()
  "Return the public MCP implementation description."
  `((name . ,emacs-agent-server-name)
    (title . "Emacs Agent Editor")
    (version . ,emacs-agent-server-version)
    (description . "Buffer-first software editor running inside Emacs")))

(defun emacs-agent-protocol-capabilities ()
  "Return capabilities shared by both protocol profiles."
  '((tools . ((listChanged . :false)))))

(defun emacs-agent-protocol--text-content (value)
  "Return a legacy text content item encoding VALUE."
  `((type . "text")
    (text . ,(decode-coding-string
              (emacs-agent-jsonrpc-encode value) 'utf-8 t))))

(defun emacs-agent-protocol-call-tool (name arguments context)
  "Invoke tool NAME with ARGUMENTS and protocol-neutral CONTEXT.
Return an MCP call result, including structured and text content."
  (let ((tool (emacs-agent-tool-get name))
        (started-at (float-time)))
    (unless tool
      (signal 'emacs-agent-jsonrpc-error
              (list emacs-agent-jsonrpc-invalid-params
                    "Unknown tool"
                    `((name . ,name)))))
    (condition-case condition
        (progn
          (emacs-agent-schema-validate
           arguments (emacs-agent-tool-input-schema tool))
          (let ((result (funcall (emacs-agent-tool-handler tool)
                                 arguments context)))
            (when (emacs-agent-tool-output-schema tool)
              (condition-case output-condition
                  (emacs-agent-schema-validate
                   result (emacs-agent-tool-output-schema tool))
                (emacs-agent-schema-error
                 (let* ((details (cadr output-condition))
                        (data
                         `((code . "OUTPUT_SCHEMA_VIOLATION")
                           (tool . ,name)
                           (schema_path . ,(alist-get 'path details))
                           (details . ,details))))
                   (when (functionp emacs-agent-protocol-tool-observer)
                     (ignore-errors
                       (funcall emacs-agent-protocol-tool-observer
                                name "failed"
                                (- (float-time) started-at) data)))
                   (signal
                    'emacs-agent-jsonrpc-error
                    (list emacs-agent-jsonrpc-internal-error
                          "Tool output contract violation"
                          data))))))
            (when (functionp emacs-agent-protocol-tool-observer)
              (ignore-errors
                (funcall emacs-agent-protocol-tool-observer
                         name "completed"
                         (- (float-time) started-at) result)))
            `((structuredContent . ,result)
              (content . [,(emacs-agent-protocol--text-content result)])
              (isError . :false))))
      (emacs-agent-schema-error
       (when (functionp emacs-agent-protocol-tool-observer)
         (ignore-errors
           (funcall emacs-agent-protocol-tool-observer
                    name "rejected"
                    (- (float-time) started-at) (cadr condition))))
       (signal 'emacs-agent-jsonrpc-error
               (list emacs-agent-jsonrpc-invalid-params
                     "Invalid tool arguments"
                     (cadr condition))))
      (emacs-agent-tool-error
       (let* ((data (or (cadr condition)
                        '((code . "tool_error"))))
              (result `((error . ,data))))
         (when (functionp emacs-agent-protocol-tool-observer)
           (ignore-errors
             (funcall emacs-agent-protocol-tool-observer
                      name "failed"
                      (- (float-time) started-at) data)))
         `((structuredContent . ,result)
           (content . [,(emacs-agent-protocol--text-content result)])
           (isError . t)))))))

(defun emacs-agent-protocol-tool-list-result (&optional modern)
  "Return deterministic tool-list result.
When MODERN is non-nil, include stateless result and cache metadata."
  (append
   (when modern
     '((resultType . "complete")
       (ttlMs . 60000)
       (cacheScope . "private")))
   `((tools . ,(vconcat
                (mapcar #'emacs-agent-schema-tool-descriptor
                        (emacs-agent-tool-list)))))))

(defun emacs-agent-protocol--version (http-request rpc-request)
  "Select the profile for HTTP-REQUEST and RPC-REQUEST."
  (or (emacs-agent-http-header http-request "mcp-protocol-version")
      (and (equal (alist-get 'method rpc-request) "initialize")
           (alist-get 'protocolVersion
                      (alist-get 'params rpc-request)))))

(defun emacs-agent-protocol--json-response
    (status object &optional headers)
  "Build a protocol response with STATUS, JSON OBJECT, and HEADERS."
  (emacs-agent-protocol-response-create
   :status status :headers headers
   :body (and object (emacs-agent-jsonrpc-encode object))))

(defun emacs-agent-protocol-handle-http-request (http-request)
  "Dispatch immutable HTTP-REQUEST to an MCP profile."
  (let ((method (emacs-agent-http-request-method http-request)))
    (cond
     ((member method '("GET" "DELETE"))
      (emacs-agent-protocol-response-create
       :status 405 :headers '(("Allow" . "POST")) :body nil))
     ((not (equal method "POST"))
      (emacs-agent-protocol-response-create :status 405 :body nil))
     (t
      (condition-case condition
          (let* ((body (decode-coding-string
                        (emacs-agent-http-request-body http-request)
                        'utf-8 t))
                 (rpc-request (emacs-agent-jsonrpc-parse body))
                 (version (emacs-agent-protocol--version
                           http-request rpc-request)))
            (pcase version
              ("2026-07-28"
               (require 'emacs-agent-protocol-2026)
               (emacs-agent-protocol-2026-handle
                http-request rpc-request))
              ((or "2025-11-25" "2025-06-18")
               (require 'emacs-agent-protocol-2025)
               (emacs-agent-protocol-2025-handle
                http-request rpc-request version))
              (_
               (emacs-agent-protocol--json-response
                400
                (emacs-agent-jsonrpc-error-result
                 (alist-get 'id rpc-request)
                 emacs-agent-jsonrpc-unsupported-protocol-version
                 "Unsupported protocol version"
                 `((supported
                    . ,(vconcat emacs-agent-protocol-versions))
                   (requested . ,version)))))))
        (emacs-agent-jsonrpc-error
         (emacs-agent-protocol--json-response
          400
          (emacs-agent-jsonrpc-condition-result nil condition)))
        (error
         (emacs-agent-protocol--json-response
          500
          (emacs-agent-jsonrpc-error-result
           nil emacs-agent-jsonrpc-internal-error "Internal error"
           `((detail . ,(error-message-string condition)))))))))))

(provide 'emacs-agent-protocol)
;;; emacs-agent-protocol.el ends here
