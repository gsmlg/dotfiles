;;; emacs-agent-http.el --- Narrow local HTTP transport  -*- lexical-binding: t; -*-

;;; Commentary:

;; A deliberately small HTTP/1.1 server for MCP.  It accepts exactly one
;; content-length framed request per connection and always closes afterward.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(declare-function emacs-agent-protocol-response-p "emacs-agent-protocol")
(declare-function emacs-agent-protocol-response-status "emacs-agent-protocol")
(declare-function emacs-agent-protocol-response-headers "emacs-agent-protocol")
(declare-function emacs-agent-protocol-response-body "emacs-agent-protocol")

(define-error 'emacs-agent-http-error "Agent editor HTTP error")

(defgroup emacs-agent-http nil
  "HTTP transport for Emacs Agent Editor."
  :group 'tools)

(defcustom emacs-agent-http-max-header-bytes (* 32 1024)
  "Maximum accepted HTTP header size in bytes."
  :type 'integer)

(defcustom emacs-agent-http-max-body-bytes (* 1024 1024)
  "Maximum accepted request body size in bytes."
  :type 'integer)

(defcustom emacs-agent-http-idle-timeout 15
  "Seconds allowed to receive one complete request."
  :type 'number)

(cl-defstruct (emacs-agent-http-request
               (:constructor emacs-agent-http-request-create))
  method target version headers body remote connection)

(cl-defstruct (emacs-agent-http-response
               (:constructor emacs-agent-http-response-create))
  status headers body)

(cl-defstruct (emacs-agent-http-server
               (:constructor emacs-agent-http-server--create))
  listener host port endpoint token allowed-origins handler clients)

(defvar emacs-agent-http--server nil
  "The package's active HTTP server, if any.")

(defconst emacs-agent-http--status-text
  '((200 . "OK") (202 . "Accepted") (400 . "Bad Request")
    (401 . "Unauthorized") (403 . "Forbidden") (404 . "Not Found")
    (405 . "Method Not Allowed") (408 . "Request Timeout")
    (413 . "Content Too Large") (415 . "Unsupported Media Type")
    (431 . "Request Header Fields Too Large")
    (500 . "Internal Server Error")))

(defun emacs-agent-http-header (request name)
  "Return case-insensitive header NAME from REQUEST."
  (cdr (assoc (downcase name) (emacs-agent-http-request-headers request))))

(defun emacs-agent-http--fail (status message &optional headers)
  "Signal an HTTP failure with STATUS, MESSAGE, and response HEADERS."
  (signal 'emacs-agent-http-error (list status message headers)))

(defun emacs-agent-http--constant-equal (left right)
  "Compare secret strings LEFT and RIGHT without a prefix-sensitive loop."
  (and (stringp left) (stringp right)
       (string= (secure-hash 'sha256 left)
                (secure-hash 'sha256 right))))

(defun emacs-agent-http--continuation-byte-p (byte)
  "Return non-nil when BYTE is a UTF-8 continuation byte."
  (and byte (<= #x80 byte) (<= byte #xBF)))

(defun emacs-agent-http--valid-utf8-p (string)
  "Return non-nil when unibyte STRING is valid RFC 3629 UTF-8."
  (let ((index 0)
        (length (length string))
        valid)
    (setq valid t)
    (while (and valid (< index length))
      (let ((first (aref string index)))
        (cond
         ((<= first #x7F)
          (setq index (1+ index)))
         ((and (<= #xC2 first) (<= first #xDF)
               (< (1+ index) length)
               (emacs-agent-http--continuation-byte-p
                (aref string (1+ index))))
          (setq index (+ index 2)))
         ((and (<= (+ index 2) (1- length))
               (emacs-agent-http--continuation-byte-p
                (aref string (+ index 2)))
               (or
                (and (= first #xE0)
                     (<= #xA0 (aref string (1+ index)))
                     (<= (aref string (1+ index)) #xBF))
                (and (or (<= #xE1 first #xEC)
                         (<= #xEE first #xEF))
                     (emacs-agent-http--continuation-byte-p
                      (aref string (1+ index))))
                (and (= first #xED)
                     (<= #x80 (aref string (1+ index)))
                     (<= (aref string (1+ index)) #x9F))))
          (setq index (+ index 3)))
         ((and (<= (+ index 3) (1- length))
               (emacs-agent-http--continuation-byte-p
                (aref string (+ index 2)))
               (emacs-agent-http--continuation-byte-p
                (aref string (+ index 3)))
               (or
                (and (= first #xF0)
                     (<= #x90 (aref string (1+ index)))
                     (<= (aref string (1+ index)) #xBF))
                (and (<= #xF1 first #xF3)
                     (emacs-agent-http--continuation-byte-p
                      (aref string (1+ index))))
                (and (= first #xF4)
                     (<= #x80 (aref string (1+ index)))
                     (<= (aref string (1+ index)) #x8F))))
          (setq index (+ index 4)))
         (t
          (setq valid nil)))))
    valid))

(defun emacs-agent-http--parse-headers (block)
  "Parse an HTTP header BLOCK and return request-line data plus headers."
  (let* ((lines (split-string block "\r\n"))
         (request-line (pop lines))
         (parts (split-string request-line " " t))
         headers)
    (unless (and (= (length parts) 3)
                 (string-match-p "\\`[A-Z]+\\'" (nth 0 parts))
                 (equal (nth 2 parts) "HTTP/1.1"))
      (emacs-agent-http--fail 400 "Malformed request line"))
    (dolist (line lines)
      (unless (string-match
               "\\`\\([!#$%&'*+.^_`|~0-9A-Za-z-]+\\):[ \t]*\\([^\r\n]*\\)\\'"
               line)
        (emacs-agent-http--fail 400 "Malformed HTTP header"))
      (let ((name (downcase (match-string 1 line)))
            (value (string-trim-right (match-string 2 line))))
        ;; Reject duplicates rather than guessing combination semantics.
        (when (assoc name headers)
          (emacs-agent-http--fail 400 "Duplicate HTTP header"))
        (push (cons name value) headers)))
    (list (nth 0 parts) (nth 1 parts) (nth 2 parts) (nreverse headers))))

(defun emacs-agent-http--content-length (headers method)
  "Return validated content length from HEADERS for METHOD."
  (when (assoc "transfer-encoding" headers)
    (emacs-agent-http--fail 400 "Transfer-Encoding is unsupported"))
  (let ((value (cdr (assoc "content-length" headers))))
    (cond
     ((and (equal method "POST") (null value))
      (emacs-agent-http--fail 400 "Content-Length is required"))
     ((null value) 0)
     ((not (string-match-p "\\`[0-9]+\\'" value))
      (emacs-agent-http--fail 400 "Invalid Content-Length"))
     (t
      (let ((length (string-to-number value)))
        (when (> length emacs-agent-http-max-body-bytes)
          (emacs-agent-http--fail 413 "Request body too large"))
        length)))))

(defun emacs-agent-http--validate-request (server request)
  "Validate endpoint and security policy for REQUEST on SERVER."
  (unless (equal (emacs-agent-http-request-target request)
                 (emacs-agent-http-server-endpoint server))
    (emacs-agent-http--fail 404 "Unknown endpoint"))
  (when-let* ((origin (emacs-agent-http-header request "origin")))
    (unless (member origin (emacs-agent-http-server-allowed-origins server))
      (emacs-agent-http--fail 403 "Origin is not allowed")))
  (when-let* ((token (emacs-agent-http-server-token server)))
    (let ((authorization
           (emacs-agent-http-header request "authorization")))
      (unless (and authorization
                   (string-prefix-p "Bearer " authorization)
                   (emacs-agent-http--constant-equal
                    (substring authorization 7)
                    token))
        (emacs-agent-http--fail
         401 "Bearer authentication required"
         '(("WWW-Authenticate" . "Bearer"))))))
  (when (equal (emacs-agent-http-request-method request) "POST")
    (let ((content-type
           (downcase
            (or (emacs-agent-http-header request "content-type") ""))))
      (unless (string-match-p
               "\\`application/json\\(?:[ \t]*;[ \t]*charset=utf-8\\)?\\'"
               content-type)
        (emacs-agent-http--fail 415 "Content-Type must be application/json")))
    (unless (emacs-agent-http--valid-utf8-p
             (emacs-agent-http-request-body request))
      (emacs-agent-http--fail 400 "Request body is not valid UTF-8")))
  request)

(defun emacs-agent-http--try-frame (server data remote)
  "Parse DATA for SERVER and REMOTE.
Return nil while incomplete, otherwise an immutable HTTP request."
  (let ((header-end (string-match "\r\n\r\n" data)))
    (unless header-end
      (when (> (string-bytes data) emacs-agent-http-max-header-bytes)
        (emacs-agent-http--fail 431 "Request headers too large")))
    (when header-end
      (when (> header-end emacs-agent-http-max-header-bytes)
        (emacs-agent-http--fail 431 "Request headers too large"))
      (let* ((parsed
              (emacs-agent-http--parse-headers
               (substring data 0 header-end)))
             (method (nth 0 parsed))
             (headers (nth 3 parsed))
             (length (emacs-agent-http--content-length headers method))
             (body-start (+ header-end 4))
             (frame-end (+ body-start length))
             (available (string-bytes data)))
        (cond
         ((< available frame-end) nil)
         ((> available frame-end)
          (emacs-agent-http--fail 400
                                  "Pipelining or trailing bytes unsupported"))
         (t
          (emacs-agent-http--validate-request
           server
           (emacs-agent-http-request-create
            :method method :target (nth 1 parsed) :version (nth 2 parsed)
            :headers headers :body (substring data body-start frame-end)
            :remote remote))))))))

(defun emacs-agent-http--send (process status body &optional headers)
  "Send STATUS, BODY, and HEADERS on PROCESS, then close it."
  (when (process-live-p process)
    (let* ((body (or body ""))
           (body (if (multibyte-string-p body)
                     (encode-coding-string body 'utf-8 t)
                   body))
           (reason (or (alist-get status emacs-agent-http--status-text)
                       "Error"))
           (all-headers
            (append
             headers
             `(("Content-Type" . "application/json; charset=utf-8")
               ("Content-Length" . ,(number-to-string
                                     (string-bytes body)))
               ("Cache-Control" . "no-store")
               ("Connection" . "close"))))
           (head
            (concat
             (format "HTTP/1.1 %d %s\r\n" status reason)
             (mapconcat (lambda (entry)
                          (format "%s: %s" (car entry) (cdr entry)))
                        all-headers "\r\n")
             "\r\n\r\n")))
      (process-send-string process (encode-coding-string head 'binary t))
      (unless (string-empty-p body)
        (process-send-string process body))
      (process-send-eof process)
      (delete-process process))))

(defun emacs-agent-http--normalize-response (response)
  "Return (STATUS HEADERS BODY) for handler RESPONSE."
  (cond
   ((emacs-agent-http-response-p response)
    (list (emacs-agent-http-response-status response)
          (emacs-agent-http-response-headers response)
          (emacs-agent-http-response-body response)))
   ((and (fboundp 'emacs-agent-protocol-response-p)
         (emacs-agent-protocol-response-p response))
    (list (emacs-agent-protocol-response-status response)
          (emacs-agent-protocol-response-headers response)
          (emacs-agent-protocol-response-body response)))
   (t (error "HTTP handler returned an invalid response"))))

(defun emacs-agent-http--dispatch (process server request)
  "Invoke SERVER's handler for REQUEST and reply on PROCESS."
  (when (process-live-p process)
    (condition-case nil
        (pcase-let ((`(,status ,headers ,body)
                     (emacs-agent-http--normalize-response
                      (funcall (emacs-agent-http-server-handler server)
                               request))))
          (emacs-agent-http--send process status body headers))
      (_error
       (emacs-agent-http--send process 500
                               "{\"error\":\"internal server error\"}")))))

(defun emacs-agent-http--error-body (message)
  "Encode public HTTP error MESSAGE as JSON bytes."
  (encode-coding-string
   (json-serialize `((error . ,message)))
   'utf-8 t))

(defun emacs-agent-http--timeout (process)
  "Terminate an incomplete request on PROCESS."
  (when (process-live-p process)
    (emacs-agent-http--send process 408
                            "{\"error\":\"request timeout\"}")))

(defun emacs-agent-http--filter (process chunk)
  "Collect CHUNK from PROCESS and enqueue a complete request."
  (unless (process-get process 'emacs-agent-dispatched)
    (condition-case condition
        (let* ((server (process-get process 'emacs-agent-server))
               (data (concat (or (process-get process 'emacs-agent-data) "")
                             (encode-coding-string chunk 'binary t)))
               (request
                (emacs-agent-http--try-frame
                 server data (process-contact process :remote))))
          (process-put process 'emacs-agent-data data)
          (when request
            (setf (emacs-agent-http-request-connection request) process)
            (process-put process 'emacs-agent-dispatched t)
            (when-let* ((timer (process-get process 'emacs-agent-timer)))
              (cancel-timer timer))
            (run-at-time 0 nil #'emacs-agent-http--dispatch
                         process server request)))
      (emacs-agent-http-error
       (let ((values (cdr condition)))
         (emacs-agent-http--send
          process (nth 0 values)
          (emacs-agent-http--error-body (nth 1 values))
          (nth 2 values))))
      (error
       (emacs-agent-http--send
        process 400 "{\"error\":\"malformed request\"}")))))

(defun emacs-agent-http--sentinel (process _event)
  "Clean transport state for PROCESS."
  (unless (process-live-p process)
    (when-let* ((timer (process-get process 'emacs-agent-timer)))
      (cancel-timer timer))
    (when-let* ((request (process-get process 'emacs-agent-request)))
      (when (fboundp 'emacs-agent-request-cancel)
        (emacs-agent-request-cancel request)))
    (when-let* ((server (process-get process 'emacs-agent-server)))
      (setf (emacs-agent-http-server-clients server)
            (delq process (emacs-agent-http-server-clients server))))))

(defun emacs-agent-http--log (listener-process client _message)
  "Initialize accepted CLIENT from LISTENER-PROCESS."
  (let ((server (process-get listener-process 'emacs-agent-server)))
    (process-put client 'emacs-agent-server server)
    (process-put client 'emacs-agent-data "")
    (set-process-coding-system client 'binary 'binary)
    (set-process-filter client #'emacs-agent-http--filter)
    (set-process-sentinel client #'emacs-agent-http--sentinel)
    (process-put
     client 'emacs-agent-timer
     (run-at-time emacs-agent-http-idle-timeout nil
                  #'emacs-agent-http--timeout client))
    (push client (emacs-agent-http-server-clients server))))

(cl-defun emacs-agent-http-start
    (handler &key (host "127.0.0.1") (port 0) (endpoint "/mcp")
             token allowed-origins)
  "Start an MCP HTTP server and call HANDLER for each request.
HOST defaults to loopback and PORT to an ephemeral port.  ENDPOINT defaults to
`/mcp'.  A non-nil TOKEN enables bearer authentication.  ALLOWED-ORIGINS is
an exact-match string list."
  (unless (functionp handler)
    (error "An HTTP request handler is required"))
  (when (and token
             (or (not (stringp token)) (string-empty-p token)))
    (error "Bearer token must be nil or a nonempty string"))
  (when (and emacs-agent-http--server
             (process-live-p
              (emacs-agent-http-server-listener
               emacs-agent-http--server)))
    (error "Emacs Agent HTTP server is already running"))
  (let* ((server
          (emacs-agent-http-server--create
           :host host :port port :endpoint endpoint :token token
           :allowed-origins allowed-origins :handler handler))
         (listener
          (make-network-process
           :name "emacs-agent-http"
           :server t :host host :service port
           :family 'ipv4 :coding 'binary :noquery t
           :log #'emacs-agent-http--log)))
    (setf (emacs-agent-http-server-listener server) listener
          (emacs-agent-http-server-port server)
          (process-contact listener :service))
    (process-put listener 'emacs-agent-server server)
    (setq emacs-agent-http--server server)
    server))

(defun emacs-agent-http-stop (&optional server)
  "Stop SERVER, or the active package server."
  (interactive)
  (let ((server (or server emacs-agent-http--server)))
    (when server
      (dolist (client (copy-sequence
                       (emacs-agent-http-server-clients server)))
        (when (process-live-p client) (delete-process client)))
      (when-let* ((listener (emacs-agent-http-server-listener server)))
        (when (process-live-p listener) (delete-process listener)))
      (when (eq server emacs-agent-http--server)
        (setq emacs-agent-http--server nil))
      t)))

(provide 'emacs-agent-http)
;;; emacs-agent-http.el ends here
