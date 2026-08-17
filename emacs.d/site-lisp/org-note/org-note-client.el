;;; org-note-client.el --- HTTP transport for Org Note -*- lexical-binding: t; -*-

;;; Commentary:
;; A small JSON-over-HTTP transport used by Org Note clients.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'url)
(require 'url-http)
(require 'url-util)

(defgroup org-note nil
  "Org Note integration."
  :group 'applications)

(defcustom org-note-endpoint "https://agent-note.gsmlg.net/"
  "Base URL for the Org Note service."
  :type 'string
  :group 'org-note)

(defcustom org-note-actor-id
  (format "emacs:%s@%s" (user-login-name) (system-name))
  "Stable identifier for this Org Note actor."
  :type 'string
  :group 'org-note)

(defcustom org-note-request-timeout 30
  "Maximum number of seconds to wait for a synchronous request."
  :type 'integer
  :group 'org-note)

(define-error 'org-note-error "Org Note error")
(define-error 'org-note-transport-error "Org Note transport error" 'org-note-error)
(define-error 'org-note-response-error "Org Note response error" 'org-note-error)
(define-error 'org-note-http-error "Org Note HTTP error" 'org-note-response-error)

(defvar org-note-client--operation-counter 0
  "Monotonically increasing suffix for operation identifiers.")

(defun org-note-client-empty-object ()
  "Return an empty JSON object represented by an equal hash table."
  (make-hash-table :test #'equal))

(defun org-note-client-new-operation-id ()
  "Return a process-unique operation identifier."
  (setq org-note-client--operation-counter
        (1+ org-note-client--operation-counter))
  (secure-hash
   'sha256
   (format "%s:%s:%s:%s:%s"
           (float-time)
           (emacs-pid)
           (random)
           (current-time-string)
           org-note-client--operation-counter)))

(defun org-note-client--query-value (value)
  "Convert query VALUE to its wire representation."
  (cond
   ((eq value t) "true")
   ((eq value :json-false) "false")
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun org-note-client--url-encode (value)
  "Percent encode VALUE as UTF-8 URL data."
  (url-hexify-string value))

(defun org-note-client-url (route &optional query)
  "Build the request URL for ROUTE and optional QUERY alist.

Nil query values are omitted.  Boolean values become JSON boolean literals."
  (let* ((base (replace-regexp-in-string "/+\\'" "" org-note-endpoint))
         (path (replace-regexp-in-string "\\`/+" "" route))
         (pairs
          (delq nil
                (mapcar
                 (lambda (entry)
                   (let ((key (car entry))
                         (value (cdr entry)))
                     (when value
                       (format "%s=%s"
                               (org-note-client--url-encode
                                (if (symbolp key) (symbol-name key) key))
                               (org-note-client--url-encode
                                (org-note-client--query-value value))))))
                 query)))
         (url (concat base "/" path)))
    (if pairs
        (concat url "?" (mapconcat #'identity pairs "&"))
      url)))

(defun org-note-client--request-data (body)
  "Encode BODY as UTF-8 JSON bytes."
  (encode-coding-string
   (json-serialize body :false-object :json-false :null-object nil)
   'utf-8))

(defun org-note-client--request-headers (body-p)
  "Return request headers, adding content type when BODY-P is non-nil."
  (append '(("Accept" . "application/json"))
          (when body-p
            '(("Content-Type" . "application/json; charset=utf-8")))))

(defun org-note-client--symbolize-json-keys (value)
  "Recursively convert JSON object keys in VALUE to symbols."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'org-note-client--symbolize-json-keys value)))
   ((listp value)
    (mapcar (lambda (entry)
              (cons (if (stringp (car entry))
                        (intern (car entry))
                      (car entry))
                    (org-note-client--symbolize-json-keys (cdr entry))))
            value))
   (t value)))

(defun org-note-client--parse-json (body)
  "Parse BODY into symbol-keyed alists and vectors.
Signal `org-note-response-error' if BODY is not valid JSON."
  (condition-case nil
      (org-note-client--symbolize-json-keys
       (json-parse-string body
                          :object-type 'alist
                          :array-type 'array
                          :null-object nil
                          :false-object :json-false))
    (json-parse-error
     (signal 'org-note-response-error
             (list (list :status nil :code nil :message "Malformed JSON response"
                         :details nil :retryable nil))))))

(defun org-note-client--fencing-token-key-p (key)
  "Return non-nil when KEY identifies a fencing token."
  (or (eq key 'fencing_token)
      (equal key "fencing_token")))

(defun org-note-client--fencing-token-values (body)
  "Return string fencing-token values found recursively in BODY."
  (let (values)
    (cl-labels
        ((collect
          (value)
          (cond
           ((hash-table-p value)
            (maphash
             (lambda (key item)
               (when (and (org-note-client--fencing-token-key-p key)
                          (stringp item)
                          (not (string-empty-p item)))
                 (push item values))
               (collect item))
             value))
           ((vectorp value)
            (mapc #'collect value))
           ((listp value)
            (dolist (entry value)
              (if (consp entry)
                  (progn
                    (when (and (org-note-client--fencing-token-key-p (car entry))
                               (stringp (cdr entry))
                               (not (string-empty-p (cdr entry))))
                      (push (cdr entry) values))
                    (collect (cdr entry)))
                (collect entry)))))))
      (collect body))
    (delete-dups values)))

(defun org-note-client--redact-string (value fencing-tokens)
  "Replace FENCING-TOKENS occurring in string VALUE."
  (dolist (token fencing-tokens value)
    (setq value (replace-regexp-in-string
                 (regexp-quote token) "[REDACTED]" value t t))))

(defun org-note-client--redact-response-value (value fencing-tokens)
  "Redact fencing-token data recursively from response VALUE."
  (cond
   ((stringp value)
    (org-note-client--redact-string value fencing-tokens))
   ((vectorp value)
    (vconcat (mapcar (lambda (item)
                       (org-note-client--redact-response-value
                        item fencing-tokens))
                     value)))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key item)
         (puthash key
                  (if (org-note-client--fencing-token-key-p key)
                      "[REDACTED]"
                    (org-note-client--redact-response-value
                     item fencing-tokens))
                  copy))
       value)
      copy))
   ((listp value)
    (mapcar
     (lambda (entry)
       (if (consp entry)
           (cons (car entry)
                 (if (org-note-client--fencing-token-key-p (car entry))
                     "[REDACTED]"
                   (org-note-client--redact-response-value
                    (cdr entry) fencing-tokens)))
         (org-note-client--redact-response-value entry fencing-tokens)))
     value))
   (t value)))

(defun org-note-client--error-properties (status body fencing-tokens)
  "Return safe error properties for HTTP STATUS and response BODY.

FENCING-TOKENS are redacted from server messages and details."
  (let ((parsed (and (not (string-empty-p body))
                     (condition-case nil
                         (org-note-client--parse-json body)
                       (org-note-response-error nil)))))
    (let* ((server-error (or (and (listp parsed) (alist-get 'error parsed))
                             parsed))
           (properties
            (list :status status
                  :code (and (listp server-error) (alist-get 'code server-error))
                  :message (and (listp server-error)
                                (org-note-client--redact-response-value
                                 (alist-get 'message server-error) fencing-tokens))
                  :details (and (listp server-error)
                                (org-note-client--redact-response-value
                                 (alist-get 'details server-error) fencing-tokens))
                  :retryable (and (listp server-error) (alist-get 'retryable server-error)))))
      properties)))

(defun org-note-client--response-body ()
  "Return the body from the current URL retrieval buffer."
  (let ((start (if (markerp url-http-end-of-headers)
                   (marker-position url-http-end-of-headers)
                 url-http-end-of-headers)))
    (buffer-substring-no-properties (or start (point-min)) (point-max))))

(defun org-note-client--response-result (fencing-tokens)
  "Parse the current URL retrieval buffer and return its response value.

FENCING-TOKENS are redacted from HTTP error data."
  (let ((status url-http-response-status)
        (body (org-note-client--response-body)))
    (cond
     ((not (integerp status))
      (signal 'org-note-transport-error
              (list (list :status nil :code nil :message "No HTTP response"
                          :details nil :retryable nil))))
     ((and (>= status 200) (< status 300))
      (if (string-empty-p body)
          nil
        (org-note-client--parse-json body)))
     (t
      (signal 'org-note-http-error
              (list (org-note-client--error-properties
                     status body fencing-tokens)))))))

(defun org-note-client--transport-error ()
  "Return a safe condition payload for a transport failure."
  (list :status nil :code nil :message "Request failed" :details nil :retryable nil))

(defun org-note-client-request (method route &optional query body)
  "Synchronously request METHOD at ROUTE with optional QUERY and JSON BODY.

Return a parsed JSON value for successful responses, nil for empty successful
responses, or signal an Org Note condition for failures."
  (let ((url-request-method method)
        (url-request-extra-headers (org-note-client--request-headers body))
        (url-request-data (and body (org-note-client--request-data body)))
        (fencing-tokens (org-note-client--fencing-token-values body))
        (buffer nil))
    (condition-case nil
        (setq buffer (url-retrieve-synchronously
                      (org-note-client-url route query) t t org-note-request-timeout))
      (error
       (signal 'org-note-transport-error (list (org-note-client--transport-error)))))
    (unless (buffer-live-p buffer)
      (signal 'org-note-transport-error (list (org-note-client--transport-error))))
    (unwind-protect
        (with-current-buffer buffer
          (org-note-client--response-result fencing-tokens))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun org-note-client-request-async (method route query body callback)
  "Asynchronously request METHOD at ROUTE and call CALLBACK once.

CALLBACK receives RESULT and ERROR.  Exactly one is non-nil unless a successful
response has an empty body, in which case both are nil."
  (let ((url-request-method method)
        (url-request-extra-headers (org-note-client--request-headers body))
        (url-request-data (and body (org-note-client--request-data body)))
        (fencing-tokens (org-note-client--fencing-token-values body))
        (completed nil))
    (condition-case nil
        (url-retrieve
         (org-note-client-url route query)
         (lambda (status)
           (unless completed
             (setq completed t)
             (let ((buffer (current-buffer)))
               (let (result request-error)
                 (unwind-protect
                     (condition-case err
                         (if (plist-get status :error)
                             (setq request-error
                                   (list 'org-note-transport-error
                                         (org-note-client--transport-error)))
                           (setq result
                                 (org-note-client--response-result fencing-tokens)))
                       (org-note-error
                        (setq request-error err))
                       (error
                        (setq request-error
                              (list 'org-note-transport-error
                                    (org-note-client--transport-error)))))
                   (when (buffer-live-p buffer)
                     (kill-buffer buffer)))
                 (funcall callback result request-error))))))
      (error
       (unless completed
         (setq completed t)
         (funcall callback nil
                  (list 'org-note-transport-error
                        (org-note-client--transport-error))))))))

(provide 'org-note-client)

;;; org-note-client.el ends here
