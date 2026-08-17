;;; org-note-client-test.el --- Tests for org-note transport -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the synchronous and asynchronous HTTP transport.

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'org-note-client)

(defun org-note-client-test--response-buffer (status body)
  "Create an HTTP response buffer with STATUS and BODY."
  (let ((buffer (generate-new-buffer " *org-note-test-response*")))
    (with-current-buffer buffer
      (insert "HTTP/1.1 " (number-to-string status) " Test\r\n")
      (insert "Content-Type: application/json\r\n\r\n" body)
      (setq-local url-http-response-status status)
      (setq-local url-http-end-of-headers (point-min))
      (goto-char (point-min))
      (search-forward "\r\n\r\n")
      (setq-local url-http-end-of-headers (point)))
    buffer))

(cl-defmacro org-note-client-test--with-sync-response ((status body) &rest forms)
  "Run FORMS with synchronous URL retrieval stubbed to STATUS and BODY."
  (declare (indent 1) (debug t))
  `(let (captured-url captured-method captured-headers captured-data)
     (cl-letf (((symbol-function 'url-retrieve-synchronously)
                (lambda (url &rest _)
                  (setq captured-url url
                        captured-method url-request-method
                        captured-headers url-request-extra-headers
                        captured-data url-request-data)
                  (org-note-client-test--response-buffer ,status ,body))))
       ,@forms)))

(ert-deftest org-note-client-defaults-are-correct ()
  (should (equal org-note-endpoint "https://agent-note.gsmlg.net/"))
  (should (equal org-note-request-timeout 30))
  (should (equal org-note-actor-id
                 (format "emacs:%s@%s" (user-login-name) (system-name))))
  (should (memq 'org-note-response-error
                (get 'org-note-http-error 'error-conditions))))

(ert-deftest org-note-client-url-normalizes-slashes-and-encodes-query ()
  (let ((org-note-endpoint "https://notes.example/api/"))
    (should
     (equal (org-note-client-url
             "/notes"
             '((revision . nil) (include_deleted . t) (flag . :json-false)
               (query . "cafe au lait") (unicode . "café")))
            "https://notes.example/api/notes?include_deleted=true&flag=false&query=cafe%20au%20lait&unicode=caf%C3%A9"))))

(ert-deftest org-note-client-sends-utf8-json-request ()
  (org-note-client-test--with-sync-response (200 "{\"id\":\"n-1\"}")
    (let ((result (org-note-client-request "POST" "/notes" nil
                                           '((title . "cafe") (body . "中文")))))
      (should (equal captured-url "https://agent-note.gsmlg.net/notes"))
      (should (equal captured-method "POST"))
      (should (equal (cdr (assoc "Accept" captured-headers)) "application/json"))
      (should (equal (cdr (assoc "Content-Type" captured-headers))
                     "application/json; charset=utf-8"))
      (should (equal (decode-coding-string captured-data 'utf-8)
                     "{\"title\":\"cafe\",\"body\":\"中文\"}"))
      (should (equal (alist-get 'id result) "n-1")))))

(ert-deftest org-note-client-parses-json-success ()
  (org-note-client-test--with-sync-response
      (200 "{\"note\":{\"id\":\"n-1\"},\"items\":[false,null]}")
    (let ((result (org-note-client-request "GET" "/notes")))
      (should (equal (alist-get 'id (alist-get 'note result)) "n-1"))
      (should (equal (alist-get 'items result) [:json-false nil])))))

(ert-deftest org-note-client-returns-nil-for-empty-success ()
  (org-note-client-test--with-sync-response (204 "")
    (should-not (org-note-client-request "DELETE" "/notes/n-1"))))

(ert-deftest org-note-client-signals-safe-stale-revision-error ()
  (org-note-client-test--with-sync-response
      (409 "{\"error\":{\"code\":\"stale_revision\",\"message\":\"Revision is stale\",\"details\":{\"expected\":2},\"retryable\":false}}")
    (let ((error-data
           (should-error (org-note-client-request "PATCH" "/notes/n-1")
                         :type 'org-note-http-error)))
      (should (equal (plist-get (cadr error-data) :status) 409))
      (should (equal (plist-get (cadr error-data) :code) "stale_revision"))
      (should (eq (plist-get (cadr error-data) :retryable) :json-false)))))

(ert-deftest org-note-client-signals-malformed-response-error ()
  (org-note-client-test--with-sync-response (200 "not json")
    (should-error (org-note-client-request "GET" "/notes")
                  :type 'org-note-response-error)))

(ert-deftest org-note-client-async-calls-back-once ()
  (let (calls response-buffer)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq response-buffer
                       (org-note-client-test--response-buffer 200 "{\"id\":\"n-1\"}"))
                 (with-current-buffer response-buffer
                     (funcall callback nil))
                 (funcall callback nil)
                 response-buffer)))
      (org-note-client-request-async
       "GET" "/notes" nil nil
       (lambda (result error)
         (push (list result error) calls))))
    (should (= (length calls) 1))
    (should (equal (alist-get 'id (caar calls)) "n-1"))
    (should-not (cadar calls))
    (should-not (buffer-live-p response-buffer)))
  (let (calls response-buffer)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq response-buffer
                       (org-note-client-test--response-buffer
                        409 "{\"error\":{\"code\":\"stale_revision\"}}"))
                 (with-current-buffer response-buffer
                   (funcall callback nil))
                 response-buffer)))
      (org-note-client-request-async
       "PATCH" "/notes/n-1" nil nil
       (lambda (result error)
         (push (list result error) calls))))
    (should (= (length calls) 1))
    (should (eq (car (cadr (car calls))) 'org-note-http-error))
    (should-not (buffer-live-p response-buffer)))
  (let (calls response-buffer)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq response-buffer
                       (org-note-client-test--response-buffer 200 "not json"))
                 (with-current-buffer response-buffer
                   (funcall callback nil))
                 response-buffer)))
      (org-note-client-request-async
       "GET" "/notes" nil nil
       (lambda (result error)
         (push (list result error) calls))))
    (should (= (length calls) 1))
    (should (eq (car (cadr (car calls))) 'org-note-response-error))
    (should-not (buffer-live-p response-buffer)))
  (let (calls response-buffer)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq response-buffer
                       (org-note-client-test--response-buffer 200 ""))
                 (with-current-buffer response-buffer
                   (funcall callback '(:error "Retrieval failed")))
                 response-buffer)))
      (org-note-client-request-async
       "GET" "/notes" nil nil
       (lambda (result error)
         (push (list result error) calls))))
    (should (= (length calls) 1))
    (should (eq (car (cadr (car calls))) 'org-note-transport-error))
    (should-not (buffer-live-p response-buffer)))
  (let ((calls nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _) (error "Immediate retrieval failure"))))
      (org-note-client-request-async
       "GET" "/notes" nil nil
       (lambda (result error)
         (push (list result error) calls))))
    (should (= (length calls) 1))
    (should (eq (car (cadr (car calls))) 'org-note-transport-error)))
  (let ((failing-calls 0))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (with-current-buffer
                     (org-note-client-test--response-buffer 200 "{\"id\":\"n-1\"}")
                   (funcall callback nil)))))
      (org-note-client-request-async
       "GET" "/notes" nil nil
       (lambda (_result _error)
         (setq failing-calls (1+ failing-calls))
         (error "Callback failed"))))
    (should (= failing-calls 1))))

(ert-deftest org-note-client-error-does-not-expose-secrets ()
  (org-note-client-test--with-sync-response
      (500 "{\"error\":{\"code\":\"conflict\",\"message\":\"Rejected supersecret\",\"details\":{\"fencing_token\":\"supersecret\",\"nested\":{\"echo\":\"supersecret\",\"safe\":\"retained\"}},\"retryable\":false}}")
    (let ((error-data
           (should-error (org-note-client-request
                          "POST" "/notes" nil
                          '((fencing_token . "supersecret")))
                         :type 'org-note-http-error)))
      (should-not (string-match-p "supersecret"
                                  (error-message-string error-data)))
      (should-not (string-match-p "supersecret"
                                  (plist-get (cadr error-data) :message)))
      (should (equal (alist-get 'fencing_token
                                (plist-get (cadr error-data) :details))
                     "[REDACTED]"))
      (should (equal (alist-get 'echo
                                (alist-get 'nested
                                           (plist-get (cadr error-data) :details)))
                     "[REDACTED]"))
      (should (equal (alist-get 'safe
                                (alist-get 'nested
                                           (plist-get (cadr error-data) :details)))
                     "retained")))))

(ert-deftest org-note-client-operation-ids-are-unique ()
  (let ((first (org-note-client-new-operation-id))
        (second (org-note-client-new-operation-id)))
    (should (stringp first))
    (should (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" first))
    (should-not (equal first second))))

;;; org-note-client-test.el ends here
