;;; org-note-operation-test.el --- Tests for Org Note operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Org Note operation-level HTTP wrappers.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'url-http)

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'org-note-operation)

(cl-defmacro org-note-operation-test--capture-request (&rest forms)
  "Run FORMS while capturing the Org Note client request arguments."
  (declare (indent 0) (debug t))
  `(let (request)
     (cl-letf (((symbol-function 'org-note-client-request)
                (lambda (method route query body)
                  (setq request (list method route query body)))))
       ,@forms
       request)))

(defun org-note-operation-test--should-equal-json-value (actual expected)
  "Assert ACTUAL has the same JSON value semantics as EXPECTED."
  (if (hash-table-p expected)
      (progn
        (should (hash-table-p actual))
        (should (= (hash-table-count actual) (hash-table-count expected)))
        (maphash
         (lambda (key expected-value)
           (let ((missing (make-symbol "missing")))
             (org-note-operation-test--should-equal-json-value
              (gethash key actual missing) expected-value)))
         expected))
    (should (equal actual expected))))

(defun org-note-operation-test--should-equal-json-object (actual expected)
  "Assert ACTUAL has exactly the key-value pairs in EXPECTED.

Key ordering is ignored, while nested JSON object semantics are preserved."
  (should (listp actual))
  (should (= (length actual) (length expected)))
  (dolist (expected-entry expected)
    (let ((actual-entry (assq (car expected-entry) actual)))
      (should actual-entry)
      (org-note-operation-test--should-equal-json-value
       (cdr actual-entry) (cdr expected-entry)))))

(defun org-note-operation-test--response-buffer (status body)
  "Return an HTTP response buffer with STATUS and BODY."
  (let ((buffer (generate-new-buffer " *org-note-operation-response*")))
    (with-current-buffer buffer
      (insert "HTTP/1.1 " (number-to-string status) " Test\r\n")
      (insert "Content-Type: application/json\r\n\r\n" body)
      (setq-local url-http-response-status status)
      (setq-local url-http-end-of-headers (point-min))
      (goto-char (point-min))
      (search-forward "\r\n\r\n")
      (setq-local url-http-end-of-headers (point)))
    buffer))

(defun org-note-operation-test--revision-map (document-id revision)
  "Return a JSON revision map for DOCUMENT-ID at REVISION."
  (let ((revisions (make-hash-table :test 'equal)))
    (puthash document-id revision revisions)
    revisions))

(defun org-note-operation-test--lease (lease-id fencing-token)
  "Return a lease JSON object with LEASE-ID and FENCING-TOKEN."
  `((lease_id . ,lease-id) (fencing_token . ,fencing-token)))

(defun org-note-operation-test--claim-response
    (workspace-id item-id document-id kind operation-id
                  &optional lease-id fencing-token expires-at)
  "Return a valid claim response for the supplied identifiers.

LEASE-ID, FENCING-TOKEN, and EXPIRES-AT default to valid test values."
  (let ((lease-id (or lease-id "lease-1"))
        (fencing-token (or fencing-token "fence-1"))
        (expires-at (or expires-at 200)))
    `((schema_version . 1)
      (workspace_id . ,workspace-id)
      (operation_id . ,operation-id)
      (lease_id . ,lease-id)
      (fencing_token . ,fencing-token)
      (expires_at . ,expires-at)
      (event_ids . ["event-1"])
      (context
       . ((workspace . ((id . ,workspace-id)))
          (workspace_revision . 4)
          (document . ((id . ,document-id) (path . "inbox.org") (revision . 4)))
          (item . ((id . ,item-id)
                   (workspace_id . ,workspace-id)
                   (document_id . ,document-id)))
          (lease . ((id . ,lease-id)
                    (workspace_id . ,workspace-id)
                    (work_item_id . ,item-id)
                    (attempt_id . "attempt-1")
                    (kind . ,kind)
                    (actor_id . ,org-note-actor-id)
                    (acquired_at . 90)
                    (last_heartbeat_at . 90)
                    (expires_at . ,expires-at)
                    (status . "active"))))))))

(defun org-note-operation-test--transition-response
    (workspace-id item-id document-id operation-id lease)
  "Return a valid transition response whose context contains LEASE."
  `((schema_version . 1)
    (workspace_id . ,workspace-id)
    (operation_id . ,operation-id)
    (event_ids . ["event-1"])
    (data
     . ((context
         . ((workspace . ((id . ,workspace-id)))
            (workspace_revision . 5)
            (document . ((id . ,document-id) (revision . 5)))
            (item . ((id . ,item-id)
                     (workspace_id . ,workspace-id)
                     (document_id . ,document-id)))
            (lease . ,lease)))))))

(defun org-note-operation-test--active-context-lease
    (workspace-id item-id lease-id kind expires-at)
  "Return an active context lease for WORKSPACE-ID and ITEM-ID."
  `((id . ,lease-id)
    (workspace_id . ,workspace-id)
    (work_item_id . ,item-id)
    (attempt_id . "attempt-1")
    (kind . ,kind)
    (actor_id . ,org-note-actor-id)
    (acquired_at . 90)
    (last_heartbeat_at . 100)
    (expires_at . ,expires-at)
    (status . "active")))

(defun org-note-operation-test--value (object key)
  "Return KEY's value from the symbol-keyed alist OBJECT."
  (cdr (assq key object)))

(cl-defmacro org-note-operation-test--with-lease-state ((now) &rest body)
  "Run BODY with isolated lease state and the clock initialized to NOW."
  (declare (indent 1) (debug ((form) body)))
  `(let ((org-note-operation--leases (make-hash-table :test #'equal))
         (org-note-operation-test--now ,now)
         (org-note-operation-test--scheduled nil)
         (org-note-operation-test--cancelled nil)
         (org-note-operation-test--timer-counter 0))
     (unwind-protect
         (cl-letf (((symbol-function 'float-time)
                    (lambda (&optional _time)
                      org-note-operation-test--now))
                   ((symbol-function 'run-at-time)
                    (lambda (delay repeat function &rest args)
                      (let ((timer
                             (list 'org-note-operation-test-timer
                                   (cl-incf org-note-operation-test--timer-counter))))
                        (push (list delay repeat function args timer)
                              org-note-operation-test--scheduled)
                        timer)))
                   ((symbol-function 'cancel-timer)
                    (lambda (timer)
                      (push timer org-note-operation-test--cancelled))))
           ,@body)
       (clrhash org-note-operation--leases))))

(ert-deftest org-note-operation-registers-claim-and-retry-leases ()
  (org-note-operation-test--with-lease-state (100.0)
    (let* ((org-note-actor-id "emacs:test@example")
           (claim-response
           (org-note-operation-test--claim-response
            "workspace-1" "item-review" "document-1" "review"
            "claim-operation" "lease-review" "fence-review" 200))
          (retry-response
           (org-note-operation-test--claim-response
            "workspace-1" "item-execution" "document-1" "execution"
            "retry-operation" "lease-execution" "fence-execution" 220)))
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (method route _query _body)
                   (if (string-suffix-p "/retry" route)
                       retry-response
                     (should (equal method "POST"))
                     claim-response))))
        (should (eq (org-note-operation-claim
                     "workspace-1" "item-review" "document-1" 3 "review"
                     :operation-id "claim-operation")
                    claim-response))
        (should (eq (org-note-operation-retry
                     "workspace-1" "item-execution" "document-1" 3
                     :operation-id "retry-operation")
                    retry-response)))
      (let ((review
             (org-note-operation-find-lease
              "workspace-1" "item-review" "review"))
            (execution
             (org-note-operation-find-lease
              "workspace-1" "item-execution" "execution")))
        (should (equal (org-note-operation-lease-document-id review) "document-1"))
        (should (equal (org-note-operation-lease-lease-id review) "lease-review"))
        (should (equal (org-note-operation-lease-fencing-token review)
                       "fence-review"))
        (should (equal (org-note-operation-lease-kind execution) "execution")))
      (should (= (length org-note-operation-test--scheduled) 2)))))

(ert-deftest org-note-operation-register-claim-rejects-unsafe-token-safely ()
  (org-note-operation-test--with-lease-state (100.0)
    (let* ((secret "must-not-appear")
           (error-data
            (should-error
             (org-note-operation-register-claim
              "workspace-1" "item-1" "document-1" "execution"
              `((lease_id . "lease-1")
                (fencing_token . (,secret))
                (expires_at . 200.0)))
             :type 'org-note-error)))
      (should-not (string-match-p secret (prin1-to-string error-data)))
      (should-not (string-match-p secret (error-message-string error-data)))
      (should-not
       (org-note-operation-find-lease "workspace-1" "item-1" "execution")))))

(ert-deftest org-note-operation-claim-and-retry-reject-malformed-responses ()
  (let ((org-note-actor-id "emacs:test@example")
        (secret "must-not-leak-from-response")
        (cases
         `(("not an object" . ,(lambda (_response) []))
           ("missing lease id"
            . ,(lambda (response)
                 (assq-delete-all 'lease_id response)))
           ("empty lease id"
            . ,(lambda (response)
                 (setcdr (assq 'lease_id response) "")
                 response))
           ("empty fencing token"
            . ,(lambda (response)
                 (setcdr (assq 'fencing_token response) "")
                 response))
           ("expired lease"
            . ,(lambda (response)
                 (setcdr (assq 'expires_at response) 100)
                 response))
           ("wrong workspace"
            . ,(lambda (response)
                 (setcdr (assq 'workspace_id response) "workspace-other")
                 response))
           ("wrong operation"
            . ,(lambda (response)
                 (setcdr (assq 'operation_id response) "operation-other")
                 response))
           ("wrong context workspace"
            . ,(lambda (response)
                 (setcdr
                  (assq 'id
                        (org-note-operation-test--value
                         (org-note-operation-test--value response 'context)
                         'workspace))
                  "workspace-other")
                 response))
           ("wrong context document"
            . ,(lambda (response)
                 (setcdr
                  (assq 'id
                        (org-note-operation-test--value
                         (org-note-operation-test--value response 'context)
                         'document))
                  "document-other")
                 response))
           ("wrong context item"
            . ,(lambda (response)
                 (setcdr
                  (assq 'id
                        (org-note-operation-test--value
                         (org-note-operation-test--value response 'context)
                         'item))
                  "item-other")
                 response))
           ("wrong lease kind"
            . ,(lambda (response)
                 (setcdr
                  (assq 'kind
                        (org-note-operation-test--value
                         (org-note-operation-test--value response 'context)
                         'lease))
                  "other")
                 response))
           ("missing context lease"
            . ,(lambda (response)
                 (setcdr
                  (assq 'lease
                        (org-note-operation-test--value response 'context))
                  nil)
                 response)))))
    (dolist (wrapper '(claim retry))
      (dolist (case cases)
        (org-note-operation-test--with-lease-state (100.0)
          (let* ((kind (if (eq wrapper 'claim) "review" "execution"))
                 (operation-id (format "%s-operation" wrapper))
                 (response
                  (org-note-operation-test--claim-response
                   "workspace-1" "item-1" "document-1" kind operation-id
                   "lease-1" secret 200))
                 (malformed (funcall (cdr case) response)))
            (ert-info ((format "%s: %s" wrapper (car case)))
              (cl-letf (((symbol-function 'org-note-client-request)
                         (lambda (&rest _arguments) malformed)))
                (let ((error-data
                       (should-error
                        (if (eq wrapper 'claim)
                            (org-note-operation-claim
                             "workspace-1" "item-1" "document-1" 3 kind
                             :operation-id operation-id)
                          (org-note-operation-retry
                           "workspace-1" "item-1" "document-1" 3
                           :operation-id operation-id))
                        :type 'org-note-error)))
                  (should-not
                   (string-match-p secret (prin1-to-string error-data)))
                  (should-not
                   (string-match-p secret (error-message-string error-data)))))
              (should (= (hash-table-count org-note-operation--leases) 0))
              (should-not org-note-operation-test--scheduled))))))))

(ert-deftest org-note-operation-lease-proofs-filter-document-and-expiry ()
  (org-note-operation-test--with-lease-state (100.0)
    (org-note-operation-register-claim
     "workspace-1" "item-current" "document-1" "execution"
     '((lease_id . "lease-current")
       (fencing_token . "fence-current")
       (expires_at . 200.0)))
    (org-note-operation-register-claim
     "workspace-1" "item-other-document" "document-2" "execution"
     '((lease_id . "lease-other")
       (fencing_token . "fence-other")
       (expires_at . 200.0)))
    (org-note-operation-register-claim
     "workspace-1" "item-expired" "document-1" "review"
     '((lease_id . "lease-expired")
       (fencing_token . "fence-expired")
       (expires_at . 101.0)))
    (setq org-note-operation-test--now 102.0)
    (let ((proofs (org-note-operation-lease-proofs "document-1")))
      (should (hash-table-p proofs))
      (should (eq (hash-table-test proofs) 'equal))
      (should (= (hash-table-count proofs) 1))
      (should
       (equal (gethash "item-current" proofs)
              '((lease_id . "lease-current")
                (kind . "execution")
                (fencing_token . "fence-current"))))
      (should-not (gethash "item-other-document" proofs))
      (should-not (gethash "item-expired" proofs)))
    (should-not
     (org-note-operation-find-lease "workspace-1" "item-expired" "review"))))

(ert-deftest org-note-operation-forget-lease-cancels-its-timer ()
  (org-note-operation-test--with-lease-state (100.0)
    (org-note-operation-register-claim
     "workspace-1" "item-1" "document-1" "execution"
     '((lease_id . "lease-1")
       (fencing_token . "fence-1")
       (expires_at . 200.0)))
    (let* ((lease
            (org-note-operation-find-lease
             "workspace-1" "item-1" "execution"))
           (timer (org-note-operation-lease-timer lease)))
      (org-note-operation-forget-lease "workspace-1" "item-1" "execution")
      (should (member timer org-note-operation-test--cancelled))
      (should-not
       (org-note-operation-find-lease "workspace-1" "item-1" "execution")))))

(ert-deftest org-note-operation-terminal-actions-forget-only-after-success ()
  (org-note-operation-test--with-lease-state (100.0)
    (dolist
        (case
         `(("execution"
            . ,(lambda ()
                 (org-note-operation-submit-result
                  "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-1"
                  "Complete")))
           ("execution"
            . ,(lambda ()
                 (org-note-operation-request-review
                  "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-1")))
           ("review"
            . ,(lambda ()
                 (org-note-operation-release
                  "workspace-1" "item-1" "document-1" 3 "lease-1" "review"
                  "fence-1")))
           ("review"
            . ,(lambda ()
                 (org-note-operation-approve-review
                  "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-1")))
           ("review"
            . ,(lambda ()
                 (org-note-operation-reject-review
                  "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-1"
                  "Revise")))))
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" (car case)
       `((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 200.0)))
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (&rest _) '((ok . t)))))
        (funcall (cdr case)))
      (should-not
       (org-note-operation-find-lease "workspace-1" "item-1" (car case))))
    (org-note-operation-register-claim
     "workspace-1" "item-1" "document-1" "review"
     '((lease_id . "lease-1")
       (fencing_token . "fence-1")
       (expires_at . 200.0)))
    (cl-letf (((symbol-function 'org-note-client-request)
               (lambda (&rest _)
                 (signal 'org-note-http-error
                         '((:status 409 :code "conflict"))))))
      (should-error
       (org-note-operation-release
        "workspace-1" "item-1" "document-1" 3 "lease-1" "review" "fence-1")
       :type 'org-note-http-error))
    (should
     (org-note-operation-find-lease "workspace-1" "item-1" "review"))))

(ert-deftest org-note-operation-terminal-errors-retain-lease-and-timer ()
  (org-note-operation-test--with-lease-state (100.0)
    (dolist
        (case
         `(("execution"
            . ,(lambda ()
                 (org-note-operation-submit-result
                  "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-1"
                  "Complete")))
           ("execution"
            . ,(lambda ()
                 (org-note-operation-request-review
                  "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-1")))
           ("review"
            . ,(lambda ()
                 (org-note-operation-approve-review
                  "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-1")))
           ("review"
            . ,(lambda ()
                 (org-note-operation-reject-review
                  "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-1"
                  "Revise")))))
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" (car case)
       '((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 200.0)))
      (let* ((lease
              (org-note-operation-find-lease
               "workspace-1" "item-1" (car case)))
             (timer (org-note-operation-lease-timer lease)))
        (cl-letf (((symbol-function 'org-note-client-request)
                   (lambda (&rest _)
                     (signal 'org-note-http-error
                             '((:status 409 :code "conflict"))))))
          (should-error (funcall (cdr case)) :type 'org-note-http-error))
        (let ((retained
               (org-note-operation-find-lease
                "workspace-1" "item-1" (car case))))
          (should (eq retained lease))
          (should (eq (org-note-operation-lease-timer retained) timer)))))))

(ert-deftest org-note-operation-old-heartbeat-stale-error-does-not-forget-replacement ()
  (org-note-operation-test--with-lease-state (100.0)
    (let (old-callback)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       '((lease_id . "lease-a")
         (fencing_token . "fence-a")
         (expires_at . 200.0)))
      (cl-letf (((symbol-function 'org-note-client-request-async)
                 (lambda (_method _route _query _body callback)
                   (setq old-callback callback))))
        (apply (nth 2 (car org-note-operation-test--scheduled))
               (nth 3 (car org-note-operation-test--scheduled))))
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       '((lease_id . "lease-b")
         (fencing_token . "fence-b")
         (expires_at . 300.0)))
      (let* ((replacement
              (org-note-operation-find-lease
               "workspace-1" "item-1" "execution"))
             (timer (org-note-operation-lease-timer replacement)))
        (funcall old-callback nil
                 '(org-note-http-error (:status 409 :code "stale_lease")))
        (let ((current
               (org-note-operation-find-lease
                "workspace-1" "item-1" "execution")))
          (should (eq current replacement))
          (should (equal (org-note-operation-lease-lease-id current) "lease-b"))
          (should (= (org-note-operation-lease-expires-at current) 300.0))
          (should (eq (org-note-operation-lease-timer current) timer))
          (should-not (org-note-operation-lease-heartbeat-p current)))))))

(ert-deftest org-note-operation-old-heartbeat-success-does-not-update-replacement ()
  (org-note-operation-test--with-lease-state (100.0)
    (let (old-callback)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "review"
       '((lease_id . "lease-a")
         (fencing_token . "fence-a")
         (expires_at . 200.0)))
      (cl-letf (((symbol-function 'org-note-client-request-async)
                 (lambda (_method _route _query _body callback)
                   (setq old-callback callback))))
        (apply (nth 2 (car org-note-operation-test--scheduled))
               (nth 3 (car org-note-operation-test--scheduled))))
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "review"
       '((lease_id . "lease-b")
         (fencing_token . "fence-b")
         (expires_at . 300.0)))
      (let* ((replacement
              (org-note-operation-find-lease
               "workspace-1" "item-1" "review"))
             (timer (org-note-operation-lease-timer replacement)))
        (funcall old-callback '((expires_at . 500.0)) nil)
        (let ((current
               (org-note-operation-find-lease
                "workspace-1" "item-1" "review")))
          (should (eq current replacement))
          (should (= (org-note-operation-lease-expires-at current) 300.0))
          (should (eq (org-note-operation-lease-timer current) timer))
          (should-not (org-note-operation-lease-heartbeat-p current)))))))

(ert-deftest org-note-operation-old-context-callback-does-not-affect-replacement ()
  (org-note-operation-test--with-lease-state (100.0)
    (let (async-calls)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       '((lease_id . "lease-a")
         (fencing_token . "fence-a")
         (expires_at . 200.0)))
      (cl-letf (((symbol-function 'org-note-client-request-async)
                 (lambda (_method _route _query _body callback)
                   (push callback async-calls))))
        (apply (nth 2 (car org-note-operation-test--scheduled))
               (nth 3 (car org-note-operation-test--scheduled)))
        (funcall (car async-calls) '((status . "ok")) nil))
      (let ((old-context-callback (car async-calls)))
        (org-note-operation-register-claim
         "workspace-1" "item-1" "document-1" "execution"
         '((lease_id . "lease-b")
           (fencing_token . "fence-b")
           (expires_at . 300.0)))
        (let* ((replacement
                (org-note-operation-find-lease
                 "workspace-1" "item-1" "execution"))
               (timer (org-note-operation-lease-timer replacement)))
          (funcall
           old-context-callback
           '((lease . ((lease_id . "different")
                       (kind . "execution")
                       (expires_at . 500.0))))
           nil)
          (let ((current
                 (org-note-operation-find-lease
                  "workspace-1" "item-1" "execution")))
            (should (eq current replacement))
            (should (= (org-note-operation-lease-expires-at current) 300.0))
            (should (eq (org-note-operation-lease-timer current) timer))
            (should-not (org-note-operation-lease-heartbeat-p current))))))))

(ert-deftest org-note-operation-register-claim-rejects-expired-response ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((secret "expired-fence-secret"))
      (let ((error-data
             (should-error
              (org-note-operation-register-claim
               "workspace-1" "item-1" "document-1" "execution"
               `((lease_id . "lease-expired")
                 (fencing_token . ,secret)
                 (expires_at . 100.0)))
              :type 'org-note-error)))
        (should-not (string-match-p secret (prin1-to-string error-data)))
        (should-not (string-match-p secret (error-message-string error-data))))
      (should (= (hash-table-count org-note-operation--leases) 0))
      (should-not org-note-operation-test--scheduled))))

(ert-deftest org-note-operation-explicit-heartbeat-quit-recovers-state ()
  (org-note-operation-test--with-lease-state (100.0)
    (org-note-operation-register-claim
     "workspace-1" "item-1" "document-1" "execution"
     '((lease_id . "lease-1")
       (fencing_token . "fence-1")
       (expires_at . 200.0)))
    (let* ((lease
            (org-note-operation-find-lease
             "workspace-1" "item-1" "execution"))
           (old-timer (org-note-operation-lease-timer lease))
           caught)
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (&rest _arguments)
                   (signal 'quit '("original quit payload"))))
                ((symbol-function 'display-warning) #'ignore))
        (setq caught
              (condition-case error-data
                  (org-note-operation-heartbeat
                   "workspace-1" "item-1" "lease-1" "execution" "fence-1"
                   :operation-id "heartbeat-operation")
                (quit error-data))))
      (should (equal caught '(quit "original quit payload")))
      (should (eq lease
                  (org-note-operation-find-lease
                   "workspace-1" "item-1" "execution")))
      (should-not (org-note-operation-lease-heartbeat-p lease))
      (should (member old-timer org-note-operation-test--cancelled))
      (should (org-note-operation-lease-timer lease))
      (should-not (eq old-timer (org-note-operation-lease-timer lease)))
      (should (= (caar org-note-operation-test--scheduled) 5)))))

(ert-deftest org-note-operation-permanent-heartbeat-error-retains-until-cleanup ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((secret "permanent-fence-secret")
          warnings)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       `((lease_id . "lease-1")
         (fencing_token . ,secret)
         (expires_at . 120.0)))
      (setf (org-note-operation-lease-heartbeat-p
             (org-note-operation-find-lease
              "workspace-1" "item-1" "execution"))
            t)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &optional _level _buffer-name)
                   (push message warnings))))
        (org-note-operation--heartbeat-finished
         '("workspace-1" "item-1" "execution") nil
         `(org-note-http-error
           (:status 400 :code "invalid" :message ,secret
            :retryable :json-false))))
      (let* ((lease
              (org-note-operation-find-lease
               "workspace-1" "item-1" "execution"))
             (cleanup (car org-note-operation-test--scheduled)))
        (should lease)
        (should-not (org-note-operation-lease-heartbeat-p lease))
        (should (= (nth 0 cleanup) 20.0))
        (should (eq (nth 2 cleanup) #'org-note-operation--expiry-cleanup-timer))
        (should-not (string-match-p secret (prin1-to-string (nth 3 cleanup))))
        (should (= (length warnings) 1))
        (should-not (string-match-p secret (car warnings)))
        (setq org-note-operation-test--now 120.0)
        (apply (nth 2 cleanup) (nth 3 cleanup))
        (should-not
         (org-note-operation-find-lease
          "workspace-1" "item-1" "execution"))))))

(ert-deftest org-note-operation-http-404-schedules-expiry-not-retry ()
  (org-note-operation-test--with-lease-state (100.0)
    (org-note-operation-register-claim
     "workspace-1" "item-1" "document-1" "review"
     '((lease_id . "lease-1")
       (fencing_token . "fence-1")
       (expires_at . 200.0)))
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (org-note-operation--heartbeat-finished
       '("workspace-1" "item-1" "review") nil
       '(org-note-http-error
         (:status 404 :code "not_found" :retryable :json-false))))
    (should
     (org-note-operation-find-lease "workspace-1" "item-1" "review"))
    (should
     (eq (nth 2 (car org-note-operation-test--scheduled))
         #'org-note-operation--expiry-cleanup-timer))))

(ert-deftest org-note-operation-http-500-retries-safely ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((secret "retry-fence-secret")
          warnings)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       `((lease_id . "lease-1")
         (fencing_token . ,secret)
         (expires_at . 110.0)))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &optional _level _buffer-name)
                   (push message warnings))))
        (org-note-operation--heartbeat-finished
         '("workspace-1" "item-1" "execution") nil
         `(org-note-http-error
           (:status 500 :code "server_error" :message ,secret
            :retryable :json-false))))
      (let ((retry (car org-note-operation-test--scheduled)))
        (should (= (nth 0 retry) 5.0))
        (should (eq (nth 2 retry) #'org-note-operation--heartbeat-timer)))
      (should-not (string-match-p secret (car warnings))))))

(ert-deftest org-note-operation-heartbeat-dispatch-signal-does-not-stick ()
  (org-note-operation-test--with-lease-state (100.0)
    (org-note-operation-register-claim
     "workspace-1" "item-1" "document-1" "execution"
     '((lease_id . "lease-1")
       (fencing_token . "fence-1")
       (expires_at . 110.0)))
    (cl-letf (((symbol-function 'org-note-client-request-async)
               (lambda (&rest _) (error "setup failed")))
              ((symbol-function 'display-warning) #'ignore))
      (apply (nth 2 (car org-note-operation-test--scheduled))
             (nth 3 (car org-note-operation-test--scheduled))))
    (let ((lease
           (org-note-operation-find-lease
            "workspace-1" "item-1" "execution")))
      (should-not (org-note-operation-lease-heartbeat-p lease))
      (should (= (nth 0 (car org-note-operation-test--scheduled)) 5.0))
      (should
       (eq (nth 2 (car org-note-operation-test--scheduled))
           #'org-note-operation--heartbeat-timer)))))

(ert-deftest org-note-operation-context-dispatch-signal-does-not-stick ()
  (org-note-operation-test--with-lease-state (100.0)
    (org-note-operation-register-claim
     "workspace-1" "item-1" "document-1" "execution"
     '((lease_id . "lease-1")
       (fencing_token . "fence-1")
       (expires_at . 110.0)))
    (cl-letf (((symbol-function 'org-note-client-request-async)
               (lambda (&rest _) (error "setup failed")))
              ((symbol-function 'display-warning) #'ignore))
      (org-note-operation--refresh-lease-context
       '("workspace-1" "item-1" "execution")))
    (let ((lease
           (org-note-operation-find-lease
            "workspace-1" "item-1" "execution")))
      (should-not (org-note-operation-lease-heartbeat-p lease))
      (should (= (nth 0 (car org-note-operation-test--scheduled)) 5.0))
      (should
       (eq (nth 2 (car org-note-operation-test--scheduled))
           #'org-note-operation--heartbeat-timer)))))

(ert-deftest org-note-operation-explicit-heartbeat-rejects-overlap-safely ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((secret "overlap-fence-secret")
          called)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       `((lease_id . "lease-1")
         (fencing_token . ,secret)
         (expires_at . 200.0)))
      (setf (org-note-operation-lease-heartbeat-p
             (org-note-operation-find-lease
              "workspace-1" "item-1" "execution"))
            t)
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (&rest _) (setq called t))))
        (let ((error-data
               (should-error
                (org-note-operation-heartbeat
                 "workspace-1" "item-1" "lease-1" "execution" secret)
                :type 'org-note-error)))
          (should-not (string-match-p secret (prin1-to-string error-data)))))
      (should-not called))))

(ert-deftest org-note-operation-explicit-heartbeat-updates-authoritative-expiry ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((response '((expires_at . 300.0))))
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "review"
       '((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 200.0)))
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (&rest _) response)))
        (should
         (eq (org-note-operation-heartbeat
              "workspace-1" "item-1" "lease-1" "review" "fence-1")
             response)))
      (let ((lease
             (org-note-operation-find-lease
              "workspace-1" "item-1" "review")))
        (should (= (org-note-operation-lease-expires-at lease) 300.0))
        (should-not (org-note-operation-lease-heartbeat-p lease))
        (should (= (nth 0 (car org-note-operation-test--scheduled)) 120.0))))))

(ert-deftest org-note-operation-explicit-heartbeat-refreshes-context ()
  (org-note-operation-test--with-lease-state (100.0)
    (let (context-callback)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       '((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 200.0)))
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (&rest _) '((status . "ok"))))
                ((symbol-function 'org-note-client-request-async)
                 (lambda (method route query body callback)
                   (should
                    (equal (list method route query body)
                           '("GET" "/api/org/items/item-1/context"
                             ((workspace_id . "workspace-1")) nil)))
                   (setq context-callback callback))))
        (org-note-operation-heartbeat
         "workspace-1" "item-1" "lease-1" "execution" "fence-1"))
      (should context-callback)
      (should
       (org-note-operation-lease-heartbeat-p
        (org-note-operation-find-lease
         "workspace-1" "item-1" "execution")))
      (funcall
       context-callback
       '((lease . ((lease_id . "lease-1")
                   (kind . "execution")
                   (expires_at . 250.0))))
       nil)
      (let ((lease
             (org-note-operation-find-lease
              "workspace-1" "item-1" "execution")))
        (should (= (org-note-operation-lease-expires-at lease) 250.0))
        (should-not (org-note-operation-lease-heartbeat-p lease))))))

(ert-deftest org-note-operation-explicit-heartbeat-error-resolves-state ()
  (org-note-operation-test--with-lease-state (100.0)
    (org-note-operation-register-claim
     "workspace-1" "item-1" "document-1" "execution"
     '((lease_id . "lease-1")
       (fencing_token . "fence-1")
       (expires_at . 200.0)))
    (cl-letf (((symbol-function 'org-note-client-request)
               (lambda (&rest _)
                 (signal 'org-note-http-error
                         '((:status 400 :code "invalid"
                            :retryable :json-false)))))
              ((symbol-function 'display-warning) #'ignore))
      (should-error
       (org-note-operation-heartbeat
        "workspace-1" "item-1" "lease-1" "execution" "fence-1")
       :type 'org-note-http-error))
    (let ((lease
           (org-note-operation-find-lease
            "workspace-1" "item-1" "execution")))
      (should lease)
      (should-not (org-note-operation-lease-heartbeat-p lease))
      (should
       (eq (nth 2 (car org-note-operation-test--scheduled))
           #'org-note-operation--expiry-cleanup-timer)))))

(ert-deftest org-note-operation-heartbeat-schedules-and-sends-one-exact-request ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((org-note-actor-id "emacs:test@example")
          async-calls)
      (org-note-operation-register-claim
       "workspace-1" "item / one" "document-1" "execution"
       '((lease_id . "lease-1")
         (fencing_token . "fence-secret")
         (expires_at . 200.0)))
      (let ((schedule (car org-note-operation-test--scheduled)))
        (should (= (nth 0 schedule) 60.0))
        (should-not (nth 1 schedule))
        (should (eq (nth 2 schedule) #'org-note-operation--heartbeat-timer)))
      (cl-letf (((symbol-function 'org-note-client-new-operation-id)
                 (lambda () "heartbeat-operation"))
                ((symbol-function 'org-note-client-request-async)
                 (lambda (method route query body callback)
                   (push (list method route query body callback) async-calls))))
        (let ((schedule (car org-note-operation-test--scheduled)))
          (apply (nth 2 schedule) (nth 3 schedule)))
        (org-note-operation--heartbeat-timer
         '("workspace-1" "item / one" "execution")))
      (should (= (length async-calls) 1))
      (let ((call (car async-calls)))
        (should (equal (cl-subseq call 0 3)
                       '("POST" "/api/org/items/item%20%2F%20one/claim/heartbeat"
                         nil)))
        (org-note-operation-test--should-equal-json-object
         (nth 3 call)
         '((schema_version . 1)
           (actor_id . "emacs:test@example")
           (operation_id . "heartbeat-operation")
           (workspace_id . "workspace-1")
           (lease_id . "lease-1")
           (kind . "execution")
           (fencing_token . "fence-secret"))))
      (let ((lease
             (org-note-operation-find-lease
              "workspace-1" "item / one" "execution")))
        (should (org-note-operation-lease-heartbeat-p lease))
        (should-not (org-note-operation-lease-timer lease))))))

(ert-deftest org-note-operation-heartbeat-accepts-authoritative-expiry ()
  (org-note-operation-test--with-lease-state (100.0)
    (let (callback)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       '((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 200.0)))
      (cl-letf (((symbol-function 'org-note-client-request-async)
                 (lambda (_method _route _query _body supplied-callback)
                   (setq callback supplied-callback))))
        (apply (nth 2 (car org-note-operation-test--scheduled))
               (nth 3 (car org-note-operation-test--scheduled))))
      (funcall callback '((data . ((expires_at . 300.0)))) nil)
      (let ((lease
             (org-note-operation-find-lease
              "workspace-1" "item-1" "execution")))
        (should (= (org-note-operation-lease-expires-at lease) 300.0))
        (should-not (org-note-operation-lease-heartbeat-p lease))
        (should (= (nth 0 (car org-note-operation-test--scheduled)) 120.0))))))

(ert-deftest org-note-operation-heartbeat-refreshes-matching-context ()
  (org-note-operation-test--with-lease-state (100.0)
    (let (async-calls)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "review"
       '((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 200.0)))
      (cl-letf (((symbol-function 'org-note-client-request-async)
                 (lambda (method route query body callback)
                   (push (list method route query body callback) async-calls))))
        (apply (nth 2 (car org-note-operation-test--scheduled))
               (nth 3 (car org-note-operation-test--scheduled)))
        (funcall (nth 4 (car async-calls)) '((status . "ok")) nil)
        (org-note-operation--refresh-lease-context
         '("workspace-1" "item-1" "review")))
      (should (= (length async-calls) 2))
      (let ((context-call (car async-calls)))
        (should
         (equal (cl-subseq context-call 0 4)
                '("GET" "/api/org/items/item-1/context"
                  ((workspace_id . "workspace-1")) nil)))
        (funcall
         (nth 4 context-call)
         '((lease . ((lease_id . "lease-1")
                     (kind . "review")
                     (expires_at . 250.0))))
         nil))
      (let ((lease
             (org-note-operation-find-lease
              "workspace-1" "item-1" "review")))
        (should (= (org-note-operation-lease-expires-at lease) 250.0))
        (should-not (org-note-operation-lease-heartbeat-p lease))
        (should (= (nth 0 (car org-note-operation-test--scheduled)) 90.0))))))

(ert-deftest org-note-operation-context-rejects-a-mismatched-lease ()
  (org-note-operation-test--with-lease-state (100.0)
    (let (callback)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "review"
       '((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 200.0)))
      (cl-letf (((symbol-function 'org-note-client-request-async)
                 (lambda (_method _route _query _body supplied-callback)
                   (setq callback supplied-callback))))
        (org-note-operation--refresh-lease-context
         '("workspace-1" "item-1" "review")))
      (funcall
       callback
       '((lease . ((lease_id . "different-lease")
                   (kind . "review")
                   (expires_at . 250.0))))
       nil)
      (should-not
       (org-note-operation-find-lease "workspace-1" "item-1" "review")))))

(ert-deftest org-note-operation-heartbeat-forgets-stale-and-expired-leases ()
  (org-note-operation-test--with-lease-state (100.0)
    (org-note-operation-register-claim
     "workspace-1" "item-stale" "document-1" "execution"
     '((lease_id . "lease-stale")
       (fencing_token . "fence-stale")
       (expires_at . 200.0)))
    (org-note-operation--heartbeat-finished
     '("workspace-1" "item-stale" "execution") nil
     '(org-note-http-error (:status 409 :code "stale_lease")))
    (should-not
     (org-note-operation-find-lease "workspace-1" "item-stale" "execution"))
    (org-note-operation-register-claim
     "workspace-1" "item-expired" "document-1" "execution"
     '((lease_id . "lease-expired")
       (fencing_token . "fence-expired")
       (expires_at . 110.0)))
    (setq org-note-operation-test--now 110.0)
    (org-note-operation--heartbeat-finished
     '("workspace-1" "item-expired" "execution") nil
     '(org-note-transport-error
       (:status nil :code nil :message "Request failed")))
    (should-not
     (org-note-operation-find-lease "workspace-1" "item-expired" "execution"))))

(ert-deftest org-note-operation-heartbeat-transient-errors-retry-safely ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((secret "fence-must-not-be-warned")
          warnings)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       `((lease_id . "lease-1")
         (fencing_token . ,secret)
         (expires_at . 110.0)))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &optional _level _buffer-name)
                   (push message warnings))))
        (org-note-operation--heartbeat-finished
         '("workspace-1" "item-1" "execution") nil
         `(org-note-http-error
           (:status 503 :code "busy" :message ,secret))))
      (should (= (nth 0 (car org-note-operation-test--scheduled)) 5.0))
      (should (= (length warnings) 1))
      (should-not (string-match-p secret (car warnings)))
      (let ((lease
             (org-note-operation-find-lease
              "workspace-1" "item-1" "execution")))
        (should-not (org-note-operation-lease-heartbeat-p lease))))))

(ert-deftest org-note-operation-context-errors-do-not-overlap-and-retry ()
  (org-note-operation-test--with-lease-state (100.0)
    (let (async-calls warnings)
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       '((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 110.0)))
      (cl-letf (((symbol-function 'org-note-client-request-async)
                 (lambda (method route query body callback)
                   (push (list method route query body callback) async-calls)))
                ((symbol-function 'display-warning)
                 (lambda (_type message &optional _level _buffer-name)
                   (push message warnings))))
        (org-note-operation--refresh-lease-context
         '("workspace-1" "item-1" "execution"))
        (org-note-operation--refresh-lease-context
         '("workspace-1" "item-1" "execution"))
        (should (= (length async-calls) 1))
        (funcall
         (nth 4 (car async-calls)) nil
         '(org-note-transport-error
           (:status nil :code nil :message "Request failed"))))
      (should (= (nth 0 (car org-note-operation-test--scheduled)) 5.0))
      (should (= (length warnings) 1))
      (let ((lease
             (org-note-operation-find-lease
              "workspace-1" "item-1" "execution")))
        (should-not (org-note-operation-lease-heartbeat-p lease))))))

(ert-deftest org-note-operation-mutation-body-adds-common-envelope ()
  (let* ((org-note-actor-id "emacs:test@example")
         (body (org-note-operation--mutation-body
                "workspace-1" '((path . "notes/today.org")) "operation-1")))
    (should (= (alist-get 'schema_version body) 1))
    (should (equal (alist-get 'actor_id body) "emacs:test@example"))
    (should (equal (alist-get 'operation_id body) "operation-1"))
    (should (equal (alist-get 'workspace_id body) "workspace-1"))
    (should (equal (alist-get 'path body) "notes/today.org"))))

(ert-deftest org-note-operation-mutation-body-generates-operation-id ()
  (let ((body (org-note-operation--mutation-body "workspace-1" nil)))
    (should (stringp (alist-get 'operation_id body)))))

(ert-deftest org-note-operation-lists-workspaces ()
  (let ((request
         (org-note-operation-test--capture-request
          (org-note-operation-list-workspaces
           :cursor "opaque+cursor" :limit 10 :include-archived t))))
    (should (equal request
                   '("GET" "/api/org/workspaces"
                           ((cursor . "opaque+cursor")
                            (limit . 10)
                            (include_archived . t))
                           nil)))))

(ert-deftest org-note-operation-gets-workspace ()
  (let ((request
         (org-note-operation-test--capture-request
          (org-note-operation-get-workspace "space / one"))))
    (should (equal request
                   '("GET" "/api/org/workspaces/space%20%2F%20one" nil nil)))))

(ert-deftest org-note-operation-lists-documents ()
  (let ((request
         (org-note-operation-test--capture-request
          (org-note-operation-list-documents
           "space / one" :cursor "next" :limit 25 :include-archived :json-false))))
    (should (equal request
                   '("GET" "/api/org/workspaces/space%20%2F%20one/documents"
                           ((cursor . "next")
                            (limit . 25)
                            (include_archived . :json-false))
                           nil)))))

(ert-deftest org-note-operation-gets-document ()
  (let ((request
         (org-note-operation-test--capture-request
          (org-note-operation-get-document "workspace / one" "document / one"))))
    (should (equal request
                   '("GET" "/api/org/documents/document%20%2F%20one"
                           ((workspace_id . "workspace / one")) nil)))))

(ert-deftest org-note-operation-puts-document-with-supplied-operation-id ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-put-document
            "workspace-1" "document / one" "notes/today.org" "* Today"
            3 '((lease . "proof")) :operation-id "operation-1"))))
    (should (equal (cl-subseq request 0 3)
                   '("PUT" "/api/org/documents/document%20%2F%20one" nil)))
    (let ((body (nth 3 request)))
      (org-note-operation-test--should-equal-json-object
       body
       '((schema_version . 1)
         (actor_id . "emacs:test@example")
         (operation_id . "operation-1")
         (workspace_id . "workspace-1")
         (path . "notes/today.org")
         (source . "* Today")
         (expected_revision . 3)
         (lease_proofs . ((lease . "proof"))))))))

(ert-deftest org-note-operation-puts-document-omits-nil-expected-revision ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-put-document
            "workspace-1" "document-1" "notes/today.org" "* Today" nil nil)))
         (body (nth 3 request))
         (operation-id (alist-get 'operation_id body)))
    (should (stringp operation-id))
    (should-not (assq 'expected_revision body))
    (org-note-operation-test--should-equal-json-object
     body
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . ,operation-id)
       (workspace_id . "workspace-1")
       (path . "notes/today.org")
       (source . "* Today")
       (lease_proofs . ,(org-note-client-empty-object))))))

(ert-deftest org-note-operation-create-document-omits-expected-revision ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-create-document
            "workspace-1" "document-new" "notes/new.org" ""
            :operation-id "operation-create")))
         (body (nth 3 request)))
    (should (equal (cl-subseq request 0 3)
                   '("PUT" "/api/org/documents/document-new" nil)))
    (should-not (assq 'expected_revision body))
    (org-note-operation-test--should-equal-json-object
     body
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-create")
       (workspace_id . "workspace-1")
       (path . "notes/new.org")
       (source . "")
       (lease_proofs . ,(org-note-client-empty-object))))))

(ert-deftest org-note-operation-archives-document ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-archive-document
            "workspace-1" "document-1" 4 :operation-id "operation-archive"))))
    (should (equal (cl-subseq request 0 3)
                   '("POST" "/api/org/documents/document-1/archive" nil)))
    (org-note-operation-test--should-equal-json-object
     (nth 3 request)
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-archive")
       (workspace_id . "workspace-1")
       (expected_revision . 4)))))

(ert-deftest org-note-operation-restores-document ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-restore-document
            "workspace-1" "document-1" 5 :operation-id "operation-restore"))))
    (should (equal (cl-subseq request 0 3)
                   '("POST" "/api/org/documents/document-1/restore" nil)))
    (org-note-operation-test--should-equal-json-object
     (nth 3 request)
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-restore")
       (workspace_id . "workspace-1")
       (expected_revision . 5)))))

(ert-deftest org-note-operation-renames-document-path ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-rename-document-path
            "workspace-1" "document-1" 6 "notes/renamed.org"
            :operation-id "operation-rename"))))
    (should (equal (cl-subseq request 0 3)
                   '("PATCH" "/api/org/documents/document-1/path" nil)))
    (org-note-operation-test--should-equal-json-object
     (nth 3 request)
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-rename")
       (workspace_id . "workspace-1")
       (expected_revision . 6)
       (new_path . "notes/renamed.org")))))

(ert-deftest org-note-operation-queries-queue-with-all-filters ()
  (let ((request
         (org-note-operation-test--capture-request
          (org-note-operation-query-queue
           :workspace-ids '("workspace-1" "workspace 2")
           :view 'assigned :item-type 'task :state 'open :priority "high"
           :tags '("work" "urgent") :assignee "user-1"
           :scheduled-from "2026-08-01" :scheduled-to "2026-08-02"
           :deadline-from "2026-08-03" :deadline-to "2026-08-04"
           :completed-from "2026-08-05" :completed-to "2026-08-06"
           :from "2026-08-07" :to "2026-08-08" :include-archived t
           :cursor "opaque+/=" :limit 50))))
    (should (equal request
                   '("GET" "/api/org/queue"
                           ((workspace_ids . "workspace-1,workspace 2")
                            (view . "assigned")
                            (item_type . task)
                            (state . open)
                            (priority . "high")
                            (tags . "work,urgent")
                            (assignee . "user-1")
                            (scheduled_from . "2026-08-01")
                            (scheduled_to . "2026-08-02")
                            (deadline_from . "2026-08-03")
                            (deadline_to . "2026-08-04")
                            (completed_from . "2026-08-05")
                            (completed_to . "2026-08-06")
                            (from . "2026-08-07")
                            (to . "2026-08-08")
                            (include_archived . t)
                            (cursor . "opaque+/=")
                            (limit . 50))
                           nil)))))

(ert-deftest org-note-operation-rejects-an-invalid-queue-view-before-request ()
  (let ((called nil))
    (cl-letf (((symbol-function 'org-note-client-request)
               (lambda (&rest _)
                 (setq called t))))
      (should-error
       (org-note-operation-query-queue
        :workspace-ids '("workspace-1") :view 'unknown)
       :type 'user-error))
    (should-not called)))

(ert-deftest org-note-operation-rejects-invalid-queue-workspace-ids ()
  (dolist (workspace-ids (list nil '() [] '("") '("workspace-1" 1)))
    (let ((called nil))
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (&rest _)
                   (setq called t))))
        (should-error
         (org-note-operation-query-queue
          :workspace-ids workspace-ids :view 'ready)
         :type 'user-error))
      (should-not called))))

(ert-deftest org-note-operation-rejects-invalid-agenda-workspace-ids ()
  (dolist (workspace-ids (list nil '() [] '("") '("workspace-1" 1)))
    (let ((called nil))
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (&rest _)
                   (setq called t))))
        (should-error
         (org-note-operation-query-agenda
          :workspace-ids workspace-ids :view 'scheduled)
         :type 'user-error))
      (should-not called))))

(ert-deftest org-note-operation-does-not-intern-invalid-view-strings ()
  (let* ((view (format "org-note-invalid-view-%s" (float-time)))
         (called nil))
    (should-not (intern-soft view))
    (cl-letf (((symbol-function 'org-note-client-request)
               (lambda (&rest _)
                 (setq called t))))
      (should-error
       (org-note-operation-query-queue
        :workspace-ids '("workspace-1") :view view)
       :type 'user-error))
    (should-not called)
    (should-not (intern-soft view))))

(ert-deftest org-note-operation-queries-agenda ()
  (let ((request
         (org-note-operation-test--capture-request
          (org-note-operation-query-agenda
           :workspace-ids '("workspace-1" "workspace-2") :view 'upcoming_deadline
           :cursor "opaque" :limit 5))))
    (should (equal request
                   '("GET" "/api/org/agenda"
                           ((workspace_ids . "workspace-1,workspace-2")
                            (view . "upcoming_deadline")
                            (item_type . nil)
                            (state . nil)
                            (priority . nil)
                            (tags . nil)
                            (assignee . nil)
                            (scheduled_from . nil)
                            (scheduled_to . nil)
                            (deadline_from . nil)
                            (deadline_to . nil)
                            (completed_from . nil)
                            (completed_to . nil)
                            (from . nil)
                            (to . nil)
                            (include_archived . nil)
                            (cursor . "opaque")
                            (limit . 5))
                           nil)))))

(ert-deftest org-note-operation-gets-item-context ()
  (let ((request
         (org-note-operation-test--capture-request
          (org-note-operation-get-item-context "workspace / one" "item / one"))))
    (should (equal request
                   '("GET" "/api/org/items/item%20%2F%20one/context"
                           ((workspace_id . "workspace / one")) nil)))))

(ert-deftest org-note-operation-lists-events-with-filters ()
  (let ((request
         (org-note-operation-test--capture-request
          (org-note-operation-list-events
           "workspace / one" :subject-kind 'document :subject-id "document-1"
           :cursor "opaque" :limit 10))))
    (should (equal request
                   '("GET" "/api/org/workspaces/workspace%20%2F%20one/events"
                           ((subject_kind . document)
                            (subject_id . "document-1")
                            (cursor . "opaque")
                           (limit . 10))
                           nil)))))

(defun org-note-operation-test--should-mutation-request
    (request method route expected-body)
  "Assert REQUEST has METHOD, ROUTE, no query, and EXPECTED-BODY."
  (should (equal (cl-subseq request 0 3) (list method route nil)))
  (org-note-operation-test--should-equal-json-object
   (nth 3 request) expected-body))

(ert-deftest org-note-operation-claims-item-with-supplied-operation-id ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((org-note-actor-id "emacs:test@example")
          request)
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (method route query body)
                   (setq request (list method route query body))
                   (org-note-operation-test--claim-response
                    "workspace-1" "item / one" "document-1" "review"
                    "operation-1"))))
        (org-note-operation-claim
         "workspace-1" "item / one" "document-1" 3 "review"
         :operation-id "operation-1"))
      (org-note-operation-test--should-mutation-request
       request "POST" "/api/org/items/item%20%2F%20one/claim"
       `((schema_version . 1)
         (actor_id . "emacs:test@example")
         (operation_id . "operation-1")
         (workspace_id . "workspace-1")
         (document_id . "document-1")
         (expected_document_revision . 3)
         (kind . "review"))))))

(ert-deftest org-note-operation-heartbeats-claim ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-heartbeat
            "workspace-1" "item-1" "lease-1" "task" "fence-4"
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/claim/heartbeat"
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (lease_id . "lease-1")
       (kind . "task")
       (fencing_token . "fence-4")))))

(ert-deftest org-note-operation-releases-claim-without-target-state ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-release
            "workspace-1" "item-1" "document-1" 3 "lease-1" "task" "fence-4"
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/claim/release"
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (kind . "task")
       (fencing_token . "fence-4")))))

(ert-deftest org-note-operation-releases-claim-with-target-state ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-release
            "workspace-1" "item-1" "document-1" 3 "lease-1" "task" "fence-4"
            :target-state "ready" :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/claim/release"
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (kind . "task")
       (fencing_token . "fence-4")
       (target_state . "ready")))))

(ert-deftest org-note-operation-reports-progress-with-empty-metadata ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-report-progress
            "workspace-1" "item-1" "lease-1" "task" "fence-4" "Half complete"
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/progress"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (lease_id . "lease-1")
       (kind . "task")
       (fencing_token . "fence-4")
       (summary . "Half complete")
       (metadata . ,(org-note-client-empty-object))))))

(ert-deftest org-note-operation-submits-result-with-empty-collections ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-submit-result
            "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-4" "Complete"
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/result"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (fencing_token . "fence-4")
       (result_summary . "Complete")
       (note_refs . [])
       (artifacts . [])
       (metadata . ,(org-note-client-empty-object))))))

(ert-deftest org-note-operation-transitions-item-with-optional-fields ()
  (let* ((org-note-actor-id "emacs:test@example")
         (lease (org-note-operation-test--lease "lease-1" "fence-4"))
         (metadata '((source . "agenda")))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-transition
            "workspace-1" "item-1" "document-1" 3 "blocked"
            :lease lease :error "Waiting" :metadata metadata
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/transition"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (target_state . "blocked")
       (lease . ,lease)
       (error . "Waiting")
       (metadata . ((source . "agenda")))))))

(ert-deftest org-note-operation-transitions-item-omits-nil-optional-fields ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-transition
            "workspace-1" "item-1" "document-1" 3 "ready"
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/transition"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (target_state . "ready")
       (metadata . ,(org-note-client-empty-object))))))

(ert-deftest org-note-operation-transition-removes-confirmed-closed-lease ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((org-note-actor-id "emacs:test@example"))
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       '((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 200.0)))
      (let* ((lease
              (org-note-operation-find-lease
               "workspace-1" "item-1" "execution"))
             (timer (org-note-operation-lease-timer lease))
             (proof '((lease_id . "lease-1")
                      (kind . "execution")
                      (fencing_token . "fence-1")))
             (response
              (org-note-operation-test--transition-response
               "workspace-1" "item-1" "document-1" "transition-1" nil)))
        (cl-letf (((symbol-function 'org-note-client-request)
                   (lambda (&rest _arguments) response)))
          (should
           (eq response
               (org-note-operation-transition
                "workspace-1" "item-1" "document-1" 3 "blocked"
                :lease proof :operation-id "transition-1"))))
        (should (member timer org-note-operation-test--cancelled))
        (should-not
         (org-note-operation-find-lease
          "workspace-1" "item-1" "execution"))))))

(ert-deftest org-note-operation-transition-retains-confirmed-live-lease ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((org-note-actor-id "emacs:test@example"))
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       '((lease_id . "lease-1")
         (fencing_token . "fence-1")
         (expires_at . 200.0)))
      (let* ((lease
              (org-note-operation-find-lease
               "workspace-1" "item-1" "execution"))
             (timer (org-note-operation-lease-timer lease))
             (proof '((lease_id . "lease-1")
                      (kind . "execution")
                      (fencing_token . "fence-1")))
             (context-lease
              (org-note-operation-test--active-context-lease
               "workspace-1" "item-1" "lease-1" "execution" 260))
             (response
              (org-note-operation-test--transition-response
               "workspace-1" "item-1" "document-1" "transition-1"
               context-lease)))
        (cl-letf (((symbol-function 'org-note-client-request)
                   (lambda (&rest _arguments) response)))
          (org-note-operation-transition
           "workspace-1" "item-1" "document-1" 3 "running"
           :lease proof :operation-id "transition-1"))
        (let ((current
               (org-note-operation-find-lease
                "workspace-1" "item-1" "execution")))
          (should (eq current lease))
          (should (= (org-note-operation-lease-expires-at current) 260))
          (should (member timer org-note-operation-test--cancelled))
          (should (org-note-operation-lease-timer current))
          (should-not
           (eq timer (org-note-operation-lease-timer current))))))))

(ert-deftest org-note-operation-transition-never-removes-replacement-lease ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((org-note-actor-id "emacs:test@example"))
      (org-note-operation-register-claim
       "workspace-1" "item-1" "document-1" "execution"
       '((lease_id . "lease-old")
         (fencing_token . "fence-old")
         (expires_at . 200.0)))
      (let* ((proof '((lease_id . "lease-old")
                      (kind . "execution")
                      (fencing_token . "fence-old")))
             (response
              (org-note-operation-test--transition-response
               "workspace-1" "item-1" "document-1" "transition-1" nil)))
        (cl-letf (((symbol-function 'org-note-client-request)
                   (lambda (&rest _arguments)
                     (org-note-operation-register-claim
                      "workspace-1" "item-1" "document-1" "execution"
                      '((lease_id . "lease-new")
                        (fencing_token . "fence-new")
                        (expires_at . 300.0)))
                     response)))
          (org-note-operation-transition
           "workspace-1" "item-1" "document-1" 3 "blocked"
           :lease proof :operation-id "transition-1"))
        (let ((replacement
               (org-note-operation-find-lease
                "workspace-1" "item-1" "execution")))
          (should (equal (org-note-operation-lease-lease-id replacement)
                         "lease-new"))
          (should (= (org-note-operation-lease-expires-at replacement) 300.0))
          (should (org-note-operation-lease-timer replacement)))))))

(ert-deftest org-note-operation-retries-item ()
  (org-note-operation-test--with-lease-state (100.0)
    (let ((org-note-actor-id "emacs:test@example")
          request)
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (method route query body)
                   (setq request (list method route query body))
                   (org-note-operation-test--claim-response
                    "workspace-1" "item-1" "document-1" "execution"
                    "operation-1"))))
        (org-note-operation-retry
         "workspace-1" "item-1" "document-1" 3
         :operation-id "operation-1"))
      (org-note-operation-test--should-mutation-request
       request "POST" "/api/org/items/item-1/retry"
       '((schema_version . 1)
         (actor_id . "emacs:test@example")
         (operation_id . "operation-1")
         (workspace_id . "workspace-1")
         (document_id . "document-1")
         (expected_document_revision . 3))))))

(ert-deftest org-note-operation-requests-review-with-empty-collections ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-request-review
            "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-4"
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/review/request"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (fencing_token . "fence-4")
       (note_refs . [])
       (artifacts . [])
       (metadata . ,(org-note-client-empty-object))))))

(ert-deftest org-note-operation-approves-review ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-approve-review
            "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-4"
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/review/approve"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (fencing_token . "fence-4")
       (metadata . ,(org-note-client-empty-object))))))

(ert-deftest org-note-operation-rejects-review ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-reject-review
            "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-4" "Revise"
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/review/reject"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (fencing_token . "fence-4")
       (reason . "Revise")
       (metadata . ,(org-note-client-empty-object))))))

(ert-deftest org-note-operation-adds-dependency-with-lease ()
  (let* ((org-note-actor-id "emacs:test@example")
         (expected-revisions
          (org-note-operation-test--revision-map "document-1" 3))
         (lease (org-note-operation-test--lease "lease-1" "fence-4"))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-add-dependency
            "workspace-1" "item-1" "dependency-1" "document-1" expected-revisions
            :lease lease :operation-id "operation-1"))))
    (should (eq (alist-get 'expected_revisions (nth 3 request))
                expected-revisions))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/dependencies"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (dependency_id . "dependency-1")
       (document_id . "document-1")
       (expected_revisions . ,expected-revisions)
       (lease . ,lease)))))

(ert-deftest org-note-operation-removes-dependency-with-delete-body ()
  (let* ((org-note-actor-id "emacs:test@example")
         (expected-revisions
          (org-note-operation-test--revision-map "document-1" 3))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-remove-dependency
            "workspace-1" "item / one" "dependency / one" "document-1"
            expected-revisions
            :operation-id "operation-1"))))
    (should (eq (alist-get 'expected_revisions (nth 3 request))
                expected-revisions))
    (org-note-operation-test--should-mutation-request
     request "DELETE" "/api/org/items/item%20%2F%20one/dependencies/dependency%20%2F%20one"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_revisions . ,expected-revisions)))))

(ert-deftest org-note-operation-links-note-with-lease ()
  (let* ((org-note-actor-id "emacs:test@example")
         (expected-revisions
          (org-note-operation-test--revision-map "document-1" 3))
         (lease (org-note-operation-test--lease "lease-1" "fence-4"))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-link-note
            "workspace-1" "item-1" "document-1" "reference" "note-1"
            "Supporting note" expected-revisions :lease lease
            :operation-id "operation-1"))))
    (should (eq (alist-get 'expected_revisions (nth 3 request))
                expected-revisions))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/note-links"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (purpose . "reference")
       (note_id . "note-1")
       (description . "Supporting note")
       (expected_revisions . ,expected-revisions)
       (lease . ,lease)))))

(ert-deftest org-note-operation-unlinks-note-with-delete-body ()
  (let* ((org-note-actor-id "emacs:test@example")
         (expected-revisions
          (org-note-operation-test--revision-map "document-1" 3))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-unlink-note
            "workspace-1" "item-1" "document-1" "reference" "note-1"
            expected-revisions
            :operation-id "operation-1"))))
    (should (eq (alist-get 'expected_revisions (nth 3 request))
                expected-revisions))
    (org-note-operation-test--should-mutation-request
     request "DELETE" "/api/org/items/item-1/note-links"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (purpose . "reference")
       (note_id . "note-1")
       (expected_revisions . ,expected-revisions)))))

(ert-deftest org-note-operation-reports-progress-with-supplied-metadata ()
  (let* ((org-note-actor-id "emacs:test@example")
         (metadata '((completed_steps . 2) (source . "agent")))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-report-progress
            "workspace-1" "item-1" "lease-1" "task" "fence-4" "Half complete"
            :metadata metadata :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/progress"
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (lease_id . "lease-1")
       (kind . "task")
       (fencing_token . "fence-4")
       (summary . "Half complete")
       (metadata . ((completed_steps . 2) (source . "agent")))))))

(ert-deftest org-note-operation-submits-result-with-supplied-json-values ()
  (let* ((org-note-actor-id "emacs:test@example")
         (note-refs ["note-1" "note-2"])
         (artifacts [((path . "report.txt"))])
         (metadata '((duration_seconds . 42)))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-submit-result
            "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-4" "Complete"
            :note-refs note-refs :artifacts artifacts :metadata metadata
            :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/result"
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (fencing_token . "fence-4")
       (result_summary . "Complete")
       (note_refs . ["note-1" "note-2"])
       (artifacts . [((path . "report.txt"))])
       (metadata . ((duration_seconds . 42)))))))

(ert-deftest org-note-operation-requests-review-with-supplied-json-values ()
  (let* ((org-note-actor-id "emacs:test@example")
         (note-refs ["note-1"])
         (artifacts [((path . "review.txt"))])
         (metadata '((reviewer . "editor")))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-request-review
            "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-4"
            :result-summary "Ready for review" :note-refs note-refs
            :artifacts artifacts :metadata metadata :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/review/request"
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (fencing_token . "fence-4")
       (result_summary . "Ready for review")
       (note_refs . ["note-1"])
       (artifacts . [((path . "review.txt"))])
       (metadata . ((reviewer . "editor")))))))

(ert-deftest org-note-operation-approves-review-with-supplied-metadata ()
  (let* ((org-note-actor-id "emacs:test@example")
         (metadata '((approved_by . "editor")))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-approve-review
            "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-4"
            :metadata metadata :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/review/approve"
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (fencing_token . "fence-4")
       (metadata . ((approved_by . "editor")))))))

(ert-deftest org-note-operation-rejects-review-with-supplied-metadata ()
  (let* ((org-note-actor-id "emacs:test@example")
         (metadata '((requested_changes . ["Add tests"])))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-reject-review
            "workspace-1" "item-1" "document-1" 3 "lease-1" "fence-4" "Revise"
            :metadata metadata :operation-id "operation-1"))))
    (org-note-operation-test--should-mutation-request
     request "POST" "/api/org/items/item-1/review/reject"
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_document_revision . 3)
       (lease_id . "lease-1")
       (fencing_token . "fence-4")
       (reason . "Revise")
       (metadata . ((requested_changes . ["Add tests"])))))))

(ert-deftest org-note-operation-rejects-non-string-fencing-tokens-before-request ()
  (let ((called nil)
        (fencing-token 987654321))
    (cl-letf (((symbol-function 'org-note-client-request)
               (lambda (&rest _)
                 (setq called t))))
      (dolist (operation
               (list
                (lambda ()
                  (org-note-operation-heartbeat
                   "workspace-1" "item-1" "lease-1" "task" fencing-token))
                (lambda ()
                  (org-note-operation-release
                   "workspace-1" "item-1" "document-1" 3 "lease-1" "task"
                   fencing-token))
                (lambda ()
                  (org-note-operation-report-progress
                   "workspace-1" "item-1" "lease-1" "task" fencing-token
                   "Half complete"))
                (lambda ()
                  (org-note-operation-submit-result
                   "workspace-1" "item-1" "document-1" 3 "lease-1" fencing-token
                   "Complete"))
                (lambda ()
                  (org-note-operation-request-review
                   "workspace-1" "item-1" "document-1" 3 "lease-1" fencing-token))
                (lambda ()
                  (org-note-operation-approve-review
                   "workspace-1" "item-1" "document-1" 3 "lease-1" fencing-token))
                (lambda ()
                  (org-note-operation-reject-review
                   "workspace-1" "item-1" "document-1" 3 "lease-1" fencing-token
                   "Revise"))))
        (let ((error-data
               (should-error (funcall operation) :type 'org-note-error)))
          (should (equal error-data
                         '(org-note-error "Org Note fencing token must be a string")))
          (should (equal (error-message-string error-data)
                         "Org Note error: \"Org Note fencing token must be a string\""))
          (should-not (string-match-p (number-to-string fencing-token)
                                      (prin1-to-string error-data)))
          (should-not (string-match-p (number-to-string fencing-token)
                                      (error-message-string error-data)))))
    (should-not called))))

(ert-deftest org-note-operation-removes-dependency-with-supplied-lease ()
  (let* ((org-note-actor-id "emacs:test@example")
         (expected-revisions
          (org-note-operation-test--revision-map "document-1" 3))
         (lease (org-note-operation-test--lease "lease-1" "fence-4"))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-remove-dependency
            "workspace-1" "item-1" "dependency-1" "document-1" expected-revisions
            :lease lease :operation-id "operation-1"))))
    (should (eq (alist-get 'expected_revisions (nth 3 request))
                expected-revisions))
    (should (eq (alist-get 'lease (nth 3 request)) lease))
    (org-note-operation-test--should-mutation-request
     request "DELETE" "/api/org/items/item-1/dependencies/dependency-1"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (expected_revisions . ,expected-revisions)
       (lease . ,lease)))))

(ert-deftest org-note-operation-unlinks-note-with-supplied-lease ()
  (let* ((org-note-actor-id "emacs:test@example")
         (expected-revisions
          (org-note-operation-test--revision-map "document-1" 3))
         (lease (org-note-operation-test--lease "lease-1" "fence-4"))
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-unlink-note
            "workspace-1" "item-1" "document-1" "reference" "note-1"
            expected-revisions :lease lease :operation-id "operation-1"))))
    (should (eq (alist-get 'expected_revisions (nth 3 request))
                expected-revisions))
    (should (eq (alist-get 'lease (nth 3 request)) lease))
    (org-note-operation-test--should-mutation-request
     request "DELETE" "/api/org/items/item-1/note-links"
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-1")
       (workspace_id . "workspace-1")
       (document_id . "document-1")
       (purpose . "reference")
       (note_id . "note-1")
       (expected_revisions . ,expected-revisions)
       (lease . ,lease)))))

(ert-deftest org-note-operation-serializes-mutation-strings-at-wire-level ()
  (let* ((expected-revisions
          (org-note-operation-test--revision-map "document-1" 3))
         (lease (org-note-operation-test--lease "lease-1" "fence-4"))
         (requests nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _)
                 (push (list url-request-method url-request-data) requests)
                 (org-note-operation-test--response-buffer 204 ""))))
      (org-note-operation-heartbeat
       "workspace-1" "item-1" "lease-1" "task" "fence-4"
       :operation-id "operation-1")
      (org-note-operation-transition
       "workspace-1" "item-1" "document-1" 3 "ready"
       :operation-id "operation-2")
      (org-note-operation-link-note
       "workspace-1" "item-1" "document-1" "reference" "note-1"
       "Supporting note" expected-revisions :lease lease
       :operation-id "operation-3"))
    (setq requests (nreverse requests))
    (should (= (length requests) 3))
    (let* ((heartbeat-body
            (json-parse-string
             (decode-coding-string (nth 1 (nth 0 requests)) 'utf-8)
             :object-type 'hash-table :array-type 'array))
           (transition-body
            (json-parse-string
             (decode-coding-string (nth 1 (nth 1 requests)) 'utf-8)
             :object-type 'hash-table :array-type 'array))
           (link-body
            (json-parse-string
             (decode-coding-string (nth 1 (nth 2 requests)) 'utf-8)
             :object-type 'hash-table :array-type 'array))
           (revisions (gethash "expected_revisions" link-body))
           (lease-body (gethash "lease" link-body)))
      (should (equal (nth 0 (nth 0 requests)) "POST"))
      (should (equal (nth 0 (nth 1 requests)) "POST"))
      (should (equal (nth 0 (nth 2 requests)) "POST"))
      (should (equal (gethash "kind" heartbeat-body) "task"))
      (should (equal (gethash "fencing_token" heartbeat-body) "fence-4"))
      (should (equal (gethash "target_state" transition-body) "ready"))
      (should (equal (gethash "purpose" link-body) "reference"))
      (should (hash-table-p revisions))
      (should (equal (gethash "document-1" revisions) 3))
      (should (hash-table-p lease-body))
      (should (equal (gethash "fencing_token" lease-body) "fence-4")))))

;;; org-note-operation-test.el ends here
