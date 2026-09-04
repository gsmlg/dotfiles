;;; org-note-operation.el --- Org Note operation wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Operation-level wrappers for the Org Note HTTP API.

;;; Code:

(require 'cl-lib)
(require 'org-note-client)
(require 'org-note-validation)

(defconst org-note-operation-queue-views
  '(ready assigned running blocked review failed expired_lease completed)
  "Supported queue view names.")

(defconst org-note-operation-agenda-views
  '(scheduled upcoming_deadline)
  "Supported agenda view names.")

(cl-defstruct (org-note-operation-lease
               (:constructor org-note-operation--make-lease))
  "An active Org Note claim lease."
  workspace-id item-id document-id kind lease-id fencing-token expires-at timer heartbeat-p)

(defvar org-note-operation--leases (make-hash-table :test #'equal)
  "Active leases keyed by workspace, item, and claim kind.")

(defun org-note-operation--lease-key (workspace-id item-id kind)
  "Return the registry key for WORKSPACE-ID, ITEM-ID, and KIND."
  (list workspace-id item-id kind))

(defun org-note-operation--response-entry (response key)
  "Return the KEY entry from a symbol-keyed alist RESPONSE."
  (and (listp response)
       (cl-find-if (lambda (entry)
                     (and (consp entry) (eq (car entry) key)))
                   response)))

(defun org-note-operation--response-value (response key)
  "Return KEY's value from the symbol-keyed alist RESPONSE."
  (cdr (org-note-operation--response-entry response key)))

(defun org-note-operation--nonempty-string-p (value)
  "Return non-nil when VALUE is a nonempty string."
  (and (stringp value) (> (length value) 0)))

(defun org-note-operation--json-object-p (value)
  "Return non-nil when VALUE is a nonempty alist JSON object."
  (and (consp value) (cl-every #'consp value)))

(defun org-note-operation--string-vector-p (value)
  "Return non-nil when VALUE is a vector of nonempty strings."
  (and (vectorp value)
       (cl-every #'org-note-operation--nonempty-string-p
                 (append value nil))))

(defun org-note-operation--claim-response-valid-p
    (response workspace-id item-id document-id kind operation-id)
  "Return non-nil when RESPONSE is the expected claim result.

WORKSPACE-ID, ITEM-ID, DOCUMENT-ID, KIND, and OPERATION-ID identify the
request whose response is being checked."
  (let* ((context (org-note-operation--response-value response 'context))
         (workspace (org-note-operation--response-value context 'workspace))
         (document (org-note-operation--response-value context 'document))
         (item (org-note-operation--response-value context 'item))
         (lease (org-note-operation--response-value context 'lease))
         (lease-id (org-note-operation--response-value response 'lease_id))
         (fencing-token
          (org-note-operation--response-value response 'fencing_token))
         (expires-at (org-note-operation--response-value response 'expires_at)))
    (and (org-note-operation--json-object-p response)
         (equal (org-note-operation--response-value response 'schema_version) 1)
         (equal (org-note-operation--response-value response 'workspace_id)
                workspace-id)
         (org-note-operation--nonempty-string-p operation-id)
         (equal (org-note-operation--response-value response 'operation_id)
                operation-id)
         (org-note-operation--nonempty-string-p lease-id)
         (org-note-operation--nonempty-string-p fencing-token)
         (integerp expires-at)
         (> expires-at (float-time))
         (org-note-operation--string-vector-p
          (org-note-operation--response-value response 'event_ids))
         (org-note-operation--json-object-p context)
         (org-note-operation--json-object-p workspace)
         (equal (org-note-operation--response-value workspace 'id) workspace-id)
         (let ((workspace-revision
                (org-note-operation--response-value context 'workspace_revision)))
           (and (integerp workspace-revision) (>= workspace-revision 0)))
         (org-note-operation--json-object-p document)
         (equal (org-note-operation--response-value document 'id) document-id)
         (org-note-operation--nonempty-string-p
          (org-note-operation--response-value document 'path))
         (let ((document-revision
                (org-note-operation--response-value document 'revision)))
           (and (integerp document-revision) (>= document-revision 0)))
         (org-note-operation--json-object-p item)
         (equal (org-note-operation--response-value item 'id) item-id)
         (equal (org-note-operation--response-value item 'workspace_id)
                workspace-id)
         (equal (org-note-operation--response-value item 'document_id)
                document-id)
         (org-note-operation--json-object-p lease)
         (equal (org-note-operation--response-value lease 'id) lease-id)
         (equal (org-note-operation--response-value lease 'workspace_id)
                workspace-id)
         (equal (org-note-operation--response-value lease 'work_item_id) item-id)
         (org-note-operation--nonempty-string-p
          (org-note-operation--response-value lease 'attempt_id))
         (equal (org-note-operation--response-value lease 'kind) kind)
         (equal (org-note-operation--response-value lease 'actor_id)
                org-note-actor-id)
         (integerp (org-note-operation--response-value lease 'acquired_at))
         (integerp
          (org-note-operation--response-value lease 'last_heartbeat_at))
         (equal (org-note-operation--response-value lease 'expires_at)
                expires-at)
         (equal (org-note-operation--response-value lease 'status) "active"))))

(defun org-note-operation--validate-claim-response
    (response workspace-id item-id document-id kind operation-id)
  "Validate and return a claim RESPONSE for the expected identifiers."
  (unless (org-note-operation--claim-response-valid-p
           response workspace-id item-id document-id kind operation-id)
    (signal 'org-note-error '("Org Note claim response is invalid")))
  response)

(defun org-note-operation--schedule-heartbeat-after (lease delay)
  "Schedule LEASE to heartbeat after DELAY seconds."
  (when (org-note-operation-lease-timer lease)
    (cancel-timer (org-note-operation-lease-timer lease))
    (setf (org-note-operation-lease-timer lease) nil))
  (setf (org-note-operation-lease-timer lease)
        (run-at-time
         delay nil #'org-note-operation--heartbeat-timer
         (org-note-operation--lease-key
          (org-note-operation-lease-workspace-id lease)
          (org-note-operation-lease-item-id lease)
          (org-note-operation-lease-kind lease)))))

(defun org-note-operation--schedule-heartbeat (lease)
  "Schedule the next heartbeat for LEASE when it remains active."
  (let ((remaining (- (org-note-operation-lease-expires-at lease)
                      (float-time))))
    (when (> remaining 0)
      (org-note-operation--schedule-heartbeat-after
       lease (max 1 (* remaining 0.6))))))

(defun org-note-operation--error-properties (error-data)
  "Return the property list carried by Org Note ERROR-DATA."
  (and (consp error-data)
       (listp (cadr error-data))
       (cadr error-data)))

(defun org-note-operation--error-code (error-data)
  "Return the safe server error code from Org Note ERROR-DATA."
  (plist-get (org-note-operation--error-properties error-data) :code))

(defun org-note-operation--retryable-error-p (error-data)
  "Return non-nil when ERROR-DATA is safe to retry."
  (let* ((error-type (car-safe error-data))
         (properties (org-note-operation--error-properties error-data))
         (status (plist-get properties :status)))
    (or (eq error-type 'org-note-transport-error)
        (and (eq error-type 'org-note-http-error)
             (or (eq (plist-get properties :retryable) t)
                 (equal status 429)
                 (and (integerp status) (>= status 500) (<= status 599)))))))

(defun org-note-operation--safe-transport-error ()
  "Return sanitized transport error data for an immediate dispatch failure."
  '(org-note-transport-error
    (:status nil :code nil :message "Request failed" :details nil :retryable nil)))

(defun org-note-operation--forget-lease-key (lease-key)
  "Forget the lease identified by LEASE-KEY."
  (apply #'org-note-operation-forget-lease lease-key))

(defun org-note-operation--retry-after-error (lease-key)
  "Warn safely and retry LEASE-KEY while its old expiry remains valid."
  (let ((lease (gethash lease-key org-note-operation--leases)))
    (when lease
      (setf (org-note-operation-lease-heartbeat-p lease) nil)
      (display-warning
       'org-note-operation
       "Org Note lease renewal failed; retrying while the lease remains active"
       :warning)
      (let ((remaining (- (org-note-operation-lease-expires-at lease)
                          (float-time))))
        (if (> remaining 0)
            (org-note-operation--schedule-heartbeat-after
             lease (max 1 (min 5 (/ remaining 2.0))))
          (org-note-operation--forget-lease-key lease-key))))))

(defun org-note-operation--expiry-cleanup-timer (lease-key lease-id)
  "Remove LEASE-KEY at expiry if it still identifies LEASE-ID."
  (let ((lease (gethash lease-key org-note-operation--leases)))
    (when (and lease
               (equal lease-id (org-note-operation-lease-lease-id lease)))
      (setf (org-note-operation-lease-timer lease) nil)
      (if (<= (org-note-operation-lease-expires-at lease) (float-time))
          (org-note-operation--forget-lease-key lease-key)
        (org-note-operation--schedule-expiry-cleanup lease-key lease)))))

(defun org-note-operation--schedule-expiry-cleanup (lease-key lease)
  "Schedule token-free local expiry cleanup for LEASE at LEASE-KEY."
  (when (org-note-operation-lease-timer lease)
    (cancel-timer (org-note-operation-lease-timer lease))
    (setf (org-note-operation-lease-timer lease) nil))
  (let ((remaining (- (org-note-operation-lease-expires-at lease)
                      (float-time))))
    (if (> remaining 0)
        (setf (org-note-operation-lease-timer lease)
              (run-at-time
               remaining nil #'org-note-operation--expiry-cleanup-timer
               lease-key (org-note-operation-lease-lease-id lease)))
      (org-note-operation--forget-lease-key lease-key))))

(defun org-note-operation--retain-after-error (lease-key)
  "Warn safely and retain LEASE-KEY until its known expiry."
  (let ((lease (gethash lease-key org-note-operation--leases)))
    (when lease
      (setf (org-note-operation-lease-heartbeat-p lease) nil)
      (display-warning
       'org-note-operation
       "Org Note lease renewal failed; retaining the lease until expiry"
       :warning)
      (org-note-operation--schedule-expiry-cleanup lease-key lease))))

(defun org-note-operation--handle-lease-error (lease-key error-data)
  "Handle safe ERROR-DATA for the lease identified by LEASE-KEY."
  (cond
   ((equal (org-note-operation--error-code error-data) "stale_lease")
    (org-note-operation--forget-lease-key lease-key))
   ((org-note-operation--retryable-error-p error-data)
    (org-note-operation--retry-after-error lease-key))
   (t
    (org-note-operation--retain-after-error lease-key))))

(defun org-note-operation--finish-current-heartbeat
    (lease-key lease result error-data)
  "Finish LEASE only if it remains current at LEASE-KEY."
  (when (eq lease (gethash lease-key org-note-operation--leases))
    (org-note-operation--heartbeat-finished lease-key result error-data)))

(defun org-note-operation--finish-current-context
    (lease-key lease result error-data)
  "Finish context refresh only if LEASE remains current at LEASE-KEY."
  (when (eq lease (gethash lease-key org-note-operation--leases))
    (org-note-operation--context-finished lease-key result error-data)))

(defun org-note-operation--response-expiry (result)
  "Return an authoritative expiry found in RESULT or its data object."
  (let* ((top-entry
          (org-note-operation--response-entry result 'expires_at))
         (data-entry (org-note-operation--response-entry result 'data))
         (nested-entry
          (org-note-operation--response-entry (cdr data-entry) 'expires_at)))
    (or (cdr top-entry) (cdr nested-entry))))

(defun org-note-operation--context-lease (result)
  "Return the lease object found in a context RESULT."
  (let* ((top-entry (org-note-operation--response-entry result 'lease))
         (data-entry (org-note-operation--response-entry result 'data))
         (nested-entry
          (org-note-operation--response-entry (cdr data-entry) 'lease)))
    (or (cdr top-entry) (cdr nested-entry))))

(defun org-note-operation--heartbeat-timer (lease-key)
  "Start a heartbeat request for LEASE-KEY when none is outstanding."
  (let ((lease (gethash lease-key org-note-operation--leases)))
    (when lease
      (setf (org-note-operation-lease-timer lease) nil)
      (cond
       ((org-note-operation-lease-heartbeat-p lease))
       ((<= (org-note-operation-lease-expires-at lease) (float-time))
        (org-note-operation--forget-lease-key lease-key))
       (t
        (setf (org-note-operation-lease-heartbeat-p lease) t)
        (condition-case nil
            (org-note-client-request-async
             "POST"
             (org-note-operation--item-route
              (org-note-operation-lease-item-id lease) "/claim/heartbeat")
             nil
             (org-note-operation--mutation-body
              (org-note-operation-lease-workspace-id lease)
              `((lease_id . ,(org-note-operation-lease-lease-id lease))
                (kind . ,(org-note-operation-lease-kind lease))
                (fencing_token
                 . ,(org-note-operation-lease-fencing-token lease))))
             (lambda (result error-data)
               (org-note-operation--finish-current-heartbeat
                lease-key lease result error-data)))
          (error
           (org-note-operation--finish-current-heartbeat
            lease-key lease nil (org-note-operation--safe-transport-error)))))))))

(defun org-note-operation--heartbeat-finished (lease-key result error-data)
  "Finish the heartbeat for LEASE-KEY with RESULT or ERROR-DATA."
  (let ((lease (gethash lease-key org-note-operation--leases)))
    (when lease
      (if error-data
          (org-note-operation--handle-lease-error lease-key error-data)
        (let ((expires-at (org-note-operation--response-expiry result)))
          (if (and (numberp expires-at) (> expires-at (float-time)))
              (progn
                (setf (org-note-operation-lease-heartbeat-p lease) nil
                      (org-note-operation-lease-expires-at lease) expires-at)
                (org-note-operation--schedule-heartbeat lease))
            (setf (org-note-operation-lease-heartbeat-p lease) nil)
            (org-note-operation--refresh-lease-context lease-key)))))))

(defun org-note-operation--context-finished (lease-key result error-data)
  "Finish the context refresh for LEASE-KEY with RESULT or ERROR-DATA."
  (let ((lease (gethash lease-key org-note-operation--leases)))
    (when lease
      (if error-data
          (org-note-operation--handle-lease-error lease-key error-data)
        (let* ((context-lease (org-note-operation--context-lease result))
               (lease-id
                (cdr (org-note-operation--response-entry
                      context-lease 'lease_id)))
               (kind
                (cdr (org-note-operation--response-entry context-lease 'kind)))
               (expires-at
                (cdr (org-note-operation--response-entry
                      context-lease 'expires_at))))
          (if (and (equal lease-id (org-note-operation-lease-lease-id lease))
                   (equal kind (org-note-operation-lease-kind lease))
                   (numberp expires-at)
                   (> expires-at (float-time)))
              (progn
                (setf (org-note-operation-lease-heartbeat-p lease) nil
                      (org-note-operation-lease-expires-at lease) expires-at)
                (org-note-operation--schedule-heartbeat lease))
            (org-note-operation--forget-lease-key lease-key)))))))

(defun org-note-operation--refresh-lease-context (lease-key)
  "Refresh server context for LEASE-KEY when no request is outstanding."
  (let ((lease (gethash lease-key org-note-operation--leases)))
    (when lease
      (cond
       ((org-note-operation-lease-heartbeat-p lease))
       ((<= (org-note-operation-lease-expires-at lease) (float-time))
        (org-note-operation--forget-lease-key lease-key))
       (t
        (when (org-note-operation-lease-timer lease)
          (cancel-timer (org-note-operation-lease-timer lease))
          (setf (org-note-operation-lease-timer lease) nil))
        (setf (org-note-operation-lease-heartbeat-p lease) t)
        (condition-case nil
            (org-note-client-request-async
             "GET"
             (org-note-operation--item-route
              (org-note-operation-lease-item-id lease) "/context")
             `((workspace_id . ,(org-note-operation-lease-workspace-id lease)))
             nil
             (lambda (result error-data)
               (org-note-operation--finish-current-context
                lease-key lease result error-data)))
          (error
           (org-note-operation--finish-current-context
            lease-key lease nil (org-note-operation--safe-transport-error)))))))))

(defun org-note-operation-find-lease (workspace-id item-id kind)
  "Return the unexpired lease for WORKSPACE-ID, ITEM-ID, and KIND.

Expired leases are forgotten before nil is returned."
  (let* ((key (org-note-operation--lease-key workspace-id item-id kind))
         (lease (gethash key org-note-operation--leases)))
    (if (and lease
             (> (org-note-operation-lease-expires-at lease) (float-time)))
        lease
      (when lease
        (org-note-operation-forget-lease workspace-id item-id kind))
      nil)))

(defun org-note-operation-register-claim
    (workspace-id item-id document-id kind response)
  "Register a claim RESPONSE for WORKSPACE-ID, ITEM-ID, DOCUMENT-ID, and KIND.

RESPONSE must contain nonempty lease credentials and a future expiry."
  (let ((lease-id (org-note-operation--response-value response 'lease_id))
        (fencing-token
         (org-note-operation--response-value response 'fencing_token))
        (expires-at (org-note-operation--response-value response 'expires_at)))
    (unless (and (org-note-operation--nonempty-string-p lease-id)
                 (org-note-operation--nonempty-string-p fencing-token)
                 (numberp expires-at)
                 (> expires-at (float-time)))
      (signal 'org-note-error
              '("Org Note claim response contains invalid lease data")))
    (org-note-operation-forget-lease workspace-id item-id kind)
    (let ((lease
           (org-note-operation--make-lease
            :workspace-id workspace-id
            :item-id item-id
            :document-id document-id
            :kind kind
            :lease-id lease-id
            :fencing-token fencing-token
            :expires-at expires-at
            :heartbeat-p nil)))
      (puthash (org-note-operation--lease-key workspace-id item-id kind)
               lease org-note-operation--leases)
      (org-note-operation--schedule-heartbeat lease)
      lease)))

(defun org-note-operation-forget-lease (workspace-id item-id kind)
  "Forget the lease for WORKSPACE-ID, ITEM-ID, and KIND.

Cancel its pending heartbeat timer before removing it from the registry."
  (let* ((key (org-note-operation--lease-key workspace-id item-id kind))
         (lease (gethash key org-note-operation--leases)))
    (when (and lease (org-note-operation-lease-timer lease))
      (cancel-timer (org-note-operation-lease-timer lease))
      (setf (org-note-operation-lease-timer lease) nil))
    (remhash key org-note-operation--leases)))

(defun org-note-operation-lease-proofs (document-id)
  "Return active lease proofs for DOCUMENT-ID keyed by item identifier."
  (let ((proofs (make-hash-table :test #'equal))
        expired-keys)
    (maphash
     (lambda (key lease)
       (if (<= (org-note-operation-lease-expires-at lease) (float-time))
           (push key expired-keys)
         (when (equal (org-note-operation-lease-document-id lease) document-id)
           (puthash
            (org-note-operation-lease-item-id lease)
            `((lease_id . ,(org-note-operation-lease-lease-id lease))
              (kind . ,(org-note-operation-lease-kind lease))
              (fencing_token . ,(org-note-operation-lease-fencing-token lease)))
            proofs))))
     org-note-operation--leases)
    (dolist (key expired-keys)
      (apply #'org-note-operation-forget-lease key))
    proofs))

(defun org-note-operation--registered-transition-lease
    (workspace-id item-id proof)
  "Return the registered lease exactly identified by PROOF, or nil."
  (let ((lease-id (org-note-operation--response-value proof 'lease_id))
        (kind (org-note-operation--response-value proof 'kind))
        (fencing-token
         (org-note-operation--response-value proof 'fencing_token)))
    (when (and (org-note-operation--nonempty-string-p lease-id)
               (org-note-operation--nonempty-string-p kind)
               (org-note-operation--nonempty-string-p fencing-token))
      (let ((lease (org-note-operation-find-lease workspace-id item-id kind)))
        (and lease
             (equal lease-id (org-note-operation-lease-lease-id lease))
             (equal fencing-token
                    (org-note-operation-lease-fencing-token lease))
             lease)))))

(defun org-note-operation--transition-context-lease-valid-p
    (lease workspace-id item-id expected-lease-id expected-kind)
  "Return non-nil when transition context LEASE is structurally valid."
  (let ((lease-id (org-note-operation--response-value lease 'id)))
    (and (org-note-operation--json-object-p lease)
         (org-note-operation--nonempty-string-p lease-id)
         (equal (org-note-operation--response-value lease 'workspace_id)
                workspace-id)
         (equal (org-note-operation--response-value lease 'work_item_id) item-id)
         (org-note-operation--nonempty-string-p
          (org-note-operation--response-value lease 'attempt_id))
         (org-note-operation--nonempty-string-p
          (org-note-operation--response-value lease 'kind))
         (org-note-operation--nonempty-string-p
          (org-note-operation--response-value lease 'actor_id))
         (integerp (org-note-operation--response-value lease 'acquired_at))
         (integerp
          (org-note-operation--response-value lease 'last_heartbeat_at))
         (integerp (org-note-operation--response-value lease 'expires_at))
         (org-note-operation--nonempty-string-p
          (org-note-operation--response-value lease 'status))
         (or (not (equal lease-id expected-lease-id))
             (and (equal (org-note-operation--response-value lease 'kind)
                         expected-kind)
                  (equal (org-note-operation--response-value lease 'actor_id)
                         org-note-actor-id))))))

(defun org-note-operation--validated-transition-context
    (response workspace-id item-id document-id operation-id
              expected-lease-id expected-kind)
  "Return RESPONSE's context after validating transition result identity."
  (let* ((data (org-note-operation--response-value response 'data))
         (context (org-note-operation--response-value data 'context))
         (workspace (org-note-operation--response-value context 'workspace))
         (document (org-note-operation--response-value context 'document))
         (item (org-note-operation--response-value context 'item))
         (lease-entry (org-note-operation--response-entry context 'lease))
         (lease (cdr lease-entry)))
    (unless
        (and (org-note-operation--json-object-p response)
             (equal (org-note-operation--response-value
                     response 'schema_version)
                    1)
             (equal (org-note-operation--response-value response 'workspace_id)
                    workspace-id)
             (org-note-operation--nonempty-string-p operation-id)
             (equal (org-note-operation--response-value response 'operation_id)
                    operation-id)
             (org-note-operation--string-vector-p
              (org-note-operation--response-value response 'event_ids))
             (org-note-operation--json-object-p data)
             (org-note-operation--json-object-p context)
             (org-note-operation--json-object-p workspace)
             (equal (org-note-operation--response-value workspace 'id)
                    workspace-id)
             (let ((workspace-revision
                    (org-note-operation--response-value
                     context 'workspace_revision)))
               (and (integerp workspace-revision) (>= workspace-revision 0)))
             (org-note-operation--json-object-p document)
             (equal (org-note-operation--response-value document 'id)
                    document-id)
             (let ((document-revision
                    (org-note-operation--response-value document 'revision)))
               (and (integerp document-revision) (>= document-revision 0)))
             (org-note-operation--json-object-p item)
             (equal (org-note-operation--response-value item 'id) item-id)
             (equal (org-note-operation--response-value item 'workspace_id)
                    workspace-id)
             (equal (org-note-operation--response-value item 'document_id)
                    document-id)
             lease-entry
             (or (null lease)
                 (org-note-operation--transition-context-lease-valid-p
                  lease workspace-id item-id expected-lease-id expected-kind)))
      (signal 'org-note-error '("Org Note transition response is invalid")))
    context))

(defun org-note-operation--reconcile-transition-lease
    (registered-lease response workspace-id item-id document-id operation-id)
  "Reconcile REGISTERED-LEASE from an authoritative transition RESPONSE."
  (let* ((kind (org-note-operation-lease-kind registered-lease))
         (lease-id (org-note-operation-lease-lease-id registered-lease))
         (lease-key (org-note-operation--lease-key workspace-id item-id kind))
         (context
          (org-note-operation--validated-transition-context
           response workspace-id item-id document-id operation-id lease-id kind))
         (context-lease (org-note-operation--response-value context 'lease)))
    (when (eq registered-lease
              (gethash lease-key org-note-operation--leases))
      (if (and context-lease
               (equal (org-note-operation--response-value context-lease 'id)
                      lease-id)
               (equal (org-note-operation--response-value context-lease 'kind)
                      kind)
               (equal (org-note-operation--response-value context-lease 'status)
                      "active")
               (> (org-note-operation--response-value context-lease 'expires_at)
                  (float-time)))
          (progn
            (setf (org-note-operation-lease-expires-at registered-lease)
                  (org-note-operation--response-value context-lease 'expires_at))
            (unless (org-note-operation-lease-heartbeat-p registered-lease)
              (org-note-operation--schedule-heartbeat registered-lease)))
        (org-note-operation--forget-lease-key lease-key)))))

(defun org-note-operation--path-segment (identifier)
  "Return IDENTIFIER encoded for use as one URL path segment."
  (url-hexify-string identifier))

(defun org-note-operation--mutation-body (workspace-id fields &optional operation-id)
  "Return a mutation envelope for WORKSPACE-ID containing FIELDS.

OPERATION-ID, when non-nil, is used as the operation identifier."
  (append `((schema_version . 1)
            (actor_id . ,org-note-actor-id)
            (operation_id . ,(or operation-id
                                 (org-note-client-new-operation-id)))
            (workspace_id . ,workspace-id))
          fields))

(defun org-note-operation--freeze-request (typed-request)
  "Freeze TYPED-REQUEST into a replayable wire envelope.

TYPED-REQUEST is a plist with `:method', `:route', optional `:query', and
`:body' (Lisp JSON value).  The body is encoded once.  The returned
envelope holds method, canonical endpoint, absolute URL, route, query,
headers, UTF-8 body bytes, body SHA-256, and memory-only redaction
secrets derived from structural fencing-token fields.  Secrets must not
be copied into durable markers or journals."
  (let* ((method (plist-get typed-request :method))
         (route (plist-get typed-request :route))
         (query (plist-get typed-request :query))
         (body (plist-get typed-request :body))
         (context (org-note-validation-endpoint-bound-read-context
                   org-note-endpoint))
         (endpoint (alist-get 'endpoint context))
         (url-builder (alist-get 'url-builder context))
         (url (funcall url-builder route query))
         (body-bytes (and body (org-note-client--request-data body)))
         (headers (org-note-client--request-headers body-bytes))
         (redaction-secrets
          (and body (org-note-client--fencing-token-values body))))
    (list :method method
          :endpoint endpoint
          :url url
          :route route
          :query query
          :headers headers
          :body body-bytes
          :body-sha256 (and body-bytes (secure-hash 'sha256 body-bytes))
          :redaction-secrets redaction-secrets)))

(defun org-note-operation--dispatch-frozen (frozen-envelope)
  "Dispatch FROZEN-ENVELOPE via raw transport without re-encoding the body.

Uses the frozen absolute URL, headers, and body bytes.  Configuration
changes after freezing do not rewrite the destination."
  (org-note-client-request-raw
   :method (plist-get frozen-envelope :method)
   :url (plist-get frozen-envelope :url)
   :route (plist-get frozen-envelope :route)
   :query (plist-get frozen-envelope :query)
   :headers (plist-get frozen-envelope :headers)
   :body (plist-get frozen-envelope :body)
   :redaction-secrets (plist-get frozen-envelope :redaction-secrets)))

(cl-defun org-note-operation-list-workspaces
    (&key cursor limit include-archived)
  "List workspaces with optional CURSOR, LIMIT, and INCLUDE-ARCHIVED filter."
  (org-note-client-request
   "GET" "/api/org/workspaces"
   `((cursor . ,cursor)
     (limit . ,limit)
     (include_archived . ,include-archived))
   nil))

(defun org-note-operation-get-workspace (workspace-id)
  "Get WORKSPACE-ID."
  (org-note-client-request
   "GET"
   (format "/api/org/workspaces/%s"
           (org-note-operation--path-segment workspace-id))
   nil nil))

(cl-defun org-note-operation-list-documents
    (workspace-id &key cursor limit include-archived)
  "List documents in WORKSPACE-ID with optional paging and archive filters."
  (org-note-client-request
   "GET"
   (format "/api/org/workspaces/%s/documents"
           (org-note-operation--path-segment workspace-id))
   `((cursor . ,cursor)
     (limit . ,limit)
     (include_archived . ,include-archived))
   nil))

(defun org-note-operation-get-document (workspace-id document-id)
  "Get DOCUMENT-ID in WORKSPACE-ID."
  (org-note-client-request
   "GET"
   (format "/api/org/documents/%s"
           (org-note-operation--path-segment document-id))
   `((workspace_id . ,workspace-id))
   nil))

(cl-defun org-note-operation-put-document
    (workspace-id document-id path source expected-revision lease-proofs
                  &key operation-id)
  "Write DOCUMENT-ID in WORKSPACE-ID with PATH and SOURCE.

EXPECTED-REVISION controls optimistic concurrency for updates.  When it is
nil, the field is omitted so the service can create the document.
LEASE-PROOFS is required; a nil value is encoded as an empty JSON object.
OPERATION-ID optionally supplies the mutation ID."
  (org-note-operation--dispatch-frozen
   (org-note-operation--freeze-request
    (list :method "PUT"
          :route (format "/api/org/documents/%s"
                         (org-note-operation--path-segment document-id))
          :query nil
          :body (org-note-operation--mutation-body
                 workspace-id
                 (append
                  `((path . ,path)
                    (source . ,source))
                  (and expected-revision
                       `((expected_revision . ,expected-revision)))
                  `((lease_proofs
                     . ,(or lease-proofs (org-note-client-empty-object)))))
                 operation-id)))))

(cl-defun org-note-operation-create-document
    (workspace-id document-id path source &key operation-id)
  "Create DOCUMENT-ID in WORKSPACE-ID at PATH with SOURCE.

SOURCE may be the empty string.  The request omits expected_revision."
  (org-note-operation-put-document
   workspace-id document-id path source nil nil
   :operation-id operation-id))

(cl-defun org-note-operation-archive-document
    (workspace-id document-id expected-revision &key operation-id)
  "Archive DOCUMENT-ID in WORKSPACE-ID at EXPECTED-REVISION."
  (org-note-client-request
   "POST"
   (format "/api/org/documents/%s/archive"
           (org-note-operation--path-segment document-id))
   nil
   (org-note-operation--mutation-body
    workspace-id `((expected_revision . ,expected-revision)) operation-id)))

(cl-defun org-note-operation-restore-document
    (workspace-id document-id expected-revision &key operation-id)
  "Restore archived DOCUMENT-ID in WORKSPACE-ID at EXPECTED-REVISION."
  (org-note-client-request
   "POST"
   (format "/api/org/documents/%s/restore"
           (org-note-operation--path-segment document-id))
   nil
   (org-note-operation--mutation-body
    workspace-id `((expected_revision . ,expected-revision)) operation-id)))

(cl-defun org-note-operation-rename-document-path
    (workspace-id document-id expected-revision new-path &key operation-id)
  "Rename DOCUMENT-ID in WORKSPACE-ID to NEW-PATH at EXPECTED-REVISION."
  (org-note-client-request
   "PATCH"
   (format "/api/org/documents/%s/path"
           (org-note-operation--path-segment document-id))
   nil
   (org-note-operation--mutation-body
    workspace-id
    `((expected_revision . ,expected-revision)
      (new_path . ,new-path))
    operation-id)))

(defun org-note-operation--validated-view (view supported-views kind)
  "Return VIEW after checking it is in SUPPORTED-VIEWS for KIND."
  (let ((name (cond
               ((symbolp view) (symbol-name view))
               ((stringp view) view))))
    (unless (member name (mapcar #'symbol-name supported-views))
      (user-error "Unsupported Org Note %s view: %s" kind view))
    name))

(defun org-note-operation--validated-workspace-ids (workspace-ids kind)
  "Return valid WORKSPACE-IDS for KIND, or signal `user-error'."
  (unless (and (sequencep workspace-ids)
               (> (length workspace-ids) 0)
               (cl-every (lambda (workspace-id)
                           (and (stringp workspace-id)
                                (> (length workspace-id) 0)))
                         workspace-ids))
    (user-error "Org Note %s queries require non-empty workspace IDs" kind))
  workspace-ids)

(defun org-note-operation--query-filters
    (workspace-ids view item-type state priority tags assignee
                   scheduled-from scheduled-to deadline-from deadline-to
                   completed-from completed-to from to include-archived cursor limit)
  "Return API query filters from operation view arguments."
  `((workspace_ids . ,(mapconcat #'identity workspace-ids ","))
    (view . ,view)
    (item_type . ,item-type)
    (state . ,state)
    (priority . ,priority)
    (tags . ,(and tags (mapconcat #'identity tags ",")))
    (assignee . ,assignee)
    (scheduled_from . ,scheduled-from)
    (scheduled_to . ,scheduled-to)
    (deadline_from . ,deadline-from)
    (deadline_to . ,deadline-to)
    (completed_from . ,completed-from)
    (completed_to . ,completed-to)
    (from . ,from)
    (to . ,to)
    (include_archived . ,include-archived)
    (cursor . ,cursor)
    (limit . ,limit)))

(cl-defun org-note-operation-query-queue
    (&key workspace-ids view item-type state priority tags assignee
          scheduled-from scheduled-to deadline-from deadline-to
          completed-from completed-to from to include-archived cursor limit)
  "Query a queue VIEW across WORKSPACE-IDS with optional filters."
  (setq workspace-ids
        (org-note-operation--validated-workspace-ids workspace-ids "queue"))
  (setq view (org-note-operation--validated-view
              view org-note-operation-queue-views "queue"))
  (org-note-client-request
   "GET" "/api/org/queue"
   (org-note-operation--query-filters
    workspace-ids view item-type state priority tags assignee
    scheduled-from scheduled-to deadline-from deadline-to
    completed-from completed-to from to include-archived cursor limit)
   nil))

(cl-defun org-note-operation-query-agenda
    (&key workspace-ids view item-type state priority tags assignee
          scheduled-from scheduled-to deadline-from deadline-to
          completed-from completed-to from to include-archived cursor limit)
  "Query an agenda VIEW across WORKSPACE-IDS with optional filters."
  (setq workspace-ids
        (org-note-operation--validated-workspace-ids workspace-ids "agenda"))
  (setq view (org-note-operation--validated-view
              view org-note-operation-agenda-views "agenda"))
  (org-note-client-request
   "GET" "/api/org/agenda"
   (org-note-operation--query-filters
    workspace-ids view item-type state priority tags assignee
    scheduled-from scheduled-to deadline-from deadline-to
    completed-from completed-to from to include-archived cursor limit)
   nil))

(defun org-note-operation-get-item-context (workspace-id item-id)
  "Get the context for ITEM-ID in WORKSPACE-ID."
  (org-note-client-request
   "GET"
   (format "/api/org/items/%s/context"
           (org-note-operation--path-segment item-id))
   `((workspace_id . ,workspace-id))
   nil))

(cl-defun org-note-operation-list-events
    (workspace-id &key subject-kind subject-id cursor limit)
  "List events in WORKSPACE-ID with optional subject and paging filters."
  (org-note-client-request
   "GET"
   (format "/api/org/workspaces/%s/events"
           (org-note-operation--path-segment workspace-id))
   `((subject_kind . ,subject-kind)
     (subject_id . ,subject-id)
     (cursor . ,cursor)
     (limit . ,limit))
   nil))

(defun org-note-operation--item-route (item-id suffix)
  "Return the item route for ITEM-ID followed by SUFFIX."
  (format "/api/org/items/%s%s"
          (org-note-operation--path-segment item-id)
          suffix))

(defun org-note-operation--require-fencing-token (fencing-token)
  "Return FENCING-TOKEN if it is a string, or signal `org-note-error'."
  (unless (stringp fencing-token)
    (signal 'org-note-error '("Org Note fencing token must be a string")))
  fencing-token)

(cl-defun org-note-operation-claim
    (workspace-id item-id document-id expected-revision kind &key operation-id)
  "Claim ITEM-ID in WORKSPACE-ID for DOCUMENT-ID at EXPECTED-REVISION.

KIND identifies the claim type.  OPERATION-ID optionally supplies the
mutation ID."
  (let* ((request-operation-id
          (or operation-id (org-note-client-new-operation-id)))
         (response
         (org-note-client-request
          "POST" (org-note-operation--item-route item-id "/claim") nil
          (org-note-operation--mutation-body
           workspace-id
           `((document_id . ,document-id)
             (expected_document_revision . ,expected-revision)
             (kind . ,kind))
           request-operation-id))))
    (org-note-operation--validate-claim-response
     response workspace-id item-id document-id kind request-operation-id)
    (org-note-operation-register-claim
     workspace-id item-id document-id kind response)
    response))

(cl-defun org-note-operation-heartbeat
    (workspace-id item-id lease-id kind fencing-token &key operation-id)
  "Renew LEASE-ID for ITEM-ID in WORKSPACE-ID with FENCING-TOKEN.

KIND identifies the claim type.  OPERATION-ID optionally supplies the
mutation ID."
  (org-note-operation--require-fencing-token fencing-token)
  (let* ((lease-key (org-note-operation--lease-key workspace-id item-id kind))
         (lease (org-note-operation-find-lease workspace-id item-id kind))
         (registered-p
          (and lease
               (equal lease-id (org-note-operation-lease-lease-id lease))
               (equal fencing-token
                      (org-note-operation-lease-fencing-token lease)))))
    (if (not registered-p)
        (org-note-client-request
         "POST" (org-note-operation--item-route item-id "/claim/heartbeat") nil
         (org-note-operation--mutation-body
          workspace-id
          `((lease_id . ,lease-id)
            (kind . ,kind)
            (fencing_token . ,fencing-token))
          operation-id))
      (when (org-note-operation-lease-heartbeat-p lease)
        (signal 'org-note-error
                '("Org Note lease heartbeat already in progress")))
      (when (org-note-operation-lease-timer lease)
        (cancel-timer (org-note-operation-lease-timer lease))
        (setf (org-note-operation-lease-timer lease) nil))
      (setf (org-note-operation-lease-heartbeat-p lease) t)
      (condition-case error-data
          (let ((result
                 (org-note-client-request
                  "POST"
                  (org-note-operation--item-route item-id "/claim/heartbeat")
                  nil
                  (org-note-operation--mutation-body
                   workspace-id
                   `((lease_id . ,lease-id)
                     (kind . ,kind)
                     (fencing_token . ,fencing-token))
                   operation-id))))
            (org-note-operation--finish-current-heartbeat
             lease-key lease result nil)
            result)
        (org-note-error
         (org-note-operation--finish-current-heartbeat
          lease-key lease nil error-data)
         (signal (car error-data) (cdr error-data)))
        (quit
         (condition-case nil
             (org-note-operation--finish-current-heartbeat
              lease-key lease nil (org-note-operation--safe-transport-error))
           ((error quit) nil))
         (signal (car error-data) (cdr error-data)))
        (error
         (org-note-operation--finish-current-heartbeat
          lease-key lease nil (org-note-operation--safe-transport-error))
         (signal (car error-data) (cdr error-data)))))))

(cl-defun org-note-operation-release
    (workspace-id item-id document-id expected-revision lease-id kind fencing-token
                  &key target-state operation-id)
  "Release LEASE-ID for ITEM-ID in WORKSPACE-ID for DOCUMENT-ID.

EXPECTED-REVISION identifies the document version.  KIND identifies the claim
type.  FENCING-TOKEN authorizes the release.  TARGET-STATE is included only
when non-nil.  OPERATION-ID optionally supplies the mutation ID."
  (org-note-operation--require-fencing-token fencing-token)
  (prog1
      (org-note-client-request
       "POST" (org-note-operation--item-route item-id "/claim/release") nil
       (org-note-operation--mutation-body
        workspace-id
        (append `((document_id . ,document-id)
                  (expected_document_revision . ,expected-revision)
                  (lease_id . ,lease-id)
                  (kind . ,kind)
                  (fencing_token . ,fencing-token))
                (and target-state `((target_state . ,target-state))))
        operation-id))
    (org-note-operation-forget-lease workspace-id item-id kind)))

(cl-defun org-note-operation-report-progress
    (workspace-id item-id lease-id kind fencing-token summary &key metadata operation-id)
  "Report SUMMARY for ITEM-ID in WORKSPACE-ID under LEASE-ID.

KIND identifies the claim type.  FENCING-TOKEN authorizes the report.
METADATA is encoded as an empty JSON object when nil.  OPERATION-ID optionally
supplies the mutation ID."
  (org-note-operation--require-fencing-token fencing-token)
  (org-note-client-request
   "POST" (org-note-operation--item-route item-id "/progress") nil
   (org-note-operation--mutation-body
    workspace-id
    `((lease_id . ,lease-id)
      (kind . ,kind)
      (fencing_token . ,fencing-token)
      (summary . ,summary)
      (metadata . ,(or metadata (org-note-client-empty-object))))
    operation-id)))

(cl-defun org-note-operation-submit-result
    (workspace-id item-id document-id expected-revision lease-id fencing-token result-summary
                  &key note-refs artifacts metadata operation-id)
  "Submit RESULT-SUMMARY for ITEM-ID in WORKSPACE-ID and DOCUMENT-ID.

EXPECTED-REVISION identifies the document version.  LEASE-ID and FENCING-TOKEN
authorize the result.  Nil NOTE-REFS and ARTIFACTS are encoded as empty JSON
arrays.  METADATA is encoded as an empty JSON object when nil.  OPERATION-ID
optionally supplies the mutation ID."
  (org-note-operation--require-fencing-token fencing-token)
  (prog1
      (org-note-client-request
       "POST" (org-note-operation--item-route item-id "/result") nil
       (org-note-operation--mutation-body
        workspace-id
        `((document_id . ,document-id)
          (expected_document_revision . ,expected-revision)
          (lease_id . ,lease-id)
          (fencing_token . ,fencing-token)
          (result_summary . ,result-summary)
          (note_refs . ,(or note-refs []))
          (artifacts . ,(or artifacts []))
          (metadata . ,(or metadata (org-note-client-empty-object))))
        operation-id))
    (org-note-operation-forget-lease workspace-id item-id "execution")))

(cl-defun org-note-operation-transition
    (workspace-id item-id document-id expected-revision target-state
                  &key lease error metadata operation-id)
  "Transition ITEM-ID in WORKSPACE-ID and DOCUMENT-ID to TARGET-STATE.

EXPECTED-REVISION identifies the document version.  LEASE and ERROR are
included only when non-nil.  METADATA is encoded as an empty JSON object when
nil.  OPERATION-ID optionally supplies the mutation ID."
  (let* ((request-operation-id
          (or operation-id (org-note-client-new-operation-id)))
         (registered-lease
          (and lease
               (org-note-operation--registered-transition-lease
                workspace-id item-id lease)))
         (response
          (org-note-client-request
           "POST" (org-note-operation--item-route item-id "/transition") nil
           (org-note-operation--mutation-body
            workspace-id
            (append `((document_id . ,document-id)
                      (expected_document_revision . ,expected-revision)
                      (target_state . ,target-state))
                    (and lease `((lease . ,lease)))
                    (and error `((error . ,error)))
                    `((metadata
                       . ,(or metadata (org-note-client-empty-object)))))
            request-operation-id))))
    (when registered-lease
      (org-note-operation--reconcile-transition-lease
       registered-lease response workspace-id item-id document-id
       request-operation-id))
    response))

(cl-defun org-note-operation-retry
    (workspace-id item-id document-id expected-revision &key operation-id)
  "Retry ITEM-ID in WORKSPACE-ID and DOCUMENT-ID at EXPECTED-REVISION.

OPERATION-ID optionally supplies the mutation ID."
  (let* ((request-operation-id
          (or operation-id (org-note-client-new-operation-id)))
         (response
         (org-note-client-request
          "POST" (org-note-operation--item-route item-id "/retry") nil
          (org-note-operation--mutation-body
           workspace-id
           `((document_id . ,document-id)
             (expected_document_revision . ,expected-revision))
           request-operation-id))))
    (org-note-operation--validate-claim-response
     response workspace-id item-id document-id "execution" request-operation-id)
    (org-note-operation-register-claim
     workspace-id item-id document-id "execution" response)
    response))

(cl-defun org-note-operation-request-review
    (workspace-id item-id document-id expected-revision lease-id fencing-token
                  &key result-summary note-refs artifacts metadata operation-id)
  "Request review for ITEM-ID in WORKSPACE-ID and DOCUMENT-ID.

EXPECTED-REVISION identifies the document version.  LEASE-ID and FENCING-TOKEN
authorize the request.  RESULT-SUMMARY is included only when non-nil.  Nil
NOTE-REFS and ARTIFACTS are encoded as empty JSON arrays.  METADATA is encoded
as an empty JSON object when nil.  OPERATION-ID optionally supplies the
mutation ID."
  (org-note-operation--require-fencing-token fencing-token)
  (prog1
      (org-note-client-request
       "POST" (org-note-operation--item-route item-id "/review/request") nil
       (org-note-operation--mutation-body
        workspace-id
        (append `((document_id . ,document-id)
                  (expected_document_revision . ,expected-revision)
                  (lease_id . ,lease-id)
                  (fencing_token . ,fencing-token))
                (and result-summary `((result_summary . ,result-summary)))
                `((note_refs . ,(or note-refs []))
                  (artifacts . ,(or artifacts []))
                  (metadata . ,(or metadata (org-note-client-empty-object)))))
        operation-id))
    (org-note-operation-forget-lease workspace-id item-id "execution")))

(cl-defun org-note-operation-approve-review
    (workspace-id item-id document-id expected-revision lease-id fencing-token
                  &key metadata operation-id)
  "Approve review for ITEM-ID in WORKSPACE-ID and DOCUMENT-ID.

EXPECTED-REVISION identifies the document version.  LEASE-ID and FENCING-TOKEN
authorize approval.  METADATA is encoded as an empty JSON object when nil.
OPERATION-ID optionally supplies the mutation ID."
  (org-note-operation--require-fencing-token fencing-token)
  (prog1
      (org-note-client-request
       "POST" (org-note-operation--item-route item-id "/review/approve") nil
       (org-note-operation--mutation-body
        workspace-id
        `((document_id . ,document-id)
          (expected_document_revision . ,expected-revision)
          (lease_id . ,lease-id)
          (fencing_token . ,fencing-token)
          (metadata . ,(or metadata (org-note-client-empty-object))))
        operation-id))
    (org-note-operation-forget-lease workspace-id item-id "review")))

(cl-defun org-note-operation-reject-review
    (workspace-id item-id document-id expected-revision lease-id fencing-token reason
                  &key metadata operation-id)
  "Reject review for ITEM-ID in WORKSPACE-ID and DOCUMENT-ID with REASON.

EXPECTED-REVISION identifies the document version.  LEASE-ID and FENCING-TOKEN
authorize rejection.  METADATA is encoded as an empty JSON object when nil.
OPERATION-ID optionally supplies the mutation ID."
  (org-note-operation--require-fencing-token fencing-token)
  (prog1
      (org-note-client-request
       "POST" (org-note-operation--item-route item-id "/review/reject") nil
       (org-note-operation--mutation-body
        workspace-id
        `((document_id . ,document-id)
          (expected_document_revision . ,expected-revision)
          (lease_id . ,lease-id)
          (fencing_token . ,fencing-token)
          (reason . ,reason)
          (metadata . ,(or metadata (org-note-client-empty-object))))
        operation-id))
    (org-note-operation-forget-lease workspace-id item-id "review")))

(cl-defun org-note-operation-add-dependency
    (workspace-id item-id dependency-id document-id expected-revisions
                  &key lease operation-id)
  "Add DEPENDENCY-ID to ITEM-ID in WORKSPACE-ID and DOCUMENT-ID.

EXPECTED-REVISIONS identifies document revisions.  LEASE is included only
when non-nil.  OPERATION-ID optionally supplies the mutation ID."
  (org-note-client-request
   "POST" (org-note-operation--item-route item-id "/dependencies") nil
   (org-note-operation--mutation-body
    workspace-id
    (append `((dependency_id . ,dependency-id)
              (document_id . ,document-id)
              (expected_revisions . ,expected-revisions))
            (and lease `((lease . ,lease))))
    operation-id)))

(cl-defun org-note-operation-remove-dependency
    (workspace-id item-id dependency-id document-id expected-revisions
                  &key lease operation-id)
  "Remove DEPENDENCY-ID from ITEM-ID in WORKSPACE-ID and DOCUMENT-ID.

EXPECTED-REVISIONS identifies document revisions.  LEASE is included only
when non-nil.  OPERATION-ID optionally supplies the mutation ID."
  (org-note-client-request
   "DELETE"
   (org-note-operation--item-route
    item-id
    (format "/dependencies/%s"
            (org-note-operation--path-segment dependency-id)))
   nil
   (org-note-operation--mutation-body
    workspace-id
    (append `((document_id . ,document-id)
              (expected_revisions . ,expected-revisions))
            (and lease `((lease . ,lease))))
    operation-id)))

(cl-defun org-note-operation-link-note
    (workspace-id item-id document-id purpose note-id description expected-revisions
                  &key lease operation-id)
  "Link NOTE-ID to ITEM-ID in WORKSPACE-ID and DOCUMENT-ID for PURPOSE.

DESCRIPTION describes the link.  EXPECTED-REVISIONS identifies document
revisions.  LEASE is included only when non-nil.  OPERATION-ID optionally
supplies the mutation ID."
  (org-note-client-request
   "POST" (org-note-operation--item-route item-id "/note-links") nil
   (org-note-operation--mutation-body
    workspace-id
    (append `((document_id . ,document-id)
              (purpose . ,purpose)
              (note_id . ,note-id)
              (description . ,description)
              (expected_revisions . ,expected-revisions))
            (and lease `((lease . ,lease))))
    operation-id)))

(cl-defun org-note-operation-unlink-note
    (workspace-id item-id document-id purpose note-id expected-revisions
                  &key lease operation-id)
  "Unlink NOTE-ID from ITEM-ID in WORKSPACE-ID and DOCUMENT-ID for PURPOSE.

EXPECTED-REVISIONS identifies document revisions.  LEASE is included only
when non-nil.  OPERATION-ID optionally supplies the mutation ID."
  (org-note-client-request
   "DELETE" (org-note-operation--item-route item-id "/note-links") nil
   (org-note-operation--mutation-body
    workspace-id
    (append `((document_id . ,document-id)
              (purpose . ,purpose)
              (note_id . ,note-id)
              (expected_revisions . ,expected-revisions))
            (and lease `((lease . ,lease))))
    operation-id)))

(provide 'org-note-operation)

;;; org-note-operation.el ends here
