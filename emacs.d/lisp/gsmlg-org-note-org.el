;;; gsmlg-org-note-org.el --- Org Note Org bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; First-party bridge between Org entrypoints and Org Note.  Cold-start
;; around-advice on Org agenda producers and `org-capture' is installed without
;; requiring Org Note or performing network I/O.  The first command
;; invocation after explicit development enablement loads Org Note, activates
;; the bridge, and owns the generated agenda feed so `org-agenda-files' is
;; feed-only.

;;; Code:

(require 'cl-lib)
(require 'gsmlg-paths)
(require 'json)

(declare-function org-note-operation-query-agenda "org-note-operation"
                  (&rest keyword-arguments))
(declare-function org-note-operation-get-item-context "org-note-operation"
                  (workspace-id item-id))
(declare-function org-note-operation-get-document "org-note-operation"
                  (workspace-id document-id))
(declare-function org-note-operation-list-documents "org-note-operation"
                  (workspace-id &rest keyword-arguments))
(declare-function org-note-operation-find-lease "org-note-operation"
                  (workspace-id item-id kind))
(declare-function org-note-operation-lease-lease-id "org-note-operation" (lease))
(declare-function org-note-operation-lease-kind "org-note-operation" (lease))
(declare-function org-note-operation-lease-fencing-token "org-note-operation"
                  (lease))
(declare-function org-note-operation--transition-typed-request
                  "org-note-operation"
                  (workspace-id item-id document-id expected-revision
                                target-state &rest keyword-arguments))
(declare-function org-note-operation--freeze-request "org-note-operation"
                  (typed-request))
(declare-function org-note-operation--dispatch-frozen "org-note-operation"
                  (frozen-envelope))
(declare-function org-note-validation-canonical-endpoint "org-note-validation"
                  (url-or-string))
(declare-function org-note-operation--validate-transition-response
                  "org-note-operation"
                  (response workspace-id item-id document-id expected-revision
                            target-state operation-id &optional expected-lease-id
                            expected-kind))
(declare-function org-note-operation--validate-claim-response
                  "org-note-operation"
                  (response workspace-id item-id document-id kind operation-id))
(declare-function org-note-operation--registered-transition-lease
                  "org-note-operation"
                  (workspace-id item-id proof))
(declare-function org-note-operation--reconcile-transition-lease
                  "org-note-operation"
                  (registered-lease context workspace-id item-id))
(declare-function org-note-operation-lease-proofs "org-note-operation"
                  (document-id))
(declare-function org-note-operation-claim "org-note-operation"
                  (workspace-id item-id document-id expected-revision kind &rest args))
(declare-function org-note-operation-register-claim "org-note-operation"
                  (workspace-id item-id document-id kind response))
(declare-function org-note-operation-forget-lease "org-note-operation"
                  (workspace-id item-id kind))
(declare-function org-note-operation-release "org-note-operation"
                  (workspace-id item-id document-id expected-revision lease-id kind
                                fencing-token &rest args))
(declare-function org-note-operation--path-segment "org-note-operation"
                  (identifier))
(declare-function org-note-operation--mutation-body "org-note-operation"
                  (workspace-id fields &optional operation-id))
(declare-function org-note-client-request "org-note-client" (&rest args))
(declare-function org-note-client-new-operation-id "org-note-client" ())
(declare-function org-note-client-empty-object "org-note-client" ())
(declare-function org-id-uuid "org-id" ())
(declare-function org-note-item-context "org-note" (workspace-id item-id))
(declare-function org-note-document--require-metadata "org-note" ())
(declare-function org-note-document--kill-buffer-safely "org-note" ())
(declare-function org-note--document-buffer-name "org-note" (workspace-id))
(declare-function org-clock-goto "org-clock" (&rest args))
(declare-function org-note-configure-agenda-workspaces "org-note" ())
(declare-function org-note-validation-page-cursor "org-note-validation"
                  (cursor))
(declare-function org-note-validation-bounded-pager-state "org-note-validation"
                  (&rest keyword-arguments))
(declare-function org-note-validation-bounded-pager-fold "org-note-validation"
                  (state page-fetcher))
(declare-function org-agenda-goto "org-agenda" (&optional highlight))
(declare-function org-entry-get "org" (pom property &optional literal selective))
(declare-function org-get-at-bol "org" (prop))
(declare-function org-get-todo-state "org" ())
(declare-function org-set-regexps-and-options "org" (&optional tags-only))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org" (&optional invisible-ok subtree-end))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-todo "org" (&optional arg))
(defvar org-agenda-files)
(defvar org-done-keywords)
(defvar org-not-done-keywords)
(defvar org-todo-key-alist)
(defvar org-todo-key-trigger)
(defvar org-todo-keywords)
(defvar org-todo-keywords-1)
(declare-function gsmlg-org-apply-path-settings "gsmlg-org" ())
(defvar org-use-fast-todo-selection)
(defvar org-note-document-workspace-id)
(defvar org-note-document-id)
(defvar org-note-document-path)
(defvar org-note-document-revision)
(defvar org-note-document-base-source)

(defgroup gsmlg-org-note-org nil
  "Org Note bridge for Org agenda and capture."
  :group 'gsmlg)

(defcustom gsmlg-org-note-org-enable nil
  "Enable the phased Org Note Org bridge.

Keep this nil for normal use until every release-gated bridge phase and
integration gate in the approved design is complete.  Developers may enable
it explicitly while implementing and testing a phase."
  :type 'boolean
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-capture-workspace-id nil
  "Workspace id used by bridge Capture for an existing document."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-capture-endpoint "https://agent-note.gsmlg.net"
  "Canonical endpoint identity for the configured Capture target.

When non-nil, it must match `org-note-endpoint' before Capture can mutate."
  :type 'string
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-capture-document-id nil
  "Stable document id used by bridge Capture for an existing document."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-capture-document-path nil
  "Expected remote path for the configured bridge Capture document."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'gsmlg-org-note-org)

(defvar-local gsmlg-org-note-org--capture-attempt nil
  "Current in-process Org Note Capture attempt for this staging buffer.")

(defvar gsmlg-org-note-org--capture-reservation nil
  "Process-local Capture reservation owner record, or nil.")

(defcustom gsmlg-org-note-org-reservation-ownerless-grace 2.0
  "Seconds to wait before an ownerless reservation may be recovered."
  :type 'number
  :group 'gsmlg-org-note-org)

(defun gsmlg-org-note-org--process-start-token (pid)
  "Return a best-effort stable start token for PID, or nil."
  (let ((attrs (and (integerp pid) (process-attributes pid))))
    (let ((token (or (cdr (assq 'start attrs))
                     (cdr (assq 'etime attrs)))))
      (and token (format "%s" token)))))

(defun gsmlg-org-note-org--capture-reservation-directory ()
  "Return the private Capture reservation directory."
  (gsmlg-state-file "org-note/capture.lock/"))

(defun gsmlg-org-note-org--capture-reservation-acquire ()
  "Acquire the atomic Capture reservation, refusing an existing owner."
  (unless gsmlg-org-note-org--capture-reservation
    (let ((directory (directory-file-name
                      (gsmlg-org-note-org--capture-reservation-directory))))
      (when (file-directory-p directory)
        (let* ((owner-file (expand-file-name "owner.json" directory))
               (owner (condition-case nil
                          (json-parse-string
                           (with-temp-buffer
                             (insert-file-contents-literally owner-file)
                             (buffer-string))
                           :object-type 'alist)
                        (error nil))))
          (if (and owner (integerp (alist-get 'pid owner))
                   (process-attributes (alist-get 'pid owner))
                   (or (null (alist-get 'start_token owner))
                       (equal (alist-get 'start_token owner)
                              (gsmlg-org-note-org--process-start-token
                               (alist-get 'pid owner)))))
              (user-error "Another Org Note Capture is already in progress")
            (if (and (not (file-exists-p owner-file))
                     (< (- (float-time)
                           (float-time (file-attribute-modification-time
                                        (file-attributes directory))))
                        gsmlg-org-note-org-reservation-ownerless-grace))
                (user-error "Org Note Capture reservation owner is being published")
              (user-error "Org Note Capture reservation is busy")))))
      (make-directory directory t)
      (set-file-modes directory #o700)
      (let ((owner `((hostname . ,(system-name))
                     (pid . ,(emacs-pid))
                     (started_at . ,(float-time))
                     (start_token . ,(gsmlg-org-note-org--process-start-token
                                      (emacs-pid)))
                     (nonce . ,(org-note-client-new-operation-id)))))
        (let ((file (expand-file-name "owner.json" directory)))
          (with-temp-file file
            (insert (json-serialize owner)))
          (set-file-modes file #o600)
          (setq gsmlg-org-note-org--capture-reservation
                (list :directory directory :owner owner)))))))

(defun gsmlg-org-note-org--capture-reservation-release ()
  "Release this process's Capture reservation using its nonce."
  (let* ((reservation gsmlg-org-note-org--capture-reservation)
         (directory (plist-get reservation :directory))
         (owner-file (and directory (expand-file-name "owner.json" directory))))
    (when (and reservation (file-readable-p owner-file))
      (let* ((raw (with-temp-buffer
                    (insert-file-contents-literally owner-file)
                    (buffer-string)))
             (owner (condition-case nil
                        (json-parse-string raw :object-type 'alist)
                      (error nil))))
        (when (equal (alist-get 'nonce owner)
                     (alist-get 'nonce (plist-get reservation :owner)))
          (delete-file owner-file)
          (delete-directory directory))))
    (setq gsmlg-org-note-org--capture-reservation nil)))

;;;###autoload
(defun gsmlg-org-note-org-recover-capture-reservation ()
  "Explicitly recover a reservation whose recorded PID is no longer live."
  (interactive)
  (let* ((directory (directory-file-name
                     (gsmlg-org-note-org--capture-reservation-directory)))
         (owner-file (expand-file-name "owner.json" directory))
         (owner (and (file-regular-p owner-file)
                     (condition-case nil
                         (json-parse-string
                          (with-temp-buffer
                            (insert-file-contents-literally owner-file)
                            (buffer-string))
                          :object-type 'alist)
                       (error nil))))
         (pid (and owner (alist-get 'pid owner)))
         (nonce (and owner (alist-get 'nonce owner))))
    (unless (and owner (integerp pid) (stringp nonce))
      (user-error "Org Note Capture reservation owner is unreadable"))
    (when (process-attributes pid)
      (user-error "Org Note Capture reservation owner is still live"))
    (unless (yes-or-no-p "Recover this stale Org Note Capture reservation? ")
      (user-error "Org Note Capture reservation recovery cancelled"))
    (let ((stale (format "%s.stale-%s" directory nonce)))
      (rename-file directory stale)
      (gsmlg-org-note-org--capture-reservation-acquire)
      (message "Recovered stale Org Note Capture reservation"))))

(defun gsmlg-org-note-org--capture-staging-kill ()
  "Release an uncommitted Capture reservation when staging is killed."
  (when (and (boundp 'gsmlg-org-note-org--capture-attempt)
             (not (memq (plist-get gsmlg-org-note-org--capture-attempt :state)
                        '(ambiguous committed-pending-journal committed
                          committed-local-divergence))))
    (gsmlg-org-note-org--capture-reservation-release)))

(defun gsmlg-org-note-org--release-transient-reservations-on-exit ()
  "Release only transient reservations during normal Emacs exit."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'gsmlg-org-note-org--capture-attempt)
                 gsmlg-org-note-org--capture-attempt
                 (not (memq (plist-get gsmlg-org-note-org--capture-attempt :state)
                            '(ambiguous committed-pending-journal committed
                              committed-local-divergence))))
        (gsmlg-org-note-org--capture-reservation-release))))
  (unless (and gsmlg-org-note-org--capture-attempt
               (memq (plist-get gsmlg-org-note-org--capture-attempt :state)
                     '(ambiguous committed-pending-journal committed-local-divergence)))
    (gsmlg-org-note-org--publication-release)))

(defconst gsmlg-org-note-org--capture-journal-schema-version 1
  "Current schema version for Org Note Capture recovery journals.")

(defconst gsmlg-org-note-org--capture-journal-max-source-bytes (* 8 1024 1024)
  "Maximum opaque Capture source size accepted by the journal.")

(defconst gsmlg-org-note-org--capture-journal-max-wire-bytes (* 16 1024 1024)
  "Maximum frozen Capture wire body size accepted by the journal.")

(defconst gsmlg-org-note-org--capture-journal-max-record-bytes (* 32 1024 1024)
  "Maximum serialized Capture journal size accepted on read.")

(defvar gsmlg-org-note-org--capture-recovery-required nil
  "Validated recovered Capture records, or a fail-closed blocked marker.")

(defun gsmlg-org-note-org--capture-journal-directory ()
  "Return the private Org Note mutation recovery directory."
  (gsmlg-state-file "org-note/mutation-recovery/"))

(defun gsmlg-org-note-org--capture-journal-safe-directory (&optional create)
  "Return the verified private recovery directory.

When CREATE is non-nil, create it before verification."
  (let ((directory (directory-file-name
                    (gsmlg-org-note-org--capture-journal-directory))))
    (when (and create (not (file-exists-p directory)))
      (make-directory directory t)
      (set-file-modes directory #o700))
    (when (or (file-exists-p directory) (file-symlink-p directory))
      (let ((attributes (file-attributes directory 'integer)))
        (unless (and attributes
                     (eq (file-attribute-type attributes) t)
                     (not (file-symlink-p directory))
                     (= (file-attribute-user-id attributes) (user-uid))
                     (= (logand (file-modes directory) #o777) #o700))
          (user-error "Org Note Capture recovery directory is unsafe"))))
    (when (or create (file-exists-p directory))
      directory)))

(defun gsmlg-org-note-org--capture-journal-file (operation-id)
  "Return the recovery journal path for OPERATION-ID."
  (unless (and (stringp operation-id)
               (<= 1 (length operation-id) 128)
               (string-match-p "\\`[[:alnum:]_.-]+\\'" operation-id))
    (user-error "Org Note Capture operation id is invalid"))
  (expand-file-name (concat operation-id ".json")
                    (gsmlg-org-note-org--capture-journal-safe-directory t)))

(defun gsmlg-org-note-org--capture-journal-json (value)
  "Return canonical UTF-8 JSON bytes for VALUE."
  (encode-coding-string
   (json-serialize value :null-object :null :false-object :false)
   'utf-8))

(defun gsmlg-org-note-org--capture-journal-empty-object ()
  "Return the canonical empty object used for Capture lease proofs."
  (make-hash-table :test #'equal))

(defun gsmlg-org-note-org--capture-journal-headers (headers)
  "Convert frozen request HEADERS to the journal array representation."
  (unless (and (listp headers)
               (cl-every (lambda (entry)
                           (and (consp entry)
                                (stringp (car entry))
                                (stringp (cdr entry))))
                         headers))
    (user-error "Org Note Capture frozen headers are invalid"))
  (vconcat (mapcar (lambda (entry) (vector (car entry) (cdr entry))) headers)))

(defun gsmlg-org-note-org--capture-journal-validate-wire
    (body-bytes operation-id workspace-id path expected-revision)
  "Validate frozen Capture BODY-BYTES against its journal identity.

OPERATION-ID, WORKSPACE-ID, PATH, and EXPECTED-REVISION are the fields that
must match the typed PUT body."
  (let* ((wire-object
          (condition-case nil
              (json-parse-string
               (decode-coding-string body-bytes 'utf-8)
               :object-type 'alist :array-type 'array
               :null-object :null :false-object :false)
            (error nil)))
         (wire-keys (and (listp wire-object) (mapcar #'car wire-object))))
    (unless (and (or (equal wire-keys
                            '(schema_version actor_id operation_id workspace_id
                              path source expected_revision lease_proofs))
                     (equal wire-keys
                            '(schema_version actor_id operation_id workspace_id
                              path source lease_proofs)))
                 (= (alist-get 'schema_version wire-object) 1)
                 (stringp (alist-get 'actor_id wire-object))
                 (equal (alist-get 'operation_id wire-object) operation-id)
                 (equal (alist-get 'workspace_id wire-object) workspace-id)
                 (equal (alist-get 'path wire-object) path)
                 (stringp (alist-get 'source wire-object))
                 (if expected-revision
                     (= (alist-get 'expected_revision wire-object)
                        expected-revision)
                   (not (assq 'expected_revision wire-object)))
                 (assq 'lease_proofs wire-object)
                 (null (cdr (assq 'lease_proofs wire-object))))
      (user-error "Org Note Capture wire body schema is unsafe"))))

(defun gsmlg-org-note-org--capture-journal-body (attempt)
  "Return the exact canonical journal body for Capture ATTEMPT."
  (require 'org-note-validation)
  (let* ((frozen (plist-get attempt :frozen))
         (source (plist-get attempt :source))
         (body-bytes (plist-get frozen :body))
         (endpoint (plist-get frozen :endpoint))
         (now (floor (* 1000 (float-time))))
         (state (plist-get attempt :state)))
    (unless (memq state '(prepared dispatched committed))
      (user-error "Org Note Capture journal state is invalid"))
    (unless (and (stringp source)
                 (<= (string-bytes source)
                     gsmlg-org-note-org--capture-journal-max-source-bytes)
                 (stringp body-bytes)
                 (<= (string-bytes body-bytes)
                     gsmlg-org-note-org--capture-journal-max-wire-bytes)
                 (null (plist-get frozen :redaction-secrets))
                 (equal (secure-hash 'sha256 body-bytes)
                        (plist-get frozen :body-sha256))
                 (equal endpoint
                        (org-note-validation-canonical-endpoint endpoint)))
      (user-error "Org Note Capture journal payload is unsafe"))
    (gsmlg-org-note-org--capture-journal-validate-wire
     body-bytes
     (plist-get attempt :operation-id)
     (plist-get attempt :workspace-id)
     (plist-get attempt :path)
     (plist-get attempt :expected-revision))
    `((operation_kind . "capture")
      (state . ,(symbol-name state))
      (operation_id . ,(plist-get attempt :operation-id))
      (endpoint . ,endpoint)
      (workspace_id . ,(plist-get attempt :workspace-id))
      (document_id . ,(plist-get attempt :document-id))
      (path . ,(plist-get attempt :path))
      ,@(and (plist-get attempt :expected-revision)
             `((expected_revision . ,(plist-get attempt :expected-revision))))
      (created_at . ,(or (plist-get attempt :created-at) now))
      (updated_at . ,now)
      (source_digest . ,(plist-get attempt :digest))
      (source . ,source)
      (lease_proofs . ,(gsmlg-org-note-org--capture-journal-empty-object))
      (wire_method . ,(plist-get frozen :method))
      (wire_url . ,(plist-get frozen :url))
      (wire_route . ,(plist-get frozen :route))
      (wire_query . ,(or (plist-get frozen :query) :null))
      (wire_headers . ,(gsmlg-org-note-org--capture-journal-headers
                        (plist-get frozen :headers)))
      (wire_body_base64 . ,(base64-encode-string body-bytes t))
      (wire_body_sha256 . ,(plist-get frozen :body-sha256)))))

(defun gsmlg-org-note-org--capture-journal-write (attempt)
  "Atomically persist Capture ATTEMPT and return its journal path."
  (let* ((body (gsmlg-org-note-org--capture-journal-body attempt))
         (body-bytes (gsmlg-org-note-org--capture-journal-json body))
         (record `((schema_version
                    . ,gsmlg-org-note-org--capture-journal-schema-version)
                   (checksum . ,(secure-hash 'sha256 body-bytes))
                   (body . ,body)))
         (bytes (gsmlg-org-note-org--capture-journal-json record))
         (file (gsmlg-org-note-org--capture-journal-file
                (plist-get attempt :operation-id)))
         (temporary (make-temp-file
                     (expand-file-name ".capture-journal-"
                                       (file-name-directory file)))))
    (when (> (string-bytes bytes)
             gsmlg-org-note-org--capture-journal-max-record-bytes)
      (delete-file temporary)
      (user-error "Org Note Capture journal exceeds its size limit"))
    (unwind-protect
        (progn
          (set-file-modes temporary #o600)
          (let ((coding-system-for-write 'no-conversion)
                (write-region-inhibit-fsync nil))
            (write-region bytes nil temporary nil 'silent)
            (rename-file temporary file t))
          file)
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(defun gsmlg-org-note-org--capture-journal-quarantine (file)
  "Quarantine unsafe recovery FILE without following it."
  (let ((quarantine
         (format "%s.quarantine.%d.%06x"
                 file (floor (* 1000 (float-time))) (random #x1000000))))
    (when (or (file-exists-p file) (file-symlink-p file))
      (rename-file file quarantine nil))
    quarantine))

(defun gsmlg-org-note-org--capture-journal-normalize-body (body)
  "Validate parsed Capture journal BODY and return its canonical form."
  (require 'org-note-validation)
  (let ((keys (and (listp body) (mapcar #'car body))))
    (unless (equal keys
                   '(operation_kind state operation_id endpoint workspace_id
                     document_id path expected_revision created_at updated_at
                     source_digest source lease_proofs wire_method wire_url
                     wire_route wire_query wire_headers wire_body_base64
                     wire_body_sha256))
      (user-error "Org Note Capture journal schema is invalid")))
  (let* ((state-name (alist-get 'state body))
         (state (and (stringp state-name) (intern-soft state-name)))
         (operation-id (alist-get 'operation_id body))
         (endpoint (alist-get 'endpoint body))
         (source (alist-get 'source body))
         (digest (alist-get 'source_digest body))
         (wire-body-base64 (alist-get 'wire_body_base64 body))
         (wire-body (and (stringp wire-body-base64)
                         (condition-case nil
                             (base64-decode-string wire-body-base64)
                           (error nil))))
         (headers (alist-get 'wire_headers body)))
    (unless (and (equal (alist-get 'operation_kind body) "capture")
                 (memq state '(prepared dispatched committed staged))
                 (stringp operation-id) (<= 1 (length operation-id) 128)
                 (string-match-p "\\`[[:alnum:]_.-]+\\'" operation-id)
                 (stringp endpoint)
                 (equal endpoint
                        (org-note-validation-canonical-endpoint endpoint))
                 (cl-every (lambda (key)
                             (let ((value (alist-get key body)))
                               (and (stringp value)
                                    (<= 1 (length value) 4096))))
                           '(workspace_id document_id path wire_url wire_route))
                 (or (null (alist-get 'expected_revision body))
                     (and (integerp (alist-get 'expected_revision body))
                          (>= (alist-get 'expected_revision body) 0)))
                 (cl-every (lambda (key)
                             (let ((value (alist-get key body)))
                               (and (integerp value) (>= value 0))))
                           '(created_at updated_at))
                 (stringp source)
                 (<= (string-bytes source)
                     gsmlg-org-note-org--capture-journal-max-source-bytes)
                 (stringp digest)
                 (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" digest)
                 (equal digest (secure-hash 'sha256 source))
                 (assq 'lease_proofs body)
                 (null (cdr (assq 'lease_proofs body)))
                 (equal (alist-get 'wire_method body) "PUT")
                 (eq (alist-get 'wire_query body) :null)
                 (vectorp headers)
                 (cl-every (lambda (entry)
                             (and (vectorp entry) (= (length entry) 2)
                                  (stringp (aref entry 0))
                                  (stringp (aref entry 1))))
                           headers)
                 (equal headers
                        [["Accept" "application/json"]
                         ["Content-Type"
                          "application/json; charset=utf-8"]])
                 wire-body
                 (<= (string-bytes wire-body)
                     gsmlg-org-note-org--capture-journal-max-wire-bytes)
                 (equal wire-body-base64 (base64-encode-string wire-body t))
                 (equal (secure-hash 'sha256 wire-body)
                        (alist-get 'wire_body_sha256 body)))
      (user-error "Org Note Capture journal values are invalid"))
    (gsmlg-org-note-org--capture-journal-validate-wire
     wire-body operation-id
     (alist-get 'workspace_id body)
     (alist-get 'path body)
     (alist-get 'expected_revision body))
    (let* ((empty (gsmlg-org-note-org--capture-journal-empty-object))
           (canonical (copy-tree body)))
      (setcdr (assq 'lease_proofs canonical) empty)
      canonical)))

(defun gsmlg-org-note-org--capture-journal-read (file)
  "Read and validate Capture recovery FILE, or quarantine and refuse it."
  (let ((directory (gsmlg-org-note-org--capture-journal-safe-directory)))
    (unless (and directory
                 (equal (file-name-directory (expand-file-name file))
                        (file-name-as-directory directory)))
      (user-error "Org Note Capture journal path is outside recovery storage"))
    (condition-case err
        (let ((attributes (file-attributes file 'integer)))
          (unless (and attributes
                       (null (file-attribute-type attributes))
                       (not (file-symlink-p file))
                       (= (file-attribute-user-id attributes) (user-uid))
                       (= (logand (file-modes file) #o777) #o600)
                       (<= (file-attribute-size attributes)
                           gsmlg-org-note-org--capture-journal-max-record-bytes))
            (user-error "Org Note Capture journal file is unsafe"))
          (let* ((raw (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally file)
                        (buffer-string)))
                 (record (json-parse-string
                          (decode-coding-string raw 'utf-8)
                          :object-type 'alist :array-type 'array
                          :null-object :null :false-object :false)))
            (unless (and (equal (mapcar #'car record)
                                '(schema_version checksum body))
                         (= (alist-get 'schema_version record)
                            gsmlg-org-note-org--capture-journal-schema-version)
                         (stringp (alist-get 'checksum record)))
              (user-error "Org Note Capture journal framing is invalid"))
            (let* ((body (gsmlg-org-note-org--capture-journal-normalize-body
                          (alist-get 'body record)))
                   (body-bytes (gsmlg-org-note-org--capture-journal-json body))
                   (canonical
                    (gsmlg-org-note-org--capture-journal-json
                     `((schema_version
                        . ,gsmlg-org-note-org--capture-journal-schema-version)
                       (checksum . ,(alist-get 'checksum record))
                       (body . ,body)))))
              (unless (and (equal (alist-get 'checksum record)
                                  (secure-hash 'sha256 body-bytes))
                           (equal raw canonical)
                           (equal (file-name-nondirectory file)
                                  (concat (alist-get 'operation_id body)
                                          ".json")))
                (user-error "Org Note Capture journal checksum is invalid"))
              (let* ((headers (alist-get 'wire_headers body))
                     (frozen-body
                      (base64-decode-string
                       (alist-get 'wire_body_base64 body)))
                     (state (intern (alist-get 'state body))))
                (list
                 :state (if (eq state 'staged) 'ambiguous state)
                 :operation-id (alist-get 'operation_id body)
                 :workspace-id (alist-get 'workspace_id body)
                 :document-id (alist-get 'document_id body)
                 :path (alist-get 'path body)
                 :expected-revision (alist-get 'expected_revision body)
                 :created-at (alist-get 'created_at body)
                 :source (alist-get 'source body)
                 :digest (alist-get 'source_digest body)
                 :frozen
                 (list :method (alist-get 'wire_method body)
                       :endpoint (alist-get 'endpoint body)
                       :url (alist-get 'wire_url body)
                       :route (alist-get 'wire_route body)
                       :query nil
                       :headers
                       (mapcar (lambda (entry)
                                 (cons (aref entry 0) (aref entry 1)))
                               headers)
                       :body frozen-body
                       :body-sha256 (alist-get 'wire_body_sha256 body)
                       :redaction-secrets nil))))))
      (error
       (gsmlg-org-note-org--capture-journal-quarantine file)
       (user-error "Org Note Capture journal is unsafe: %s"
                   (error-message-string err))))))

(defun gsmlg-org-note-org-check-capture-recovery ()
  "Perform a local-only startup check for Capture recovery records."
  (interactive)
  (setq gsmlg-org-note-org--capture-recovery-required nil)
  (condition-case err
      (let ((directory (gsmlg-org-note-org--capture-journal-safe-directory)))
        (when directory
          (dolist (file (directory-files directory t "\\`[^.].*\\.json\\'"))
            (push (gsmlg-org-note-org--capture-journal-read file)
                  gsmlg-org-note-org--capture-recovery-required))))
    (error
     (setq gsmlg-org-note-org--capture-recovery-required
           (list :blocked (error-message-string err)))))
  gsmlg-org-note-org--capture-recovery-required)

(defun gsmlg-org-note-org--state-string-valid-p (value)
  "Return non-nil when VALUE is a valid Org Note TODO state string."
  (and (stringp value)
       (not (string-empty-p value))
       (not (string-match-p "[[:space:]\n\r\t\f\v|()]" value))))

(defun gsmlg-org-note-org--fast-key-char-valid-p (key)
  "Return non-nil when KEY is a printable non-reserved Org fast-selection char."
  (and (characterp key)
       (not (memq key '(?! ?@ ?/ ?\s ?\t ?\n ?\r ?\f ?\v)))
       (>= key 33)
       (<= key 126)))

(defun gsmlg-org-note-org--format-keyword (state fast-keys)
  "Return STATE, or STATE(key) when FAST-KEYS maps a character to STATE.

FAST-KEYS uses bridge polarity (CHAR . STATE).  Embedding the key in the
keyword string lets Org parse the explicit mapping during temporary setup."
  (let ((entry (rassoc state fast-keys)))
    (if entry
        (format "%s(%c)" state (car entry))
      state)))

(defun gsmlg-org-note-org--fast-keys-to-org-alist (fast-keys)
  "Convert bridge FAST-KEYS (CHAR . STATE) to Org `org-todo-key-alist' polarity.

Org stores (STATE . CHAR).  Bridge defcustom
`gsmlg-org-note-state-fast-keys' keeps (CHAR . STATE); convert only at
the precompute/publish/apply-buffer boundary."
  (mapcar (lambda (pair) (cons (cdr pair) (car pair))) fast-keys))

(defun gsmlg-org-note-org--validate-state-configuration
    (todo-states done-states fast-keys archive-target)
  "Validate the four-field state tuple and return precomputed keyword tables.

TODO-STATES and DONE-STATES are the active and completed state lists.
FAST-KEYS maps selection characters to states, and ARCHIVE-TARGET is the
completed state used for archive operations.
Signals `user-error' without mutating defcustoms or live buffers.
`:key-alist' in the returned plist is Org-native (STATE . CHAR)."
  (require 'org)
  (unless (and (listp todo-states) todo-states
               (cl-every #'gsmlg-org-note-org--state-string-valid-p todo-states))
    (user-error "Org Note active TODO states are invalid"))
  (unless (and (listp done-states) done-states
               (cl-every #'gsmlg-org-note-org--state-string-valid-p done-states))
    (user-error "Org Note done TODO states are invalid"))
  (let ((all (append todo-states done-states)))
    (unless (= (length all) (length (cl-delete-duplicates (copy-sequence all)
                                                          :test #'equal)))
      (user-error "Org Note TODO states must be unique and disjoint"))
    (unless (and (stringp archive-target)
                 (member archive-target done-states))
      (user-error "Org Note archive target must be a configured done state"))
    (unless (listp fast-keys)
      (user-error "Org Note state fast keys must be an alist"))
    (let ((seen-keys nil)
          (bridge-keys nil))
      (dolist (entry fast-keys)
        (unless (and (consp entry)
                     (gsmlg-org-note-org--fast-key-char-valid-p (car entry))
                     (member (cdr entry) all))
          (user-error "Org Note state fast keys are invalid"))
        (when (memq (car entry) seen-keys)
          (user-error "Org Note state fast keys must be unique"))
        (push (car entry) seen-keys)
        (push (cons (car entry) (cdr entry)) bridge-keys))
      (setq bridge-keys (nreverse bridge-keys))
      (let* ((sequence
              (append '("sequence")
                      (mapcar (lambda (state)
                                (gsmlg-org-note-org--format-keyword
                                 state bridge-keys))
                              todo-states)
                      '("|")
                      (mapcar (lambda (state)
                                (gsmlg-org-note-org--format-keyword
                                 state bridge-keys))
                              done-states)))
             (keywords (list sequence))
             (prior-keywords (default-value 'org-todo-keywords))
             (built-alist nil)
             (built-trigger nil))
        ;; Org reads `(default-value 'org-todo-keywords)' inside
        ;; `org-set-regexps-and-options'; buffer-local setq is ignored.
        (unwind-protect
            (progn
              (setq-default org-todo-keywords keywords)
              (with-temp-buffer
                (delay-mode-hooks (org-mode))
                (org-set-regexps-and-options)
                ;; Prove Org accepts the union and every explicit mapping.
                ;; Org may auto-assign keys for unmapped states; reject
                ;; missing/remapped explicit keys, then strip extras.
                (dolist (pair bridge-keys)
                  (let* ((ch (car pair))
                         (state (cdr pair))
                         (found (assoc state org-todo-key-alist)))
                    (unless (member state org-todo-keywords-1)
                      (user-error "Org Note fast key target %s is not a TODO keyword"
                                  state))
                    (unless (and found (eq (cdr found) ch))
                      (user-error "Org Note fast-key map failed Org round-trip"))))
                ;; Normalized Org-native map: explicit mappings only.
                ;; Empty bridge map installs no trigger.
                (setq built-alist
                      (gsmlg-org-note-org--fast-keys-to-org-alist bridge-keys)
                      built-trigger (and built-alist t))
                (dolist (pair bridge-keys)
                  (unless (eq (cdr (assoc (cdr pair) built-alist)) (car pair))
                    (user-error "Org Note fast-key map failed Org round-trip")))
                (list :sequence sequence
                      :keywords keywords
                      :key-alist (copy-sequence built-alist)
                      :key-trigger built-trigger
                      :todo-states (copy-sequence todo-states)
                      :done-states (copy-sequence done-states)
                      :fast-keys (copy-tree fast-keys)
                      :archive-target archive-target)))
          (setq-default org-todo-keywords prior-keywords))))))

(defun gsmlg-org-note-org--bridge-buffer-p ()
  "Return non-nil when the current buffer is a bridge-owned Org presentation."
  (or (and (boundp 'org-note-document-mode)
           (derived-mode-p 'org-note-document-mode))
      (and (stringp buffer-file-name)
           (or (equal buffer-file-name (gsmlg-org-note-org-feed-file))
               (string-prefix-p
                (file-name-as-directory (expand-file-name gsmlg-cache-directory))
                (file-name-as-directory
                 (file-name-directory (expand-file-name buffer-file-name))))))
      (and (stringp (buffer-name))
           (or (string-prefix-p "*Org Note" (buffer-name))
               (string-match-p "Org Note" (buffer-name))))))

(defun gsmlg-org-note-org--apply-keywords-in-buffer (precomputed)
  "Install PRECOMPUTED keyword tables in the current Org buffer.

`:key-alist' must already be Org-native (STATE . CHAR)."
  (when (derived-mode-p 'org-mode)
    (setq-local org-todo-keywords (plist-get precomputed :keywords))
    (org-set-regexps-and-options)
    ;; Overwrite Org auto-assigned extras with the normalized explicit map.
    (setq-local org-todo-key-alist (copy-sequence (plist-get precomputed :key-alist))
                org-todo-key-trigger (plist-get precomputed :key-trigger))))

(defun gsmlg-org-note-org--recompute-live-buffers (precomputed)
  "Recompute keyword tables in every live bridge Org buffer from PRECOMPUTED."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (gsmlg-org-note-org--bridge-buffer-p)
        (gsmlg-org-note-org--apply-keywords-in-buffer precomputed)))))

(defun gsmlg-org-note-org--publish-keyword-defaults (precomputed)
  "Publish PRECOMPUTED keyword tables as Org defaults and buffer locals.

`org-set-regexps-and-options' is a no-op outside `org-mode' and reads
`(default-value \\='org-todo-keywords)', so install must set defaults and
recompute inside a temporary Org buffer.  `:key-alist' is Org-native
\=(STATE . CHAR); bridge (CHAR . STATE) polarity stays on the defcustom."
  (let ((keywords (plist-get precomputed :keywords))
        (key-alist (copy-sequence (plist-get precomputed :key-alist)))
        (key-trigger (plist-get precomputed :key-trigger)))
    (setq-default org-todo-keywords keywords)
    (setq org-todo-keywords keywords)
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (org-set-regexps-and-options)
      (setq-default org-todo-keywords-1 (copy-sequence org-todo-keywords-1)
                    org-done-keywords (copy-sequence org-done-keywords)
                    org-not-done-keywords (copy-sequence org-not-done-keywords)
                    org-todo-key-alist (copy-sequence key-alist)
                    org-todo-key-trigger key-trigger))
    (setq-default org-todo-key-alist (copy-sequence key-alist)
                  org-todo-key-trigger key-trigger)
    (setq org-todo-key-alist (copy-sequence key-alist)
          org-todo-key-trigger key-trigger)))

(defun gsmlg-org-note-org--install-todo-keywords ()
  "Install the validated current state configuration into Org."
  (require 'org)
  (let ((precomputed
         (gsmlg-org-note-org--validate-state-configuration
          gsmlg-org-note-todo-states
          gsmlg-org-note-done-states
          gsmlg-org-note-state-fast-keys
          gsmlg-org-note-archive-target)))
    (gsmlg-org-note-org--publish-keyword-defaults precomputed)
    (gsmlg-org-note-org--recompute-live-buffers precomputed)
    precomputed))

(defun gsmlg-org-note-apply-state-configuration
    (todo-states done-states fast-keys archive-target)
  "Atomically commit the four Org Note TODO configuration fields.

TODO-STATES and DONE-STATES are the active and completed state lists.
FAST-KEYS maps selection characters to states, and ARCHIVE-TARGET is the
completed state used for archive operations.
Validates once, precomputes keyword tables, commits all values, then
recomputes live bridge buffers.  Any failure restores the prior
defcustom values and republishes/recomputes the prior keyword tables in
defaults and live bridge buffers."
  (let* ((prior-todo gsmlg-org-note-todo-states)
         (prior-done gsmlg-org-note-done-states)
         (prior-keys gsmlg-org-note-state-fast-keys)
         (prior-archive gsmlg-org-note-archive-target)
         (prior-precomputed
          (gsmlg-org-note-org--validate-state-configuration
           prior-todo prior-done prior-keys prior-archive))
         (precomputed
          (gsmlg-org-note-org--validate-state-configuration
           todo-states done-states fast-keys archive-target)))
    (condition-case err
        (progn
          (setq gsmlg-org-note-todo-states
                (plist-get precomputed :todo-states)
                gsmlg-org-note-done-states
                (plist-get precomputed :done-states)
                gsmlg-org-note-state-fast-keys
                (plist-get precomputed :fast-keys)
                gsmlg-org-note-archive-target
                (plist-get precomputed :archive-target))
          (gsmlg-org-note-org--publish-keyword-defaults precomputed)
          (gsmlg-org-note-org--recompute-live-buffers precomputed)
          t)
      (error
       (setq gsmlg-org-note-todo-states prior-todo
             gsmlg-org-note-done-states prior-done
             gsmlg-org-note-state-fast-keys prior-keys
             gsmlg-org-note-archive-target prior-archive)
       (condition-case restore-err
           (progn
             (gsmlg-org-note-org--publish-keyword-defaults prior-precomputed)
             (gsmlg-org-note-org--recompute-live-buffers prior-precomputed))
         (error
          (signal 'error
                  (list
                   (format
                    "Org Note state apply failed (%s); restore also failed (%s)"
                    (error-message-string err)
                    (error-message-string restore-err))))))
       (signal (car err) (cdr err))))))

(defun gsmlg-org-note-org--set-todo-states (symbol value)
  "Defcustom setter for SYMBOL with candidate VALUE."
  (gsmlg-org-note-apply-state-configuration
   value
   gsmlg-org-note-done-states
   gsmlg-org-note-state-fast-keys
   gsmlg-org-note-archive-target)
  (set-default symbol gsmlg-org-note-todo-states))

(defun gsmlg-org-note-org--set-done-states (symbol value)
  "Defcustom setter for SYMBOL with candidate VALUE."
  (gsmlg-org-note-apply-state-configuration
   gsmlg-org-note-todo-states
   value
   gsmlg-org-note-state-fast-keys
   gsmlg-org-note-archive-target)
  (set-default symbol gsmlg-org-note-done-states))

(defun gsmlg-org-note-org--set-fast-keys (symbol value)
  "Defcustom setter for SYMBOL with candidate VALUE."
  (gsmlg-org-note-apply-state-configuration
   gsmlg-org-note-todo-states
   gsmlg-org-note-done-states
   value
   gsmlg-org-note-archive-target)
  (set-default symbol gsmlg-org-note-state-fast-keys))

(defun gsmlg-org-note-org--set-archive-target (symbol value)
  "Defcustom setter for SYMBOL with candidate VALUE."
  (gsmlg-org-note-apply-state-configuration
   gsmlg-org-note-todo-states
   gsmlg-org-note-done-states
   gsmlg-org-note-state-fast-keys
   value)
  (set-default symbol gsmlg-org-note-archive-target))

(defcustom gsmlg-org-note-todo-states '("TODO" "RUNNING")
  "Active Org Note TODO state strings used by the bridge."
  :type '(repeat string)
  :initialize #'custom-initialize-default
  :set #'gsmlg-org-note-org--set-todo-states
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-done-states '("DONE")
  "Done Org Note TODO state strings used by the bridge."
  :type '(repeat string)
  :initialize #'custom-initialize-default
  :set #'gsmlg-org-note-org--set-done-states
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-state-fast-keys nil
  "Optional alist mapping unique characters to configured TODO states.

Entries use bridge polarity (CHAR . STATE).  Installation converts to
Org-native `org-todo-key-alist' polarity (STATE . CHAR)."
  :type '(alist :key-type character :value-type string)
  :initialize #'custom-initialize-default
  :set #'gsmlg-org-note-org--set-fast-keys
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-archive-target "DONE"
  "Configured done state used as the item archive target."
  :type 'string
  :initialize #'custom-initialize-default
  :set #'gsmlg-org-note-org--set-archive-target
  :group 'gsmlg-org-note-org)

(with-eval-after-load 'org
  (when gsmlg-org-note-org-enable
    (gsmlg-org-note-org--install-todo-keywords)))

(defun gsmlg-org-note-org--ordered-states ()
  "Return configured active states followed by done states."
  (append gsmlg-org-note-todo-states gsmlg-org-note-done-states))

(defun gsmlg-org-note-org--cycle-state (current direction)
  "Return the next/previous configured state from CURRENT.

DIRECTION is `forward' or `backward'.  Never cycles to an empty state."
  (let* ((states (gsmlg-org-note-org--ordered-states))
         (len (length states))
         (idx (or (cl-position current states :test #'equal) -1))
         (next (pcase direction
                 ('forward (mod (1+ idx) len))
                 ('backward (mod (1- (if (< idx 0) 0 idx)) len))
                 (_ (user-error "Invalid TODO cycle direction")))))
    (nth next states)))

(defun gsmlg-org-note-org--read-fast-todo-key ()
  "Read one fast-selection character for bridge TODO targeting."
  (let* ((prompt
          (mapconcat
           (lambda (pair)
             (format "[%c] %s" (car pair) (cdr pair)))
           gsmlg-org-note-state-fast-keys
           " "))
         (ch (read-char-exclusive (concat "Org Note TODO: " prompt " "))))
    ch))

(defun gsmlg-org-note-org--resolve-fast-key (ch)
  "Resolve character CH through `gsmlg-org-note-state-fast-keys'."
  (let ((state (alist-get ch gsmlg-org-note-state-fast-keys)))
    (unless state
      (user-error "Unknown Org Note TODO fast key"))
    state))

(defun gsmlg-org-note-org--resolve-todo-target
    (arg current-state &optional interactive-p)
  "Resolve Org TODO ARG to a configured server state string.

CURRENT-STATE is the authoritative current keyword.  INTERACTIVE-P is
non-nil for interactive nil calls that may open the fast-key prompt.
Does not compare equality with CURRENT-STATE; callers do that after
preflight."
  (gsmlg-org-note-org--validate-state-configuration
   gsmlg-org-note-todo-states
   gsmlg-org-note-done-states
   gsmlg-org-note-state-fast-keys
   gsmlg-org-note-archive-target)
  (cond
   ((memq arg '(none nextset previousset))
    (user-error "Org Note bridge does not support TODO argument %S" arg))
   ((equal arg "")
    (user-error "Org Note bridge does not support clearing TODO state"))
   ((and (consp arg) (memq (car arg) '(4 16 64)))
    (user-error "Org Note bridge does not support TODO argument %S" arg))
   ((and (integerp arg) (< arg 0))
    (user-error "Org Note bridge does not support TODO repeater cancel"))
   ((eq arg 'done)
    (car gsmlg-org-note-done-states))
   ((and (integerp arg) (> arg 0))
    (let ((states (gsmlg-org-note-org--ordered-states)))
      (unless (<= arg (length states))
        (user-error "Org Note TODO prefix %s is out of range" arg))
      (nth (1- arg) states)))
   ((or (eq arg 0) (eq arg 'right))
    (gsmlg-org-note-org--cycle-state current-state 'forward))
   ((eq arg 'left)
    (gsmlg-org-note-org--cycle-state current-state 'backward))
   ((stringp arg)
    (unless (member arg (gsmlg-org-note-org--ordered-states))
      (user-error "Unknown Org Note TODO state: %s" arg))
    arg)
   ((null arg)
    (let ((fast-enabled
           (memq org-use-fast-todo-selection '(auto t expert))))
      (if (and interactive-p
               fast-enabled
               gsmlg-org-note-state-fast-keys)
          (gsmlg-org-note-org--resolve-fast-key
           (gsmlg-org-note-org--read-fast-todo-key))
        (gsmlg-org-note-org--cycle-state current-state 'forward))))
   (t
    (user-error "Unsupported Org Note TODO argument: %S" arg))))

(defconst gsmlg-org-note-org--feed-tag "ORGNOTE"
  "Tag marking generated Org Note feed headings.")

(defconst gsmlg-org-note-org--feed-schema-version 1
  "Schema version written into generated agenda feed headers.")

(defconst gsmlg-org-note-org--agenda-page-limit 100
  "Explicit page size for exhaustive Org Note agenda view fetches.")
(defvar gsmlg-org-note-org--feed-file
  (gsmlg-cache-file "org-note-agenda-feed.org")
  "Generated Org file exposing Org Note scheduled and deadline items.

This is the last-good snapshot path for the current process.  Phase 1
uses a single-process write.  Multi-process publication reservation
locks (spec blockers 69, 74, 79) are deferred to Phase 7.")

(defvar gsmlg-org-note-org--selected-feed-file nil
  "Feed path currently selected for `org-agenda-files'.

May be the last-good snapshot or an endpoint-keyed empty feed.")

(defvar gsmlg-org-note-org--last-workspace-ids nil
  "Workspace IDs used for the current feed snapshot.")

(defvar gsmlg-org-note-org--refresh-active nil
  "Non-nil while `gsmlg-org-note-org-refresh-feed' is running.")

(defvar gsmlg-org-note-org--guards-installed nil
  "Non-nil after cold-start advice has been installed.")

(defvar gsmlg-org-note-org--activated nil
  "Non-nil after `gsmlg-org-note-org-activate' has completed.")

(defvar gsmlg-org-note-org--activating nil
  "Reentrancy guard while activating the bridge.")

(defvar gsmlg-org-note-org--feed-hooks-installed nil
  "Non-nil after feed refresh and goto advice are installed.")

(defvar gsmlg-org-note-org--mutation-hooks-installed nil
  "Non-nil after TODO/refile/archive/clock refuse-or-bridge advice is installed.")

(defvar gsmlg-org-note-org--transition-ambiguities (make-hash-table :test #'equal)
  "In-memory fail-closed ambiguity records keyed by (workspace . item).")

(defvar gsmlg-org-note-org--frozen-transitions (make-hash-table :test #'equal)
  "In-memory frozen transition envelopes for same-process replay.")

(defvar gsmlg-org-note-org--document-ambiguities (make-hash-table :test #'equal)
  "In-memory fail-closed document PUT ambiguity records keyed by document-id.")

(defvar gsmlg-org-note-org--clock-presentation nil
  "Active bridge clock presentation and registered lease metadata.")

(defvar gsmlg-org-note-org--clock-ambiguities (make-hash-table :test #'equal)
  "Frozen claim or release attempts awaiting same-operation-id recovery.")

(defvar gsmlg-org-note-org--archive-ambiguities (make-hash-table :test #'equal)
  "Frozen whole-document archive attempts awaiting explicit replay.")

(defconst gsmlg-org-note-org--archive-page-limit 100
  "Maximum rows requested per archive reconciliation page.")

(defun gsmlg-org-note-org--archive-list-documents (workspace-id)
  "Exhaustively list archived documents in WORKSPACE-ID with bounded paging."
  (require 'org-note-validation)
  (org-note-validation-bounded-pager-fold
   (org-note-validation-bounded-pager-state :limit gsmlg-org-note-org--archive-page-limit)
   (lambda (cursor)
     (let* ((response (org-note-operation-list-documents
                       workspace-id :cursor cursor
                       :limit gsmlg-org-note-org--archive-page-limit
                       :include-archived t))
            (data (or (alist-get 'data response) response))
            (rows (or (alist-get 'documents data)
                      (alist-get 'items data)
                      (alist-get 'rows data)))
            (next (or (alist-get 'next_cursor data)
                      (alist-get 'next_cursor response))))
       (when (vectorp rows)
         (setq rows (append rows nil)))
       (unless (listp rows)
         (user-error "Org Note archive reconciliation returned malformed rows"))
       (list :rows rows :next-cursor next)))))

(defun gsmlg-org-note-org--archive-row (rows document-id path)
  "Find the unique archived DOCUMENT-ID at PATH in ROWS."
  (let ((matches (cl-remove-if-not
                  (lambda (row)
                    (and (equal (alist-get 'id row) document-id)
                         (equal (alist-get 'path row) path)))
                  rows)))
    (unless (= (length matches) 1)
      (user-error "Org Note archive reconciliation did not find a unique document"))
    (let* ((row (car matches))
           (archived-at (alist-get 'archived_at row))
           (revision (alist-get 'revision row)))
      (unless (and (integerp archived-at) (> archived-at 0)
                   (integerp revision))
        (user-error "Org Note archive reconciliation returned invalid metadata"))
      row)))

(defun gsmlg-org-note-org--archive-response-revision (response _document-id)
  "Return the archived document revision from POST RESPONSE when present."
  (or (alist-get 'revision response)
      (alist-get 'document_revision response)))

(defun gsmlg-org-note-org--finish-archive-attempt (record response)
  "Validate archived RESPONSE and clean up the document described by RECORD."
  (let* ((workspace-id (plist-get record :workspace-id))
         (document-id (plist-get record :document-id))
         (path (plist-get record :path))
         (expected (plist-get record :expected-revision))
         (post-revision (gsmlg-org-note-org--archive-response-revision
                         response document-id))
         (rows (gsmlg-org-note-org--archive-list-documents workspace-id))
         (row (gsmlg-org-note-org--archive-row rows document-id path))
         (revision (alist-get 'revision row)))
    (unless (and (or (null post-revision)
                     (and (integerp post-revision) (> post-revision expected)))
                 (> revision expected))
      (user-error "Org Note archive revision did not advance"))
    (remhash document-id gsmlg-org-note-org--archive-ambiguities)
    (let ((buffer (plist-get record :buffer))
          (list-buffer (plist-get record :list-buffer)))
      (condition-case err
          (progn
            (when (buffer-live-p list-buffer)
              (with-current-buffer list-buffer
                (when (fboundp 'org-note--refresh-document-list-buffer)
                  (org-note--refresh-document-list-buffer list-buffer))))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (org-note-document--kill-buffer-safely)))
            response)
        ((quit error)
         (message "Org Note archive committed; cleanup pending (%s)"
                  (error-message-string err))
         response)))))

(defun gsmlg-org-note-org-retry-ambiguous-archive (document-id)
  "Replay frozen whole-document archive for DOCUMENT-ID."
  (interactive "sDocument ID: ")
  (let ((record (gethash document-id gsmlg-org-note-org--archive-ambiguities)))
    (unless record
      (user-error "No replayable Org Note archive for %s" document-id))
    (gsmlg-org-note-org--finish-archive-attempt
     record (org-note-operation--dispatch-frozen (plist-get record :frozen)))))

(defun gsmlg-org-note-org--archive-document ()
  "Archive the current Org Note document through one frozen operation."
  (require 'org-note-operation)
  (org-note-document--require-metadata)
  (when (buffer-modified-p)
    (user-error "Save or discard Org Note document edits before archive"))
  (let* ((workspace-id org-note-document-workspace-id)
         (document-id org-note-document-id)
         (path org-note-document-path)
         (expected org-note-document-revision)
         (operation-id (org-note-client-new-operation-id))
         (typed (list :method "POST"
                      :route (format "/api/org/documents/%s/archive"
                                     (org-note-operation--path-segment document-id))
                      :query nil
                      :body (org-note-operation--mutation-body
                             workspace-id `((expected_revision . ,expected)) operation-id)))
         (frozen (org-note-operation--freeze-request typed))
         (record (list :operation-id operation-id :frozen frozen
                       :workspace-id workspace-id :document-id document-id
                       :path path :expected-revision expected
                       :buffer (current-buffer)
                       :list-buffer (get-buffer
                                     (org-note--document-buffer-name workspace-id))))
         (dispatched nil))
    (when (gethash document-id gsmlg-org-note-org--archive-ambiguities)
      (user-error "Org Note archive for %s is ambiguous; resolve before retrying" document-id))
    (condition-case err
        (progn
          (setq dispatched t)
          (gsmlg-org-note-org--finish-archive-attempt
           record (org-note-operation--dispatch-frozen frozen)))
      ((quit error)
       (let* ((data (cdr err))
              (props (and (listp (car data)) (car data)))
              (status (plist-get props :status)))
         (if (and (eq (car err) 'org-note-http-error) (= status 409))
             (progn
               (message "Org Note archive conflict; no archive was committed")
               (signal (car err) (cdr err)))
           (when dispatched
             (puthash document-id record gsmlg-org-note-org--archive-ambiguities))
           (signal (car err) (cdr err))))))))

(defun gsmlg-org-note-org--clock-claim-response-validator
    (response workspace-id item-id document-id expected-revision kind operation-id)
  "Validate a claim RESPONSE.

Require identity fields WORKSPACE-ID, ITEM-ID, DOCUMENT-ID, KIND, and
OPERATION-ID to match, and require its document revision to be at least
EXPECTED-REVISION."
  (org-note-operation--validate-claim-response
   response workspace-id item-id document-id kind operation-id)
  (let* ((context (gsmlg-org-note-org--context-field
                   (gsmlg-org-note-org--context-field response 'context)
                   'document))
         (revision (gsmlg-org-note-org--context-field context 'revision)))
    (unless (and (integerp revision) (>= revision expected-revision))
      (user-error "Org Note claim response revision is older than preflight")))
  response)

(defun gsmlg-org-note-org--clock-register-presentation
    (record response lease)
  "Register LEASE and presentation from successful claim RESPONSE and RECORD."
  (setq gsmlg-org-note-org--clock-presentation
        (list :workspace-id (plist-get record :workspace-id)
              :item-id (plist-get record :item-id)
              :document-id (plist-get record :document-id)
              :kind (plist-get record :kind)
              :operation-id (plist-get record :operation-id)
              :started-at (or (plist-get record :started-at) (float-time))
              :response response
              :lease lease)))

(defun gsmlg-org-note-org-retry-ambiguous-clock (operation-id)
  "Replay frozen clock attempt OPERATION-ID after an ambiguous outcome."
  (interactive "sClock operation ID: ")
  (let ((record (gethash operation-id gsmlg-org-note-org--clock-ambiguities)))
    (unless record
      (user-error "No replayable Org Note clock operation %s" operation-id))
    (let ((response (org-note-operation--dispatch-frozen
                     (plist-get record :frozen))))
      (if (eq (plist-get record :action) 'claim)
          (progn
            (gsmlg-org-note-org--clock-claim-response-validator
             response (plist-get record :workspace-id)
             (plist-get record :item-id) (plist-get record :document-id)
             (plist-get record :expected-revision) (plist-get record :kind)
             operation-id)
            (let ((lease (org-note-operation-register-claim
                          (plist-get record :workspace-id)
                          (plist-get record :item-id)
                          (plist-get record :document-id)
                          (plist-get record :kind) response)))
              (remhash operation-id gsmlg-org-note-org--clock-ambiguities)
              (gsmlg-org-note-org--clock-register-presentation
               record response lease)
              (message "Org Note clock started")))
        (let* ((context (org-note-operation-get-item-context
                         (plist-get record :workspace-id)
                         (plist-get record :item-id)))
               (lease (gsmlg-org-note-org--context-field
                       (gsmlg-org-note-org--context-field context 'data)
                       'context 'lease)))
          (when (and lease
                     (equal (gsmlg-org-note-org--context-field lease 'id)
                            (plist-get record :lease-id)))
            (user-error "Org Note clock release remains unresolved"))
          (org-note-operation-forget-lease
           (plist-get record :workspace-id) (plist-get record :item-id)
           (plist-get record :kind))
          (remhash operation-id gsmlg-org-note-org--clock-ambiguities)
          (setq gsmlg-org-note-org--clock-presentation nil)
          (message "Org Note clock stopped"))))))

(defun gsmlg-org-note-org--clock-reconcile ()
  "Return the active clock presentation when its lease remains registered."
  (let ((clock gsmlg-org-note-org--clock-presentation))
    (when clock
      (unless (org-note-operation-find-lease
               (plist-get clock :workspace-id)
               (plist-get clock :item-id)
               (plist-get clock :kind))
        (setq gsmlg-org-note-org--clock-presentation nil)
        (user-error "Org Note clock lease is no longer active")))
    gsmlg-org-note-org--clock-presentation))

(defvar gsmlg-org-note-org--agenda-command-active nil
  "Non-nil while the outermost guarded Agenda producer is running.")

(defconst gsmlg-org-note-org--plain-local-refuse-fmt
  "Org Note bridge refuses %s in plain local .org buffers; use Org Note documents or agenda."
  "User-error format for plain-local refuse.")

(defconst gsmlg-org-note-org--agenda-entrypoints
  '(org-agenda
    org-agenda-list
    org-todo-list
    org-tags-view
    org-search-view
    org-agenda-list-stuck-projects
    org-occur-in-agenda-files
    org-store-agenda-views
    org-agenda-redo
    org-agenda-redo-all)
  "Public Agenda producers guarded against cold-start local-file access.")

;;;###autoload
(defun gsmlg-org-note-org-install-guards ()
  "Install inert around-advice on Org agenda and capture entrypoints.

This must not load Org Note or perform network I/O."
  (unless gsmlg-org-note-org--guards-installed
    (setq gsmlg-org-note-org--guards-installed t)
    (dolist (command gsmlg-org-note-org--agenda-entrypoints)
      (autoload command "org-agenda" nil t)
      (advice-add command :around #'gsmlg-org-note-org--around-agenda))
    (autoload 'org-agenda-files "org" nil nil)
    (advice-add #'org-agenda-files
                :around #'gsmlg-org-note-org--around-agenda-files)
    (autoload 'org-capture "org-capture" nil t)
    (advice-add #'org-capture :around #'gsmlg-org-note-org--around-capture)
    (gsmlg-org-note-org--install-mutation-hooks)))

(defun gsmlg-org-note-org-feed-file ()
  "Return the selected Org Note agenda feed path."
  (or gsmlg-org-note-org--selected-feed-file
      gsmlg-org-note-org--feed-file))

(defun gsmlg-org-note-org-agenda-files ()
  "Return agenda files for the active Org Note bridge.

The list contains exactly the selected feed path (or empty-feed path)."
  (list (gsmlg-org-note-org-feed-file)))

;;;###autoload
(defun gsmlg-org-note-org-activate ()
  "Activate the Org Note Org bridge once.

May `(require \\='org-note)'.  Idempotent under a reentrancy guard.
Applies feed-only `org-agenda-files' when path settings are available.
Does not truncate a valid last-good snapshot."
  (unless gsmlg-org-note-org-enable
    (user-error "Org Note Org bridge is release-gated and disabled"))
  (unless (or gsmlg-org-note-org--activated
              gsmlg-org-note-org--activating)
    (setq gsmlg-org-note-org--activating t)
    (unwind-protect
        (progn
          (require 'org-note)
          (require 'org)
          (gsmlg-org-note-org--install-todo-keywords)
          (gsmlg-org-note-org--install-feed-hooks)
          (gsmlg-org-note-org--install-mutation-hooks)
          (setq gsmlg-org-note-org--activated t)
          (when (fboundp #'gsmlg-org-apply-path-settings)
            (gsmlg-org-apply-path-settings)))
      (setq gsmlg-org-note-org--activating nil))))

(defun gsmlg-org-note-org--plain-local-org-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a plain local Org file outside the bridge."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'org-mode)
         (not (and (fboundp 'org-note-document-mode)
                   (derived-mode-p 'org-note-document-mode)))
         (not (gsmlg-org-note-org--bridge-buffer-p))
         (or buffer-file-name
             (and (boundp 'buffer-file-truename) buffer-file-truename)))))

(defun gsmlg-org-note-org--refuse-if-plain-local (command-label)
  "Signal `user-error' for COMMAND-LABEL in a plain local Org buffer."
  (when (and gsmlg-org-note-org-enable
             gsmlg-org-note-org--activated
             (gsmlg-org-note-org--plain-local-org-buffer-p))
    (user-error gsmlg-org-note-org--plain-local-refuse-fmt command-label)))

(defun gsmlg-org-note-org--origin-item-ids ()
  "Return (WORKSPACE . ITEM) from Org Note properties at point, or nil.

Reads only `ORG_NOTE_WORKSPACE_ID' / `ORG_NOTE_ITEM_ID' (feed or document
properties).  Never infers identity from the headline title."
  (require 'org)
  (let ((workspace (org-entry-get (point) "ORG_NOTE_WORKSPACE_ID" 'selective))
        (item (org-entry-get (point) "ORG_NOTE_ITEM_ID" 'selective)))
    (and (gsmlg-org-note-org--string-or-nil workspace)
         (gsmlg-org-note-org--string-or-nil item)
         (cons workspace item))))

(defun gsmlg-org-note-org--context-field (context &rest keys)
  "Walk symbol-keyed CONTEXT alist by KEYS."
  (let ((node context))
    (dolist (key keys node)
      (setq node (cdr (assq key (if (listp node) node nil)))))))

(defun gsmlg-org-note-org--preflight-identified-item (workspace-id item-id)
  "Run identified-item preflight for WORKSPACE-ID / ITEM-ID.

Returns a plist with `:workspace-id' `:item-id' `:document-id' `:revision'
`:state' `:lease-proof'.  Signals `user-error' fail-closed on ambiguity,
unsaved document edits, or invalid/mismatched context."
  (require 'org-note-operation)
  (require 'org-id)
  (when (gethash (cons workspace-id item-id)
                 gsmlg-org-note-org--transition-ambiguities)
    (user-error
     "Org Note transition for %s/%s is ambiguous; resolve before retrying"
     workspace-id item-id))
  (when (and (fboundp 'org-note-document-mode)
             (derived-mode-p 'org-note-document-mode)
             (buffer-modified-p))
    (user-error
     "Save or discard Org Note document edits before TODO transition"))
  (let* ((response (org-note-operation-get-item-context workspace-id item-id))
         (data (gsmlg-org-note-org--context-field response 'data))
         (context (gsmlg-org-note-org--context-field data 'context))
         (document (gsmlg-org-note-org--context-field context 'document))
         (item (gsmlg-org-note-org--context-field context 'item))
         (document-id (gsmlg-org-note-org--context-field document 'id))
         (revision (gsmlg-org-note-org--context-field document 'revision))
         (state (gsmlg-org-note-org--context-field item 'state))
         (item-ws (gsmlg-org-note-org--context-field item 'workspace_id))
         (item-doc (gsmlg-org-note-org--context-field item 'document_id))
         (item-id* (gsmlg-org-note-org--context-field item 'id)))
    (unless (and (equal item-ws workspace-id)
                 (equal item-id* item-id)
                 (stringp document-id)
                 (integerp revision)
                 (stringp state))
      (user-error "Org Note item context is invalid for transition"))
    (when (and (fboundp 'org-note-document-mode)
               (derived-mode-p 'org-note-document-mode)
               (boundp 'org-note-document-id)
               (not (equal org-note-document-id document-id)))
      (user-error "Org Note document origin does not match item context"))
    (unless (equal item-doc document-id)
      (user-error "Org Note item context document mismatch"))
    (let* ((lease (org-note-operation-find-lease workspace-id item-id
                                                 "execution"))
           (proof
            (and lease
                 `((lease_id . ,(org-note-operation-lease-lease-id lease))
                   (kind . ,(org-note-operation-lease-kind lease))
                   (fencing_token
                    . ,(org-note-operation-lease-fencing-token lease))))))
      (list :workspace-id workspace-id
            :item-id item-id
            :document-id document-id
            :revision revision
            :state state
            :lease-proof proof))))

(defun gsmlg-org-note-org--mark-transition-ambiguous (workspace-id item-id
                                                      operation-id frozen
                                                      &rest properties)
  "Record an in-memory ambiguity for WORKSPACE-ID and ITEM-ID.

Store OPERATION-ID, FROZEN request data, and additional PROPERTIES."
  (puthash (cons workspace-id item-id)
           (append (list :operation-id operation-id :frozen frozen
                         :workspace-id workspace-id :item-id item-id)
                   properties)
           gsmlg-org-note-org--transition-ambiguities))

(defun gsmlg-org-note-org--mark-document-ambiguous (document-id operation-id
                                                   frozen &rest properties)
  "Record an in-memory document PUT ambiguity for DOCUMENT-ID.

Store OPERATION-ID, FROZEN request data, and additional PROPERTIES."
  (puthash document-id
           (append (list :operation-id operation-id :frozen frozen)
                   properties)
           gsmlg-org-note-org--document-ambiguities))

(defun gsmlg-org-note-org--finish-transition-attempt (record response)
  "Validate and finish the transition attempt in RECORD using RESPONSE."
  (let* ((workspace-id (plist-get record :workspace-id))
         (item-id (plist-get record :item-id))
         (registered-lease (plist-get record :registered-lease))
         (context
          (org-note-operation--validate-transition-response
           response workspace-id item-id
           (plist-get record :document-id)
           (plist-get record :expected-revision)
           (plist-get record :target-state)
           (plist-get record :operation-id)
           (and registered-lease
                (org-note-operation-lease-lease-id registered-lease))
           (and registered-lease
                (org-note-operation-lease-kind registered-lease)))))
    (remhash (cons workspace-id item-id)
             gsmlg-org-note-org--transition-ambiguities)
    (remhash (cons workspace-id item-id)
             gsmlg-org-note-org--frozen-transitions)
    (condition-case refresh-err
        (progn
          (when registered-lease
            (org-note-operation--reconcile-transition-lease
             registered-lease context workspace-id item-id))
          (gsmlg-org-note-org-refresh-feed t)
          (list :committed-p t
                :operation-id (plist-get record :operation-id)
                :response response
                :message nil))
      ((quit error)
       (list :committed-p t
             :operation-id (plist-get record :operation-id)
             :response response
             :message
             (format "transition succeeded; view stale (%s)"
                     (error-message-string refresh-err)))))))

(defun gsmlg-org-note-org-retry-ambiguous-transition (workspace-id item-id)
  "Replay the in-process ambiguous transition for WORKSPACE-ID and ITEM-ID."
  (interactive "sWorkspace ID: \nsItem ID: ")
  (require 'org-note-operation)
  (let* ((key (cons workspace-id item-id))
         (record (gethash key gsmlg-org-note-org--transition-ambiguities)))
    (unless (and record (plist-get record :frozen))
      (user-error "No replayable Org Note transition for %s/%s"
                  workspace-id item-id))
    (gsmlg-org-note-org--finish-transition-attempt
     record
     (org-note-operation--dispatch-frozen (plist-get record :frozen)))))

(defun gsmlg-org-note-org--attempt-identified-transition (target-state origin)
  "Attempt an identified transition to TARGET-STATE from ORIGIN plist.

ORIGIN must include `:workspace-id' and `:item-id'.  Marks committed after
successful validation and before refresh.  Refresh failures return
`:committed-p' with a stale-view message and never re-dispatch.  Pre-commit
errors mark the item ambiguous fail-closed and re-signal."
  (require 'org-note-operation)
  (let* ((workspace-id (plist-get origin :workspace-id))
         (item-id (plist-get origin :item-id))
         (pre (gsmlg-org-note-org--preflight-identified-item
               workspace-id item-id))
         (current (plist-get pre :state)))
    (when (equal target-state current)
      (user-error "Already %s" current))
    (let* ((operation-id (org-note-client-new-operation-id))
           (lease-proof (plist-get pre :lease-proof))
           (registered-lease
            (and lease-proof
                 (org-note-operation--registered-transition-lease
                  workspace-id item-id lease-proof)))
           (typed
            (org-note-operation--transition-typed-request
             workspace-id item-id
             (plist-get pre :document-id)
             (plist-get pre :revision)
             target-state
             :lease lease-proof
             :operation-id operation-id))
           (frozen (org-note-operation--freeze-request typed))
           (record
            (list :operation-id operation-id :frozen frozen
                  :workspace-id workspace-id :item-id item-id
                  :document-id (plist-get pre :document-id)
                  :expected-revision (plist-get pre :revision)
                  :target-state target-state
                  :registered-lease registered-lease))
           (committed-p nil)
           response)
      (puthash (cons workspace-id item-id) frozen
               gsmlg-org-note-org--frozen-transitions)
      (condition-case err
          (progn
            (setq response (org-note-operation--dispatch-frozen frozen))
            (setq committed-p t)
            (gsmlg-org-note-org--finish-transition-attempt record response))
        ((quit error)
         (unless committed-p
           (puthash (cons workspace-id item-id) record
                    gsmlg-org-note-org--transition-ambiguities))
         (signal (car err) (cdr err)))))))

(defun gsmlg-org-note-org--agenda-bulk-or-region-todo-p ()
  "Return non-nil when agenda TODO would be bulk or region scoped."
  (or (and (boundp 'org-agenda-bulk-marked-entries)
           org-agenda-bulk-marked-entries)
      (and (use-region-p)
           (not (eq (region-beginning) (region-end))))
      (and (boundp 'org-agenda-bulk-action)
           org-agenda-bulk-action)))

(defun gsmlg-org-note-org--agenda-row-origin ()
  "Return origin plist for the current agenda row, or signal."
  (require 'org-agenda)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-get-at-bol 'org-marker)))
         (workspace
          (and marker
               (with-current-buffer (marker-buffer marker)
                 (org-entry-get (marker-position marker)
                                "ORG_NOTE_WORKSPACE_ID" 'selective))))
         (item
          (and marker
               (with-current-buffer (marker-buffer marker)
                 (org-entry-get (marker-position marker)
                                "ORG_NOTE_ITEM_ID" 'selective))))
         (state
          (and marker
               (with-current-buffer (marker-buffer marker)
                 (org-entry-get (marker-position marker) "TODO")))))
    (unless (and workspace item)
      (user-error
       "Agenda TODO requires Org Note item ids; refusing native mutation"))
    (list :workspace-id workspace
          :item-id item
          :state (or state "TODO"))))

(defun gsmlg-org-note-org--put-response-revision (response document-id)
  "Return integer revision for DOCUMENT-ID from PUT RESPONSE."
  (let* ((revisions (cdr (assq 'document_revisions response)))
         (value
          (cond
           ((hash-table-p revisions)
            (or (gethash document-id revisions)
                (gethash (intern document-id) revisions)))
           ((listp revisions)
            (or (cdr (assoc document-id revisions))
                (cdr (assq (intern document-id) revisions)))))))
    (unless (integerp value)
      (user-error "Org Note document PUT response revision is invalid"))
    value))

(defun gsmlg-org-note-org--finish-document-todo-attempt (document-id record
                                                                     response)
  "Validate RESPONSE and finish DOCUMENT-ID TODO attempt described by RECORD."
  (let ((new-revision
         (gsmlg-org-note-org--put-response-revision response document-id))
        (expected-revision (plist-get record :expected-revision))
        (buffer (plist-get record :buffer))
        (proposed (plist-get record :proposed-source)))
    (unless (> new-revision expected-revision)
      (user-error "Org Note document PUT did not advance revision"))
    (remhash document-id gsmlg-org-note-org--document-ambiguities)
    (condition-case local-err
        (progn
          (unless (buffer-live-p buffer)
            (error "Org Note document buffer for %s is no longer live"
                   document-id))
          (with-current-buffer buffer
            (unless (equal org-note-document-id document-id)
              (error "Org Note document recovery buffer identity changed"))
            (setq-local org-note-document-revision new-revision
                        org-note-document-base-source proposed)
            (if (= (buffer-modified-tick) (plist-get record :origin-tick))
                (progn
                  (erase-buffer)
                  (insert proposed)
                  (set-buffer-modified-p nil)
                  (goto-char
                   (min (plist-get record :origin-point) (point-max))))
              (set-buffer-modified-p t)
              (message
               "Remote document TODO committed; local in-flight edits preserved")))
          (message "document text updated; no item transition")
          nil)
      ((quit error)
       (list :committed-p t
             :message
             (format "document TODO succeeded; local update failed (%s)"
                     (error-message-string local-err)))))))

(defun gsmlg-org-note-org-retry-ambiguous-document-todo (document-id)
  "Replay the in-process ambiguous TODO PUT for DOCUMENT-ID."
  (interactive "sDocument ID: ")
  (require 'org-note-operation)
  (let ((record (gethash document-id
                         gsmlg-org-note-org--document-ambiguities)))
    (unless (and record (plist-get record :frozen))
      (user-error "No replayable Org Note document TODO for %s" document-id))
    (gsmlg-org-note-org--finish-document-todo-attempt
     document-id record
     (org-note-operation--dispatch-frozen (plist-get record :frozen)))))

(defun gsmlg-org-note-org--build-todo-transformed-source (arg)
  "Return buffer source after applying TODO ARG at point in a temp clone."
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (pos (point))
        (current (org-get-todo-state)))
    (with-temp-buffer
      (org-mode)
      (insert source)
      (goto-char pos)
      (let ((target
             (gsmlg-org-note-org--resolve-todo-target
              arg (or current "") nil))
            (gsmlg-org-note-org-enable nil)
            (gsmlg-org-note-org--activated nil))
        (when (and current (equal target current))
          (user-error "Already %s" current))
        (org-todo target)
        (buffer-string)))))

(defun gsmlg-org-note-org--attempt-idless-document-todo (arg)
  "Apply TODO ARG via frozen document PUT for an id-less heading."
  (require 'org-note-operation)
  (unless (and (fboundp 'org-note-document-mode)
               (derived-mode-p 'org-note-document-mode))
    (user-error "Id-less TODO requires an Org Note document buffer"))
  (when (gsmlg-org-note-org--origin-item-ids)
    (user-error "Internal error: identified heading reached id-less TODO path"))
  (when (buffer-modified-p)
    (user-error
     "Save or discard edits before id-less Org Note document TODO"))
  (unless (and (gsmlg-org-note-org--string-or-nil org-note-document-workspace-id)
               (gsmlg-org-note-org--string-or-nil org-note-document-id)
               (gsmlg-org-note-org--string-or-nil org-note-document-path)
               (integerp org-note-document-revision))
    (user-error "Org Note document metadata is incomplete"))
  (when (gethash org-note-document-id gsmlg-org-note-org--document-ambiguities)
    (user-error
     "Org Note document PUT for %s is ambiguous; resolve before retrying"
     org-note-document-id))
  (let* ((origin-source (buffer-substring-no-properties (point-min) (point-max)))
         (origin-tick (buffer-modified-tick))
         (origin-point (point))
         (workspace-id org-note-document-workspace-id)
         (document-id org-note-document-id)
         (document-path org-note-document-path)
         (expected-revision org-note-document-revision)
         (proposed (gsmlg-org-note-org--build-todo-transformed-source arg))
         (operation-id (org-note-client-new-operation-id))
         (lease-proofs (org-note-operation-lease-proofs document-id))
         (typed
          (list :method "PUT"
                :route (format "/api/org/documents/%s"
                               (org-note-operation--path-segment document-id))
                :query nil
                :body (org-note-operation--mutation-body
                       workspace-id
                       (append
                        `((path . ,document-path)
                          (source . ,proposed)
                          (expected_revision . ,expected-revision))
                        `((lease_proofs
                           . ,(or lease-proofs
                                  (org-note-client-empty-object)))))
                       operation-id)
                :response-validator
                (lambda (response)
                  (let ((revision
                         (gsmlg-org-note-org--put-response-revision
                          response document-id)))
                    (unless (> revision expected-revision)
                      (user-error
                       "Org Note document PUT did not advance revision"))))))
         (frozen (org-note-operation--freeze-request typed))
         (record
          (list :operation-id operation-id :frozen frozen
                :buffer (current-buffer) :origin-tick origin-tick
                :origin-point origin-point :origin-source origin-source
                :proposed-source proposed :expected-revision expected-revision))
         (dispatched-p nil)
         (committed-p nil)
         response)
    (when (equal proposed origin-source)
      (user-error "Already %s" (or (org-get-todo-state) "")))
    (condition-case err
        (progn
          (setq dispatched-p t)
          (setq response (org-note-operation--dispatch-frozen frozen))
          (prog1
              (gsmlg-org-note-org--finish-document-todo-attempt
               document-id record response)
            (setq committed-p t)))
      ((quit error)
       (cond
        (committed-p
         ;; Post-commit UI/recovery failure: keep confirmed metadata.
         nil)
        (dispatched-p
         ;; Post-dispatch non-validated outcome is ambiguous; never pretend
         ;; the mutation did not happen by restoring prior revision/base.
         (puthash document-id record
                  gsmlg-org-note-org--document-ambiguities))
        ;; Pre-dispatch: leave buffer metadata unchanged.
        )
       (signal (car err) (cdr err))))))

(defun gsmlg-org-note-org--around-todo (orig &rest args)
  "Call ORIG with ARGS or bridge `org-todo' to an Org Note mutation."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "TODO")
    (let* ((arg (car args))
           (ids (gsmlg-org-note-org--origin-item-ids)))
      (cond
       (ids
        (let* ((pre (gsmlg-org-note-org--preflight-identified-item
                     (car ids) (cdr ids)))
               (target
                (gsmlg-org-note-org--resolve-todo-target
                 arg
                 (plist-get pre :state)
                 (called-interactively-p 'any)))
               (result
                (gsmlg-org-note-org--attempt-identified-transition
                 target
                 (list :workspace-id (car ids) :item-id (cdr ids)))))
          (when (plist-get result :message)
            (message "%s" (plist-get result :message)))
          t))
       ((and (fboundp 'org-note-document-mode)
             (derived-mode-p 'org-note-document-mode))
        (gsmlg-org-note-org--attempt-idless-document-todo arg)
        t)
       (t
        (user-error
         "Org Note TODO requires item ids or an Org Note document buffer"))))))

(defun gsmlg-org-note-org--around-agenda-todo (orig &rest args)
  "Call ORIG with ARGS or bridge a single-row `org-agenda-todo'."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (when (gsmlg-org-note-org--agenda-bulk-or-region-todo-p)
      (user-error
       "Org Note bridge refuses bulk or region agenda TODO"))
    (let* ((origin (gsmlg-org-note-org--agenda-row-origin))
           (pre (gsmlg-org-note-org--preflight-identified-item
                 (plist-get origin :workspace-id)
                 (plist-get origin :item-id)))
           (target
            (gsmlg-org-note-org--resolve-todo-target
             (car args)
             (plist-get pre :state)
             (called-interactively-p 'any)))
           (result
            (gsmlg-org-note-org--attempt-identified-transition
             target
             (list :workspace-id (plist-get origin :workspace-id)
                   :item-id (plist-get origin :item-id)))))
      (when (plist-get result :message)
        (message "%s" (plist-get result :message)))
      ;; Do not call native agenda todo / maybe-loop / line postprocessing.
      t)))

(defun gsmlg-org-note-org--around-refile (orig &rest args)
  "Call ORIG with ARGS or perform a same-document Org Note refile."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "refile")
    (unless (and (fboundp 'org-note-document-mode)
                 (derived-mode-p 'org-note-document-mode))
      (user-error "Org Note refile requires an open Org Note document"))
    (when (buffer-modified-p)
      (user-error "Save or discard edits before Org Note refile"))
    (unless (and (stringp org-note-document-id)
                 (stringp org-note-document-workspace-id)
                 (stringp org-note-document-path)
                 (integerp org-note-document-revision))
      (user-error "Org Note document metadata is incomplete"))
    (let* ((source-pos (save-excursion (org-back-to-heading t) (point)))
           (source-end (save-excursion (goto-char source-pos) (org-end-of-subtree t t)))
           (choices nil))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (let ((pos (line-beginning-position))
                (title (org-get-heading t t t t)))
            (unless (and (>= pos source-pos) (< pos source-end))
              (push (cons title pos) choices)))))
      (unless choices
        (user-error "No valid same-document Org Note refile target"))
      (let* ((choices (nreverse choices))
             (selected (completing-read "Refile under: " choices nil t))
             (target-pos (cdr (assoc selected choices))))
        (unless target-pos
          (user-error "Org Note refile target is invalid"))
        (let* ((origin-source (buffer-substring-no-properties (point-min) (point-max)))
               (origin-tick (buffer-modified-tick))
               (origin-point (point))
               (document-id org-note-document-id)
               (workspace-id org-note-document-workspace-id)
               (expected-revision org-note-document-revision)
               (proposed
                (with-temp-buffer
                  (org-mode)
                  (insert origin-source)
                  (goto-char source-pos)
                  (let ((source-marker (copy-marker (point) t))
                        (source-end-marker (copy-marker source-end t))
                        (target-marker (copy-marker target-pos t))
                        (moved (buffer-substring-no-properties source-pos source-end)))
                    (goto-char source-marker)
                    (delete-region source-marker source-end-marker)
                    (goto-char target-marker)
                    (org-end-of-subtree t t)
                    (unless (bolp) (insert "\n"))
                    (insert moved)
                    (buffer-string))))
               (operation-id (org-note-client-new-operation-id))
               (typed (list :method "PUT"
                            :route (format "/api/org/documents/%s"
                                           (org-note-operation--path-segment document-id))
                            :query nil
                            :body (org-note-operation--mutation-body
                                   workspace-id
                                   `((path . ,org-note-document-path)
                                     (source . ,proposed)
                                     (expected_revision . ,expected-revision)
                                     (lease_proofs . ,(or
                                                      (org-note-operation-lease-proofs document-id)
                                                      (org-note-client-empty-object))))
                                   operation-id)
                            :response-validator
                            (lambda (response)
                              (let ((revision
                                     (gsmlg-org-note-org--put-response-revision
                                      response document-id)))
                                (unless (> revision expected-revision)
                                  (user-error "Org Note refile PUT did not advance revision"))))))
               (frozen (org-note-operation--freeze-request typed))
               (record (list :operation-id operation-id :frozen frozen
                             :buffer (current-buffer) :origin-tick origin-tick
                             :origin-point origin-point :origin-source origin-source
                             :proposed-source proposed :expected-revision expected-revision))
               response)
          (when (gethash document-id gsmlg-org-note-org--document-ambiguities)
            (user-error "Org Note refile for %s is ambiguous; resolve before retrying" document-id))
          (condition-case err
              (progn
                (setq response (org-note-operation--dispatch-frozen frozen))
                (gsmlg-org-note-org--finish-document-todo-attempt document-id record response)
                (message "Org Note refile committed"))
            ((quit error)
             ;; A validated HTTP 409 is definitive non-commit.  Do not retain
             ;; a replay record for it; only transport/ambiguous failures may
             ;; be retried with the frozen wire envelope.
             (if (and (eq (car err) 'org-note-http-error)
                      (= (or (plist-get (cadr err) :status) 0) 409))
                 (remhash document-id gsmlg-org-note-org--document-ambiguities)
               (puthash document-id record gsmlg-org-note-org--document-ambiguities))
             (signal (car err) (cdr err)))))))))

(defun gsmlg-org-note-org--around-clock-in (orig &rest args)
  "Call ORIG with ARGS or claim the current identified Org Note item."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "clock")
    (when gsmlg-org-note-org--clock-presentation
      (user-error "An Org Note clock is already active"))
    (let* ((ids (gsmlg-org-note-org--origin-item-ids)))
      (unless ids
        (user-error "Org Note clock-in requires item ids"))
      (let* ((origin (gsmlg-org-note-org--preflight-identified-item
                      (car ids) (cdr ids)))
             (operation-id (org-note-client-new-operation-id))
             (workspace-id (car ids)) (item-id (cdr ids))
             (document-id (plist-get origin :document-id))
             (expected-revision (plist-get origin :revision))
             (kind "execution")
             (typed (list :method "POST"
                          :route (format "/api/org/items/%s/claim"
                                         (org-note-operation--path-segment item-id))
                          :query nil
                          :body (org-note-operation--mutation-body
                                 workspace-id
                                 `((document_id . ,document-id)
                                   (expected_document_revision . ,expected-revision)
                                   (kind . ,kind))
                                 operation-id)
                          :response-validator
                          (lambda (response)
                            (gsmlg-org-note-org--clock-claim-response-validator
                             response workspace-id item-id document-id
                             expected-revision kind operation-id))))
             (frozen (org-note-operation--freeze-request typed))
             (record (list :action 'claim :operation-id operation-id
                           :workspace-id workspace-id :item-id item-id
                           :document-id document-id :expected-revision expected-revision
                           :kind kind :started-at (float-time) :frozen frozen)))
        (condition-case err
            (let* ((response (org-note-operation--dispatch-frozen frozen))
                   (lease (org-note-operation-register-claim
                           workspace-id item-id document-id kind response)))
              (remhash operation-id gsmlg-org-note-org--clock-ambiguities)
              (gsmlg-org-note-org--clock-register-presentation record response lease)
              (message "Org Note clock started"))
          ((quit error)
           (if (and (eq (car err) 'org-note-http-error)
                    (= (or (plist-get (cadr err) :status) 0) 409))
               (remhash operation-id gsmlg-org-note-org--clock-ambiguities)
             (puthash operation-id record gsmlg-org-note-org--clock-ambiguities))
           (signal (car err) (cdr err))))))))

(defun gsmlg-org-note-org--around-clock-out (orig &rest args)
  "Call ORIG with ARGS or release the stored bridge clock lease."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "clock")
    (let* ((clock (gsmlg-org-note-org--clock-reconcile))
           (lease (and clock
                       (org-note-operation-find-lease
                        (plist-get clock :workspace-id)
                        (plist-get clock :item-id)
                        (plist-get clock :kind)))))
      (unless (and clock lease)
        (user-error "No active Org Note clock"))
      (let* ((workspace-id (plist-get clock :workspace-id))
             (item-id (plist-get clock :item-id))
             (document-id (plist-get clock :document-id))
             (pre (gsmlg-org-note-org--preflight-identified-item workspace-id item-id))
             (operation-id (org-note-client-new-operation-id))
             (kind (plist-get clock :kind))
             (lease-id (org-note-operation-lease-lease-id lease))
             (fencing-token (org-note-operation-lease-fencing-token lease))
             (typed (list :method "POST"
                          :route (format "/api/org/items/%s/claim/release"
                                         (org-note-operation--path-segment item-id))
                          :query nil
                          :body (org-note-operation--mutation-body
                                 workspace-id
                                 `((document_id . ,document-id)
                                   (expected_document_revision . ,(plist-get pre :revision))
                                   (lease_id . ,lease-id) (kind . ,kind)
                                   (fencing_token . ,fencing-token))
                                 operation-id)))
             (frozen (org-note-operation--freeze-request typed))
             (record (list :action 'release :operation-id operation-id
                           :workspace-id workspace-id :item-id item-id
                           :document-id document-id :expected-revision (plist-get pre :revision)
                           :kind kind :lease-id lease-id :frozen frozen)))
        (condition-case err
            (progn
              (org-note-operation--dispatch-frozen frozen)
              (let* ((context (org-note-operation-get-item-context workspace-id item-id))
                     (remote-lease (gsmlg-org-note-org--context-field
                                    (gsmlg-org-note-org--context-field context 'data)
                                    'context 'lease)))
                (when (and remote-lease
                           (equal (gsmlg-org-note-org--context-field remote-lease 'id)
                                  lease-id))
                  (user-error "Org Note clock release was not confirmed")))
              (org-note-operation-forget-lease workspace-id item-id kind)
              (setq gsmlg-org-note-org--clock-presentation nil)
              (message "Org Note clock stopped"))
          ((quit error)
           (if (and (eq (car err) 'org-note-http-error)
                    (= (or (plist-get (cadr err) :status) 0) 409))
               (remhash operation-id gsmlg-org-note-org--clock-ambiguities)
             (puthash operation-id record gsmlg-org-note-org--clock-ambiguities))
           (signal (car err) (cdr err))))))))

(defun gsmlg-org-note-org--around-clock-cancel (orig &rest args)
  "Call ORIG with ARGS or release the stored bridge clock lease."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "clock")
    (apply #'gsmlg-org-note-org--around-clock-out
           (lambda (&rest _ignored) nil) args)))

(defun gsmlg-org-note-org--around-clock-goto (orig &rest args)
  "Call ORIG with ARGS or open the active bridge clock item context."
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (let ((clock (gsmlg-org-note-org--clock-reconcile)))
      (unless clock
        (user-error "No active Org Note clock"))
      (org-note-item-context (plist-get clock :workspace-id)
                             (plist-get clock :item-id)))))

(defun gsmlg-org-note-org--around-archive-subtree (orig &rest args)
  "Call ORIG with ARGS or bridge an Org Note archive operation."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "archive")
    (cond
     ((and (fboundp 'org-note-document-mode)
           (derived-mode-p 'org-note-document-mode)
           (eq (car-safe args) nil))
      (gsmlg-org-note-org--archive-document))
     ((gsmlg-org-note-org--agenda-bulk-or-region-todo-p)
      (user-error "Org Note archive refuses marked or bulk Agenda entries"))
     ((gsmlg-org-note-org--origin-item-ids)
      (gsmlg-org-note-org--attempt-identified-transition
       gsmlg-org-note-archive-target
       (let ((ids (gsmlg-org-note-org--origin-item-ids)))
         (list :workspace-id (car ids) :item-id (cdr ids))))
      t)
     (t
      (user-error "Org Note archive requires an identified item or document buffer")))))

(defun gsmlg-org-note-org--install-mutation-hooks ()
  "Install TODO/refile/archive/clock advice once."
  (unless gsmlg-org-note-org--mutation-hooks-installed
    (setq gsmlg-org-note-org--mutation-hooks-installed t)
    (autoload 'org-todo "org" nil t)
    (autoload 'org-agenda-todo "org-agenda" nil t)
    (autoload 'org-refile "org-refile" nil t)
    (autoload 'org-clock-in "org-clock" nil t)
    (autoload 'org-clock-out "org-clock" nil t)
    (autoload 'org-clock-cancel "org-clock" nil t)
    (autoload 'org-clock-goto "org-clock" nil t)
    (autoload 'org-note-document-archive "org-note" nil t)
    (advice-add #'org-todo :around #'gsmlg-org-note-org--around-todo)
    (advice-add #'org-agenda-todo :around #'gsmlg-org-note-org--around-agenda-todo)
    (advice-add #'org-refile :around #'gsmlg-org-note-org--around-refile)
    (advice-add #'org-clock-in :around #'gsmlg-org-note-org--around-clock-in)
    (advice-add #'org-clock-out :around #'gsmlg-org-note-org--around-clock-out)
    (advice-add #'org-clock-cancel :around #'gsmlg-org-note-org--around-clock-cancel)
    (advice-add #'org-clock-goto :around #'gsmlg-org-note-org--around-clock-goto)
    (advice-add #'org-note-document-archive :around
                #'gsmlg-org-note-org--around-archive-subtree)
    (dolist (entry '((org-archive-subtree . "org-archive")
                     (org-toggle-archive-tag . "org-archive")
                     (org-archive-to-archive-sibling . "org-archive")
                     (org-archive-set-tag . "org-archive")))
      (autoload (car entry) (cdr entry) nil t)
      (advice-add (car entry) :around
                  #'gsmlg-org-note-org--around-archive-subtree))))

(defun gsmlg-org-note-org--install-feed-hooks ()
  "Install feed refresh and agenda-goto advice once."
  (unless gsmlg-org-note-org--feed-hooks-installed
    (setq gsmlg-org-note-org--feed-hooks-installed t)
    (advice-add #'org-agenda-goto :around #'gsmlg-org-note-org--goto)))

(defun gsmlg-org-note-org--around-agenda (orig &rest args)
  "Guard an Agenda producer before calling ORIG with ARGS."
  (if (or (not gsmlg-org-note-org-enable)
          gsmlg-org-note-org--agenda-command-active)
      (apply orig args)
    (let ((gsmlg-org-note-org--agenda-command-active t))
      (require 'org-note)
      (gsmlg-org-note-org-activate)
      (gsmlg-org-note-org--refresh-before-agenda)
      (let ((org-agenda-files (gsmlg-org-note-org-agenda-files)))
        (apply orig args)))))

(defun gsmlg-org-note-org--around-agenda-files (orig &rest args)
  "Return only the bridge feed while active, otherwise call ORIG with ARGS.

This lower-level guard defeats native restrictions and custom Agenda command
bindings that dynamically replace `org-agenda-files' after entrypoint advice."
  (if (and gsmlg-org-note-org-enable
           gsmlg-org-note-org--activated)
      (gsmlg-org-note-org-agenda-files)
    (apply orig args)))

(defun gsmlg-org-note-org--around-capture (orig &rest args)
  "Activate the bridge then call ORIG with ARGS.

Capture templates are switched to bridge-owned staging by activation."
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (require 'org-note)
    (gsmlg-org-note-org-activate)
    (apply orig args)))

(defun gsmlg-org-note-org-capture-target ()
  "Prepare the current Org Capture buffer as a non-file bridge staging area.

The remote mutation is deliberately performed by the finalize path, not by
this target function.  Keeping the target non-file prevents Org Capture from
writing under `gsmlg-org-directory'."
  (interactive)
  (when gsmlg-org-note-org--capture-recovery-required
    (user-error "Resolve the existing Org Note Capture recovery record first"))
  (require 'org-note-client)
  (gsmlg-org-note-org--capture-reservation-acquire)
  (require 'org)
  (let ((staging (generate-new-buffer " *Org Note Capture Target*")))
    (set-buffer staging)
    (setq-local buffer-file-name nil
                buffer-offer-save nil
                gsmlg-org-note-org--capture-attempt nil)
    (add-hook 'kill-buffer-hook #'gsmlg-org-note-org--capture-staging-kill nil t)
    (org-mode)
    (goto-char (point-max))))

(defun gsmlg-org-note-org-capture-prepare-finalize ()
  "Validate bridge Capture staging before native finalize processing.

Remote dispatch belongs to `gsmlg-org-note-org-capture-before-finalize'; this
early hook must remain free of network I/O."
  (when (and gsmlg-org-note-org-enable
             (derived-mode-p 'org-mode))
    (unless (string= (or buffer-file-name "") "")
      (user-error "Org Note Capture staging must not have a file target"))))

(defun gsmlg-org-note-org--capture-document (workspace-id document-id
                                                          expected-path)
  "Fetch and validate the existing Capture target.

WORKSPACE-ID, DOCUMENT-ID, and EXPECTED-PATH must all match the returned
active document.  Return a plist containing its path, source, and revision."
  (let* ((document (org-note-operation-get-document workspace-id document-id))
         (id (alist-get 'id document))
         (document-workspace-id (alist-get 'workspace_id document))
         (path (alist-get 'path document))
         (source (alist-get 'source document))
         (revision (alist-get 'revision document))
         (archived-entry (assq 'archived_at document))
         (archived-at (cdr archived-entry)))
    (unless (and (equal id document-id)
                 (equal document-workspace-id workspace-id)
                 (equal path expected-path)
                 (stringp source)
                 (integerp revision) (>= revision 0)
                 archived-entry
                 (or (null archived-at)
                     (and (integerp archived-at) (>= archived-at 0))))
      (user-error "Org Note Capture target metadata is invalid"))
    (when (and (integerp archived-at) (> archived-at 0))
      (user-error "Org Note Capture target is archived"))
    (list :path path :source source :revision revision)))

(defun gsmlg-org-note-org--capture-list-documents (workspace-id)
  "Return bounded document rows for WORKSPACE-ID, including archived rows."
  (require 'org-note-validation)
  (org-note-validation-bounded-pager-fold
   (org-note-validation-bounded-pager-state :limit 100)
   (lambda (cursor)
     (let* ((response (org-note-operation-list-documents
                       workspace-id :cursor cursor :limit 100
                       :include-archived t))
            (data (or (alist-get 'data response) response))
            (rows (or (alist-get 'documents data)
                      (alist-get 'items data)
                      (alist-get 'rows data) '()))
            (next (or (alist-get 'next_cursor data)
                      (alist-get 'next_cursor response))))
       (list :rows (if (vectorp rows) (append rows nil) rows)
             :next-cursor next)))))

(defun gsmlg-org-note-org--capture-resolve-target ()
  "Resolve configured Capture target, returning a target plist.

When no document id is configured, prompt to select an existing document or
create a new one.  The create branch allocates its id exactly once and marks
the returned target as pending-create."
  (require 'org-note-client)
  (require 'org-note-operation)
  (require 'org-note-validation)
  (let* ((endpoint (org-note-validation-canonical-endpoint
                    (or (and (boundp 'org-note-endpoint) org-note-endpoint)
                        "https://agent-note.gsmlg.net")))
         (configured-endpoint gsmlg-org-note-capture-endpoint)
         (workspace-id gsmlg-org-note-capture-workspace-id)
         (document-id gsmlg-org-note-capture-document-id)
         (path gsmlg-org-note-capture-document-path))
    (when (and configured-endpoint
               (not (equal endpoint
                           (org-note-validation-canonical-endpoint
                            configured-endpoint))))
      (user-error "Org Note Capture endpoint changed; reconfigure target"))
    (unless (and (stringp workspace-id) (not (string-empty-p workspace-id)))
      (user-error "Org Note Capture workspace is not configured"))
    (if (and (stringp document-id) (not (string-empty-p document-id)))
        (list :endpoint endpoint :workspace-id workspace-id
              :document-id document-id :path path :pending-create nil)
      (let* ((rows (gsmlg-org-note-org--capture-list-documents workspace-id))
             (choices (cons (cons "[Create new document]" :create)
                            (mapcar (lambda (row)
                                      (cons (format "%s%s"
                                                    (or (alist-get 'path row) "")
                                                    (if (and (alist-get 'archived_at row)
                                                             (> (alist-get 'archived_at row) 0))
                                                        " [archived]" ""))
                                            row)) rows)))
             (selected (completing-read "Capture target: " choices nil t)))
        (if (eq (cdr (assoc selected choices)) :create)
            (let ((new-path (read-string "New document path: "
                                         (or path "inbox.org"))))
              (unless (and (stringp new-path)
                           (string-match-p "\\`[^/].*\\.org\\'" new-path))
                (user-error "Invalid Org Note Capture document path: %s" new-path))
              (list :endpoint endpoint :workspace-id workspace-id
                    :document-id (org-id-uuid) :path new-path
                    :pending-create t))
          (let ((row (cdr (assoc selected choices))))
            (unless (and (listp row) (stringp (alist-get 'id row))
                         (stringp (alist-get 'path row)))
              (user-error "Org Note Capture target selection is invalid"))
            (list :endpoint endpoint :workspace-id workspace-id
                  :document-id (alist-get 'id row)
                  :path (alist-get 'path row) :pending-create nil)))))))

(defun gsmlg-org-note-org--finish-capture-attempt (attempt response)
  "Finish existing-document Capture ATTEMPT using RESPONSE.

The response must advance the frozen expected revision.  After commit, any
change to the staging source or modification tick is retained read-only."
  (let* ((document-id (plist-get attempt :document-id))
         (expected-revision (plist-get attempt :expected-revision))
         (new-revision
          (gsmlg-org-note-org--put-response-revision response document-id)))
    (unless (if expected-revision (> new-revision expected-revision)
              (>= new-revision 0))
      (user-error (if expected-revision
                      "Org Note Capture PUT did not advance revision"
                    "Org Note Capture CREATE response revision is invalid")))
    (when (plist-get attempt :pending-create)
      (setopt gsmlg-org-note-capture-endpoint
              (plist-get (plist-get attempt :frozen) :endpoint)
              gsmlg-org-note-capture-workspace-id
              (plist-get attempt :workspace-id)
              gsmlg-org-note-capture-document-id
              (plist-get attempt :document-id)
              gsmlg-org-note-capture-document-path
              (plist-get attempt :path)))
    (setq gsmlg-org-note-org--capture-attempt
          (plist-put attempt :state 'committed-pending-journal)
          buffer-read-only t)
    (condition-case err
        (gsmlg-org-note-org--capture-journal-write
         (plist-put (copy-sequence attempt) :state 'committed))
      ((quit error)
       (signal (car err) (cdr err))))
    (gsmlg-org-note-org--complete-capture-after-journal response)))

(defun gsmlg-org-note-org--complete-capture-after-journal (&optional response)
  "Complete local Capture state after its committed journal is durable.

Return RESPONSE for the mutation completion path."
  (let ((attempt gsmlg-org-note-org--capture-attempt))
    (setq gsmlg-org-note-org--capture-attempt
          (plist-put attempt :state 'committed))
    (if (and (equal (buffer-substring-no-properties (point-min) (point-max))
                    (plist-get attempt :source))
             (equal (secure-hash
                     'sha256
                     (buffer-substring-no-properties (point-min) (point-max)))
                    (plist-get attempt :digest))
             (= (buffer-chars-modified-tick) (plist-get attempt :tick)))
        (setq buffer-read-only nil)
      (setq gsmlg-org-note-org--capture-attempt
            (plist-put gsmlg-org-note-org--capture-attempt
                       :state 'committed-local-divergence)
            buffer-read-only t)
      (user-error
       "Org Note Capture committed, but the staging buffer changed locally"))
    (message "Org Note Capture committed to %s" (plist-get attempt :path))
    (gsmlg-org-note-org--capture-reservation-release)
    response))

(defun gsmlg-org-note-org-retry-capture-journal ()
  "Retry only local durability for a remotely committed Capture attempt."
  (interactive)
  (let ((attempt gsmlg-org-note-org--capture-attempt))
    (unless (eq (plist-get attempt :state) 'committed-pending-journal)
      (user-error "No committed Org Note Capture journal needs retry"))
    (gsmlg-org-note-org--capture-journal-write
     (plist-put (copy-sequence attempt) :state 'committed))
    (gsmlg-org-note-org--complete-capture-after-journal)))

(defun gsmlg-org-note-org-retry-ambiguous-capture ()
  "Retry this buffer's ambiguous Capture with its exact frozen request."
  (interactive)
  (require 'org-note-operation)
  (let ((attempt gsmlg-org-note-org--capture-attempt))
    (unless (and (eq (plist-get attempt :state) 'ambiguous)
                 (plist-get attempt :frozen))
      (user-error "No replayable ambiguous Org Note Capture attempt"))
    (condition-case err
        (gsmlg-org-note-org--finish-capture-attempt
         attempt
         (org-note-operation--dispatch-frozen (plist-get attempt :frozen)))
      ((quit error)
       (unless (memq (plist-get gsmlg-org-note-org--capture-attempt :state)
                     '(committed-pending-journal committed
                       committed-local-divergence))
         (setq gsmlg-org-note-org--capture-attempt
               (plist-put attempt :state 'ambiguous)))
       (setq buffer-read-only t)
       (signal (car err) (cdr err))))))

(defun gsmlg-org-note-org-capture-before-finalize ()
  "Commit the current bridge Capture attempt to the configured document.

This first Capture slice supports an existing, unclaimed document.  Creation,
journaling, and replay recovery are layered on in later slices."
  (require 'org-note-operation)
  (pcase (plist-get gsmlg-org-note-org--capture-attempt :state)
    ('committed
     nil)
    ('committed-local-divergence
     (user-error "Resolve divergent committed Org Note Capture text first"))
    ('committed-pending-journal
     (user-error "Retry the committed Org Note Capture journal locally"))
    ('ambiguous
     (user-error "Retry the ambiguous Org Note Capture attempt explicitly"))
    (_
     (let* ((target (gsmlg-org-note-org--capture-resolve-target))
            (workspace-id (plist-get target :workspace-id))
            (document-id (plist-get target :document-id))
            (path (plist-get target :path))
            (pending-create (plist-get target :pending-create))
            (document (and (not pending-create)
                           (gsmlg-org-note-org--capture-document
                            workspace-id document-id path)))
            (remote-source (if document (plist-get document :source) ""))
            (revision (and document (plist-get document :revision)))
            (proofs (and document
                         (org-note-operation-lease-proofs document-id))))
         (when (and pending-create
                    (or (null path) (string-empty-p path)))
           (user-error "Org Note Capture document path is not configured"))
         (when (and document
                    (not (and (hash-table-p proofs) (= (hash-table-count proofs) 0))))
           (user-error
            "Org Note Capture refuses a document with an active lease"))
         (let* ((capture-source
                 (buffer-substring-no-properties (point-min) (point-max)))
                (capture-tick (buffer-chars-modified-tick))
                (combined
                 (if pending-create
                     capture-source
                   (concat remote-source
                           (unless (string-suffix-p "\n" remote-source) "\n")
                           capture-source)))
                (operation-id (org-note-client-new-operation-id))
                (typed
                 (list
                  :method "PUT"
                  :route
                  (format "/api/org/documents/%s"
                          (org-note-operation--path-segment document-id))
                  :query nil
                  :body
                   (org-note-operation--mutation-body
                    workspace-id
                    (append `((path . ,path)
                              (source . ,combined))
                            (and revision `((expected_revision . ,revision)))
                            `((lease_proofs . ,(org-note-client-empty-object))))
                   operation-id)
                  :response-validator
                  (lambda (response)
                    (let ((new-revision
                           (gsmlg-org-note-org--put-response-revision
                            response document-id)))
                      (unless (if revision (> new-revision revision)
                                (>= new-revision 0))
                      (user-error
                       (if revision
                           "Org Note Capture PUT did not advance revision"
                         "Org Note Capture CREATE response revision is invalid")))))))
                (frozen (org-note-operation--freeze-request typed))
                (attempt
                 (list :state 'prepared :operation-id operation-id
                       :frozen frozen :workspace-id workspace-id
                       :document-id document-id :path path
                       :expected-revision revision :source capture-source
                       :pending-create pending-create
                       :digest (secure-hash 'sha256 capture-source)
                       :tick capture-tick
                       :created-at (floor (* 1000 (float-time))))))
           (setq gsmlg-org-note-org--capture-attempt attempt)
           (gsmlg-org-note-org--capture-journal-write attempt)
           (setq attempt (plist-put attempt :state 'dispatched)
                 gsmlg-org-note-org--capture-attempt attempt)
           (gsmlg-org-note-org--capture-journal-write attempt)
             (condition-case err
                (gsmlg-org-note-org--finish-capture-attempt
                attempt (org-note-operation--dispatch-frozen frozen))
             ((quit error)
              (let* ((data (cdr err))
                     (properties (and (listp (car data)) (car data)))
                     (status (plist-get properties :status)))
                (if (and (eq (car err) 'org-note-http-error)
                         (= status 409))
                    (progn
                      (setq gsmlg-org-note-org--capture-attempt nil
                            buffer-read-only nil)
                      (gsmlg-org-note-org--capture-reservation-release))
                  (unless
                      (memq (plist-get gsmlg-org-note-org--capture-attempt :state)
                            '(committed-pending-journal committed
                              committed-local-divergence))
                    (setq gsmlg-org-note-org--capture-attempt
                          (plist-put attempt :state 'ambiguous)))
                  (setq buffer-read-only t)))
              (signal (car err) (cdr err)))))))))

(defun gsmlg-org-note-org--symbol-alist-p (value)
  "Return non-nil when VALUE is a nonempty alist."
  (and (listp value) (cl-every #'consp value)))

(defun gsmlg-org-note-org--string-or-nil (value)
  "Return VALUE when it is a nonempty string."
  (and (stringp value) (not (string-empty-p value)) value))

(defun gsmlg-org-note-org--workspace-ids ()
  "Return configured Org Note agenda workspace IDs, or nil."
  (when (boundp 'org-note-agenda-workspace-ids)
    (let ((ids org-note-agenda-workspace-ids))
      (when (and (listp ids) ids)
        (let ((copy (copy-sequence ids)))
          (when (and copy (cl-every #'gsmlg-org-note-org--string-or-nil copy))
            copy))))))

(defun gsmlg-org-note-org--timestamp-raw (timestamp)
  "Return the Org timestamp text from Org Note TIMESTAMP, or nil."
  (when (gsmlg-org-note-org--symbol-alist-p timestamp)
    (gsmlg-org-note-org--string-or-nil (alist-get 'raw timestamp))))

(defun gsmlg-org-note-org--item-id (item)
  "Return the validated item ID from ITEM."
  (gsmlg-org-note-org--string-or-nil (alist-get 'id item)))

(defun gsmlg-org-note-org--item-key (item)
  "Return a deduplication key for ITEM."
  (let ((workspace (gsmlg-org-note-org--string-or-nil
                    (alist-get 'workspace_id item)))
        (id (gsmlg-org-note-org--item-id item)))
    (when (and workspace id)
      (cons workspace id))))

(defun gsmlg-org-note-org--item-less-p (left right)
  "Return non-nil when LEFT sort precedes RIGHT by canonical identity."
  (let ((left-key (gsmlg-org-note-org--item-key left))
        (right-key (gsmlg-org-note-org--item-key right)))
    (or (string< (car left-key) (car right-key))
        (and (equal (car left-key) (car right-key))
             (string< (cdr left-key) (cdr right-key))))))

(defun gsmlg-org-note-org--item-tags (item)
  "Return a `:TAG:' suffix string for ITEM."
  (let ((tags (alist-get 'tags item)))
    (when (and (listp tags) tags)
      (mapconcat #'identity
                 (cl-remove-if-not #'gsmlg-org-note-org--string-or-nil tags)
                 ":"))))

(defun gsmlg-org-note-org--item-headline (item)
  "Return an Org headline line for ITEM."
  (let* ((state (or (gsmlg-org-note-org--string-or-nil
                     (alist-get 'state item))
                    "TODO"))
         (title (or (gsmlg-org-note-org--string-or-nil
                     (alist-get 'title item))
                    "Org Note item"))
         (priority (gsmlg-org-note-org--string-or-nil
                    (alist-get 'priority item)))
         (tags (gsmlg-org-note-org--item-tags item))
         (tag-string
          (mapconcat #'identity
                     (delq nil
                           (list gsmlg-org-note-org--feed-tag tags))
                     ":")))
    (format "* %s%s %s :%s:"
            state
            (if priority (format " [#%s]" priority) "")
            (replace-regexp-in-string "[\n\r\t]+" " " title)
            tag-string)))

(defun gsmlg-org-note-org--item-properties (item)
  "Return Org property drawer lines for ITEM."
  (let ((workspace (gsmlg-org-note-org--string-or-nil
                    (alist-get 'workspace_id item)))
        (id (gsmlg-org-note-org--item-id item)))
    (when (and workspace id)
      (format ":PROPERTIES:\n:ORG_NOTE_WORKSPACE_ID: %s\n:ORG_NOTE_ITEM_ID: %s\n:END:\n"
              workspace id))))

(defun gsmlg-org-note-org--item-timestamp-lines (item view)
  "Return scheduled/deadline lines for ITEM and agenda VIEW."
  (let ((scheduled (gsmlg-org-note-org--timestamp-raw
                    (alist-get 'scheduled item)))
        (deadline (gsmlg-org-note-org--timestamp-raw
                   (alist-get 'deadline item))))
    (delq nil
          (list
           (when (and scheduled (memq view '(scheduled all)))
             (format "SCHEDULED: %s" scheduled))
           (when (and deadline (memq view '(upcoming_deadline all)))
             (format "DEADLINE: %s" deadline))))))

(defun gsmlg-org-note-org--item-text (item view)
  "Return Org text for ITEM in agenda VIEW."
  (let ((lines (append (list (gsmlg-org-note-org--item-headline item))
                       (gsmlg-org-note-org--item-timestamp-lines item view)
                       (list (gsmlg-org-note-org--item-properties item)))))
    (mapconcat #'identity (delq nil lines) "\n")))

(defun gsmlg-org-note-org--page-items (response)
  "Return item alists from an Org Note agenda page RESPONSE."
  (unless (gsmlg-org-note-org--symbol-alist-p response)
    (error "Org Note agenda page is malformed"))
  (let ((raw-items (alist-get 'items response)))
    (unless (listp raw-items)
      (error "Org Note agenda page items are malformed"))
    (mapcar (lambda (row)
              (let ((item (alist-get 'item row)))
                (unless (gsmlg-org-note-org--symbol-alist-p item)
                  (error "Org Note agenda row item is malformed"))
                item))
            raw-items)))

(defun gsmlg-org-note-org--page-next-cursor (response)
  "Return the next cursor from Org Note agenda page RESPONSE.

Nil ends pagination.  An empty string fails closed as `org-note-error'."
  (require 'org-note-validation)
  (org-note-validation-page-cursor (alist-get 'next_cursor response)))

(defun gsmlg-org-note-org--fetch-view-items (workspace-ids view)
  "Fetch all agenda items for VIEW across WORKSPACE-IDS.

Uses the shared bounded pager so empty or repeated cursors, repeated
row identities, and page/row/request/time budgets fail closed.  Failures
propagate to `gsmlg-org-note-org-refresh-feed' for last-good handling."
  (require 'org-note-validation)
  (let ((state (org-note-validation-bounded-pager-state
                :limit gsmlg-org-note-org--agenda-page-limit)))
    (org-note-validation-bounded-pager-fold
     state
     (lambda (cursor)
       (let* ((response (org-note-operation-query-agenda
                         :workspace-ids workspace-ids
                         :view view
                         :cursor cursor
                         :limit gsmlg-org-note-org--agenda-page-limit))
              (items (gsmlg-org-note-org--page-items response))
              (next (gsmlg-org-note-org--page-next-cursor response)))
         (list :rows items :next-cursor next))))))
(defun gsmlg-org-note-org--write-feed (contents &optional path)
  "Write CONTENTS to PATH or the last-good Org Note agenda feed file.

Publication locking is performed by the refresh entrypoint before fetch."
  (let ((target (or path gsmlg-org-note-org--feed-file)))
    (gsmlg-ensure-parent-directory target)
    (unless (and (file-readable-p target)
                 (with-temp-buffer
                   (insert-file-contents target)
                   (equal contents (buffer-string))))
      (write-region contents nil target nil 'silent))
    target))

(defvar gsmlg-org-note-org--publication-reservation nil)

(defun gsmlg-org-note-org--publication-reservation-directory (workspace-ids)
  "Return endpoint/workspace keyed publication lock for WORKSPACE-IDS."
  (let* ((key (concat (gsmlg-org-note-org--endpoint-identity) "\0"
                      (mapconcat #'identity workspace-ids "\0")))
         (digest (secure-hash 'sha256 key)))
    (gsmlg-state-file (format "org-note/publication-%s.lock/"
                              (substring digest 0 32)))))

(defun gsmlg-org-note-org--publication-acquire (workspace-ids)
  "Acquire the WORKSPACE-IDS publication reservation before any remote fetch."
  (let ((directory (directory-file-name
                    (gsmlg-org-note-org--publication-reservation-directory
                     workspace-ids))))
    (condition-case nil
        (progn
          (make-directory directory t)
          (set-file-modes directory #o700)
          (let ((owner `((pid . ,(emacs-pid))
                         (start_token . ,(gsmlg-org-note-org--process-start-token
                                          (emacs-pid)))
                         (nonce . ,(org-note-client-new-operation-id)))))
            (with-temp-file (expand-file-name "owner.json" directory)
              (insert (json-serialize owner)))
            (set-file-modes (expand-file-name "owner.json" directory) #o600)
            (setq gsmlg-org-note-org--publication-reservation
                  (list :directory directory :owner owner))))
      (file-already-exists
       (user-error "Another Org Note agenda publication is already in progress")))))

(defun gsmlg-org-note-org--publication-release ()
  "Release publication reservation when still owned by this process."
  (let* ((reservation gsmlg-org-note-org--publication-reservation)
         (directory (plist-get reservation :directory))
         (owner-file (and directory (expand-file-name "owner.json" directory))))
    (when (and reservation (file-readable-p owner-file))
      (let ((owner (condition-case nil
                       (json-parse-string
                        (with-temp-buffer
                          (insert-file-contents-literally owner-file)
                          (buffer-string))
                        :object-type 'alist)
                     (error nil))))
        (when (equal (alist-get 'nonce owner)
                     (alist-get 'nonce (plist-get reservation :owner)))
          (delete-file owner-file)
          (delete-directory directory))))
    (setq gsmlg-org-note-org--publication-reservation nil)))

(defun gsmlg-org-note-org--empty-feed-contents (&optional workspace-ids)
  "Return the contents of an empty Org Note agenda feed.

When WORKSPACE-IDS is non-nil, embed matching schema metadata."
  (concat
   (format "#+TITLE: Org Note Agenda Feed\n#+FILETAGS: %s\n"
           gsmlg-org-note-org--feed-tag)
   (format "#+ORG_NOTE_FEED_SCHEMA: %s\n"
           gsmlg-org-note-org--feed-schema-version)
   (when workspace-ids
     (format "#+ORG_NOTE_WORKSPACE_IDS: %s\n"
             (mapconcat #'identity workspace-ids " ")))))

(defun gsmlg-org-note-org--endpoint-identity ()
  "Return the canonical Org Note endpoint string used for empty feeds."
  (if (and (boundp 'org-note-endpoint)
           (stringp org-note-endpoint)
           (not (string-empty-p org-note-endpoint)))
      org-note-endpoint
    "default"))

(defun gsmlg-org-note-org--empty-feed-file ()
  "Return an endpoint-keyed empty feed path that does not clobber last-good."
  (let* ((endpoint (gsmlg-org-note-org--endpoint-identity))
         (digest (secure-hash 'sha256 endpoint)))
    (gsmlg-cache-file
     (format "org-note-agenda-empty-%s.org" (substring digest 0 16)))))

(defun gsmlg-org-note-org--select-feed (path)
  "Record PATH as the selected agenda feed and return it."
  (setq gsmlg-org-note-org--selected-feed-file path)
  path)

(defun gsmlg-org-note-org--parse-feed-keyword (contents keyword)
  "Return the value of KEYWORD from feed CONTENTS, or nil."
  (when (string-match
         (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote keyword))
         contents)
    (string-trim (match-string 1 contents))))

(defun gsmlg-org-note-org--last-good-matches-p (workspace-ids)
  "Return non-nil when last-good matches schema and WORKSPACE-IDS."
  (and (file-readable-p gsmlg-org-note-org--feed-file)
       (with-temp-buffer
         (insert-file-contents gsmlg-org-note-org--feed-file)
         (let* ((text (buffer-string))
                (schema (gsmlg-org-note-org--parse-feed-keyword
                         text "ORG_NOTE_FEED_SCHEMA"))
                (ids (gsmlg-org-note-org--parse-feed-keyword
                      text "ORG_NOTE_WORKSPACE_IDS"))
                (parsed-ids (and ids (split-string ids nil t))))
           (and (equal schema
                       (number-to-string
                        gsmlg-org-note-org--feed-schema-version))
                (equal parsed-ids workspace-ids))))))

(defun gsmlg-org-note-org--offer-last-good-or-abort (workspace-ids err)
  "Offer matching last-good for WORKSPACE-IDS after ERR, or signal."
  (let ((prompt
         (format
          "Org Note agenda refresh failed (%s). Use last-good snapshot? "
          (error-message-string err))))
    (unless (and (gsmlg-org-note-org--last-good-matches-p workspace-ids)
                 (yes-or-no-p prompt))
      (signal (car err) (cdr err)))
    (setq gsmlg-org-note-org--last-workspace-ids
          (copy-sequence workspace-ids))
    (gsmlg-org-note-org--select-feed gsmlg-org-note-org--feed-file)))

(defun gsmlg-org-note-org--fetch-views (workspace-ids)
  "Fetch scheduled and upcoming_deadline items for WORKSPACE-IDS.

Return a cons (SCHEDULED . DEADLINES)."
  (cons (gsmlg-org-note-org--fetch-view-items workspace-ids 'scheduled)
        (gsmlg-org-note-org--fetch-view-items
         workspace-ids 'upcoming_deadline)))

(defun gsmlg-org-note-org--build-feed-contents (workspace-ids)
  "Return Org feed contents for WORKSPACE-IDS."
  (let* ((views (gsmlg-org-note-org--fetch-views workspace-ids))
         (scheduled (car views))
         (deadlines (cdr views))
         (seen (make-hash-table :test #'equal))
         (items nil))
    (dolist (item (append scheduled deadlines))
      (let ((key (gsmlg-org-note-org--item-key item)))
        (when (and key (not (gethash key seen)))
          (puthash key t seen)
          (push item items))))
    (setq items (sort items #'gsmlg-org-note-org--item-less-p))
    (if items
        (concat (gsmlg-org-note-org--empty-feed-contents workspace-ids)
                (mapconcat (lambda (item)
                             (gsmlg-org-note-org--item-text item 'all))
                           items "\n\n")
                "\n")
      (gsmlg-org-note-org--empty-feed-contents workspace-ids))))

(defun gsmlg-org-note-org--ensure-workspaces ()
  "Return workspace IDs, prompting to configure when unset.

On cancel or still-empty selection, return nil.
Noninteractive sessions skip configure (treat as cancel) so batch
and ERT never block on a minibuffer."
  (or (gsmlg-org-note-org--workspace-ids)
      (progn
        (when (and (not noninteractive)
                   (fboundp #'org-note-configure-agenda-workspaces))
          (condition-case nil
              (org-note-configure-agenda-workspaces)
            (quit nil)))
        (gsmlg-org-note-org--workspace-ids))))

(defun gsmlg-org-note-org--use-empty-feed ()
  "Write and select the endpoint-keyed empty feed without clobbering last-good."
  (let ((empty (gsmlg-org-note-org--empty-feed-file)))
    (setq gsmlg-org-note-org--last-workspace-ids nil)
    (gsmlg-org-note-org--write-feed
     (gsmlg-org-note-org--empty-feed-contents)
     empty)
    (gsmlg-org-note-org--select-feed empty)))

(defun gsmlg-org-note-org-refresh-feed (&optional force)
  "Refresh the generated Org Note agenda feed file.

When FORCE is nil and workspace selection is unchanged, reuse the
existing snapshot.  Unset workspaces trigger configure-on-empty; cancel
selects an endpoint-keyed empty feed without overwriting last-good.
Pre-rename failures offer a matching last-good snapshot via
`yes-or-no-p', or abort."
  (unless gsmlg-org-note-org--refresh-active
    (setq gsmlg-org-note-org--refresh-active t)
    (unwind-protect
        (let ((workspace-ids (gsmlg-org-note-org--ensure-workspaces)))
          (cond
           ((null workspace-ids)
            (gsmlg-org-note-org--use-empty-feed))
           ((and (not force)
                 (equal workspace-ids gsmlg-org-note-org--last-workspace-ids)
                 (file-readable-p gsmlg-org-note-org--feed-file))
            (gsmlg-org-note-org--select-feed gsmlg-org-note-org--feed-file))
           (t
            (require 'org-note-client)
            (gsmlg-org-note-org--publication-acquire workspace-ids)
            (unwind-protect
                (condition-case err
                    (progn
                      (gsmlg-org-note-org--write-feed
                       (gsmlg-org-note-org--build-feed-contents workspace-ids))
                      (setq gsmlg-org-note-org--last-workspace-ids
                            (copy-sequence workspace-ids))
                      (gsmlg-org-note-org--select-feed
                       gsmlg-org-note-org--feed-file))
                  (error
                   (gsmlg-org-note-org--offer-last-good-or-abort
                    workspace-ids err)))
              (gsmlg-org-note-org--publication-release)))))
      (setq gsmlg-org-note-org--refresh-active nil)))
  (gsmlg-org-note-org-feed-file))

(defun gsmlg-org-note-org--refresh-before-agenda (&rest _)
  "Refresh the Org Note feed before building an agenda buffer."
  (require 'org-note)
  (gsmlg-org-note-org-refresh-feed))

(defun gsmlg-org-note-org--goto (orig-fun &optional highlight)
  "Open Org Note item context or call ORIG-FUN with HIGHLIGHT."
  (let* ((marker (org-get-at-bol 'org-marker))
         (workspace (and marker
                         (with-current-buffer (marker-buffer marker)
                           (org-entry-get (marker-position marker)
                                          "ORG_NOTE_WORKSPACE_ID"
                                          'selective))))
         (item (and marker
                    (with-current-buffer (marker-buffer marker)
                      (org-entry-get (marker-position marker)
                                     "ORG_NOTE_ITEM_ID"
                                     'selective)))))
    (if (and workspace item)
        (org-note-item-context workspace item)
      (funcall orig-fun highlight))))

(gsmlg-org-note-org-check-capture-recovery)

(add-hook 'kill-emacs-hook
          #'gsmlg-org-note-org--release-transient-reservations-on-exit)

(provide 'gsmlg-org-note-org)
;;; gsmlg-org-note-org.el ends here
