;;; org-note-org-bridge-capture-journal-test.el --- Capture journal tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for the private Org Note Capture recovery journal.

;;; Code:

(require 'test-helper)
(require 'gsmlg-org-note-org)
(require 'org-note-operation)
(require 'json)

(defmacro gsmlg-org-note-capture-journal-test--with-directory (&rest body)
  "Run BODY with an isolated Capture recovery directory."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "gsmlg-capture-journal-" t))
          (gsmlg-state-directory (file-name-as-directory root)))
     (unwind-protect
         (progn ,@body)
       (delete-directory root t))))

(defun gsmlg-org-note-capture-journal-test--frozen ()
  "Return a representative secret-free frozen Capture PUT."
  (let ((body (encode-coding-string
               "{\"schema_version\":1,\"actor_id\":\"actor\",\"operation_id\":\"op-1\",\"workspace_id\":\"ws-1\",\"path\":\"inbox.org\",\"source\":\"* Existing\\n* fencing_token is ordinary text\\n\",\"expected_revision\":4,\"lease_proofs\":{}}"
               'utf-8)))
    (list :method "PUT"
          :endpoint "https://agent-note.gsmlg.net"
          :url "https://agent-note.gsmlg.net/api/org/documents/doc-1"
          :route "/api/org/documents/doc-1"
          :query nil
          :headers '(("Accept" . "application/json")
                     ("Content-Type" . "application/json; charset=utf-8"))
          :body body
          :body-sha256 (secure-hash 'sha256 body)
          :redaction-secrets nil)))

(defun gsmlg-org-note-capture-journal-test--attempt (&optional state)
  "Return a representative Capture attempt in STATE."
  (let ((source "* fencing_token is ordinary text\n"))
    (list :state (or state 'prepared)
          :operation-id "op-1"
          :frozen (gsmlg-org-note-capture-journal-test--frozen)
          :workspace-id "ws-1"
          :document-id "doc-1"
          :path "inbox.org"
          :expected-revision 4
          :source source
          :digest (secure-hash 'sha256 source)
          :tick 1
          :created-at 1000)))

(ert-deftest gsmlg-org-note-capture-journal-has-exact-secret-free-schema ()
  "Capture journals are canonical, private, bounded, and structurally safe."
  (gsmlg-org-note-capture-journal-test--with-directory
    (let* ((attempt (gsmlg-org-note-capture-journal-test--attempt))
           (file (gsmlg-org-note-org--capture-journal-write attempt))
           (directory (file-name-directory file))
           (raw (with-temp-buffer
                  (insert-file-contents-literally file)
                  (buffer-string)))
           (record (json-parse-string raw :object-type 'alist
                                      :array-type 'array
                                      :null-object :null
                                      :false-object :false))
           (body (alist-get 'body record)))
      (should (= (logand (file-modes directory) #o777) #o700))
      (should (= (logand (file-modes file) #o777) #o600))
      (should (equal (mapcar #'car record)
                     '(schema_version checksum body)))
      (should (equal (mapcar #'car body)
                     '(operation_kind state operation_id endpoint workspace_id
                       document_id path expected_revision created_at updated_at
                       source_digest source lease_proofs wire_method wire_url
                       wire_route wire_query wire_headers wire_body_base64
                       wire_body_sha256)))
      (should (assq 'lease_proofs body))
      (should-not (cdr (assq 'lease_proofs body)))
      (should-not (assq 'fencing_token body))
      (should (string-match-p "fencing_token is ordinary text" raw))
      (should (equal (plist-get
                      (gsmlg-org-note-org--capture-journal-read file) :state)
                     'prepared)))))

(ert-deftest gsmlg-org-note-capture-journal-replacement-is-atomic-and-fsynced ()
  "Replacement leaves the old record authoritative until same-dir rename."
  (gsmlg-org-note-capture-journal-test--with-directory
    (let* ((attempt (gsmlg-org-note-capture-journal-test--attempt))
           (file (gsmlg-org-note-org--capture-journal-write attempt))
           (real-rename (symbol-function 'rename-file))
           (real-write (symbol-function 'write-region))
           (saw-old nil)
           (saw-fsync-binding nil))
      (cl-letf (((symbol-function 'write-region)
                 (lambda (start end filename &rest args)
                   (setq saw-fsync-binding
                         (null write-region-inhibit-fsync))
                   (apply real-write start end filename args)))
                ((symbol-function 'rename-file)
                 (lambda (temporary target ok-if-exists)
                   (setq saw-old
                         (eq (plist-get
                              (gsmlg-org-note-org--capture-journal-read target)
                              :state)
                             'prepared)
                         saw-fsync-binding
                         (null write-region-inhibit-fsync))
                   (funcall real-rename temporary target ok-if-exists))))
        (gsmlg-org-note-org--capture-journal-write
         (plist-put attempt :state 'dispatched)))
      (should saw-old)
      (should saw-fsync-binding)
      (should (eq (plist-get
                   (gsmlg-org-note-org--capture-journal-read file) :state)
                  'dispatched)))))

(ert-deftest gsmlg-org-note-capture-journal-fails-closed-on-corruption ()
  "Malformed, mode-unsafe, and symlink journals are quarantined and refused."
  (dolist (attack '(corrupt checksum permissions symlink))
    (gsmlg-org-note-capture-journal-test--with-directory
      (let* ((attempt (gsmlg-org-note-capture-journal-test--attempt))
             (file (gsmlg-org-note-org--capture-journal-write attempt)))
        (pcase attack
          ('corrupt
           (with-temp-file file (insert "{\"bad\":true}"))
           (set-file-modes file #o600))
          ('checksum
           (with-temp-buffer
             (insert-file-contents-literally file)
             (goto-char (point-min))
             (search-forward "* fencing_token")
             (replace-match "* changed-token")
             (write-region (point-min) (point-max) file nil 'silent))
           (set-file-modes file #o600))
          ('permissions (set-file-modes file #o644))
          ('symlink
           (let ((target (concat file ".target")))
             (rename-file file target)
             (make-symbolic-link target file))))
        (should-error (gsmlg-org-note-org--capture-journal-read file)
                      :type 'user-error)
        (should-not (file-exists-p file))
        (should (directory-files
                 (file-name-directory file) nil
                 (concat (regexp-quote (file-name-nondirectory file))
                         "\\.quarantine\\.")))))))

(ert-deftest gsmlg-org-note-capture-journal-refuses-unsafe-parent ()
  "The recovery parent must remain an owned non-symlink mode-0700 directory."
  (gsmlg-org-note-capture-journal-test--with-directory
    (let* ((attempt (gsmlg-org-note-capture-journal-test--attempt))
           (file (gsmlg-org-note-org--capture-journal-write attempt))
           (directory (file-name-directory file)))
      (unwind-protect
          (progn
            (set-file-modes directory #o755)
            (should-error
             (gsmlg-org-note-org--capture-journal-read file)
             :type 'user-error))
        (set-file-modes directory #o700)))))

(ert-deftest gsmlg-org-note-capture-journal-startup-check-is-local-only ()
  "Recovery discovery reads local state without service access."
  (gsmlg-org-note-capture-journal-test--with-directory
    (gsmlg-org-note-org--capture-journal-write
     (gsmlg-org-note-capture-journal-test--attempt 'dispatched))
    (let ((gsmlg-org-note-org--capture-recovery-required nil))
      (cl-letf (((symbol-function 'org-note-client-request)
                 (lambda (&rest _args) (ert-fail "unexpected network read")))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (&rest _args) (ert-fail "unexpected dispatch"))))
        (let ((records (gsmlg-org-note-org-check-capture-recovery)))
          (should (= (length records) 1))
          (should (eq (plist-get (car records) :state) 'dispatched)))))))

(ert-deftest gsmlg-org-note-capture-journal-commit-write-failure-never-resends ()
  "A remote commit followed by local failure retries only journal durability."
  (gsmlg-org-note-capture-journal-test--with-directory
    (let ((dispatches 0)
          (writes 0)
          (real-write
           (symbol-function 'gsmlg-org-note-org--capture-journal-write)))
      (with-temp-buffer
        (let ((org-mode-hook nil)
              (gsmlg-org-note-org-enable t)
              (gsmlg-org-note-capture-workspace-id "ws-1")
              (gsmlg-org-note-capture-document-id "doc-1")
              (gsmlg-org-note-capture-document-path "inbox.org"))
          (org-mode)
          (insert "* Captured\n")
          (cl-letf (((symbol-function 'org-note-operation-get-document)
                     (lambda (_workspace _document)
                       '((id . "doc-1") (workspace_id . "ws-1")
                         (path . "inbox.org") (source . "* Existing\n")
                         (revision . 4) (archived_at . nil))))
                    ((symbol-function 'org-note-operation-lease-proofs)
                     (lambda (_document) (make-hash-table :test #'equal)))
                    ((symbol-function 'org-note-client-new-operation-id)
                     (lambda () "op-1"))
                    ((symbol-function 'org-note-operation--dispatch-frozen)
                     (lambda (_frozen)
                       (cl-incf dispatches)
                       '((document_revisions . ((doc-1 . 5))))))
                    ((symbol-function
                      'gsmlg-org-note-org--capture-journal-write)
                     (lambda (attempt)
                       (cl-incf writes)
                       (if (eq (plist-get attempt :state) 'committed)
                           (error "Disk full")
                         (funcall real-write attempt)))))
            (should-error (gsmlg-org-note-org-capture-before-finalize))
            (should (= dispatches 1))
            (should (eq (plist-get gsmlg-org-note-org--capture-attempt :state)
                        'committed-pending-journal))
            (should buffer-read-only))
          (cl-letf (((symbol-function 'org-note-operation--dispatch-frozen)
                     (lambda (_frozen) (cl-incf dispatches))))
            (gsmlg-org-note-org-retry-capture-journal)
            (should (= dispatches 1))
            (should (eq (plist-get gsmlg-org-note-org--capture-attempt :state)
                        'committed))))))))

(provide 'org-note-org-bridge-capture-journal-test)
;;; org-note-org-bridge-capture-journal-test.el ends here
