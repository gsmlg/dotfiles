;;; emacs-agent-document.el --- Buffer-first documents  -*- lexical-binding: t; -*-

;;; Commentary:

;; Canonical buffer ownership, opaque revisions, bounded reads, and external
;; filesystem reconciliation for one editor runtime.

;;; Code:

(require 'cl-lib)
(require 'emacs-agent-policy)
(require 'emacs-agent-runtime)
(require 'subr-x)

(declare-function emacs-agent-changeset-changeset-id
                  "emacs-agent-changeset" (changeset))
(declare-function emacs-agent-changeset-list
                  "emacs-agent-changeset" (&optional runtime))
(declare-function emacs-agent-changeset-status
                  "emacs-agent-changeset" (changeset))
(declare-function emacs-agent-changeset-touched-documents
                  "emacs-agent-changeset" (changeset))

(defvar emacs-agent-document-cursors (make-hash-table :test #'equal)
  "Opaque read cursor registry.")

(defcustom emacs-agent-document-cursor-ttl 300
  "Seconds for which an unused read cursor remains valid."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-document-default-max-chars (* 256 1024)
  "Default maximum number of characters returned by a document read."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-document-default-max-lines 2000
  "Default maximum number of lines returned by a document read."
  :type 'integer
  :group 'emacs-agent-editor)

(cl-defstruct (emacs-agent-document
               (:constructor emacs-agent-document--create))
  canonical-path buffer major-mode cached-revision content-hash buffer-tick
  disk-fingerprint modified externally-modified coding-system eol-style
  last-changeset-id diagnostics-revision runtime degraded)

(defun emacs-agent-document--assert-runtime-target (runtime target)
  "Require RUNTIME and resolved TARGET, returning TARGET."
  (unless (emacs-agent-runtime-p runtime)
    (emacs-agent-signal 'runtime_not_started))
  (unless (emacs-agent-resolved-target-p target)
    (signal 'wrong-type-argument
            (list 'emacs-agent-resolved-target target)))
  target)

(defun emacs-agent-document--registry (runtime)
  "Return RUNTIME's canonical document registry."
  (or (emacs-agent-runtime-document-registry runtime)
      (let ((registry (make-hash-table :test #'equal)))
        (setf (emacs-agent-runtime-document-registry runtime) registry)
        registry)))

(defun emacs-agent-document--epoch (runtime)
  "Return the revision epoch belonging to RUNTIME."
  (emacs-agent-runtime-server-epoch runtime))

(defun emacs-agent-document-output-fields (target)
  "Return canonical public path metadata for resolved TARGET."
  (emacs-agent-policy-target-fields target))

(defun emacs-agent-document--disk-fingerprint (path)
  "Return an inexpensive disk fingerprint for PATH, or `missing'."
  (if-let* ((attributes (file-attributes path 'integer)))
      (if (file-regular-p path)
          (list (file-attribute-size attributes)
                (file-attribute-modification-time attributes)
                (file-attribute-inode-number attributes)
                (file-attribute-device-number attributes)
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally path)
                  (secure-hash 'sha256 (current-buffer))))
        (list 'special
              (file-attribute-type attributes)
              (file-attribute-modification-time attributes)))
    'missing))

(defun emacs-agent-document--existing-buffer (canonical)
  "Return a live visiting buffer for CANONICAL."
  (or (get-file-buffer canonical)
      (cl-find-if
       (lambda (buffer)
         (when-let* ((name (buffer-file-name buffer)))
           (condition-case nil
               (equal (file-truename name) canonical)
             (file-error nil))))
       (buffer-list))))

(defun emacs-agent-document--buffer-binary-p (buffer)
  "Return non-nil if BUFFER has a NUL character."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (search-forward (string 0)
                        (min (point-max) (+ (point-min) 8192))
                        t)))))

(defun emacs-agent-document--assert-buffer-safe (buffer path)
  "Reject binary or oversized BUFFER representing PATH."
  (when (emacs-agent-document--buffer-binary-p buffer)
    (emacs-agent-signal
     'unsupported_document_type :path path :reason 'binary))
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (when (> (string-bytes
                (buffer-substring-no-properties (point-min) (point-max)))
               emacs-agent-policy-maximum-document-bytes)
        (emacs-agent-signal 'document_too_large :path path)))))

(defun emacs-agent-document--eol-style (coding-system)
  "Return the public EOL style name for CODING-SYSTEM."
  (pcase (coding-system-eol-type coding-system)
    (0 "lf")
    (1 "crlf")
    (2 "cr")
    (_ "undecided")))

;;;###autoload
(defun emacs-agent-document-open (runtime target &optional for-create)
  "Return the canonical document for TARGET in RUNTIME.

TARGET must already be resolved by the path policy.  Existing visiting
buffers, including buffers with unsaved changes, always win.  FOR-CREATE
permits a missing leaf but does not insert or save any content."
  (emacs-agent-document--assert-runtime-target runtime target)
  (emacs-agent-policy-assert-document-target runtime target)
  (let* ((canonical
          (emacs-agent-resolved-target-canonical-path target))
         (registry (emacs-agent-document--registry runtime))
         (known (gethash canonical registry))
         (existing (emacs-agent-document--existing-buffer canonical)))
    (if (and known
             (buffer-live-p (emacs-agent-document-buffer known)))
        (progn
          (emacs-agent-document--assert-buffer-safe
           (emacs-agent-document-buffer known) canonical)
          known)
      (when (and (not for-create)
                 (not (file-exists-p canonical))
                 (not (buffer-live-p existing)))
        (emacs-agent-signal 'document_not_found :path canonical))
      (let* ((buffer
              (or existing
                  (let ((enable-local-variables :safe)
                        (enable-local-eval nil)
                        (noninteractive t))
                    (find-file-noselect canonical nil nil nil))))
             (document
              (emacs-agent-document--create
               :canonical-path canonical
               :buffer buffer
               :disk-fingerprint
               (emacs-agent-document--disk-fingerprint canonical)
               :runtime runtime)))
        (condition-case error-data
            (emacs-agent-document--assert-buffer-safe buffer canonical)
          (error
           (unless existing
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))
           (signal (car error-data) (cdr error-data))))
        (with-current-buffer buffer
          (setf (emacs-agent-document-major-mode document) major-mode
                (emacs-agent-document-modified document) (buffer-modified-p)
                (emacs-agent-document-coding-system document)
                buffer-file-coding-system
                (emacs-agent-document-eol-style document)
                (emacs-agent-document--eol-style
                 buffer-file-coding-system)))
        (puthash canonical document registry)
        (emacs-agent-document-revision document)
        document))))

;;;###autoload
(defun emacs-agent-document-revision (document)
  "Return DOCUMENT's opaque revision, refreshing its cached hash as needed."
  (let ((buffer (emacs-agent-document-buffer document)))
    (unless (buffer-live-p buffer)
      (emacs-agent-signal
       'document_not_found
       :path (emacs-agent-document-canonical-path document)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let* ((tick (buffer-chars-modified-tick))
               (coding (or buffer-file-coding-system 'undecided))
               (eol (emacs-agent-document--eol-style coding))
               (hash
                (secure-hash
                 'sha256
                 (concat
                  (symbol-name coding) "\0" eol "\0"
                  (buffer-substring-no-properties
                   (point-min) (point-max))))))
          (unless (and
                   (equal hash
                          (emacs-agent-document-content-hash document))
                   (equal coding
                          (emacs-agent-document-coding-system document)))
            (setf (emacs-agent-document-buffer-tick document) tick
                  (emacs-agent-document-content-hash document) hash
                  (emacs-agent-document-coding-system document) coding
                  (emacs-agent-document-eol-style document) eol
                  (emacs-agent-document-cached-revision document)
                  (format
                   "rev:%s:%s"
                   (emacs-agent-document--epoch
                    (emacs-agent-document-runtime document))
                   hash)))
          (setf (emacs-agent-document-modified document) (buffer-modified-p))
          (emacs-agent-document-cached-revision document))))))

(defun emacs-agent-document--unvisited-revision (runtime absolute)
  "Return a content revision for unvisited ABSOLUTE in RUNTIME."
  (with-temp-buffer
    (insert-file-contents absolute)
    (let* ((coding (or buffer-file-coding-system 'undecided))
           (eol (emacs-agent-document--eol-style coding))
           (hash
            (secure-hash
             'sha256
             (concat
              (symbol-name coding) "\0" eol "\0"
              (buffer-substring-no-properties (point-min) (point-max))))))
      (format "rev:%s:%s"
              (emacs-agent-document--epoch runtime)
              hash))))

(defun emacs-agent-document--buffer-content (buffer)
  "Return the widened authoritative text in BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun emacs-agent-document--checkpoint-mark-dirty (document)
  "Mark DOCUMENT's authoritative visiting buffer as requiring reconciliation."
  (let ((buffer (emacs-agent-document-buffer document)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((visited-file buffer-file-name))
          (unwind-protect
              (progn
                (setq buffer-file-name nil)
                (set-buffer-modified-p t))
            (setq buffer-file-name visited-file)))))
    (setf (emacs-agent-document-modified document) t)))

(defun emacs-agent-document--checkpoint-assert-identity
    (document partial-completion)
  "Require DOCUMENT's path and buffer identity to remain stable.
PARTIAL-COMPLETION describes whether a save has already returned."
  (let* ((runtime (emacs-agent-document-runtime document))
         (path (emacs-agent-document-canonical-path document))
         (buffer (emacs-agent-document-buffer document))
         (buffer-path
          (and (buffer-live-p buffer)
               (buffer-local-value 'buffer-file-name buffer)))
         path-target buffer-target)
    (condition-case nil
        (setq
         path-target
         (emacs-agent-policy-resolve-target
          runtime path :for-create t)
         buffer-target
         (and buffer-path
              (emacs-agent-policy-resolve-target
               runtime buffer-path :for-create t)))
      (error
       (emacs-agent-signal
        'checkpoint_failed
        :path path
        :reason 'target_identity_changed
        :checkpointed nil
        :partial_completion partial-completion
        :reconciliation_required t
        :filesystem_rollback_guaranteed nil)))
    (unless
        (and path-target
             buffer-target
             (equal
              (emacs-agent-resolved-target-canonical-path path-target)
              path)
             (equal
              (emacs-agent-resolved-target-canonical-path buffer-target)
              path))
      (emacs-agent-signal
       'checkpoint_failed
       :path path
       :reason 'target_identity_changed
       :checkpointed nil
       :partial_completion partial-completion
       :reconciliation_required t
       :filesystem_rollback_guaranteed nil))))

(defun emacs-agent-document--checkpoint-disk-snapshot-1
    (document partial-completion)
  "Build a bounded, stable disk snapshot for DOCUMENT.
PARTIAL-COMPLETION describes whether a save has already returned."
  (emacs-agent-document--checkpoint-assert-identity
   document partial-completion)
  (let* ((path (emacs-agent-document-canonical-path document))
         (buffer (emacs-agent-document-buffer document))
         (coding-system
          (with-current-buffer buffer
            (or buffer-file-coding-system 'undecided)))
         (attributes (file-attributes path 'integer)))
    (if (null attributes)
        (list :fingerprint 'missing :content nil)
      (let ((size (file-attribute-size attributes)))
        (unless (and (null (file-attribute-type attributes))
                     (integerp size)
                     (<= size
                         emacs-agent-policy-maximum-document-bytes))
          (emacs-agent-signal
           'checkpoint_failed
           :path path
           :reason 'unsafe_disk_result
           :checkpointed nil
           :partial_completion partial-completion
           :reconciliation_required t
           :filesystem_rollback_guaranteed nil))
        (let* ((raw
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally
                   path nil 0
                   (1+ emacs-agent-policy-maximum-document-bytes))
                  (buffer-string)))
               (after-attributes (file-attributes path 'integer)))
          (emacs-agent-document--checkpoint-assert-identity
           document partial-completion)
          (unless
              (and after-attributes
                   (null (file-attribute-type after-attributes))
                   (= (length raw) size)
                   (= (file-attribute-size after-attributes) size)
                   (equal
                    (file-attribute-modification-time after-attributes)
                    (file-attribute-modification-time attributes))
                   (equal
                    (file-attribute-inode-number after-attributes)
                    (file-attribute-inode-number attributes))
                   (equal
                    (file-attribute-device-number after-attributes)
                    (file-attribute-device-number attributes)))
            (emacs-agent-signal
             'checkpoint_failed
             :path path
             :reason 'disk_changed_during_checkpoint
             :checkpointed nil
             :partial_completion partial-completion
             :reconciliation_required t
             :filesystem_rollback_guaranteed nil))
          (list
           :fingerprint
           (list
            size
            (file-attribute-modification-time attributes)
            (file-attribute-inode-number attributes)
            (file-attribute-device-number attributes)
            (secure-hash 'sha256 raw))
           :content
           (decode-coding-string raw coding-system)))))))

(defun emacs-agent-document--checkpoint-disk-snapshot
    (document partial-completion)
  "Return a bounded, stable disk snapshot for DOCUMENT.
PARTIAL-COMPLETION describes whether a save has already returned."
  (condition-case error-data
      (emacs-agent-document--checkpoint-disk-snapshot-1
       document partial-completion)
    (emacs-agent-error
     (when partial-completion
       (emacs-agent-document--checkpoint-mark-dirty document))
     (signal (car error-data) (cdr error-data)))
    (error
     (when partial-completion
       (emacs-agent-document--checkpoint-mark-dirty document))
     (emacs-agent-signal
      'checkpoint_failed
      :path (emacs-agent-document-canonical-path document)
      :message (error-message-string error-data)
      :reason 'disk_verification_failed
      :checkpointed nil
      :partial_completion partial-completion
      :reconciliation_required t
      :filesystem_rollback_guaranteed nil))))

(defvar emacs-agent-document--checkpoint-write-document nil
  "Document guarded at the active checkpoint write boundary.")

(defvar emacs-agent-document--checkpoint-write-fingerprint nil
  "Expected disk fingerprint at the active checkpoint write boundary.")

(defvar emacs-agent-document--checkpoint-write-rejection nil
  "Structured rejection raised by the active checkpoint write guard.")

(defvar emacs-agent-document--checkpoint-primary-write-completed nil
  "Whether the active checkpoint completed its primary buffer write.")

(defun emacs-agent-document--checkpoint-mark-primary-write-completed ()
  "Mark the active checkpoint's primary buffer write as completed."
  (setq emacs-agent-document--checkpoint-primary-write-completed t))

(defun emacs-agent-document--checkpoint-validate-write-boundary
    (check-fingerprint)
  "Revalidate the active checkpoint immediately before a write.
When CHECK-FINGERPRINT is non-nil, also require the original disk
fingerprint.  The later `write-region' boundary omits that comparison because
Emacs may already have renamed the original file to make a backup."
  (when emacs-agent-document--checkpoint-write-document
    (let* ((document
            emacs-agent-document--checkpoint-write-document)
           (path (emacs-agent-document-canonical-path document)))
      (emacs-agent-document--checkpoint-assert-identity document nil)
      (condition-case nil
          (emacs-agent-document--assert-buffer-safe
           (emacs-agent-document-buffer document) path)
        (emacs-agent-error
         (emacs-agent-signal
          'checkpoint_failed
          :path path
          :reason 'unsafe_buffer_result
          :checkpointed nil
          :partial_completion nil
          :reconciliation_required nil
          :filesystem_rollback_guaranteed t)))
      (when check-fingerprint
        (let ((fingerprint
               (plist-get
                (emacs-agent-document--checkpoint-disk-snapshot
                 document nil)
                :fingerprint)))
          (unless
              (equal
               fingerprint
               emacs-agent-document--checkpoint-write-fingerprint)
            (setf
             (emacs-agent-document-externally-modified document) t)
            (emacs-agent-signal
             'external_change_conflict
             :path path
             :reason 'disk_changed_before_write
             :checkpointed nil
             :partial_completion nil
             :reconciliation_required t
             :filesystem_rollback_guaranteed t))))))
  nil)

(defun emacs-agent-document--checkpoint-run-write-guard
    (check-fingerprint)
  "Run the active write guard and remember a structured rejection.
CHECK-FINGERPRINT is forwarded to the boundary validator."
  (condition-case error-data
      (emacs-agent-document--checkpoint-validate-write-boundary
       check-fingerprint)
    (emacs-agent-error
     (setq emacs-agent-document--checkpoint-write-rejection
           error-data)
     (signal (car error-data) (cdr error-data)))))

(defun emacs-agent-document--checkpoint-write-guard ()
  "Revalidate the active checkpoint after `before-save-hook'.
Return nil so Emacs proceeds with its normal buffer writer."
  (emacs-agent-document--checkpoint-run-write-guard t))

(defun emacs-agent-document--checkpoint-write-region-guard (_start _end)
  "Revalidate the active checkpoint at its `write-region' boundary.
_START and _END are the bounds about to be written."
  (emacs-agent-document--checkpoint-run-write-guard nil))

;;;###autoload
(defun emacs-agent-document-checkpoint (document)
  "Save DOCUMENT and verify its authoritative text on disk.

The verification is deliberately non-mutating: it reads the saved file with
the visiting buffer's coding system instead of reverting the buffer.  A
successful return value is DOCUMENT's stable post-save revision.  Save hooks
may legitimately change the buffer before it is written.  A hook that leaves
the buffer dirty, a disk-only rewrite, or any later revision change produces a
stable checkpoint error without clearing authoritative buffer content."
  (unless (emacs-agent-document-p document)
    (signal 'wrong-type-argument
            (list 'emacs-agent-document document)))
  (let* ((buffer (emacs-agent-document-buffer document))
         (path (emacs-agent-document-canonical-path document))
         (_ (unless (buffer-live-p buffer)
              (emacs-agent-signal 'document_not_found :path path)))
         (before-snapshot
          (emacs-agent-document--checkpoint-disk-snapshot
           document nil))
         (before-fingerprint
          (plist-get before-snapshot :fingerprint))
         (stored-fingerprint
          (emacs-agent-document-disk-fingerprint document))
         save-error write-rejection primary-write-completed
         post-save-revision)
    (unless (equal stored-fingerprint before-fingerprint)
      (setf (emacs-agent-document-externally-modified document) t)
      (emacs-agent-signal
       'external_change_conflict
       :path path
       :reason 'disk_changed_before_checkpoint
       :checkpointed nil
       :partial_completion nil
       :reconciliation_required t
       :filesystem_rollback_guaranteed t))
    (condition-case error-data
        (with-current-buffer buffer
          (let ((emacs-agent-document--checkpoint-write-document
                 document)
                (emacs-agent-document--checkpoint-write-fingerprint
                 before-fingerprint)
                (emacs-agent-document--checkpoint-write-rejection nil)
                (emacs-agent-document--checkpoint-primary-write-completed
                 nil)
                (write-contents-functions
                 (cons
                  #'emacs-agent-document--checkpoint-write-guard
                  write-contents-functions))
                (write-region-annotate-functions
                 (cons
                  #'emacs-agent-document--checkpoint-write-region-guard
                  write-region-annotate-functions))
                (after-save-hook
                 (cons
                  #'emacs-agent-document--checkpoint-mark-primary-write-completed
                  after-save-hook)))
            (unwind-protect
                (save-buffer)
              (setq
               write-rejection
               emacs-agent-document--checkpoint-write-rejection
               primary-write-completed
               emacs-agent-document--checkpoint-primary-write-completed))))
      (error
       (setq save-error error-data)))
    (when (and write-rejection
               (not primary-write-completed))
      (signal (car write-rejection) (cdr write-rejection)))
    (setq post-save-revision
          (emacs-agent-document-revision document))
    (if save-error
        (let (after-snapshot verification-error)
          (condition-case error-data
              (setq
               after-snapshot
               (emacs-agent-document--checkpoint-disk-snapshot
                document t))
            (error
             (setq verification-error error-data)))
          (let* ((buffer-modified
                  (with-current-buffer buffer (buffer-modified-p)))
                 (after-fingerprint
                  (and after-snapshot
                       (plist-get after-snapshot :fingerprint)))
                 (disk-changed
                  (and after-snapshot
                       (not
                        (equal before-fingerprint after-fingerprint))))
                 (current-revision
                  (emacs-agent-document-revision document))
                 (buffer-safe
                  (condition-case nil
                      (progn
                        (emacs-agent-document--assert-buffer-safe
                         buffer path)
                        t)
                    (emacs-agent-error nil)))
                 (aligned
                  (and (not verification-error)
                       buffer-safe
                       (not buffer-modified)
                       (not (eq after-fingerprint 'missing))
                       (equal
                        (plist-get after-snapshot :content)
                        (emacs-agent-document--buffer-content buffer))
                       (equal current-revision post-save-revision))))
            (if aligned
                (setf
                 (emacs-agent-document-disk-fingerprint document)
                 after-fingerprint
                 (emacs-agent-document-externally-modified document)
                 nil)
              (progn
                (emacs-agent-document--checkpoint-mark-dirty document)
                (when disk-changed
                  (setf
                   (emacs-agent-document-externally-modified document) t))))
            (emacs-agent-signal
             'save_failed
             :path path
             :message (error-message-string save-error)
             :reason 'save_error
             :checkpointed nil
             :partial_completion
             (and (or disk-changed verification-error) t)
             :reconciliation_required (not aligned)
             :filesystem_rollback_guaranteed nil)))
      (condition-case nil
          (emacs-agent-document--assert-buffer-safe buffer path)
        (emacs-agent-error
         (emacs-agent-document--checkpoint-mark-dirty document)
         (emacs-agent-signal
          'checkpoint_failed
          :path path
          :reason 'unsafe_buffer_result
          :checkpointed nil
          :partial_completion t
          :reconciliation_required t
          :filesystem_rollback_guaranteed nil)))
      (let* ((modified
              (with-current-buffer buffer (buffer-modified-p)))
             (disk-snapshot
              (emacs-agent-document--checkpoint-disk-snapshot
               document t))
             (disk-fingerprint
              (plist-get disk-snapshot :fingerprint))
             (disk-content
              (plist-get disk-snapshot :content))
             (current-revision
              (emacs-agent-document-revision document))
             (current-content
              (emacs-agent-document--buffer-content buffer)))
        (cond
         (modified
          (unless (eq disk-fingerprint 'missing)
            (setf (emacs-agent-document-disk-fingerprint document)
                  disk-fingerprint
                  (emacs-agent-document-externally-modified document)
                  nil))
          (emacs-agent-signal
           'checkpoint_failed
           :path path
           :reason 'buffer_modified_after_save
           :checkpointed nil
           :partial_completion t
           :reconciliation_required nil
           :filesystem_rollback_guaranteed nil))
         ((or (eq disk-fingerprint 'missing)
              (not (equal disk-content current-content))
              (not (equal current-revision post-save-revision)))
          (emacs-agent-document--checkpoint-mark-dirty document)
          (setf (emacs-agent-document-externally-modified document) t)
          (emacs-agent-signal
           'checkpoint_failed
           :path path
           :reason 'disk_content_mismatch
           :checkpointed nil
           :partial_completion t
           :reconciliation_required t
           :filesystem_rollback_guaranteed nil))
         (t
          (setf (emacs-agent-document-disk-fingerprint document)
                disk-fingerprint
                (emacs-agent-document-externally-modified document)
                nil)
          post-save-revision))))))

;;;###autoload
(defun emacs-agent-document-revision-for-target (runtime target)
  "Return the current opaque revision for TARGET in RUNTIME."
  (let ((document (emacs-agent-document-open runtime target)))
    (emacs-agent-document-reconcile document)
    (emacs-agent-document-revision document)))

(defun emacs-agent-document--active-changesets (runtime canonical)
  "Return active change-set IDs touching CANONICAL in RUNTIME."
  (let (active)
    (when (fboundp 'emacs-agent-changeset-list)
      (dolist (changeset (emacs-agent-changeset-list runtime))
        (when (and
               (memq (emacs-agent-changeset-status changeset)
                     '(applied checkpointed reviewed conflicted))
               (member canonical
                       (emacs-agent-changeset-touched-documents changeset)))
          (push (emacs-agent-changeset-changeset-id changeset) active))))
    (nreverse active)))

;;;###autoload
(defun emacs-agent-document-status (runtime target)
  "Return status for TARGET in RUNTIME without visiting an unvisited file."
  (emacs-agent-document--assert-runtime-target runtime target)
  (emacs-agent-policy-assert-document-target runtime target)
  (let* ((absolute
          (emacs-agent-resolved-target-canonical-path target))
         (buffer (emacs-agent-document--existing-buffer absolute))
         (known
          (gethash absolute
                   (emacs-agent-document--registry runtime)))
         (document
          (cond
           ((and known
                 (buffer-live-p (emacs-agent-document-buffer known)))
            known)
           ((buffer-live-p buffer)
            (emacs-agent-document-open runtime target t))))
         (exists (file-exists-p absolute))
         (disk-fingerprint
          (emacs-agent-document--disk-fingerprint absolute))
         (known-fingerprint
          (and document
               (emacs-agent-document-disk-fingerprint document)))
         (disk-changed
          (and document
               (not (equal known-fingerprint disk-fingerprint))))
         (modified
          (and (buffer-live-p buffer)
               (with-current-buffer buffer (buffer-modified-p)))))
    (append
     (emacs-agent-document-output-fields target)
     (list
      :visited (and (buffer-live-p buffer) t)
      :exists_on_disk (and exists t)
      :modified (and modified t)
      :checkpointed (not modified)
      :disk_changed (and disk-changed t)
      :conflicted
      (and document
           (or (emacs-agent-document-externally-modified document)
               (emacs-agent-document-degraded document)
               (and modified disk-changed))
           t)
      :revision
      (cond
       (document (emacs-agent-document-revision document))
       ((file-regular-p absolute)
        (emacs-agent-document--unvisited-revision runtime absolute)))
      :coding_system
      (and (buffer-live-p buffer)
           (with-current-buffer buffer
             (symbol-name (or buffer-file-coding-system 'undecided))))
      :eol_style
      (and document (emacs-agent-document-eol-style document))
      :major_mode
      (and (buffer-live-p buffer)
           (with-current-buffer buffer (symbol-name major-mode)))
      :read_only
      (and (buffer-live-p buffer)
           (with-current-buffer buffer (and buffer-read-only t)))
      :active_changesets
      (emacs-agent-document--active-changesets runtime absolute)))))

;;;###autoload
(defun emacs-agent-document-modified-documents (runtime)
  "Return modified or conflicted managed document statuses in RUNTIME."
  (unless (emacs-agent-runtime-p runtime)
    (emacs-agent-signal 'runtime_not_started))
  (let (results)
    (maphash
     (lambda (canonical document)
       (when (and (emacs-agent-document-p document)
                  (buffer-live-p (emacs-agent-document-buffer document)))
         (let ((status
                (condition-case nil
                    (emacs-agent-document-status
                     runtime
                     (emacs-agent-policy-resolve-target
                      runtime canonical :for-create t))
                  (emacs-agent-error nil))))
           (when (and status
                      (or (plist-get status :modified)
                          (plist-get status :disk_changed)
                          (plist-get status :conflicted)
                          (not (plist-get status :exists_on_disk))))
             (push status results)))))
     (emacs-agent-document--registry runtime))
    (sort results
          (lambda (left right)
            (string< (plist-get left :path)
                     (plist-get right :path))))))

;;;###autoload
(defun emacs-agent-document-reconcile (document)
  "Reconcile DOCUMENT with disk, reloading only a clean buffer.

Return DOCUMENT.  A dirty buffer and changed disk produce
`external_change_conflict'."
  (let* ((runtime (emacs-agent-document-runtime document))
         (path (emacs-agent-document-canonical-path document))
         (target
          (emacs-agent-policy-resolve-target
           runtime path :for-create t))
         (buffer (emacs-agent-document-buffer document))
         (old (emacs-agent-document-disk-fingerprint document)))
    (emacs-agent-policy-assert-document-target runtime target)
    (let ((new (emacs-agent-document--disk-fingerprint path)))
      (if (emacs-agent-document-degraded document)
          (if (with-current-buffer buffer (buffer-modified-p))
              (emacs-agent-signal
               'external_change_conflict
               :path path
               :reason 'reconciliation_required
               :current_revision (emacs-agent-document-revision document))
            (when (eq new 'missing)
              (emacs-agent-signal 'document_not_found :path path))
            (with-current-buffer buffer
              (let ((inhibit-message t)
                    (enable-local-variables :safe)
                    (enable-local-eval nil))
                (revert-buffer :ignore-auto :noconfirm)))
            (emacs-agent-document--assert-buffer-safe buffer path)
            (setf (emacs-agent-document-disk-fingerprint document) new
                  (emacs-agent-document-externally-modified document) nil
                  (emacs-agent-document-degraded document) nil))
        (unless (equal old new)
          (if (with-current-buffer buffer (buffer-modified-p))
              (progn
                (setf (emacs-agent-document-externally-modified document) t)
                (emacs-agent-signal
                 'external_change_conflict
                 :path path
                 :current_revision (emacs-agent-document-revision document)))
            (if (eq new 'missing)
                (emacs-agent-signal 'document_not_found :path path)
              (with-current-buffer buffer
                (let ((inhibit-message t)
                      (enable-local-variables :safe)
                      (enable-local-eval nil))
                  (revert-buffer :ignore-auto :noconfirm)))
              (emacs-agent-document--assert-buffer-safe buffer path)
              (setf (emacs-agent-document-disk-fingerprint document) new
                    (emacs-agent-document-externally-modified document)
                    nil))))))
    (emacs-agent-document-revision document)
    document))

(defun emacs-agent-document--field (object key)
  "Read KEY from plist, alist, or string-keyed hash OBJECT."
  (cond
   ((hash-table-p object)
    (or (gethash key object) (gethash (symbol-name key) object)))
   ((and (listp object) (keywordp (car object)))
    (plist-get object (intern (concat ":" (symbol-name key)))))
   ((listp object)
    (or (alist-get key object)
        (alist-get (symbol-name key) object nil nil #'string=)))))

;;;###autoload
(defun emacs-agent-document-position (document position)
  "Convert public POSITION in DOCUMENT to a widened buffer position.

POSITION has a one-based `line' and a zero-based character `column'."
  (let ((line (emacs-agent-document--field position 'line))
        (column (emacs-agent-document--field position 'column))
        (buffer (emacs-agent-document-buffer document)))
    (unless (and (integerp line) (> line 0)
                 (integerp column) (>= column 0))
      (emacs-agent-signal 'invalid_position :position position))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (unless (= (forward-line (1- line)) 0)
            (emacs-agent-signal 'invalid_position :position position))
          (let ((line-end (line-end-position)))
            (when (> (+ (point) column) line-end)
              (emacs-agent-signal 'invalid_position :position position))
            (+ (point) column)))))))

(defun emacs-agent-document--new-cursor
    (runtime target document offset end start-line end-line)
  "Create a RUNTIME cursor for TARGET and DOCUMENT at OFFSET, bounded by END.
START-LINE and END-LINE preserve the original public read range."
  (let ((id
         (concat
          "cur:"
          (substring
           (secure-hash
            'sha256
            (format "%s:%s:%s:%s:%s"
                    (emacs-agent-runtime-instance-id runtime)
                    (current-time) (random) offset end))
           0 24))))
    (puthash
     id
     (list
      :runtime_id (emacs-agent-runtime-instance-id runtime)
      :document document
      :target_fields
      (copy-sequence (emacs-agent-document-output-fields target))
      :revision (emacs-agent-document-revision document)
      :offset offset
      :end end
      :start_line start-line
      :end_line end-line
      :expires (+ (float-time) emacs-agent-document-cursor-ttl))
     emacs-agent-document-cursors)
    id))

(defun emacs-agent-document--consume-cursor
    (cursor runtime target document start-line end-line)
  "Resolve CURSOR for TARGET and DOCUMENT in RUNTIME and remove the handle.
START-LINE and END-LINE may be nil or must match the original read range."
  (let ((state (and (stringp cursor)
                    (gethash cursor emacs-agent-document-cursors))))
    (remhash cursor emacs-agent-document-cursors)
    (unless (and
             state
             (equal (emacs-agent-runtime-instance-id runtime)
                    (plist-get state :runtime_id))
             (eq document (plist-get state :document))
             (equal (emacs-agent-document-output-fields target)
                    (plist-get state :target_fields))
             (or (null start-line)
                 (equal start-line
                        (plist-get state :start_line)))
             (or (null end-line)
                 (equal end-line
                        (plist-get state :end_line)))
             (> (plist-get state :expires) (float-time))
             (equal (plist-get state :revision)
                    (emacs-agent-document-revision document)))
      (emacs-agent-signal
       'revision_conflict
       :reason 'invalid_cursor
       :requires_reread t))
    state))

;;;###autoload
(defun emacs-agent-document-read
    (runtime target &optional start-line end-line max-chars cursor)
  "Read authoritative TARGET content from RUNTIME.

START-LINE and END-LINE are inclusive line bounds.  MAX-CHARS bounds the
returned page.  CURSOR continues a prior truncated read."
  (let* ((document (emacs-agent-document-open runtime target))
         (_ (emacs-agent-document-reconcile document))
         (buffer (emacs-agent-document-buffer document))
         (revision (emacs-agent-document-revision document))
         (limit (or max-chars emacs-agent-document-default-max-chars))
         (cursor-state
          (and cursor
               (emacs-agent-document--consume-cursor
                cursor runtime target document
                start-line end-line))))
    (unless (and (integerp limit) (> limit 0))
      (emacs-agent-signal 'invalid_position :field 'max_chars))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (let* ((total-lines (line-number-at-pos (point-max) t))
                 (requested-start
                  (or (plist-get cursor-state :start_line)
                      start-line 1))
                 (requested-end
                  (min (or (plist-get cursor-state :end_line)
                           end-line
                           (+ requested-start
                              (1- emacs-agent-document-default-max-lines)))
                       total-lines))
                 begin finish page-end truncated next-cursor)
            (unless (and (integerp requested-start)
                         (> requested-start 0)
                         (<= requested-start total-lines)
                         (integerp requested-end)
                         (>= requested-end requested-start))
              (emacs-agent-signal
               'invalid_position
               :start_line start-line
               :end_line end-line))
            (goto-char (point-min))
            (forward-line (1- requested-start))
            (setq begin (or (plist-get cursor-state :offset) (point)))
            (goto-char (point-min))
            (forward-line requested-end)
            (setq finish (or (plist-get cursor-state :end) (point))
                  page-end (min finish (+ begin limit))
                  truncated (< page-end finish))
            (when truncated
              (setq next-cursor
                    (emacs-agent-document--new-cursor
                     runtime target document page-end finish
                     requested-start requested-end)))
            (append
             (emacs-agent-document-output-fields target)
             (list
              :revision revision
              :modified (with-current-buffer buffer (buffer-modified-p))
              :checkpointed
              (not (with-current-buffer buffer (buffer-modified-p)))
              :coding_system
              (symbol-name
               (or (emacs-agent-document-coding-system document)
                   'undecided))
              :eol_style (emacs-agent-document-eol-style document)
              :start_line requested-start
              :end_line requested-end
              :total_lines total-lines
              :truncated truncated
              :cursor next-cursor
              :content
              (buffer-substring-no-properties begin page-end)))))))))

(provide 'emacs-agent-document)
;;; emacs-agent-document.el ends here
