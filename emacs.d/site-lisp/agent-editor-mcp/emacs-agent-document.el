;;; emacs-agent-document.el --- Buffer-first documents  -*- lexical-binding: t; -*-

;;; Commentary:

;; Canonical buffer ownership, opaque revisions, bounded reads, and external
;; filesystem reconciliation.

;;; Code:

(require 'cl-lib)
(require 'emacs-agent-policy)
(require 'subr-x)

(defvar emacs-agent-document-server-epoch
  (substring (secure-hash 'sha256
                          (format "%s:%s:%s:%s"
                                  (emacs-pid) (current-time) (random)
                                  (user-uid)))
             0 16)
  "Fallback process epoch used when a workspace supplies none.")

(defvar emacs-agent-document-registry (make-hash-table :test #'equal)
  "Fallback canonical-path to document registry.")

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
  relative-path canonical-path buffer major-mode cached-revision
  content-hash buffer-tick
  disk-fingerprint modified externally-modified coding-system eol-style
  last-changeset-id diagnostics-revision workspace degraded)

(defun emacs-agent-document--workspace-root (workspace)
  "Return canonical root for WORKSPACE."
  (emacs-agent-policy--root workspace))

(defun emacs-agent-document--registry (workspace)
  "Return the document registry belonging to WORKSPACE."
  (if (and (fboundp 'emacs-agent-workspace-p)
           (emacs-agent-workspace-p workspace)
           (fboundp 'emacs-agent-workspace-document-registry))
      (or (emacs-agent-workspace-document-registry workspace)
          emacs-agent-document-registry)
    emacs-agent-document-registry))

(defun emacs-agent-document--epoch (workspace)
  "Return the server epoch belonging to WORKSPACE."
  (or (and (fboundp 'emacs-agent-workspace-p)
           (emacs-agent-workspace-p workspace)
           (fboundp 'emacs-agent-workspace-server-epoch)
           (emacs-agent-workspace-server-epoch workspace))
      emacs-agent-document-server-epoch))

(defun emacs-agent-document--disk-fingerprint (path)
  "Return an inexpensive disk fingerprint for PATH, or `missing'."
  (if-let* ((attributes (file-attributes path 'integer)))
      (list (file-attribute-size attributes)
            (file-attribute-modification-time attributes)
            (file-attribute-inode-number attributes)
            (file-attribute-device-number attributes)
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally path)
              (secure-hash 'sha256 (current-buffer))))
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
        (search-forward (string 0) (min (point-max) (+ (point-min) 8192)) t)))))

(defun emacs-agent-document--eol-style (coding-system)
  "Return the public EOL style name for CODING-SYSTEM."
  (pcase (coding-system-eol-type coding-system)
    (0 "lf")
    (1 "crlf")
    (2 "cr")
    (_ "undecided")))

;;;###autoload
(defun emacs-agent-document-open (workspace path &optional for-create)
  "Return the canonical document for PATH in WORKSPACE.

Existing visiting buffers, including buffers with unsaved changes, always win.
FOR-CREATE permits a missing leaf but does not insert or save any content."
  (let* ((canonical (emacs-agent-policy-assert-document
                     workspace path for-create))
         (registry (emacs-agent-document--registry workspace))
         (known (gethash canonical registry)))
    (if (and known (buffer-live-p (emacs-agent-document-buffer known)))
        known
      (when (and (not for-create) (not (file-exists-p canonical)))
        (emacs-agent-signal 'document_not_found :path path))
      (let* ((existing (emacs-agent-document--existing-buffer canonical))
             (buffer
              (or existing
                  (let ((enable-local-variables :safe)
                        (enable-local-eval nil)
                        (noninteractive t))
                    (find-file-noselect canonical nil nil nil))))
             (document
              (emacs-agent-document--create
               :relative-path
               (file-relative-name canonical
                                   (emacs-agent-document--workspace-root
                                    workspace))
               :canonical-path canonical
               :buffer buffer
               :disk-fingerprint
               (emacs-agent-document--disk-fingerprint canonical)
               :workspace workspace)))
        (when (emacs-agent-document--buffer-binary-p buffer)
          (unless existing (kill-buffer buffer))
          (emacs-agent-signal 'unsupported_document_type
                              :path path :reason 'binary))
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
      (emacs-agent-signal 'document_not_found
                          :path (emacs-agent-document-relative-path document)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let ((tick (buffer-chars-modified-tick)))
          (unless (equal tick (emacs-agent-document-buffer-tick document))
            (let ((hash (secure-hash 'sha256 (current-buffer)
                                     (point-min) (point-max))))
              (setf (emacs-agent-document-buffer-tick document) tick
                    (emacs-agent-document-content-hash document) hash
                    (emacs-agent-document-cached-revision document)
                    (format "rev:%s:%s:%s"
                            (emacs-agent-document--epoch
                             (emacs-agent-document-workspace document))
                            tick hash))))
          (setf (emacs-agent-document-modified document) (buffer-modified-p))
          (emacs-agent-document-cached-revision document))))))

;;;###autoload
(defun emacs-agent-document-revision-for-path (workspace path)
  "Return the current opaque revision for PATH in WORKSPACE."
  (let ((document (emacs-agent-document-open workspace path)))
    (emacs-agent-document-reconcile document)
    (emacs-agent-document-revision document)))

;;;###autoload
(defun emacs-agent-document-reconcile (document)
  "Reconcile DOCUMENT with disk, reloading only a clean buffer.

Return DOCUMENT.  A dirty buffer and changed disk produce
`external_change_conflict'."
  (let* ((path (emacs-agent-document-canonical-path document))
         (buffer (emacs-agent-document-buffer document))
         (old (emacs-agent-document-disk-fingerprint document))
         (new (emacs-agent-document--disk-fingerprint path)))
    (if (emacs-agent-document-degraded document)
        (if (with-current-buffer buffer (buffer-modified-p))
            (emacs-agent-signal
             'external_change_conflict
             :path (emacs-agent-document-relative-path document)
             :reason 'reconciliation_required
             :current_revision (emacs-agent-document-revision document))
          (when (eq new 'missing)
            (emacs-agent-signal
             'document_not_found
             :path (emacs-agent-document-relative-path document)))
          (with-current-buffer buffer
            (let ((inhibit-message t)
                  (enable-local-variables :safe)
                  (enable-local-eval nil))
              (revert-buffer :ignore-auto :noconfirm)))
          (setf (emacs-agent-document-disk-fingerprint document) new
                (emacs-agent-document-externally-modified document) nil
                (emacs-agent-document-degraded document) nil))
      (unless (equal old new)
      (if (with-current-buffer buffer (buffer-modified-p))
          (progn
            (setf (emacs-agent-document-externally-modified document) t)
            (emacs-agent-signal
             'external_change_conflict
             :path (emacs-agent-document-relative-path document)
             :current_revision (emacs-agent-document-revision document)))
        (if (eq new 'missing)
            (emacs-agent-signal
             'document_not_found
             :path (emacs-agent-document-relative-path document))
          (with-current-buffer buffer
            (let ((inhibit-message t)
                  (enable-local-variables :safe)
                  (enable-local-eval nil))
              (revert-buffer :ignore-auto :noconfirm)))
          (setf (emacs-agent-document-disk-fingerprint document) new
                (emacs-agent-document-externally-modified document) nil)))))
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
    (or (alist-get key object) (alist-get (symbol-name key) object nil nil
                                          #'string=)))))

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

(defun emacs-agent-document--new-cursor (document offset end)
  "Create a cursor for DOCUMENT at OFFSET, bounded by END."
  (let ((id (concat "cur:"
                    (substring
                     (secure-hash 'sha256
                                  (format "%s:%s:%s:%s"
                                          (current-time) (random) offset end))
                     0 24))))
    (puthash id
             (list :document document
                   :revision (emacs-agent-document-revision document)
                   :offset offset :end end
                   :expires (+ (float-time)
                               emacs-agent-document-cursor-ttl))
             emacs-agent-document-cursors)
    id))

(defun emacs-agent-document--consume-cursor (cursor document)
  "Resolve CURSOR for DOCUMENT and remove the handle."
  (let ((state (and (stringp cursor)
                    (gethash cursor emacs-agent-document-cursors))))
    (remhash cursor emacs-agent-document-cursors)
    (unless (and state
                 (eq document (plist-get state :document))
                 (> (plist-get state :expires) (float-time))
                 (equal (plist-get state :revision)
                        (emacs-agent-document-revision document)))
      (emacs-agent-signal 'revision_conflict
                          :reason 'invalid_cursor
                          :requires_reread t))
    state))

;;;###autoload
(defun emacs-agent-document-read
    (workspace path &optional start-line end-line max-chars cursor)
  "Read authoritative PATH content from WORKSPACE.

START-LINE and END-LINE are inclusive line bounds.  MAX-CHARS bounds the
returned page.  CURSOR continues a prior truncated read."
  (let* ((document (emacs-agent-document-open workspace path))
         (_ (emacs-agent-document-reconcile document))
         (buffer (emacs-agent-document-buffer document))
         (revision (emacs-agent-document-revision document))
         (limit (or max-chars emacs-agent-document-default-max-chars))
         (cursor-state (and cursor
                            (emacs-agent-document--consume-cursor
                             cursor document))))
    (unless (and (integerp limit) (> limit 0))
      (emacs-agent-signal 'invalid_position :field 'max_chars))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (let* ((total-lines (line-number-at-pos (point-max) t))
                 (requested-start (or start-line 1))
                 (requested-end
                  (min (or end-line
                           (+ requested-start
                              (1- emacs-agent-document-default-max-lines)))
                       total-lines))
                 begin finish page-end truncated next-cursor)
            (unless (and (integerp requested-start) (> requested-start 0)
                         (<= requested-start total-lines)
                         (integerp requested-end)
                         (>= requested-end requested-start))
              (emacs-agent-signal 'invalid_position
                                  :start_line start-line :end_line end-line))
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
                     document page-end finish)))
            (list
             :path (emacs-agent-document-relative-path document)
             :revision revision
             :modified (with-current-buffer buffer (buffer-modified-p))
             :checkpointed (not (with-current-buffer buffer
                                  (buffer-modified-p)))
             :coding_system
             (symbol-name
              (or (emacs-agent-document-coding-system document) 'undecided))
             :eol_style (emacs-agent-document-eol-style document)
             :start_line requested-start
             :end_line requested-end
             :total_lines total-lines
             :truncated truncated
             :cursor next-cursor
             :content (buffer-substring-no-properties begin page-end))))))))

(provide 'emacs-agent-document)
;;; emacs-agent-document.el ends here
