;;; org-note-document.el --- Editable Org Note document buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Open and save remote Org Note documents in dedicated Org buffers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-note-operation)

(define-error 'org-note-response-error
  "Invalid Org Note document response" 'org-note-error)

(defconst org-note-document--discard-changes-prompt
  "Discard unsaved Org Note changes? "
  "Prompt used before discarding modified Org Note document buffers.")

(defconst org-note-document--reload-conflict-prompt
  "Reload remote Org Note and discard local changes? "
  "Prompt used before reloading a modified conflicted document buffer.")

(defconst org-note-document--rebase-conflict-prompt
  "Rebase local Org Note changes onto remote revision? "
  "Prompt used before rebasing a modified conflicted document buffer.")

(defvar-local org-note-document-workspace-id nil
  "Workspace identifier for the current Org Note document buffer.")

(defvar-local org-note-document-id nil
  "Document identifier for the current Org Note document buffer.")

(defvar-local org-note-document-path nil
  "Remote path for the current Org Note document buffer.")

(defvar-local org-note-document-revision nil
  "Last confirmed remote revision for the current Org Note document buffer.")

(defvar-local org-note-document-content-hash nil
  "Content hash returned for the current Org Note document buffer.")

(defvar-local org-note-document-base-source nil
  "Last confirmed source for the current Org Note document buffer.")

(defvar-local org-note-document--conflict nil
  "Conflict state for the current Org Note document buffer.")

(defvar-local org-note-document--ediff-remote-buffer nil
  "Remote comparison buffer owned by the current Ediff control buffer.")

(defvar-local org-note-document--writing-p nil
  "Non-nil while the write contents handler saves a remote document.")

(define-derived-mode org-note-document-mode org-mode "Org-Note-Document"
  "Major mode for editable Org Note document buffers."
  (setq-local buffer-offer-save t)
  (add-hook 'write-contents-functions
            #'org-note-document--write-contents nil t)
  (add-hook 'after-save-hook
            #'org-note-document--preserve-in-flight-edits t t)
  (add-hook 'kill-buffer-query-functions
            #'org-note-document--kill-buffer-query nil t))

(keymap-set org-note-document-mode-map
            "<remap> <save-buffer>" #'org-note-document-save)

(defun org-note-document--write-contents ()
  "Save the current document remotely and suppress local file writing."
  (unless org-note-document--writing-p
    (let ((org-note-document--writing-p t))
      (org-note-document--save-remote)))
  t)

(defun org-note-document--preserve-in-flight-edits ()
  "Re-mark the buffer when it differs from the source confirmed by PUT."
  (when (and org-note-document-base-source
             (not (equal (org-note-document--source)
                         org-note-document-base-source)))
    (set-buffer-modified-p t)))

(defun org-note-document--kill-buffer-query ()
  "Ask before killing a modified Org Note document buffer."
  (or (not (buffer-modified-p))
      (y-or-n-p org-note-document--discard-changes-prompt)))

(defun org-note-document--response-value (response key)
  "Return KEY from symbol-keyed alist RESPONSE."
  (cdr (assq key response)))

(defun org-note-document--object-value (object key)
  "Return KEY from JSON OBJECT decoded as an alist or hash table.

Both string and symbol keys are accepted for document identifiers."
  (cond
   ((hash-table-p object)
    (or (gethash key object)
        (gethash (if (stringp key) (intern key) (symbol-name key)) object)))
   ((listp object)
    (let ((entry (or (assoc key object)
                     (and (stringp key) (assq (intern key) object))
                     (and (symbolp key) (assoc (symbol-name key) object)))))
      (cdr entry)))
   (t nil)))

(defun org-note-document--response-revision (response document-id)
  "Return DOCUMENT-ID's confirmed revision from put-document RESPONSE.

Signal `org-note-response-error' when the response is invalid."
  (let* ((revisions
          (org-note-document--response-value response 'document_revisions))
         (revision (org-note-document--object-value revisions document-id)))
    (if (and revisions (integerp revision) (>= revision 0))
        revision
      (org-note-document--invalid-response))))

(defun org-note-document--invalid-response ()
  "Signal a safe error for an invalid document service response."
  (signal 'org-note-response-error '("Invalid Org Note document response")))

(defun org-note-document--non-empty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (> (length value) 0)))

(defun org-note-document--validate-response (response workspace-id document-id)
  "Validate RESPONSE for requested WORKSPACE-ID and DOCUMENT-ID.

Return RESPONSE when its required document metadata is valid."
  (let ((response-workspace-id
         (org-note-document--response-value response 'workspace_id))
        (response-document-id (org-note-document--response-value response 'id))
        (path (org-note-document--response-value response 'path))
        (source (org-note-document--response-value response 'source))
        (content-hash (org-note-document--response-value response 'content_hash))
        (revision (org-note-document--response-value response 'revision)))
    (unless (and (org-note-document--non-empty-string-p response-workspace-id)
                 (org-note-document--non-empty-string-p response-document-id)
                 (equal response-workspace-id workspace-id)
                 (equal response-document-id document-id)
                 (org-note-document--non-empty-string-p path)
                 (stringp source)
                 (stringp content-hash)
                 (> (length content-hash) 0)
                 (integerp revision)
                 (>= revision 0))
      (org-note-document--invalid-response))
    response))

(defun org-note-document--validate-conflict-response (response)
  "Validate latest conflict RESPONSE against the current document metadata."
  (org-note-document--validate-response response
                                        org-note-document-workspace-id
                                        org-note-document-id)
  response)

(defun org-note-document--find-buffer (workspace-id document-id)
  "Return the live document buffer for WORKSPACE-ID and DOCUMENT-ID, if any."
  (cl-find-if
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (and (derived-mode-p 'org-note-document-mode)
                 (equal org-note-document-workspace-id workspace-id)
                 (equal org-note-document-id document-id)))))
   (buffer-list)))

(defun org-note-document--populate-buffer (buffer response)
  "Populate BUFFER with a document RESPONSE from the Org Note service."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (source (org-note-document--response-value response 'source)))
      (unless (stringp source)
        (signal 'org-note-error '("Org Note document source must be a string")))
      (erase-buffer)
      (org-note-document-mode)
      (insert source)
      (setq-local org-note-document-id
                  (org-note-document--response-value response 'id)
                  org-note-document-workspace-id
                  (org-note-document--response-value response 'workspace_id)
                  org-note-document-path
                  (org-note-document--response-value response 'path)
                  org-note-document-content-hash
                  (org-note-document--response-value response 'content_hash)
                  org-note-document-revision
                  (org-note-document--response-value response 'revision)
                  org-note-document-base-source source
                  org-note-document--conflict nil)
      (set-buffer-modified-p nil))))

(defun org-note-document--discard-buffer (buffer)
  "Discard newly-created BUFFER without allowing cleanup to signal."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local kill-buffer-query-functions nil
                  kill-buffer-hook nil)
      (set-buffer-modified-p nil))
    (ignore-errors (kill-buffer buffer))))

(defun org-note-document-open (workspace-id document-id)
  "Open DOCUMENT-ID in WORKSPACE-ID in an editable Org Note buffer."
  (unless (and (org-note-document--non-empty-string-p workspace-id)
               (org-note-document--non-empty-string-p document-id))
    (user-error "Org Note document identifiers must be non-empty strings"))
  (let ((existing (org-note-document--find-buffer workspace-id document-id)))
    (if existing
        (pop-to-buffer existing)
      (let* ((response (org-note-operation-get-document workspace-id document-id))
             (_response
              (org-note-document--validate-response
               response workspace-id document-id))
             (path (org-note-document--response-value response 'path))
             (buffer (generate-new-buffer (format "*Org Note: %s*" path))))
        (condition-case error-data
            (progn
              (org-note-document--populate-buffer buffer response)
              (pop-to-buffer buffer))
          (t
           (org-note-document--discard-buffer buffer)
           (signal (car error-data) (cdr error-data))))))))

(defun org-note-document--source ()
  "Return the full source in the current Org Note document buffer."
  (save-restriction
    (widen)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-note-document--require-metadata ()
  "Return non-nil when the current buffer has required save metadata."
  (unless (derived-mode-p 'org-note-document-mode)
    (user-error "Current buffer is not an Org Note document"))
  (unless (and (org-note-document--non-empty-string-p
                org-note-document-workspace-id)
               (org-note-document--non-empty-string-p org-note-document-id)
               (org-note-document--non-empty-string-p org-note-document-path)
               (integerp org-note-document-revision)
               (>= org-note-document-revision 0))
    (user-error "Org Note document metadata is incomplete"))
  t)

(defun org-note-document--require-conflict ()
  "Return the cached conflict response for the current document buffer."
  (org-note-document--require-metadata)
  (unless org-note-document--conflict
    (user-error "No cached Org Note conflict"))
  org-note-document--conflict)

(defun org-note-document--stale-revision-error-p (error-data)
  "Return non-nil when ERROR-DATA is a stale revision HTTP error."
  (and (eq (car error-data) 'org-note-http-error)
       (let* ((payload (cadr error-data))
              (code (and (listp payload) (plist-get payload :code))))
         (or (equal code "stale_revision")
             (eq code 'stale_revision)))))

(defun org-note-document--record-conflict ()
  "Fetch and cache the current remote document for a stale save conflict.

An unsuccessful fetch or validation leaves the existing conflict cache intact."
  (org-note-document--require-metadata)
  (let ((conflict org-note-document--conflict))
    (condition-case nil
        (let ((response
               (org-note-operation-get-document org-note-document-workspace-id
                                                org-note-document-id)))
          (org-note-document--validate-conflict-response response)
          (setq-local org-note-document--conflict response)
          response)
      (error
       (setq-local org-note-document--conflict conflict)
       nil))))

(defun org-note-document--make-remote-buffer (response)
  "Return a ready read-only comparison buffer populated from RESPONSE."
  (let* ((path (org-note-document--response-value response 'path))
         (revision (org-note-document--response-value response 'revision))
         (source (org-note-document--response-value response 'source))
         (remote
          (generate-new-buffer
           (format "*Org Note Remote: %s r%s*" path revision))))
    (condition-case error-data
        (with-current-buffer remote
          (org-mode)
          (insert source)
          (setq-local buffer-read-only t)
          (set-buffer-modified-p nil)
          remote)
      (t
       (org-note-document--discard-buffer remote)
       (signal (car error-data) (cdr error-data))))))

(defun org-note-document--cleanup-ediff-remote-buffer ()
  "Discard this Ediff control buffer's owned remote comparison buffer."
  (let ((remote org-note-document--ediff-remote-buffer))
    (remove-hook 'ediff-after-quit-hook-internal
                 #'org-note-document--cleanup-ediff-remote-buffer t)
    (setq-local org-note-document--ediff-remote-buffer nil)
    (org-note-document--discard-buffer remote)))

(defun org-note-document--install-ediff-cleanup (remote)
  "Make the current Ediff control buffer discard REMOTE after it quits."
  (setq-local org-note-document--ediff-remote-buffer remote)
  (add-hook 'ediff-after-quit-hook-internal
            #'org-note-document--cleanup-ediff-remote-buffer nil t))

(defun org-note-document-compare-latest ()
  "Compare the current buffer with its cached remote conflict response."
  (interactive)
  (require 'ediff)
  (let* ((response (org-note-document--require-conflict))
         (local (current-buffer))
         (control-before (and (buffer-live-p ediff-control-buffer)
                              ediff-control-buffer))
         remote)
    (setq remote (org-note-document--make-remote-buffer response))
    (condition-case error-data
        (progn
          (ediff-buffers local remote)
          (when (buffer-live-p ediff-control-buffer)
            (with-current-buffer ediff-control-buffer
              (org-note-document--install-ediff-cleanup remote)))
          remote)
      (t
       (if (and (buffer-live-p ediff-control-buffer)
                (not (eq ediff-control-buffer control-before)))
           (condition-case nil
               (with-current-buffer ediff-control-buffer
                 (org-note-document--install-ediff-cleanup remote))
             (error
              (org-note-document--discard-buffer remote)))
         (org-note-document--discard-buffer remote))
       (signal (car error-data) (cdr error-data))))))

(defun org-note-document-reload ()
  "Replace the current buffer with its cached remote conflict response."
  (interactive)
  (let ((response (org-note-document--require-conflict)))
    (when (and (buffer-modified-p)
               (not (yes-or-no-p org-note-document--reload-conflict-prompt)))
      (user-error "Org Note reload cancelled"))
    (let ((source (org-note-document--response-value response 'source))
          (point-position (point))
          (minimum (point-min))
          (maximum (point-max))
          (narrowed (buffer-narrowed-p)))
      (condition-case error-data
          (atomic-change-group
            (save-excursion
              (save-restriction
                (widen)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert source)))))
        (t
         (widen)
         (when narrowed
           (narrow-to-region minimum maximum))
         (goto-char (min (max point-position (point-min)) (point-max)))
         (signal (car error-data) (cdr error-data))))
      (let ((end (save-restriction
                   (widen)
                   (point-max))))
        (widen)
        (when narrowed
          (narrow-to-region (min minimum end) (min maximum end)))
        (goto-char (min (max point-position (point-min)) (point-max))))
      (setq-local org-note-document-path
                  (org-note-document--response-value response 'path)
                  org-note-document-revision
                  (org-note-document--response-value response 'revision)
                  org-note-document-content-hash
                  (org-note-document--response-value response 'content_hash)
                  org-note-document-base-source source
                  org-note-document--conflict nil)
      (goto-char (min point-position (point-max)))
      (set-buffer-modified-p nil))))

(defun org-note-document-rebase ()
  "Use the cached remote conflict response as the next save base revision."
  (interactive)
  (let ((response (org-note-document--require-conflict)))
    (unless (yes-or-no-p org-note-document--rebase-conflict-prompt)
      (user-error "Org Note rebase cancelled"))
    (setq-local org-note-document-path
                (org-note-document--response-value response 'path)
                org-note-document-revision
                (org-note-document--response-value response 'revision)
                org-note-document-content-hash
                (org-note-document--response-value response 'content_hash)
                org-note-document-base-source
                (org-note-document--response-value response 'source)
                org-note-document--conflict nil)
    (set-buffer-modified-p t)))

(defun org-note-document-save (&optional _arg)
  "Save the current Org Note document through Emacs's normal save hooks."
  (interactive "P")
  (org-note-document--require-metadata)
  (save-buffer))

(defun org-note-document--apply-lifecycle-metadata (response document-id)
  "Update path and revision from lifecycle RESPONSE for DOCUMENT-ID."
  (org-note-document--require-metadata)
  (let* ((data (org-note-document--response-value response 'data))
         (path (org-note-document--object-value data 'path)))
    (unless (org-note-document--non-empty-string-p path)
      (org-note-document--invalid-response))
    (setq-local org-note-document-path path
                org-note-document-revision
                (org-note-document--response-revision response document-id))
    (rename-buffer (format "*Org Note: %s*" path) t)
    path))

(defun org-note-document--kill-buffer-safely ()
  "Kill the current Org Note document buffer without a kill query."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer)))

(defun org-note-document--save-remote ()
  "Save the current Org Note document while preserving its view state."
  (org-note-document--require-metadata)
  (let ((modified-p (buffer-modified-p))
        (revision org-note-document-revision)
        (base-source org-note-document-base-source)
        (conflict org-note-document--conflict)
        (committed-p nil)
        source)
    (condition-case error-data
        (save-excursion
          (save-restriction
            (setq source (org-note-document--source))
            (let* ((response
                    (org-note-operation-put-document
                     org-note-document-workspace-id
                     org-note-document-id
                     org-note-document-path
                     source
                     org-note-document-revision
                     (org-note-operation-lease-proofs org-note-document-id)))
                   (new-revision
                    (org-note-document--response-revision
                     response org-note-document-id)))
              (setq-local org-note-document-revision new-revision
                          org-note-document-base-source source
                          org-note-document--conflict nil)
              (setq committed-p t)
              (set-buffer-modified-p
               (not (equal (org-note-document--source) source)))
              (message "Saved Org Note %s at revision %s"
                       org-note-document-path new-revision))))
      (t
       (if (and (not committed-p)
                (org-note-document--stale-revision-error-p error-data))
           (progn
             (org-note-document--record-conflict)
             (set-buffer-modified-p t))
         (unless committed-p
           (setq-local org-note-document-revision revision
                       org-note-document-base-source base-source
                       org-note-document--conflict conflict)
           (set-buffer-modified-p
            (or modified-p
                (and source
                     (not (equal (org-note-document--source) source)))))))
       (signal (car error-data) (cdr error-data))))))

(provide 'org-note-document)

;;; org-note-document.el ends here
