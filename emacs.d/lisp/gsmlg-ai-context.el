;;; gsmlg-ai-context.el --- AI workbench context management -*- lexical-binding: t; -*-

;;; Commentary:
;; In-memory context entries, manager UI, snapshots, size and sensitive checks.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'project)
(require 'gsmlg-ai)

(declare-function dired-get-marked-files "dired"
                  (&optional localp arg filter distinguish-one-marked error))
(declare-function project-files "project" (project &optional dirs))

(cl-defstruct (gsmlg-ai-context
               (:constructor gsmlg-ai-context--create)
               (:copier nil))
  id name entries created-at updated-at)

(cl-defstruct (gsmlg-ai-context-entry
               (:constructor gsmlg-ai-context-entry--create)
               (:copier nil))
  id kind display-name canonical-file source-buffer
  start-marker end-marker editable-p remote-p added-at)

(cl-defstruct (gsmlg-ai-snapshot
               (:constructor gsmlg-ai-snapshot--create)
               (:copier nil))
  id display-path canonical-file source-kind source-buffer
  source-buffer-tick source-content-hash source-file-attributes
  original-content proposed-content proposal-revision operation
  editable-p remote-p apply-status conflict-reason)

(defvar gsmlg-ai-context--current nil
  "The current in-memory workbench context.")

(defconst gsmlg-ai-context-buffer-name "*GSMLG AI Context*"
  "Name of the context manager buffer.")

(defun gsmlg-ai-context--now ()
  "Return the current time as a float."
  (float-time))

(defun gsmlg-ai-context--new-id (prefix)
  "Return a new opaque identifier beginning with PREFIX."
  (format "%s-%s-%04x"
          prefix
          (format-time-string "%Y%m%d%H%M%S")
          (random #xffff)))

(defun gsmlg-ai-context-ensure ()
  "Return the current context, creating an empty one when needed."
  (unless gsmlg-ai-context--current
    (setq gsmlg-ai-context--current
          (gsmlg-ai-context--create
           :id (gsmlg-ai-context--new-id "ctx")
           :name "default"
           :entries nil
           :created-at (gsmlg-ai-context--now)
           :updated-at (gsmlg-ai-context--now))))
  gsmlg-ai-context--current)

(defun gsmlg-ai-context-current-entries ()
  "Return the current context entries."
  (gsmlg-ai-context-entries (gsmlg-ai-context-ensure)))

(defun gsmlg-ai-context--touch ()
  "Update the current context timestamp."
  (setf (gsmlg-ai-context-updated-at (gsmlg-ai-context-ensure))
        (gsmlg-ai-context--now)))
(defun gsmlg-ai-context--normalize-file (file)
  "Return a normalized absolute identity for FILE."
  (when file
    (expand-file-name file)))

(defun gsmlg-ai-context--file-remote-p (file)
  "Return non-nil when FILE is remote."
  (and file (file-remote-p file)))

(defun gsmlg-ai-context--text-buffer-p (&optional buffer)
  "Return non-nil when BUFFER appears to contain text."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (not (save-excursion
             (goto-char (point-min))
             (search-forward "\0" nil t))))))

(defun gsmlg-ai-context--reject-binary (label)
  "Signal a user error for binary content identified by LABEL."
  (user-error "Refusing binary content: %s" label))

(defun gsmlg-ai-context--buffer-bytes (&optional buffer)
  "Return the widened byte size of BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (string-bytes (buffer-substring-no-properties (point-min) (point-max))))))

(defun gsmlg-ai-context--find-file-entry (file)
  "Return an existing context entry for FILE, if any."
  (let ((canonical (gsmlg-ai-context--normalize-file file)))
    (cl-find-if
     (lambda (entry)
       (and (gsmlg-ai-context-entry-canonical-file entry)
            (equal (gsmlg-ai-context-entry-canonical-file entry) canonical)))
     (gsmlg-ai-context-current-entries))))

(defun gsmlg-ai-context--same-file-kind-p (a b)
  "Return non-nil when A and B are the same kind of file-backed entry.
Region entries never replace whole-file/buffer entries and vice versa."
  (let ((ka (gsmlg-ai-context-entry-kind a))
        (kb (gsmlg-ai-context-entry-kind b)))
    (or (eq ka kb)
        (and (memq ka '(file buffer))
             (memq kb '(file buffer))))))

(defun gsmlg-ai-context--add-entry (entry)
  "Add ENTRY to the current context, deduplicating matching file entries.
Whole-file and buffer entries sharing a canonical path replace each
other.  Region entries only replace other region entries for that path."
  (let* ((context (gsmlg-ai-context-ensure))
         (canonical (gsmlg-ai-context-entry-canonical-file entry))
         (entries (gsmlg-ai-context-entries context)))
    (when canonical
      (setq entries
            (cl-remove-if
             (lambda (existing)
               (and (equal (gsmlg-ai-context-entry-canonical-file existing)
                           canonical)
                    (gsmlg-ai-context--same-file-kind-p existing entry)))
             entries)))
    (setf (gsmlg-ai-context-entries context) (append entries (list entry)))
    (gsmlg-ai-context--touch)
    entry))

(defun gsmlg-ai-context-add-current-buffer ()
  "Add the current buffer as a whole-buffer context entry."
  (interactive)
  (unless (gsmlg-ai-context--text-buffer-p)
    (gsmlg-ai-context--reject-binary (buffer-name)))
  (when (> (gsmlg-ai-context--buffer-bytes) gsmlg-ai-max-file-bytes)
    (user-error "Buffer exceeds gsmlg-ai-max-file-bytes: %s" (buffer-name)))
  (let* ((file (buffer-file-name))
         (canonical (gsmlg-ai-context--normalize-file file))
         (editable (and file (not buffer-read-only)))
         (entry
          (gsmlg-ai-context-entry--create
           :id (gsmlg-ai-context--new-id "ent")
           :kind (if file 'file 'buffer)
           :display-name (or file (buffer-name))
           :canonical-file canonical
           :source-buffer (current-buffer)
           :start-marker nil
           :end-marker nil
           :editable-p editable
           :remote-p (or (and file (gsmlg-ai-context--file-remote-p file))
                         (and default-directory
                              (file-remote-p default-directory)))
           :added-at (gsmlg-ai-context--now))))
    (gsmlg-ai-context--add-entry entry)
    (message "Added context entry: %s" (gsmlg-ai-context-entry-display-name entry))
    entry))

(defun gsmlg-ai-context-add-current-region ()
  "Add the active region as read-only context."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((beg (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties beg end)))
    (when (string-search "\0" text)
      (gsmlg-ai-context--reject-binary "region"))
    (when (> (string-bytes text) gsmlg-ai-max-file-bytes)
      (user-error "Region exceeds gsmlg-ai-max-file-bytes"))
    (let ((entry
           (gsmlg-ai-context-entry--create
            :id (gsmlg-ai-context--new-id "ent")
            :kind 'region
            :display-name (format "%s:%d-%d"
                                  (or (buffer-file-name) (buffer-name))
                                  (line-number-at-pos beg)
                                  (line-number-at-pos end))
            :canonical-file (gsmlg-ai-context--normalize-file
                             (buffer-file-name))
            :source-buffer (current-buffer)
            :start-marker (copy-marker beg t)
            :end-marker (copy-marker end t)
            :editable-p nil
            :remote-p (and default-directory (file-remote-p default-directory))
            :added-at (gsmlg-ai-context--now))))
      (gsmlg-ai-context--add-entry entry)
      (message "Added region context: %s"
               (gsmlg-ai-context-entry-display-name entry))
      entry)))

(defun gsmlg-ai-context--add-file (file)
  "Add FILE to the workbench context."
  (let* ((canonical (gsmlg-ai-context--normalize-file file))
         (existing (get-file-buffer canonical)))
    (cond
     (existing
      (with-current-buffer existing
        (gsmlg-ai-context-add-current-buffer)))
     ((not (file-regular-p canonical))
      (user-error "Not a regular file: %s" canonical))
     (t
      (with-temp-buffer
        (insert-file-contents canonical)
        (when (save-excursion
                (goto-char (point-min))
                (search-forward "\0" nil t))
          (gsmlg-ai-context--reject-binary canonical))
        (when (> (string-bytes
                  (buffer-substring-no-properties (point-min) (point-max)))
                 gsmlg-ai-max-file-bytes)
          (user-error "File exceeds gsmlg-ai-max-file-bytes: %s" canonical))
        (gsmlg-ai-context--add-entry
         (gsmlg-ai-context-entry--create
          :id (gsmlg-ai-context--new-id "ent")
          :kind 'file
          :display-name canonical
          :canonical-file canonical
          :source-buffer nil
          :start-marker nil
          :end-marker nil
          :editable-p t
          :remote-p (gsmlg-ai-context--file-remote-p canonical)
          :added-at (gsmlg-ai-context--now))))))))

(defun gsmlg-ai-context-add-files (files)
  "Add FILES to the workbench context.
Interactively prompt for one or more files."
  (interactive
   (list (list (read-file-name "Add file to AI context: " nil nil t))))
  (dolist (file files)
    (gsmlg-ai-context--add-file file))
  (message "Added %d file(s) to AI context" (length files)))

(defun gsmlg-ai-context-add-from-project ()
  "Select explicit project files and add them to the context."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (candidates (project-files project))
         (chosen
          (completing-read-multiple
           "Project files: "
           (mapcar (lambda (file)
                     (file-relative-name file root))
                   candidates)
           nil t)))
    (unless chosen
      (user-error "No project files selected"))
    (gsmlg-ai-context-add-files
     (mapcar (lambda (rel) (expand-file-name rel root)) chosen))))

(defun gsmlg-ai-context-add-from-dired ()
  "Add marked Dired files to the workbench context."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a Dired buffer"))
  (let ((files
         (cl-remove-if-not
          #'file-regular-p
          (dired-get-marked-files nil nil nil t))))
    (unless files
      (user-error "No marked regular files"))
    (gsmlg-ai-context-add-files files)))

(defun gsmlg-ai-context-clear-all (&optional force)
  "Clear the current in-memory context after confirmation.
When FORCE is non-nil, skip confirmation."
  (interactive)
  (let ((entries (gsmlg-ai-context-current-entries)))
    (when (and entries
               (not force)
               (not (yes-or-no-p
                     (format "Clear %d AI context entries? "
                             (length entries)))))
      (user-error "Context clear cancelled"))
    (setq gsmlg-ai-context--current nil)
    (when (get-buffer gsmlg-ai-context-buffer-name)
      (gsmlg-ai-context--refresh-buffer))
    (message "AI context cleared")))

(defun gsmlg-ai-context--entry-content (entry)
  "Return the live text for ENTRY."
  (pcase (gsmlg-ai-context-entry-kind entry)
    ('region
     (let ((buffer (gsmlg-ai-context-entry-source-buffer entry))
           (start (gsmlg-ai-context-entry-start-marker entry))
           (end (gsmlg-ai-context-entry-end-marker entry)))
       (unless (and (buffer-live-p buffer) (markerp start) (markerp end))
         (user-error "Region context is no longer live"))
       (with-current-buffer buffer
         (buffer-substring-no-properties start end))))
    (_
     (let ((buffer (gsmlg-ai-context-entry-source-buffer entry))
           (file (gsmlg-ai-context-entry-canonical-file entry)))
       (cond
        ((buffer-live-p buffer)
         (with-current-buffer buffer
           (save-restriction
             (widen)
             (buffer-substring-no-properties (point-min) (point-max)))))
        (file
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string)))
        (t
         (user-error "Context entry has no readable source")))))))

(defun gsmlg-ai-context--hash (content)
  "Return a SHA-256 hex digest for CONTENT."
  (secure-hash 'sha256 content))

(defun gsmlg-ai-context-snapshot-entry (entry)
  "Create an immutable snapshot from live ENTRY."
  (let* ((content (gsmlg-ai-context--entry-content entry))
         (bytes (string-bytes content))
         (buffer (gsmlg-ai-context-entry-source-buffer entry))
         (file (gsmlg-ai-context-entry-canonical-file entry))
         (kind (cond
                ((eq (gsmlg-ai-context-entry-kind entry) 'region) 'live-buffer)
                ((buffer-live-p buffer) 'live-buffer)
                (file 'disk)
                (t 'live-buffer))))
    (when (> bytes gsmlg-ai-max-file-bytes)
      (user-error "Snapshot exceeds gsmlg-ai-max-file-bytes: %s"
                  (gsmlg-ai-context-entry-display-name entry)))
    (when (string-search "\0" content)
      (gsmlg-ai-context--reject-binary
       (gsmlg-ai-context-entry-display-name entry)))
    (gsmlg-ai-snapshot--create
     :id (gsmlg-ai-context-entry-id entry)
     :display-path (gsmlg-ai-context-entry-display-name entry)
     :canonical-file file
     :source-kind kind
     :source-buffer (and (buffer-live-p buffer) buffer)
     :source-buffer-tick (and (buffer-live-p buffer)
                              (buffer-chars-modified-tick buffer))
     :source-content-hash (gsmlg-ai-context--hash content)
     :source-file-attributes (and file (file-attributes file))
     :original-content content
     :proposed-content content
     :proposal-revision 0
     :operation 'unchanged
     :editable-p (gsmlg-ai-context-entry-editable-p entry)
     :remote-p (gsmlg-ai-context-entry-remote-p entry)
     :apply-status 'pending
     :conflict-reason nil)))

(defun gsmlg-ai-context-snapshot-entries (entries)
  "Snapshot ENTRIES and enforce aggregate size limits."
  (let* ((snapshots (mapcar #'gsmlg-ai-context-snapshot-entry entries))
         (total
          (cl-loop for snap in snapshots
                   sum (string-bytes (gsmlg-ai-snapshot-original-content snap)))))
    (when (> total gsmlg-ai-max-context-bytes)
      (user-error
       "Aggregate context exceeds gsmlg-ai-max-context-bytes (%d > %d)"
       total gsmlg-ai-max-context-bytes))
    snapshots))

(defun gsmlg-ai-context-default-entries (&optional for-edit)
  "Return context entries for a request, applying fallback rules.
When FOR-EDIT is non-nil, prefer editable file-backed context."
  (let ((entries (gsmlg-ai-context-current-entries)))
    (cond
     (entries entries)
     (for-edit
      (unless (buffer-file-name)
        (user-error
         "Edit requires file-backed context; use rewrite-region for a region"))
      (list (gsmlg-ai-context-add-current-buffer)))
     ((use-region-p)
      (list (gsmlg-ai-context-add-current-region)))
     (t
      (list (gsmlg-ai-context-add-current-buffer))))))

(defun gsmlg-ai-context-serialize (snapshots)
  "Serialize SNAPSHOTS into a bounded prompt section."
  (let ((parts nil)
        (used 0))
    (dolist (snap snapshots)
      (let* ((header
              (format "----- BEGIN CONTEXT id=%s path=%s -----\n"
                      (gsmlg-ai-snapshot-id snap)
                      (gsmlg-ai-snapshot-display-path snap)))
             (body (gsmlg-ai-snapshot-original-content snap))
             (footer "\n----- END CONTEXT -----\n")
             (chunk (concat header body footer))
             (bytes (string-bytes chunk)))
        (when (> (+ used bytes) gsmlg-ai-max-inline-context-bytes)
          (user-error
           "Inline context exceeds gsmlg-ai-max-inline-context-bytes"))
        (push chunk parts)
        (setq used (+ used bytes))))
    (string-join (nreverse parts) "\n")))

(defun gsmlg-ai-context-paths (entries)
  "Return display paths for ENTRIES."
  (mapcar #'gsmlg-ai-context-entry-display-name entries))

(defun gsmlg-ai-context--entry-status (entry)
  "Return a short live/modified status string for ENTRY."
  (let ((buffer (gsmlg-ai-context-entry-source-buffer entry)))
    (cond
     ((not (buffer-live-p buffer))
      (if (gsmlg-ai-context-entry-canonical-file entry) "disk" "gone"))
     ((buffer-modified-p buffer) "modified")
     (t "live"))))

(defun gsmlg-ai-context--refresh-buffer ()
  "Refresh the tabulated context manager buffer."
  (when-let* ((buffer (get-buffer gsmlg-ai-context-buffer-name)))
    (with-current-buffer buffer
      (setq tabulated-list-entries
            (mapcar
             (lambda (entry)
               (list (gsmlg-ai-context-entry-id entry)
                     (vector
                      (symbol-name (gsmlg-ai-context-entry-kind entry))
                      (gsmlg-ai-context-entry-display-name entry)
                      (gsmlg-ai-context--entry-status entry)
                      (format "%s"
                              (condition-case nil
                                  (string-bytes
                                   (gsmlg-ai-context--entry-content entry))
                                (error "?")))
                      (if (gsmlg-ai-context-entry-editable-p entry)
                          "edit" "ro")
                      (if (gsmlg-ai-context-entry-remote-p entry)
                          "remote" "local"))))
             (gsmlg-ai-context-current-entries)))
      (tabulated-list-print t))))

(defvar-keymap gsmlg-ai-context-mode-map
  :doc "Keymap for the AI context manager."
  :parent tabulated-list-mode-map
  "RET" #'gsmlg-ai-context-visit-entry
  "b" #'gsmlg-ai-context-add-current-buffer
  "f" #'gsmlg-ai-context-add-files
  "p" #'gsmlg-ai-context-add-from-project
  "d" #'gsmlg-ai-context-delete-at-point
  "c" #'gsmlg-ai-context-clear-all
  "g" #'gsmlg-ai-context-show-buffer
  "q" #'quit-window)

(define-derived-mode gsmlg-ai-context-mode tabulated-list-mode
  "AI-Context"
  "Major mode for the GSMLG AI context manager."
  (setq tabulated-list-format
        [("Type" 8 t)
         ("Name" 48 t)
         ("Status" 10 t)
         ("Bytes" 10 t)
         ("Cap" 6 t)
         ("Where" 8 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun gsmlg-ai-context-entry-at-point ()
  "Return the context entry at point."
  (let ((id (tabulated-list-get-id)))
    (cl-find id (gsmlg-ai-context-current-entries)
             :key #'gsmlg-ai-context-entry-id
             :test #'equal)))

(defun gsmlg-ai-context-visit-entry ()
  "Visit the source of the context entry at point."
  (interactive)
  (when-let* ((entry (gsmlg-ai-context-entry-at-point)))
    (cond
     ((buffer-live-p (gsmlg-ai-context-entry-source-buffer entry))
      (pop-to-buffer (gsmlg-ai-context-entry-source-buffer entry)))
     ((gsmlg-ai-context-entry-canonical-file entry)
      (find-file (gsmlg-ai-context-entry-canonical-file entry)))
     (t
      (user-error "No visitable source")))))

(defun gsmlg-ai-context-delete-at-point ()
  "Delete the context entry at point."
  (interactive)
  (when-let* ((entry (gsmlg-ai-context-entry-at-point))
              (context (gsmlg-ai-context-ensure)))
    (setf (gsmlg-ai-context-entries context)
          (cl-remove entry (gsmlg-ai-context-entries context)))
    (gsmlg-ai-context--touch)
    (gsmlg-ai-context--refresh-buffer)))

(defun gsmlg-ai-context-show-buffer ()
  "Display the AI context manager buffer."
  (interactive)
  (let ((buffer (get-buffer-create gsmlg-ai-context-buffer-name)))
    (with-current-buffer buffer
      (gsmlg-ai-context-mode)
      (gsmlg-ai-context--refresh-buffer))
    (pop-to-buffer buffer)))

(provide 'gsmlg-ai-context)
;;; gsmlg-ai-context.el ends here
