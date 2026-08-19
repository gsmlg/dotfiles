;;; org-note.el --- Browse and edit Agent Note Org data -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2"))

;;; Commentary:
;; Entry package and tabulated workspace and document browsers for Org Note.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'subr-x)
(require 'tabulated-list)
(require 'org-note-client)
(require 'org-note-operation)
(require 'org-note-document)

(defconst org-note--workspace-buffer-name "*Org Note Workspaces*"
  "Name of the Org Note workspace browser buffer.")

(defconst org-note--queue-buffer-name "*Org Note Queue*"
  "Name of the Org Note queue browser buffer.")

(defconst org-note--agenda-buffer-name "*Org Note Agenda*"
  "Name of the Org Note agenda browser buffer.")

(defconst org-note--event-buffer-name "*Org Note Events*"
  "Name of the Org Note event browser buffer.")

(defvar-local org-note--browser-fetcher nil
  "Function used to fetch the current browser page.")

(defvar-local org-note--browser-row-parser nil
  "Function used to validate and render one browser row.")

(defvar-local org-note--browser-current-cursor nil
  "Opaque cursor used to fetch the displayed browser page.")

(defvar-local org-note--browser-next-cursor nil
  "Opaque cursor for the next browser page, or nil.")

(defvar-local org-note--browser-cursor-history nil
  "Stack of opaque cursors for earlier browser pages.")

(defvar-local org-note--browser-row-data nil
  "Complete response rows keyed by their displayed identifiers.")

(defvar-local org-note--browser-workspace-id nil
  "Workspace identifier associated with a document browser.")

(defvar-local org-note--browser-context-key nil
  "Identity of the request context installed in a reusable browser.")

(defvar-local org-note--browser-request-generation 0
  "Monotonic generation of the latest browser request.")

(defvar-local org-note--browser-request-token nil
  "Private ownership token for the latest browser request.")

(defvar-local org-note--operational-workspace-ids nil
  "Workspace identifiers associated with an operational browser.")

(defvar-local org-note--operational-view nil
  "Queue or agenda view associated with an operational browser.")

(defvar-local org-note--event-workspace-id nil
  "Workspace identifier associated with an event browser.")

(defvar-local org-note--event-subject-kind nil
  "Optional subject kind associated with an event browser.")

(defvar-local org-note--event-subject-id nil
  "Optional subject identifier associated with an event browser.")

(defvar-local org-note--item-context-workspace-id nil
  "Workspace identifier associated with an item context buffer.")

(defvar-local org-note--item-context-item-id nil
  "Work-item identifier associated with an item context buffer.")

(defvar-local org-note--item-context-data nil
  "Complete validated response displayed by an item context buffer.")

(defvar-local org-note--item-context-request-generation 0
  "Monotonic generation of the latest item-context request.")

(defvar-local org-note--item-context-request-token nil
  "Private ownership token for the latest item-context request.")

(defvar-keymap org-note-workspace-list-mode-map
  :doc "Keymap for `org-note-workspace-list-mode'."
  :parent tabulated-list-mode-map
  "RET" #'org-note-workspace-open
  "g" #'org-note-browser-refresh
  "n" #'org-note-browser-next-page
  "p" #'org-note-browser-previous-page
  "q" #'quit-window)

(defvar-keymap org-note-document-list-mode-map
  :doc "Keymap for `org-note-document-list-mode'."
  :parent tabulated-list-mode-map
  "RET" #'org-note-document-list-open
  "g" #'org-note-browser-refresh
  "n" #'org-note-browser-next-page
  "p" #'org-note-browser-previous-page
  "q" #'quit-window)

(defvar-keymap org-note-queue-mode-map
  :doc "Keymap for `org-note-queue-mode'."
  :parent tabulated-list-mode-map
  "RET" #'org-note-operational-open
  "g" #'org-note-browser-refresh
  "n" #'org-note-browser-next-page
  "p" #'org-note-browser-previous-page
  "a" #'org-note-item-dispatch
  "q" #'quit-window)

(defvar-keymap org-note-agenda-mode-map
  :doc "Keymap for `org-note-agenda-mode'."
  :parent tabulated-list-mode-map
  "RET" #'org-note-operational-open
  "g" #'org-note-browser-refresh
  "n" #'org-note-browser-next-page
  "p" #'org-note-browser-previous-page
  "a" #'org-note-item-dispatch
  "q" #'quit-window)

(defvar-keymap org-note-event-list-mode-map
  :doc "Keymap for `org-note-event-list-mode'."
  :parent tabulated-list-mode-map
  "g" #'org-note-browser-refresh
  "n" #'org-note-browser-next-page
  "p" #'org-note-browser-previous-page
  "q" #'quit-window)

(defvar-keymap org-note-item-context-mode-map
  :doc "Keymap for `org-note-item-context-mode'."
  :parent org-mode-map
  "g" #'org-note-item-context-refresh
  "a" #'org-note-item-dispatch
  "q" #'quit-window)

(defun org-note--initialize-browser ()
  "Initialize generic browser state in the current buffer."
  (setq-local org-note--browser-fetcher nil
              org-note--browser-row-parser nil
              org-note--browser-current-cursor nil
              org-note--browser-next-cursor nil
              org-note--browser-cursor-history nil
              org-note--browser-row-data (make-hash-table :test #'equal)
              org-note--browser-workspace-id nil
              org-note--browser-context-key nil
              org-note--browser-request-generation 0
              org-note--browser-request-token nil
              tabulated-list-padding 2
              tabulated-list-sort-key nil))

(define-derived-mode org-note-workspace-list-mode tabulated-list-mode
  "Org Note Workspaces"
  "Major mode for browsing Org Note workspaces."
  (org-note--initialize-browser)
  (setq-local tabulated-list-format
              [("Workspace" 28 t)
               ("Slug" 24 t)
               ("Revision" 10 t)
               ("Ready" 8 t)
               ("Running" 9 t)
               ("Blocked" 9 t)
               ("Review" 8 t)])
  (tabulated-list-init-header))

(define-derived-mode org-note-document-list-mode tabulated-list-mode
  "Org Note Documents"
  "Major mode for browsing Org Note documents."
  (org-note--initialize-browser)
  (setq-local tabulated-list-format
              [("Path" 56 t)
               ("Revision" 10 t)])
  (tabulated-list-init-header))

(defun org-note--initialize-operational-browser ()
  "Initialize common operational browser state in the current buffer."
  (org-note--initialize-browser)
  (setq-local org-note--operational-workspace-ids nil
              org-note--operational-view nil
              tabulated-list-format
              [("Type" 12 t)
               ("Title" 32 t)
               ("State" 16 t)
               ("Priority" 10 t)
               ("Assignee" 20 t)
               ("When" 26 t)
               ("Attempt" 18 t)
               ("Ready" 20 t)
               ("Lease" 32 t)])
  (tabulated-list-init-header))

(define-derived-mode org-note-queue-mode tabulated-list-mode
  "Org Note Queue"
  "Major mode for browsing an indexed Org Note queue."
  (org-note--initialize-operational-browser))

(define-derived-mode org-note-agenda-mode tabulated-list-mode
  "Org Note Agenda"
  "Major mode for browsing an indexed Org Note agenda."
  (org-note--initialize-operational-browser))

(define-derived-mode org-note-event-list-mode tabulated-list-mode
  "Org Note Events"
  "Major mode for browsing Org Note workspace events."
  (org-note--initialize-browser)
  (setq-local org-note--event-workspace-id nil
              org-note--event-subject-kind nil
              org-note--event-subject-id nil
              tabulated-list-format
              [("Sequence" 10 t)
               ("Time" 14 t)
               ("Type" 24 t)
               ("Subject" 30 t)
               ("Actor" 24 t)
               ("Previous" 16 t)
               ("Result" 16 t)
               ("Summary" 40 t)])
  (tabulated-list-init-header))

(define-derived-mode org-note-item-context-mode org-mode
  "Org Note Context"
  "Major mode for inspecting a read-only Org Note item context."
  (setq-local buffer-file-name nil
              org-note--item-context-workspace-id nil
              org-note--item-context-item-id nil
              org-note--item-context-data nil
              org-note--item-context-request-generation 0
              org-note--item-context-request-token nil)
  (setq buffer-read-only t))

(defun org-note--symbol-alist-p (object)
  "Return non-nil when OBJECT is a proper symbol-keyed alist.

Duplicate keys are rejected."
  (let ((tail object)
        keys
        (valid t))
    (while (and valid (consp tail))
      (let ((entry (car tail)))
        (if (and (consp entry)
                 (symbolp (car entry))
                 (not (memq (car entry) keys)))
            (push (car entry) keys)
          (setq valid nil)))
      (setq tail (cdr tail)))
    (and valid (null tail))))

(defun org-note--required-value (alist key context)
  "Return KEY from ALIST or reject the malformed CONTEXT."
  (let ((entry (assq key alist)))
    (unless entry
      (signal 'org-note-error
              (list (format "Org Note %s lacks %s" context key))))
    (cdr entry)))

(defun org-note--required-string (alist key context &optional nonempty)
  "Return string KEY from ALIST for CONTEXT.

When NONEMPTY is non-nil, reject the empty string."
  (let ((value (org-note--required-value alist key context)))
    (unless (and (stringp value)
                 (or (not nonempty) (> (length value) 0)))
      (signal 'org-note-error
              (list (format "Org Note %s has invalid %s" context key))))
    value))

(defun org-note--required-count (alist key context)
  "Return nonnegative integer KEY from ALIST for CONTEXT."
  (let ((value (org-note--required-value alist key context)))
    (unless (and (integerp value) (>= value 0))
      (signal 'org-note-error
              (list (format "Org Note %s has invalid %s" context key))))
    value))

(defun org-note--workspace-revision (row)
  "Return the nonnegative workspace revision from ROW.

Accept `workspace_revision' from the live API, with `revision' as a
compatibility alias used by older fixtures."
  (let ((entry (or (assq 'workspace_revision row)
                   (assq 'revision row))))
    (unless entry
      (signal 'org-note-error
              '("Org Note workspace row lacks workspace_revision")))
    (let ((value (cdr entry)))
      (unless (and (integerp value) (>= value 0))
        (signal 'org-note-error
                '("Org Note workspace row has invalid workspace_revision")))
      value)))

(defconst org-note-document-template-path-prefix "templates/"
  "Path prefix that marks Org documents usable as create templates.")

(defun org-note--template-document-path-p (path)
  "Return non-nil when PATH is under the template prefix."
  (and (stringp path)
       (string-prefix-p org-note-document-template-path-prefix path)))

(defun org-note--new-document-path-p (path)
  "Return non-nil when PATH is valid for a newly created document."
  (and (stringp path)
       (> (length path) 0)
       (not (file-name-absolute-p path))
       (not (string-match-p "\\`/" path))
       (not (string-match-p "/\\'" path))
       (not (string-match-p "//" path))
       (not (org-note--template-document-path-p path))
       (let ((segments (split-string path "/" t)))
         (and segments
              (cl-every (lambda (segment) (> (length segment) 0))
                        segments)))))

(defun org-note--filter-template-documents (rows)
  "Return ROWS whose paths are template documents."
  (cl-remove-if-not
   (lambda (row)
     (org-note--template-document-path-p (alist-get 'path row)))
   rows))

(defun org-note--workspace-row (row)
  "Validate and return the tabulated representation of workspace ROW."
  (unless (org-note--symbol-alist-p row)
    (signal 'org-note-error '("Org Note workspace row is malformed")))
  (let* ((id (org-note--required-string
              row 'workspace_id "workspace row" t))
         (slug (org-note--required-string row 'slug "workspace row"))
         (revision (org-note--workspace-revision row))
         (counts (org-note--required-value row 'counts "workspace row")))
    (unless (org-note--symbol-alist-p counts)
      (signal 'org-note-error '("Org Note workspace counts are malformed")))
    (cons
     id
     (vector id slug (number-to-string revision)
             (number-to-string
              (org-note--required-count counts 'ready "workspace counts"))
             (number-to-string
              (org-note--required-count counts 'running "workspace counts"))
             (number-to-string
              (org-note--required-count counts 'blocked "workspace counts"))
             (number-to-string
              (org-note--required-count counts 'review "workspace counts"))))))

(defun org-note--document-row (row)
  "Validate and return the tabulated representation of document ROW."
  (unless (org-note--symbol-alist-p row)
    (signal 'org-note-error '("Org Note document row is malformed")))
  (let ((id (org-note--required-string row 'id "document row" t))
        (path (org-note--required-string row 'path "document row" t))
        (revision (org-note--required-count row 'revision "document row")))
    (cons id (vector path (number-to-string revision)))))

(defun org-note--sanitize-tabulated-vector (columns)
  "Return safe string COLUMNS for a tabulated row."
  (unless (vectorp columns)
    (signal 'org-note-error '("Org Note tabulated columns are malformed")))
  (apply
   #'vector
   (mapcar
    (lambda (cell)
      (unless (stringp cell)
        (signal 'org-note-error '("Org Note tabulated cell is malformed")))
      (org-note--safe-display-string cell))
    (append columns nil))))

(defun org-note--prepare-page (response row-parser)
  "Validate RESPONSE with ROW-PARSER and return prepared page data."
  (unless (org-note--symbol-alist-p response)
    (signal 'org-note-error '("Org Note page response is malformed")))
  (let* ((raw-items (org-note--required-value response 'items "page response"))
         (next-cursor
          (org-note--required-value response 'next_cursor "page response"))
         (row-data (make-hash-table :test #'equal))
         entries)
    (unless (org-note--json-array-or-list-p raw-items)
      (signal 'org-note-error '("Org Note page items are malformed")))
    (dolist (row (org-note--as-proper-list raw-items))
      (let* ((rendered (funcall row-parser row))
             (id (car rendered)))
        (when (gethash id row-data)
          (signal 'org-note-error
                  '("Org Note page contains duplicate row identifiers")))
        (puthash id row row-data)
        (push (list id (org-note--sanitize-tabulated-vector (cdr rendered)))
              entries)))
    (list (nreverse entries) row-data next-cursor)))

(defun org-note--goto-row-id (id)
  "Move point to the tabulated row identified by ID.

Return non-nil when a matching row is found."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (< (point) (point-max)))
      (if (equal (tabulated-list-get-id) id)
          (setq found t)
        (forward-line 1)))
    found))

(defun org-note--browser-request-owned-p
    (target generation token fetcher row-parser context-key installing-p)
  "Return non-nil when TARGET still owns a browser request.

GENERATION and TOKEN identify the request.  FETCHER, ROW-PARSER, and
CONTEXT-KEY identify an existing configuration unless INSTALLING-P is
non-nil."
  (and
   (buffer-live-p target)
   (with-current-buffer target
     (and (= generation org-note--browser-request-generation)
          (eq token org-note--browser-request-token)
          (or installing-p
              (and (eq fetcher org-note--browser-fetcher)
                   (eq row-parser org-note--browser-row-parser)
                   (equal context-key org-note--browser-context-key)))))))

(defun org-note--assert-browser-request-owner
    (target generation token fetcher row-parser context-key installing-p)
  "Require TARGET to own the identified browser request.

GENERATION, TOKEN, FETCHER, ROW-PARSER, CONTEXT-KEY, and INSTALLING-P
have the meanings used by `org-note--browser-request-owned-p'."
  (unless (buffer-live-p target)
    (signal 'org-note-error '("Org Note browser target was killed")))
  (unless (org-note--browser-request-owned-p
           target generation token fetcher row-parser context-key installing-p)
    (signal 'org-note-error '("Org Note browser request was superseded"))))

(defun org-note--finish-browser-request (target generation token)
  "Clear TOKEN for GENERATION when TARGET still owns it."
  (when (buffer-live-p target)
    (with-current-buffer target
      (when (and (= generation org-note--browser-request-generation)
                 (eq token org-note--browser-request-token))
        (setq org-note--browser-request-token nil)))))

(defun org-note--render-browser-page
    (entries format padding sort-key printer)
  "Render ENTRIES off-target with FORMAT, PADDING, SORT-KEY, and PRINTER.

Return the rendered text and the final entries retained by
`tabulated-list-print'."
  (with-temp-buffer
    (tabulated-list-mode)
    (setq-local tabulated-list-format format
                tabulated-list-padding padding
                tabulated-list-sort-key sort-key
                tabulated-list-printer printer
                tabulated-list-entries entries)
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (list (buffer-substring (point-min) (point-max))
          tabulated-list-entries)))

(defun org-note--browser-load-page
    (cursor history &optional fetcher row-parser context-key installing-p)
  "Fetch CURSOR and commit it with prior cursor HISTORY atomically.

FETCHER, ROW-PARSER, and CONTEXT-KEY install a new configuration when
INSTALLING-P is non-nil.  Otherwise the current buffer configuration is
used."
  (let* ((target (current-buffer))
         (selected-id (tabulated-list-get-id))
         (old-point (point))
         (request-fetcher (or fetcher org-note--browser-fetcher))
         (request-parser (or row-parser org-note--browser-row-parser))
         (request-context
          (if installing-p context-key org-note--browser-context-key))
         (format tabulated-list-format)
         (padding tabulated-list-padding)
         (sort-key tabulated-list-sort-key)
         (printer tabulated-list-printer)
         generation token)
    (unless (and (functionp request-fetcher) (functionp request-parser))
      (user-error "Current buffer is not an initialized Org Note browser"))
    (setq generation (1+ org-note--browser-request-generation)
          token (list 'org-note-browser-request generation)
          org-note--browser-request-generation generation
          org-note--browser-request-token token)
    (condition-case error-data
        (let* ((response (funcall request-fetcher cursor))
               (_owner-after-fetch
                (org-note--assert-browser-request-owner
                 target generation token request-fetcher request-parser
                 request-context installing-p))
               (page (org-note--prepare-page response request-parser))
               (_owner-after-prepare
                (org-note--assert-browser-request-owner
                 target generation token request-fetcher request-parser
                 request-context installing-p))
               (entries (nth 0 page))
               (row-data (nth 1 page))
               (next-cursor (nth 2 page))
               (render-result
                (org-note--render-browser-page
                 entries format padding sort-key printer))
               (rendered (nth 0 render-result))
               (final-entries (nth 1 render-result)))
          (org-note--assert-browser-request-owner
           target generation token request-fetcher request-parser
           request-context installing-p)
          (with-current-buffer target
            (atomic-change-group
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (erase-buffer)
                (insert rendered)))
            (setq tabulated-list-entries final-entries
                  org-note--browser-row-data row-data
                  org-note--browser-current-cursor cursor
                  org-note--browser-next-cursor next-cursor
                  org-note--browser-cursor-history history
                  org-note--browser-request-token nil)
            (when installing-p
              (setq org-note--browser-fetcher request-fetcher
                    org-note--browser-row-parser request-parser
                    org-note--browser-context-key request-context))
            (set-buffer-modified-p nil)
            (goto-char (min old-point (point-max)))
            (when (and selected-id (gethash selected-id row-data))
              (org-note--goto-row-id selected-id))))
      ((error quit)
       (org-note--finish-browser-request target generation token)
       (signal (car error-data) (cdr error-data))))))

(defun org-note-browser-refresh ()
  "Refresh the current Org Note browser page."
  (interactive)
  (org-note--browser-load-page org-note--browser-current-cursor
                               org-note--browser-cursor-history))

(defun org-note-browser-next-page ()
  "Visit the next Org Note browser page when one is available."
  (interactive)
  (if org-note--browser-next-cursor
      (org-note--browser-load-page
       org-note--browser-next-cursor
       (cons org-note--browser-current-cursor
             org-note--browser-cursor-history))
    (message "No next Org Note page")))

(defun org-note-browser-previous-page ()
  "Visit the previous Org Note browser page when one is available."
  (interactive)
  (if org-note--browser-cursor-history
      (org-note--browser-load-page
       (car org-note--browser-cursor-history)
       (cdr org-note--browser-cursor-history))
    (message "No previous Org Note page")))

(defun org-note--fetch-workspaces (cursor)
  "Fetch the workspace page identified by opaque CURSOR."
  (org-note-operation-list-workspaces :cursor cursor))

(defun org-note--fetch-documents (workspace-id cursor)
  "Fetch opaque CURSOR for documents in WORKSPACE-ID."
  (org-note-operation-list-documents workspace-id :cursor cursor))

(defun org-note--valid-identifier-p (identifier)
  "Return non-nil when IDENTIFIER is a nonempty string."
  (and (stringp identifier) (> (length identifier) 0)))

(defun org-note--document-buffer-name (workspace-id)
  "Return the document browser buffer name for WORKSPACE-ID."
  (format "*Org Note Documents: %s*" workspace-id))

(defun org-note-workspaces ()
  "Display the Org Note workspace browser and return its buffer."
  (interactive)
  (let ((buffer (get-buffer-create org-note--workspace-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-note-workspace-list-mode)
        (org-note-workspace-list-mode)
        (setq-local org-note--browser-fetcher #'org-note--fetch-workspaces
                    org-note--browser-row-parser #'org-note--workspace-row))
      (org-note-browser-refresh))
    (pop-to-buffer buffer)
    buffer))

(defun org-note-documents (workspace-id)
  "Display documents in WORKSPACE-ID and return their browser buffer."
  (interactive (list (read-string "Workspace ID: ")))
  (unless (org-note--valid-identifier-p workspace-id)
    (user-error "Org Note workspace ID must be a nonempty string"))
  (let ((buffer (get-buffer-create
                 (org-note--document-buffer-name workspace-id))))
    (with-current-buffer buffer
      (unless (and (derived-mode-p 'org-note-document-list-mode)
                   (equal org-note--browser-workspace-id workspace-id))
        (org-note-document-list-mode)
        (setq-local org-note--browser-workspace-id workspace-id
                    org-note--browser-fetcher
                    (apply-partially #'org-note--fetch-documents workspace-id)
                    org-note--browser-row-parser #'org-note--document-row))
      (org-note-browser-refresh))
    (pop-to-buffer buffer)
    buffer))

(defun org-note--current-row ()
  "Return the complete row selected in the current browser."
  (let* ((id (tabulated-list-get-id))
         (row (and id (gethash id org-note--browser-row-data))))
    (unless row
      (user-error "No Org Note row is selected"))
    row))

(defun org-note-workspace-open ()
  "Open the document browser for the selected workspace."
  (interactive)
  (let ((row (org-note--current-row)))
    (org-note-documents
     (org-note--required-string row 'workspace_id "workspace row" t))))

(defun org-note-document-list-open ()
  "Open the selected remote Org Note document."
  (interactive)
  (unless (org-note--valid-identifier-p org-note--browser-workspace-id)
    (user-error "Current document browser has no workspace context"))
  (let ((row (org-note--current-row)))
    (org-note-document-open
     org-note--browser-workspace-id
     (org-note--required-string row 'id "document row" t))))

(defun org-note--proper-list-p (object)
  "Return non-nil when OBJECT is a finite proper list."
  (let ((tail object))
    (while (consp tail)
      (setq tail (cdr tail)))
    (null tail)))

(defun org-note--as-proper-list (value)
  "Return VALUE as a proper list when it is a JSON array or proper list.

JSON arrays arrive as vectors from `org-note-client--parse-json'.  Proper
lists, including nil, are returned unchanged.  Signal nothing here; callers
must reject unsupported shapes."
  (cond
   ((vectorp value) (append value nil))
   ((org-note--proper-list-p value) value)
   (t value)))

(defun org-note--json-array-or-list-p (value)
  "Return non-nil when VALUE is a JSON array vector or a proper list."
  (or (vectorp value) (org-note--proper-list-p value)))

(defun org-note--required-list (alist key context)
  "Return proper list KEY from ALIST for CONTEXT.

JSON arrays are accepted and normalized to lists."
  (let ((value (org-note--required-value alist key context)))
    (unless (org-note--json-array-or-list-p value)
      (signal 'org-note-error
              (list (format "Org Note %s has invalid %s" context key))))
    (org-note--as-proper-list value)))

(defun org-note--required-object (alist key context)
  "Return symbol-keyed object KEY from ALIST for CONTEXT."
  (let ((value (org-note--required-value alist key context)))
    (unless (org-note--symbol-alist-p value)
      (signal 'org-note-error
              (list (format "Org Note %s has invalid %s" context key))))
    value))

(defun org-note--optional-string (alist key context &optional nonempty)
  "Return optional string KEY from ALIST for CONTEXT.

When NONEMPTY is non-nil, reject an empty string."
  (let ((value (org-note--required-value alist key context)))
    (unless (or (null value)
                (and (stringp value)
                     (or (not nonempty) (> (length value) 0))))
      (signal 'org-note-error
              (list (format "Org Note %s has invalid %s" context key))))
    value))

(defun org-note--optional-count (alist key context)
  "Return optional nonnegative integer KEY from ALIST for CONTEXT."
  (let ((value (org-note--required-value alist key context)))
    (unless (or (null value) (and (integerp value) (>= value 0)))
      (signal 'org-note-error
              (list (format "Org Note %s has invalid %s" context key))))
    value))

(defun org-note--required-boolean (alist key context)
  "Return JSON boolean KEY from ALIST for CONTEXT."
  (let ((value (org-note--required-value alist key context)))
    (unless (memq value '(t :json-false))
      (signal 'org-note-error
              (list (format "Org Note %s has invalid %s" context key))))
    value))

(defun org-note--required-string-list (alist key context)
  "Return a list of nonempty strings from KEY in ALIST for CONTEXT."
  (let ((values (org-note--required-list alist key context)))
    (dolist (value values)
      (unless (and (stringp value) (> (length value) 0))
        (signal 'org-note-error
                (list (format "Org Note %s has invalid %s" context key)))))
    values))

(defun org-note--validate-timestamp (timestamp context)
  "Validate optional TIMESTAMP for CONTEXT and return it."
  (when timestamp
    (unless (org-note--symbol-alist-p timestamp)
      (signal 'org-note-error
              (list (format "Org Note %s timestamp is malformed" context))))
    (org-note--required-string timestamp 'raw context t)
    (org-note--required-string timestamp 'local context t)
    (org-note--required-string timestamp 'timezone context t)
    (org-note--required-count timestamp 'utc_timestamp context))
  timestamp)

(defun org-note--validate-item
    (item context &optional workspace-id item-id document-id)
  "Validate ITEM for CONTEXT and optional expected identity values.

WORKSPACE-ID, ITEM-ID, and DOCUMENT-ID are checked when non-nil."
  (unless (org-note--symbol-alist-p item)
    (signal 'org-note-error
            (list (format "Org Note %s item is malformed" context))))
  (let ((id (org-note--required-string item 'id context t))
        (workspace (org-note--required-string item 'workspace_id context t))
        (document (org-note--required-string item 'document_id context t))
        (priority (org-note--optional-string item 'priority context t)))
    (when (and workspace-id (not (equal workspace workspace-id)))
      (signal 'org-note-error
              (list (format "Org Note %s has mismatched workspace_id" context))))
    (when (and item-id (not (equal id item-id)))
      (signal 'org-note-error
              (list (format "Org Note %s has mismatched id" context))))
    (when (and document-id (not (equal document document-id)))
      (signal 'org-note-error
              (list (format "Org Note %s has mismatched document_id" context))))
    (when (and priority (/= (length priority) 1))
      (signal 'org-note-error
              (list (format "Org Note %s has invalid priority" context))))
    (org-note--optional-string item 'parent_id context t)
    (org-note--required-string item 'item_type context t)
    (org-note--required-string item 'title context t)
    (org-note--optional-string item 'state context t)
    (org-note--validate-timestamp
     (org-note--required-value item 'scheduled context) context)
    (org-note--validate-timestamp
     (org-note--required-value item 'deadline context) context)
    (org-note--optional-string item 'assignee context t)
    (org-note--required-boolean item 'requires_review context)
    (org-note--required-count item 'created_at context)
    (org-note--required-string-list item 'tags context))
  item)

(defun org-note--validate-lease (lease context &optional workspace-id item-id)
  "Validate LEASE for CONTEXT and optional WORKSPACE-ID and ITEM-ID."
  (unless (org-note--symbol-alist-p lease)
    (signal 'org-note-error
            (list (format "Org Note %s lease is malformed" context))))
  (org-note--required-string lease 'id context t)
  (let ((workspace
         (org-note--required-string lease 'workspace_id context t))
        (item (org-note--required-string lease 'work_item_id context t)))
    (when (and workspace-id (not (equal workspace workspace-id)))
      (signal 'org-note-error
              (list (format "Org Note %s lease has mismatched workspace_id"
                            context))))
    (when (and item-id (not (equal item item-id)))
      (signal 'org-note-error
              (list (format "Org Note %s lease has mismatched work_item_id"
                            context)))))
  (org-note--required-string lease 'attempt_id context t)
  (org-note--required-string lease 'kind context t)
  (org-note--required-string lease 'actor_id context t)
  (org-note--required-count lease 'acquired_at context)
  (org-note--required-count lease 'last_heartbeat_at context)
  (org-note--required-count lease 'expires_at context)
  (org-note--required-string lease 'status context t)
  lease)

(defun org-note--operational-page (response)
  "Validate and return an operational page RESPONSE."
  (unless (org-note--symbol-alist-p response)
    (signal 'org-note-error '("Org Note operational page is malformed")))
  (org-note--required-count response 'evaluated_at "operational page")
  response)

(defun org-note--operational-when (row item view)
  "Return the view-relevant time from operational ROW and ITEM for VIEW."
  (let ((scheduled (alist-get 'scheduled item))
        (deadline (alist-get 'deadline item))
        (lease (alist-get 'lease row)))
    (cond
     ((eq view 'completed)
      (if-let ((completion (alist-get 'completion_at row)))
          (number-to-string completion)
        "-"))
     ((eq view 'expired_lease)
      (if lease
          (number-to-string (alist-get 'expires_at lease))
        "-"))
     ((eq view 'scheduled)
      (if scheduled (alist-get 'raw scheduled) "-"))
     ((eq view 'upcoming_deadline)
      (if deadline (alist-get 'raw deadline) "-"))
     (scheduled (alist-get 'raw scheduled))
     (deadline (alist-get 'raw deadline))
     (t "-"))))

(defun org-note--operational-row (workspace-ids view row)
  "Validate and render operational ROW for WORKSPACE-IDS and VIEW."
  (unless (org-note--symbol-alist-p row)
    (signal 'org-note-error '("Org Note operational row is malformed")))
  (let* ((item (org-note--required-object row 'item "operational row"))
         (id (org-note--required-string item 'id "operational item" t))
         (workspace-id
          (org-note--required-string
           item 'workspace_id "operational item" t))
         (attempt-count
          (org-note--required-count row 'attempt_count "operational row"))
         (attempt-status
          (org-note--optional-string
           row 'current_attempt_status "operational row" t))
         (ready-status
          (org-note--optional-string row 'ready_status "operational row" t))
         (review-status
          (org-note--optional-string
           row 'review_lease_status "operational row" t))
         (lease (org-note--required-value row 'lease "operational row")))
    (unless (member workspace-id workspace-ids)
      (signal 'org-note-error
              '("Org Note operational row has an unexpected workspace")))
    (org-note--validate-item item "operational item" workspace-id id)
    (org-note--required-boolean row 'retry_exhausted "operational row")
    (org-note--optional-count row 'completion_at "operational row")
    (when lease
      (org-note--validate-lease
       lease "operational row" workspace-id id))
    (cons
     id
     (vector
      (alist-get 'item_type item)
      (alist-get 'title item)
      (or (alist-get 'state item) "-")
      (or (alist-get 'priority item) "-")
      (or (alist-get 'assignee item) "-")
      (org-note--operational-when row item view)
      (if attempt-status
          (format "%s / %d" attempt-status attempt-count)
        (number-to-string attempt-count))
      (or ready-status "-")
      (if lease
          (format "%s / %s / %s"
                  (alist-get 'kind lease)
                  (alist-get 'status lease)
                  (alist-get 'actor_id lease))
        (or review-status "-"))))))

(defun org-note--validated-workspace-ids (workspace-ids)
  "Return a copy of valid, unique WORKSPACE-IDS or signal a user error."
  (unless (and (org-note--proper-list-p workspace-ids) workspace-ids)
    (user-error "Select at least one Org Note workspace"))
  (let (seen)
    (dolist (workspace-id workspace-ids)
      (unless (org-note--valid-identifier-p workspace-id)
        (user-error "Org Note workspace IDs must be nonempty strings"))
      (when (member workspace-id seen)
        (user-error "Org Note workspace IDs must be unique"))
      (push workspace-id seen)))
  (copy-sequence workspace-ids))

(defun org-note--validated-view (view allowed-views label)
  "Return VIEW from ALLOWED-VIEWS for browser LABEL.

Known string views are accepted without interning arbitrary input."
  (let ((validated
         (cond
          ((memq view allowed-views) view)
          ((stringp view)
           (cl-find-if
            (lambda (candidate)
              (string= view (symbol-name candidate)))
            allowed-views)))))
    (unless validated
      (user-error "Unknown Org Note %s view: %s" label view))
    validated))

(defun org-note--read-workspace-ids ()
  "Read one or more comma-separated workspace identifiers."
  (org-note--validated-workspace-ids
   (split-string (read-string "Workspace IDs (comma separated): ")
                 "," t "[[:space:]]+")))

(defun org-note--read-view (prompt views)
  "Read with PROMPT one exact view from VIEWS without arbitrary interning."
  (let* ((choices (mapcar #'symbol-name views))
         (selected (completing-read prompt choices nil t)))
    (cl-find-if
     (lambda (view) (string= selected (symbol-name view))) views)))

(defun org-note--fetch-queue (workspace-ids view cursor)
  "Fetch queue VIEW for WORKSPACE-IDS at opaque CURSOR."
  (org-note--operational-page
   (org-note-operation-query-queue
    :workspace-ids workspace-ids :view view :cursor cursor)))

(defun org-note--fetch-agenda (workspace-ids view cursor)
  "Fetch agenda VIEW for WORKSPACE-IDS at opaque CURSOR."
  (org-note--operational-page
   (org-note-operation-query-agenda
    :workspace-ids workspace-ids :view view :cursor cursor)))

(defun org-note--show-configured-browser
    (buffer-name mode context-key fetcher row-parser)
  "Load and return BUFFER-NAME using MODE and request CONTEXT-KEY.

FETCHER and ROW-PARSER replace the prior request context only after the
new page loads successfully."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p mode)
        (funcall mode))
      (let* ((same-context (equal context-key org-note--browser-context-key))
             (cursor (and same-context org-note--browser-current-cursor))
             (history (and same-context org-note--browser-cursor-history)))
        (org-note--browser-load-page
         cursor history fetcher row-parser context-key t)))
    buffer))

(defun org-note--show-operational
    (buffer-name mode workspace-ids view allowed-views operation label)
  "Show an operational BUFFER-NAME using MODE and OPERATION.

WORKSPACE-IDS and VIEW identify the request.  ALLOWED-VIEWS validates the
view for LABEL."
  (setq workspace-ids (org-note--validated-workspace-ids workspace-ids)
        view (org-note--validated-view view allowed-views label))
  (let* ((workspace-copy (copy-sequence workspace-ids))
         (context-key (list operation workspace-copy view))
         (fetch-function
          (if (eq operation 'queue)
              #'org-note--fetch-queue
            #'org-note--fetch-agenda))
         (buffer
          (org-note--show-configured-browser
           buffer-name mode context-key
           (apply-partially fetch-function workspace-copy view)
           (apply-partially
            #'org-note--operational-row workspace-copy view))))
    (with-current-buffer buffer
      (setq-local org-note--operational-workspace-ids workspace-copy
                  org-note--operational-view view))
    (pop-to-buffer buffer)
    buffer))

(defun org-note-queue (workspace-ids view)
  "Display queue VIEW across WORKSPACE-IDS and return its buffer."
  (interactive
   (list (org-note--read-workspace-ids)
         (org-note--read-view
          "Queue view: " org-note-operation-queue-views)))
  (org-note--show-operational
   org-note--queue-buffer-name 'org-note-queue-mode workspace-ids view
   org-note-operation-queue-views 'queue "queue"))

(defun org-note-agenda (workspace-ids view)
  "Display agenda VIEW across WORKSPACE-IDS and return its buffer."
  (interactive
   (list (org-note--read-workspace-ids)
         (org-note--read-view
          "Agenda view: " org-note-operation-agenda-views)))
  (org-note--show-operational
   org-note--agenda-buffer-name 'org-note-agenda-mode workspace-ids view
   org-note-operation-agenda-views 'agenda "agenda"))

(defun org-note-operational-open ()
  "Open context for the selected queue or agenda work item."
  (interactive)
  (let* ((row (org-note--current-row))
         (item (org-note--required-object row 'item "operational row"))
         (workspace-id
          (org-note--required-string
           item 'workspace_id "operational item" t))
         (item-id
          (org-note--required-string item 'id "operational item" t)))
    (org-note-item-context workspace-id item-id)))

(defun org-note--validate-event (event context &optional workspace-id)
  "Validate EVENT for CONTEXT and optional expected WORKSPACE-ID."
  (unless (org-note--symbol-alist-p event)
    (signal 'org-note-error
            (list (format "Org Note %s event is malformed" context))))
  (org-note--required-string event 'id context t)
  (let ((workspace
         (org-note--required-string event 'workspace_id context t)))
    (when (and workspace-id (not (equal workspace workspace-id)))
      (signal 'org-note-error
              (list (format "Org Note %s has mismatched workspace_id"
                            context)))))
  (org-note--required-count event 'sequence context)
  (org-note--required-string event 'subject_kind context t)
  (org-note--required-string event 'subject_id context t)
  (org-note--required-string event 'actor_id context t)
  (org-note--optional-string event 'attempt_id context t)
  (org-note--required-string event 'event_type context t)
  (org-note--required-count event 'occurred_at context)
  (org-note--required-string event 'summary context)
  (org-note--required-value event 'metadata context)
  (org-note--optional-string event 'previous_state context t)
  (org-note--optional-string event 'resulting_state context t)
  event)

(defun org-note--event-row (workspace-id subject-kind subject-id row)
  "Validate and render event ROW for WORKSPACE-ID.

SUBJECT-KIND and SUBJECT-ID constrain the active filters when non-nil."
  (org-note--validate-event row "event row" workspace-id)
  (when (and subject-kind
             (not (equal subject-kind (alist-get 'subject_kind row))))
    (signal 'org-note-error
            '("Org Note event row does not match the subject-kind filter")))
  (when (and subject-id
             (not (equal subject-id (alist-get 'subject_id row))))
    (signal 'org-note-error
            '("Org Note event row does not match the subject-id filter")))
  (let ((id (alist-get 'id row)))
    (cons
     id
     (vector
      (number-to-string (alist-get 'sequence row))
      (number-to-string (alist-get 'occurred_at row))
      (alist-get 'event_type row)
      (format "%s / %s"
              (alist-get 'subject_kind row)
              (alist-get 'subject_id row))
      (alist-get 'actor_id row)
      (or (alist-get 'previous_state row) "-")
      (or (alist-get 'resulting_state row) "-")
      (alist-get 'summary row)))))

(defun org-note--fetch-events
    (workspace-id subject-kind subject-id cursor)
  "Fetch events for WORKSPACE-ID and optional subject filters at CURSOR."
  (org-note-operation-list-events
   workspace-id :subject-kind subject-kind :subject-id subject-id
   :cursor cursor))

(defun org-note--read-optional-string (prompt)
  "Read PROMPT and return nil for an empty answer."
  (let ((value (string-trim (read-string prompt))))
    (unless (string-empty-p value) value)))

(defun org-note-events (workspace-id &optional subject-kind subject-id)
  "Display events in WORKSPACE-ID filtered by SUBJECT-KIND and SUBJECT-ID."
  (interactive
   (list (read-string "Workspace ID: ")
         (org-note--read-optional-string "Subject kind (optional): ")
         (org-note--read-optional-string "Subject ID (optional): ")))
  (unless (org-note--valid-identifier-p workspace-id)
    (user-error "Org Note workspace ID must be a nonempty string"))
  (dolist (filter (list subject-kind subject-id))
    (unless (or (null filter) (org-note--valid-identifier-p filter))
      (user-error "Org Note event filters must be nonempty strings")))
  (let* ((context-key (list 'events workspace-id subject-kind subject-id))
         (buffer
          (org-note--show-configured-browser
           org-note--event-buffer-name 'org-note-event-list-mode context-key
           (apply-partially
            #'org-note--fetch-events workspace-id subject-kind subject-id)
           (apply-partially
            #'org-note--event-row workspace-id subject-kind subject-id))))
    (with-current-buffer buffer
      (setq-local org-note--event-workspace-id workspace-id
                  org-note--event-subject-kind subject-kind
                  org-note--event-subject-id subject-id))
    (pop-to-buffer buffer)
    buffer))

(defun org-note--validate-note-link (link context)
  "Validate note LINK for CONTEXT."
  (unless (org-note--symbol-alist-p link)
    (signal 'org-note-error
            (list (format "Org Note %s note link is malformed" context))))
  (org-note--required-string link 'purpose context t)
  (org-note--required-string link 'note_id context t)
  (org-note--required-string link 'description context)
  (org-note--required-boolean link 'available context)
  link)

(defun org-note--validate-attempt-note (note context)
  "Validate attempt NOTE for CONTEXT."
  (unless (org-note--symbol-alist-p note)
    (signal 'org-note-error
            (list (format "Org Note %s attempt note is malformed" context))))
  (org-note--required-string note 'purpose context t)
  (org-note--required-string note 'note_id context t)
  (org-note--required-string note 'description context)
  note)

(defun org-note--validate-artifact (artifact context)
  "Validate ARTIFACT for CONTEXT."
  (unless (org-note--symbol-alist-p artifact)
    (signal 'org-note-error
            (list (format "Org Note %s artifact is malformed" context))))
  (org-note--required-string artifact 'uri context t)
  (org-note--required-string artifact 'media_type context t)
  (org-note--required-string artifact 'name context)
  (org-note--required-string artifact 'description context)
  artifact)

(defun org-note--validate-attempt (attempt context workspace-id item-id)
  "Validate ATTEMPT for CONTEXT, WORKSPACE-ID, and ITEM-ID."
  (unless (org-note--symbol-alist-p attempt)
    (signal 'org-note-error
            (list (format "Org Note %s attempt is malformed" context))))
  (org-note--required-string attempt 'id context t)
  (unless (equal (org-note--required-string
                  attempt 'workspace_id context t)
                 workspace-id)
    (signal 'org-note-error
            (list (format "Org Note %s has mismatched workspace_id" context))))
  (unless (equal (org-note--required-string
                  attempt 'work_item_id context t)
                 item-id)
    (signal 'org-note-error
            (list (format "Org Note %s has mismatched work_item_id" context))))
  (org-note--required-count attempt 'attempt_number context)
  (org-note--required-string attempt 'actor_id context t)
  (org-note--required-string attempt 'status context t)
  (org-note--required-count attempt 'started_at context)
  (org-note--optional-count attempt 'ended_at context)
  (org-note--optional-string attempt 'error context)
  (org-note--optional-string attempt 'result_summary context)
  (org-note--optional-string attempt 'review_outcome context)
  (dolist (note (org-note--required-list attempt 'note_refs context))
    (org-note--validate-attempt-note note context))
  (dolist (artifact (org-note--required-list attempt 'artifacts context))
    (org-note--validate-artifact artifact context))
  (org-note--required-value attempt 'metadata context)
  attempt)

(defun org-note--validate-operational-context (operational context)
  "Validate OPERATIONAL classifications and recovery data for CONTEXT."
  (unless (org-note--symbol-alist-p operational)
    (signal 'org-note-error
            (list (format "Org Note %s operational data is malformed" context))))
  (org-note--required-string-list operational 'classifications context)
  (org-note--optional-string operational 'readiness context t)
  (org-note--required-string-list operational 'blockers context)
  (let ((budget
         (org-note--required-object operational 'attempt_budget context))
        (recovery
         (org-note--required-object operational 'recovery context)))
    (org-note--required-count budget 'execution_attempt_count context)
    (org-note--required-count budget 'max_attempts context)
    (org-note--required-count budget 'remaining_attempts context)
    (org-note--required-boolean budget 'retry_exhausted context)
    (org-note--required-boolean recovery 'eligible context)
    (org-note--required-boolean recovery 'candidate context)
    (org-note--required-string-list recovery 'blockers context))
  operational)

(defun org-note--validate-origin (origin context)
  "Validate optional ORIGIN for CONTEXT."
  (when origin
    (unless (org-note--symbol-alist-p origin)
      (signal 'org-note-error
              (list (format "Org Note %s origin is malformed" context))))
    (let ((kind (org-note--required-string origin 'kind context t)))
      (cond
       ((equal kind "work_item")
        (let ((declared-id
               (org-note--required-string origin 'work_item_id context t)))
        (let ((item (org-note--required-value origin 'item context)))
          (when item
              (org-note--validate-item item context)
              (unless (equal declared-id (alist-get 'id item))
                (signal
                 'org-note-error
                 (list (format
                        "Org Note %s origin has mismatched work_item_id"
                        context))))))))
       ((equal kind "event")
        (let ((declared-id
               (org-note--required-string origin 'event_id context t)))
        (let ((event (org-note--required-value origin 'event context)))
          (when event
              (org-note--validate-event event context)
              (unless (equal declared-id (alist-get 'id event))
                (signal
                 'org-note-error
                 (list (format
                        "Org Note %s origin has mismatched event_id"
                        context))))))))
       (t
        (signal 'org-note-error
                (list (format "Org Note %s has invalid origin kind" context)))))))
  origin)

(defun org-note--validate-item-context (response workspace-id item-id)
  "Validate item context RESPONSE for WORKSPACE-ID and ITEM-ID."
  (unless (org-note--symbol-alist-p response)
    (signal 'org-note-error '("Org Note item context is malformed")))
  (let* ((workspace
          (org-note--required-object response 'workspace "item context"))
         (response-workspace-id
          (org-note--required-string workspace 'id "context workspace" t))
         (document
          (org-note--required-object response 'document "item context"))
         (document-id
          (org-note--required-string document 'id "context document" t))
         (item (org-note--required-object response 'item "item context")))
    (unless (equal response-workspace-id workspace-id)
      (signal 'org-note-error
              '("Org Note item context has a mismatched workspace")))
    (org-note--required-string workspace 'slug "context workspace" t)
    (org-note--required-string workspace 'display_name "context workspace" t)
    (org-note--required-string workspace 'description "context workspace")
    (org-note--required-string workspace 'timezone "context workspace" t)
    (org-note--required-count
     workspace 'policy_schema_version "context workspace")
    (org-note--required-value workspace 'policy "context workspace")
    (org-note--required-count workspace 'revision "context workspace")
    (org-note--optional-count workspace 'archived_at "context workspace")
    (org-note--required-count response 'workspace_revision "item context")
    (org-note--required-string document 'path "context document" t)
    (org-note--required-count document 'revision "context document")
    (org-note--validate-item
     item "context item" workspace-id item-id document-id)
    (let ((parent-id (alist-get 'parent_id item))
          (parent (org-note--required-value response 'parent "item context")))
      (cond
       ((and parent-id (null parent))
        (signal 'org-note-error
                '("Org Note context item parent is missing")))
       ((and (null parent-id) parent)
        (signal 'org-note-error
                '("Org Note context item has an unexpected parent")))
       (parent
        (org-note--validate-item
         parent "context parent" workspace-id parent-id document-id))))
    (dolist (child (org-note--required-list response 'children "item context"))
      (org-note--validate-item
       child "context child" workspace-id nil document-id)
      (unless (equal (alist-get 'parent_id child) item-id)
        (signal 'org-note-error
                '("Org Note context child has a mismatched parent_id"))))
    (dolist (dependency
             (org-note--required-list response 'dependencies "item context"))
      (unless (org-note--symbol-alist-p dependency)
        (signal 'org-note-error
                '("Org Note context dependency is malformed")))
      (org-note--validate-item
       (org-note--required-object dependency 'item "context dependency")
       "context dependency item" workspace-id)
      (org-note--required-boolean
       dependency 'satisfied "context dependency"))
    (dolist (link
             (org-note--required-list response 'note_links "item context"))
      (org-note--validate-note-link link "context note link"))
    (dolist (attempt
             (org-note--required-list response 'attempts "item context"))
      (org-note--validate-attempt
       attempt "context attempt" workspace-id item-id))
    (org-note--validate-origin
     (org-note--required-value response 'origin "item context")
     "item context")
    (dolist (segment
             (org-note--required-list
              response 'history_segments "item context"))
      (unless (org-note--symbol-alist-p segment)
        (signal 'org-note-error
                '("Org Note context history segment is malformed")))
      (let ((segment-workspace
             (org-note--required-string
              segment 'workspace_id "context history" t)))
        (dolist (event
                 (org-note--required-list
                  segment 'events "context history"))
          (org-note--validate-event
           event "context history event" segment-workspace))))
    (let ((lease (org-note--required-value response 'lease "item context")))
      (when lease
        (org-note--validate-lease
         lease "context lease" workspace-id item-id)))
    (org-note--validate-operational-context
     (org-note--required-object response 'operational "item context")
     "item context"))
  response)

(defun org-note--safe-display-string (value)
  "Return VALUE as deterministic single-line display text."
  (replace-regexp-in-string
   "[[:cntrl:]\n\r\t]+" " " (substring-no-properties value)))

(defun org-note--context-value (value)
  "Return VALUE as safe deterministic context text."
  (cond
   ((stringp value) (org-note--safe-display-string value))
   ((integerp value) (number-to-string value))
   ((eq value t) "true")
   ((eq value :json-false) "false")
   ((null value) "None")
   (t "Unavailable")))

(defun org-note--context-field (label value)
  "Insert a fixed-width context field with LABEL and VALUE."
  (insert ": " label ": " (org-note--context-value value) "\n"))

(defun org-note--context-item-line (item)
  "Return a safe one-line summary for ITEM."
  (format "%s / %s / %s / %s"
          (org-note--context-value (alist-get 'id item))
          (org-note--context-value (alist-get 'item_type item))
          (org-note--context-value (alist-get 'state item))
          (org-note--context-value (alist-get 'title item))))

(defun org-note--render-item-context (context)
  "Return deterministic Org text for validated item CONTEXT."
  (with-temp-buffer
    (let* ((workspace (alist-get 'workspace context))
           (document (alist-get 'document context))
           (item (alist-get 'item context))
           (operational (alist-get 'operational context))
           (budget (alist-get 'attempt_budget operational))
           (recovery (alist-get 'recovery operational)))
      (insert "#+title: Org Note Item Context\n\n* Identity\n")
      (org-note--context-field "Workspace ID" (alist-get 'id workspace))
      (org-note--context-field
       "Workspace" (alist-get 'display_name workspace))
      (org-note--context-field
       "Workspace revision" (alist-get 'workspace_revision context))
      (org-note--context-field "Document ID" (alist-get 'id document))
      (org-note--context-field "Document path" (alist-get 'path document))
      (org-note--context-field "Document revision" (alist-get 'revision document))

      (insert "\n* Item\n")
      (org-note--context-field "ID" (alist-get 'id item))
      (org-note--context-field "Type" (alist-get 'item_type item))
      (org-note--context-field "Title" (alist-get 'title item))
      (org-note--context-field "State" (alist-get 'state item))
      (org-note--context-field "Priority" (alist-get 'priority item))
      (org-note--context-field "Assignee" (alist-get 'assignee item))
      (org-note--context-field "Scheduled"
                               (and (alist-get 'scheduled item)
                                    (alist-get 'raw
                                               (alist-get 'scheduled item))))
      (org-note--context-field "Deadline"
                               (and (alist-get 'deadline item)
                                    (alist-get 'raw
                                               (alist-get 'deadline item))))

      (insert "\n* Hierarchy\n** Parent\n")
      (if-let ((parent (alist-get 'parent context)))
          (org-note--context-field "Item" (org-note--context-item-line parent))
        (insert ": No parent\n"))
      (insert "\n** Children\n")
      (if-let ((children (alist-get 'children context)))
          (dolist (child children)
            (org-note--context-field
             "Item" (org-note--context-item-line child)))
        (insert ": No children\n"))

      (insert "\n* Dependencies and readiness blockers\n** Dependencies\n")
      (if-let ((dependencies (alist-get 'dependencies context)))
          (dolist (dependency dependencies)
            (org-note--context-field
             (if (eq (alist-get 'satisfied dependency) t)
                 "Satisfied"
               "Blocking")
             (org-note--context-item-line (alist-get 'item dependency))))
        (insert ": No dependencies\n"))
      (insert "\n** Readiness blockers\n")
      (if-let ((blockers (alist-get 'blockers operational)))
          (dolist (blocker blockers)
            (org-note--context-field "Blocker" blocker))
        (insert ": No readiness blockers\n"))

      (insert "\n* Linked notes\n")
      (if-let ((links (alist-get 'note_links context)))
          (dolist (link links)
            (org-note--context-field
             "Note"
             (format "%s / %s / %s / available %s"
                     (org-note--context-value (alist-get 'purpose link))
                     (org-note--context-value (alist-get 'note_id link))
                     (org-note--context-value (alist-get 'description link))
                     (org-note--context-value (alist-get 'available link)))))
        (insert ": No linked notes\n"))

      (insert "\n* Attempts, results, and recovery\n** Attempts\n")
      (if-let ((attempts (alist-get 'attempts context)))
          (dolist (attempt attempts)
            (org-note--context-field
             "Attempt"
             (format "%s / %s / %s / result %s / review %s / error %s"
                     (org-note--context-value
                      (alist-get 'attempt_number attempt))
                     (org-note--context-value (alist-get 'id attempt))
                     (org-note--context-value (alist-get 'status attempt))
                     (org-note--context-value
                      (alist-get 'result_summary attempt))
                     (org-note--context-value
                      (alist-get 'review_outcome attempt))
                     (org-note--context-value (alist-get 'error attempt))))
            (dolist (note (alist-get 'note_refs attempt))
              (org-note--context-field
               "Attempt note"
               (format "%s / %s / %s"
                       (org-note--context-value (alist-get 'purpose note))
                       (org-note--context-value (alist-get 'note_id note))
                       (org-note--context-value
                        (alist-get 'description note)))))
            (dolist (artifact (alist-get 'artifacts attempt))
              (org-note--context-field
               "Artifact"
               (format "%s / %s / %s"
                       (org-note--context-value (alist-get 'name artifact))
                       (org-note--context-value (alist-get 'media_type artifact))
                       (org-note--context-value (alist-get 'uri artifact))))))
        (insert ": No attempts\n"))
      (insert "\n** Recovery\n")
      (org-note--context-field
       "Execution attempts" (alist-get 'execution_attempt_count budget))
      (org-note--context-field "Maximum attempts" (alist-get 'max_attempts budget))
      (org-note--context-field
       "Remaining attempts" (alist-get 'remaining_attempts budget))
      (org-note--context-field "Eligible" (alist-get 'eligible recovery))
      (org-note--context-field "Candidate" (alist-get 'candidate recovery))
      (if-let ((recovery-blockers (alist-get 'blockers recovery)))
          (dolist (blocker recovery-blockers)
            (org-note--context-field "Recovery blocker" blocker))
        (insert ": No recovery blockers\n"))

      (insert "\n* Lease and operational classifications\n")
      (if-let ((lease (alist-get 'lease context)))
          (progn
            (org-note--context-field "Lease ID" (alist-get 'id lease))
            (org-note--context-field "Kind" (alist-get 'kind lease))
            (org-note--context-field "Actor" (alist-get 'actor_id lease))
            (org-note--context-field "Status" (alist-get 'status lease))
            (org-note--context-field "Expires" (alist-get 'expires_at lease)))
        (insert ": No current lease\n"))
      (if-let ((classifications (alist-get 'classifications operational)))
          (dolist (classification classifications)
            (org-note--context-field "Classification" classification))
        (insert ": No operational classifications\n"))
      (org-note--context-field "Readiness" (alist-get 'readiness operational))

      (insert "\n* Origin\n")
      (if-let ((origin (alist-get 'origin context)))
          (let ((kind (alist-get 'kind origin)))
            (org-note--context-field "Kind" kind)
            (org-note--context-field
             "ID"
             (if (equal kind "work_item")
                 (alist-get 'work_item_id origin)
               (alist-get 'event_id origin))))
        (insert ": No origin\n"))

      (insert "\n* History\n")
      (if-let ((segments (alist-get 'history_segments context)))
          (dolist (segment segments)
            (insert "** Workspace "
                    (org-note--context-value (alist-get 'workspace_id segment))
                    "\n")
            (if-let ((events (alist-get 'events segment)))
                (dolist (event events)
                  (org-note--context-field
                   "Event"
                   (format "%s / %s / %s / %s"
                           (org-note--context-value
                            (alist-get 'sequence event))
                           (org-note--context-value
                            (alist-get 'event_type event))
                           (org-note--context-value
                            (alist-get 'actor_id event))
                           (org-note--context-value
                            (alist-get 'summary event)))))
              (insert ": No events\n")))
        (insert ": No history segments\n")))
    (buffer-string)))

(defun org-note--item-context-buffer-name (workspace-id item-id)
  "Return the context buffer name for WORKSPACE-ID and ITEM-ID."
  (format "*Org Note Context: %s/%s*" workspace-id item-id))

(defun org-note--item-context-request-owned-p
    (target generation token workspace-id item-id)
  "Return non-nil when TARGET owns the identified item-context request.

GENERATION and TOKEN identify the request for WORKSPACE-ID and ITEM-ID."
  (and
   (buffer-live-p target)
   (with-current-buffer target
     (and (= generation org-note--item-context-request-generation)
          (eq token org-note--item-context-request-token)
          (or (null org-note--item-context-workspace-id)
              (and (equal workspace-id org-note--item-context-workspace-id)
                   (equal item-id org-note--item-context-item-id)))))))

(defun org-note--assert-item-context-request-owner
    (target generation token workspace-id item-id)
  "Require TARGET to own an item-context request.

GENERATION and TOKEN identify the request for WORKSPACE-ID and ITEM-ID."
  (unless (buffer-live-p target)
    (signal 'org-note-error '("Org Note item context target was killed")))
  (unless (org-note--item-context-request-owned-p
           target generation token workspace-id item-id)
    (signal 'org-note-error
            '("Org Note item context request was superseded"))))

(defun org-note--finish-item-context-request (target generation token)
  "Clear TOKEN for GENERATION when TARGET still owns it."
  (when (buffer-live-p target)
    (with-current-buffer target
      (when (and (= generation org-note--item-context-request-generation)
                 (eq token org-note--item-context-request-token))
        (setq org-note--item-context-request-token nil)))))

(defun org-note--load-item-context (workspace-id item-id)
  "Fetch and atomically display context for WORKSPACE-ID and ITEM-ID."
  (let* ((target (current-buffer))
         (old-point (point))
         (old-modified-p (buffer-modified-p))
         (generation (1+ org-note--item-context-request-generation))
         (token (list 'org-note-item-context-request generation)))
    (when (and org-note--item-context-workspace-id
               (not (and
                     (equal workspace-id org-note--item-context-workspace-id)
                     (equal item-id org-note--item-context-item-id))))
      (user-error "Current buffer owns a different Org Note item context"))
    (setq org-note--item-context-request-generation generation
          org-note--item-context-request-token token)
    (condition-case error-data
        (let ((response
               (org-note-operation-get-item-context workspace-id item-id)))
          (org-note--assert-item-context-request-owner
           target generation token workspace-id item-id)
          (org-note--validate-item-context response workspace-id item-id)
          (org-note--assert-item-context-request-owner
           target generation token workspace-id item-id)
          (let ((rendered (org-note--render-item-context response)))
            (org-note--assert-item-context-request-owner
             target generation token workspace-id item-id)
            (with-current-buffer target
              (atomic-change-group
                (save-restriction
                  (widen)
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (erase-buffer)
                    (insert rendered))))
              (goto-char (min old-point (point-max)))
              (set-buffer-modified-p nil)
              (setq org-note--item-context-workspace-id workspace-id
                    org-note--item-context-item-id item-id
                    org-note--item-context-data response
                    org-note--item-context-request-token nil))
            response))
      ((error quit)
       (when (org-note--item-context-request-owned-p
              target generation token workspace-id item-id)
         (with-current-buffer target
           (set-buffer-modified-p old-modified-p)
           (goto-char (min old-point (point-max)))))
       (org-note--finish-item-context-request target generation token)
       (signal (car error-data) (cdr error-data))))))

(defun org-note-item-context (workspace-id item-id)
  "Display context for ITEM-ID in WORKSPACE-ID and return its buffer."
  (interactive
   (list (read-string "Workspace ID: ")
         (read-string "Item ID: ")))
  (unless (org-note--valid-identifier-p workspace-id)
    (user-error "Org Note workspace ID must be a nonempty string"))
  (unless (org-note--valid-identifier-p item-id)
    (user-error "Org Note item ID must be a nonempty string"))
  (let ((buffer
         (get-buffer-create
          (org-note--item-context-buffer-name workspace-id item-id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-note-item-context-mode)
        (org-note-item-context-mode))
      (org-note--load-item-context workspace-id item-id))
    (pop-to-buffer buffer)
    buffer))

(defun org-note-item-context-refresh ()
  "Refresh the current read-only Org Note item context."
  (interactive)
  (unless (and (org-note--valid-identifier-p
                org-note--item-context-workspace-id)
               (org-note--valid-identifier-p org-note--item-context-item-id))
    (user-error "Current buffer has no Org Note item context"))
  (org-note--load-item-context
   org-note--item-context-workspace-id org-note--item-context-item-id))

(defvar org-note--action-text-history nil
  "Minibuffer history for non-secret Org Note action text.")

(defvar org-note--action-id-history nil
  "Minibuffer history for non-secret Org Note action identifiers.")

(defvar org-note--action-json-history nil
  "Minibuffer history for non-secret Org Note action JSON.")

(defvar org-note--action-choice-history nil
  "Minibuffer history for non-secret Org Note action choices.")

(cl-defstruct (org-note--action-context
               (:constructor org-note--make-action-context))
  "Validated identity and origin ownership for one work-item action."
  origin-kind origin-buffer generation context-key row-data data
  workspace-id item-id document-id)

(defun org-note--action-base-context ()
  "Return validated action identity and origin from the current buffer."
  (cond
   ((derived-mode-p 'org-note-queue-mode 'org-note-agenda-mode)
    (let* ((row (org-note--current-row))
           (item (org-note--required-object row 'item "operational row"))
           (workspace-id
            (org-note--required-string
             item 'workspace_id "operational action item" t))
           (item-id
            (org-note--required-string
             item 'id "operational action item" t))
           (document-id
            (org-note--required-string
             item 'document_id "operational action item" t)))
      (org-note--validate-item
       item "operational action item" workspace-id item-id document-id)
      (unless (member workspace-id org-note--operational-workspace-ids)
        (signal 'org-note-error
                '("Org Note action row has an unexpected workspace")))
      (org-note--make-action-context
       :origin-kind 'browser
       :origin-buffer (current-buffer)
       :generation org-note--browser-request-generation
       :context-key org-note--browser-context-key
       :row-data org-note--browser-row-data
       :workspace-id workspace-id
       :item-id item-id
       :document-id document-id)))
   ((derived-mode-p 'org-note-item-context-mode)
    (unless (and (org-note--valid-identifier-p
                  org-note--item-context-workspace-id)
                 (org-note--valid-identifier-p
                  org-note--item-context-item-id)
                 org-note--item-context-data)
      (user-error "Current buffer has no validated Org Note item context"))
    (let* ((workspace-id org-note--item-context-workspace-id)
           (item-id org-note--item-context-item-id)
           (data org-note--item-context-data))
      (org-note--validate-item-context data workspace-id item-id)
      (org-note--make-action-context
       :origin-kind 'context
       :origin-buffer (current-buffer)
       :generation org-note--item-context-request-generation
       :data data
       :workspace-id workspace-id
       :item-id item-id
       :document-id
       (org-note--required-string
        (org-note--required-object data 'document "item context")
        'id "context document" t))))
   (t
    (user-error "Org Note actions require a queue, agenda, or item context"))))

(defun org-note--action-origin-current-p (context)
  "Return non-nil when action CONTEXT still owns its originating UI."
  (let ((buffer (org-note--action-context-origin-buffer context)))
    (and
     (buffer-live-p buffer)
     (with-current-buffer buffer
       (pcase (org-note--action-context-origin-kind context)
         ('browser
          (and (derived-mode-p 'org-note-queue-mode 'org-note-agenda-mode)
               (= (org-note--action-context-generation context)
                  org-note--browser-request-generation)
               (equal (org-note--action-context-context-key context)
                      org-note--browser-context-key)
               (eq (org-note--action-context-row-data context)
                   org-note--browser-row-data)))
         ('context
          (and (derived-mode-p 'org-note-item-context-mode)
               (= (org-note--action-context-generation context)
                  org-note--item-context-request-generation)
               (equal (org-note--action-context-workspace-id context)
                      org-note--item-context-workspace-id)
               (equal (org-note--action-context-item-id context)
                      org-note--item-context-item-id)
               (eq (org-note--action-context-data context)
                   org-note--item-context-data)))
         (_ nil))))))

(defun org-note--action-revision-context (context)
  "Return CONTEXT with an authoritative document revision DTO.

Item-context buffers already own a validated DTO.  Queue and agenda rows do
not carry document revisions, so those origins fetch and validate one context
read before the mutation."
  (if (org-note--action-context-data context)
      context
    (unless (org-note--action-origin-current-p context)
      (user-error "Org Note action origin is no longer current"))
    (let* ((workspace-id (org-note--action-context-workspace-id context))
           (item-id (org-note--action-context-item-id context))
           (response
            (org-note-operation-get-item-context workspace-id item-id)))
      (org-note--validate-item-context response workspace-id item-id)
      (unless
          (equal
           (org-note--required-string
            (org-note--required-object response 'document "item context")
            'id "context document" t)
           (org-note--action-context-document-id context))
        (signal 'org-note-error
                '("Org Note item context has a mismatched document")))
      (setf (org-note--action-context-data context) response)
      context)))

(defun org-note--action-document-revision (context)
  "Return the validated current document revision from action CONTEXT."
  (org-note--required-count
   (org-note--required-object
    (org-note--action-context-data context) 'document "item context")
   'revision "context document"))

(defun org-note--action-refresh-origin (context)
  "Refresh action CONTEXT's still-current originating buffer."
  (when (org-note--action-origin-current-p context)
    (with-current-buffer (org-note--action-context-origin-buffer context)
      (condition-case nil
          (pcase (org-note--action-context-origin-kind context)
            ('browser (org-note-browser-refresh))
            ('context (org-note-item-context-refresh)))
        (quit
         (message
          "Org Note action succeeded, but its view refresh was cancelled"))
        (error
         (message "Org Note action succeeded, but its view refresh failed"))))))

(defun org-note--perform-action (context function arguments)
  "Call mutation FUNCTION once with ARGUMENTS for action CONTEXT.

Refresh only the still-current originating buffer after success."
  (unless (org-note--action-origin-current-p context)
    (user-error "Org Note action origin is no longer current"))
  (let ((response (apply function arguments)))
    (org-note--action-refresh-origin context)
    response))

(defun org-note--read-required-action-string (prompt &optional id-p)
  "Read nonblank action text with PROMPT.

When ID-P is non-nil, use the non-secret identifier history."
  (let ((value
         (string-trim
          (read-string prompt nil
                       (if id-p
                           'org-note--action-id-history
                         'org-note--action-text-history)))))
    (when (string-empty-p value)
      (user-error "Org Note action input must not be blank"))
    value))

(defun org-note--read-optional-action-string (prompt)
  "Read optional non-secret action text with PROMPT."
  (let ((value
         (string-trim
          (read-string prompt nil 'org-note--action-text-history))))
    (unless (string-empty-p value) value)))

(defun org-note--read-action-json (prompt expected-type)
  "Read optional JSON with PROMPT and require EXPECTED-TYPE.

EXPECTED-TYPE is `object' or `array'.  Blank input means the optional value is
omitted.  Objects are returned as hash tables and arrays as vectors."
  (let ((source
         (string-trim
          (read-string prompt nil 'org-note--action-json-history))))
    (unless (string-empty-p source)
      (let ((value
             (condition-case nil
                 (json-parse-string
                  source :object-type 'hash-table :array-type 'array
                  :null-object nil :false-object :json-false)
               (json-parse-error
                (user-error "Org Note action JSON is invalid")))))
        (unless (pcase expected-type
                  ('object (hash-table-p value))
                  ('array (vectorp value))
                  (_ nil))
          (user-error "Org Note action JSON has the wrong top-level type"))
        value))))

(defun org-note--confirm-action (prompt)
  "Require affirmative confirmation for action PROMPT."
  (unless (yes-or-no-p prompt)
    (user-error "Org Note action cancelled")))

(defun org-note--action-lease-candidates (context)
  "Return registered live lease candidates for action CONTEXT."
  (let ((workspace-id (org-note--action-context-workspace-id context))
        (item-id (org-note--action-context-item-id context))
        candidates)
    (dolist (kind '("execution" "review"))
      (when-let ((lease
                  (org-note-operation-find-lease workspace-id item-id kind)))
        (push (cons kind lease) candidates)))
    (nreverse candidates)))

(defun org-note--read-required-action-lease (context)
  "Select and return a registered live lease for action CONTEXT."
  (let ((candidates (org-note--action-lease-candidates context)))
    (unless candidates
      (user-error "No active Org Note lease is registered for this item"))
    (if (null (cdr candidates))
        (cdar candidates)
      (let ((kind
             (completing-read
              "Lease kind: " (mapcar #'car candidates) nil t nil
              'org-note--action-choice-history)))
        (cdr (assoc kind candidates))))))

(defun org-note--require-action-lease (context kind)
  "Return CONTEXT's registered live lease of KIND or signal a user error."
  (or (org-note-operation-find-lease
       (org-note--action-context-workspace-id context)
       (org-note--action-context-item-id context) kind)
      (user-error "No active Org Note %s lease is registered" kind)))

(defun org-note--read-optional-action-lease (context)
  "Select an optional registered live lease for action CONTEXT."
  (let ((candidates (org-note--action-lease-candidates context)))
    (when candidates
      (let* ((none "none")
             (kind
              (completing-read
               "Lease proof (optional): "
               (cons none (mapcar #'car candidates)) nil t nil
               'org-note--action-choice-history none)))
        (unless (equal kind none)
          (cdr (assoc kind candidates)))))))

(defun org-note--action-lease-proof (lease)
  "Return the wire proof for registered LEASE."
  `((lease_id . ,(org-note-operation-lease-lease-id lease))
    (kind . ,(org-note-operation-lease-kind lease))
    (fencing_token . ,(org-note-operation-lease-fencing-token lease))))

(defun org-note--action-revision-map (context)
  "Return the exact current-document revision map for action CONTEXT."
  (let ((revisions (make-hash-table :test #'equal)))
    (puthash (org-note--action-context-document-id context)
             (org-note--action-document-revision context)
             revisions)
    revisions))

(defun org-note--action-options (&rest pairs)
  "Return a keyword argument list for non-nil PAIRS.

PAIRS alternate keyword symbols and their optional values."
  (let (options)
    (while pairs
      (let ((keyword (pop pairs))
            (value (pop pairs)))
        (when value
          (setq options (append options (list keyword value))))))
    options))

(defun org-note--action-dependency-ids (context)
  "Return dependency identifiers stored in action CONTEXT."
  (mapcar
   (lambda (dependency)
     (org-note--required-string
      (org-note--required-object dependency 'item "context dependency")
      'id "context dependency item" t))
   (org-note--required-list
    (org-note--action-context-data context) 'dependencies "item context")))

(defun org-note--read-dependency-to-remove (context)
  "Read the dependency to remove for action CONTEXT."
  (if (org-note--action-context-data context)
      (let ((ids (org-note--action-dependency-ids context)))
        (unless ids
          (user-error "This Org Note item has no dependencies"))
        (completing-read
         "Dependency to remove: " ids nil t nil
         'org-note--action-choice-history))
    (org-note--read-required-action-string "Dependency item ID: " t)))

(defun org-note--assert-action-dependency (context dependency-id)
  "Require DEPENDENCY-ID to exist in action CONTEXT."
  (unless (member dependency-id (org-note--action-dependency-ids context))
    (user-error "The selected Org Note dependency is no longer present")))

(defun org-note--action-note-link-choices (context)
  "Return labeled note-link DTO choices stored in action CONTEXT."
  (let (choices)
    (dolist (link
             (org-note--required-list
              (org-note--action-context-data context) 'note_links "item context"))
      (let* ((purpose
              (org-note--required-string link 'purpose "context note link" t))
             (note-id
              (org-note--required-string link 'note_id "context note link" t))
             (label (format "%s / %s" purpose note-id)))
        (when (assoc label choices)
          (signal 'org-note-error
                  '("Org Note context contains duplicate note links")))
        (push (cons label (cons purpose note-id)) choices)))
    (nreverse choices)))

(defun org-note--read-note-link-to-remove (context)
  "Read a purpose and note ID pair to unlink from action CONTEXT."
  (if (org-note--action-context-data context)
      (let ((choices (org-note--action-note-link-choices context)))
        (unless choices
          (user-error "This Org Note item has no linked notes"))
        (cdr
         (assoc
          (completing-read
           "Note link to remove: " (mapcar #'car choices) nil t nil
           'org-note--action-choice-history)
          choices)))
    (cons
     (org-note--read-required-action-string "Note purpose: ")
     (org-note--read-required-action-string "Note ID: " t))))

(defun org-note--assert-action-note-link (context purpose note-id)
  "Require PURPOSE and NOTE-ID to identify a link in action CONTEXT."
  (unless
      (cl-find-if
       (lambda (choice)
         (and (equal purpose (car (cdr choice)))
              (equal note-id (cdr (cdr choice)))))
       (org-note--action-note-link-choices context))
    (user-error "The selected Org Note link is no longer present")))

(defun org-note-item-claim ()
  "Claim the current Org Note item for execution or review."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (kind
          (completing-read
           "Claim kind: " '("execution" "review") nil t nil
           'org-note--action-choice-history "execution")))
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-claim
     (list (org-note--action-context-workspace-id context)
           (org-note--action-context-item-id context)
           (org-note--action-context-document-id context)
           (org-note--action-document-revision context)
           kind))))

(defun org-note-item-heartbeat ()
  "Send an explicit heartbeat for a registered lease on the current item."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (lease (org-note--read-required-action-lease context)))
    (org-note--perform-action
     context #'org-note-operation-heartbeat
     (list (org-note--action-context-workspace-id context)
           (org-note--action-context-item-id context)
           (org-note-operation-lease-lease-id lease)
           (org-note-operation-lease-kind lease)
           (org-note-operation-lease-fencing-token lease)))))

(defun org-note-item-release ()
  "Release a registered lease on the current Org Note item."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (lease (org-note--read-required-action-lease context))
         (target-state
          (org-note--read-optional-action-string
           "Target state after release (optional): ")))
    (org-note--confirm-action "Release this Org Note claim? ")
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-release
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            (org-note--action-context-document-id context)
            (org-note--action-document-revision context)
            (org-note-operation-lease-lease-id lease)
            (org-note-operation-lease-kind lease)
            (org-note-operation-lease-fencing-token lease))
      (org-note--action-options :target-state target-state)))))

(defun org-note-item-report-progress ()
  "Report progress for the current Org Note item's execution lease."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (lease (org-note--require-action-lease context "execution"))
         (summary
          (org-note--read-required-action-string "Progress summary: "))
         (metadata
          (org-note--read-action-json "Metadata JSON object (optional): "
                                      'object)))
    (org-note--perform-action
     context #'org-note-operation-report-progress
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            (org-note-operation-lease-lease-id lease)
            (org-note-operation-lease-kind lease)
            (org-note-operation-lease-fencing-token lease)
            summary)
      (org-note--action-options :metadata metadata)))))

(defun org-note-item-submit-result ()
  "Submit a result for the current Org Note item's execution lease."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (lease (org-note--require-action-lease context "execution"))
         (summary
          (org-note--read-required-action-string "Result summary: "))
         (note-refs
          (org-note--read-action-json
           "Note references JSON array (optional): " 'array))
         (artifacts
          (org-note--read-action-json
           "Artifacts JSON array (optional): " 'array))
         (metadata
          (org-note--read-action-json
           "Metadata JSON object (optional): " 'object)))
    (org-note--confirm-action "Submit this Org Note result? ")
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-submit-result
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            (org-note--action-context-document-id context)
            (org-note--action-document-revision context)
            (org-note-operation-lease-lease-id lease)
            (org-note-operation-lease-fencing-token lease)
            summary)
      (org-note--action-options
       :note-refs note-refs :artifacts artifacts :metadata metadata)))))

(defun org-note-item-transition ()
  "Transition the current Org Note item to a server-defined state."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (target-state
          (org-note--read-required-action-string "Target state: "))
         (error-text
          (org-note--read-optional-action-string "Error text (optional): "))
         (metadata
          (org-note--read-action-json
           "Metadata JSON object (optional): " 'object))
         (lease (org-note--read-optional-action-lease context)))
    (org-note--confirm-action "Transition this Org Note item? ")
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-transition
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            (org-note--action-context-document-id context)
            (org-note--action-document-revision context)
            target-state)
      (org-note--action-options
       :lease (and lease (org-note--action-lease-proof lease))
       :error error-text :metadata metadata)))))

(defun org-note-item-retry ()
  "Retry the current eligible failed or expired Org Note item."
  (interactive)
  (let ((context (org-note--action-base-context)))
    (org-note--confirm-action "Retry this Org Note item? ")
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-retry
     (list (org-note--action-context-workspace-id context)
           (org-note--action-context-item-id context)
           (org-note--action-context-document-id context)
           (org-note--action-document-revision context)))))

(defun org-note-item-request-review ()
  "Request review for the current Org Note item's execution lease."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (lease (org-note--require-action-lease context "execution"))
         (result-summary
          (org-note--read-optional-action-string
           "Result summary (optional): "))
         (note-refs
          (org-note--read-action-json
           "Note references JSON array (optional): " 'array))
         (artifacts
          (org-note--read-action-json
           "Artifacts JSON array (optional): " 'array))
         (metadata
          (org-note--read-action-json
           "Metadata JSON object (optional): " 'object)))
    (org-note--confirm-action "Request review for this Org Note item? ")
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-request-review
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            (org-note--action-context-document-id context)
            (org-note--action-document-revision context)
            (org-note-operation-lease-lease-id lease)
            (org-note-operation-lease-fencing-token lease))
      (org-note--action-options
       :result-summary result-summary :note-refs note-refs
       :artifacts artifacts :metadata metadata)))))

(defun org-note-item-approve-review ()
  "Approve the current Org Note item under a registered review lease."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (lease (org-note--require-action-lease context "review"))
         (metadata
          (org-note--read-action-json
           "Metadata JSON object (optional): " 'object)))
    (org-note--confirm-action "Approve this Org Note review? ")
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-approve-review
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            (org-note--action-context-document-id context)
            (org-note--action-document-revision context)
            (org-note-operation-lease-lease-id lease)
            (org-note-operation-lease-fencing-token lease))
      (org-note--action-options :metadata metadata)))))

(defun org-note-item-reject-review ()
  "Reject the current Org Note item under a registered review lease."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (lease (org-note--require-action-lease context "review"))
         (reason
          (org-note--read-required-action-string "Rejection reason: "))
         (metadata
          (org-note--read-action-json
           "Metadata JSON object (optional): " 'object)))
    (org-note--confirm-action "Reject this Org Note review? ")
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-reject-review
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            (org-note--action-context-document-id context)
            (org-note--action-document-revision context)
            (org-note-operation-lease-lease-id lease)
            (org-note-operation-lease-fencing-token lease)
            reason)
      (org-note--action-options :metadata metadata)))))

(defun org-note-item-add-dependency ()
  "Add a dependency to the current Org Note item."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (dependency-id
          (org-note--read-required-action-string "Dependency item ID: " t))
         (lease (org-note--read-optional-action-lease context)))
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-add-dependency
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            dependency-id
            (org-note--action-context-document-id context)
            (org-note--action-revision-map context))
      (org-note--action-options
       :lease (and lease (org-note--action-lease-proof lease)))))))

(defun org-note-item-remove-dependency ()
  "Remove a dependency from the current Org Note item."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (dependency-id (org-note--read-dependency-to-remove context))
         (lease (org-note--read-optional-action-lease context)))
    (org-note--confirm-action "Remove this Org Note dependency? ")
    (setq context (org-note--action-revision-context context))
    (org-note--assert-action-dependency context dependency-id)
    (org-note--perform-action
     context #'org-note-operation-remove-dependency
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            dependency-id
            (org-note--action-context-document-id context)
            (org-note--action-revision-map context))
      (org-note--action-options
       :lease (and lease (org-note--action-lease-proof lease)))))))

(defun org-note-item-link-note ()
  "Link a Markdown note to the current Org Note item."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (purpose
          (org-note--read-required-action-string "Note purpose: "))
         (note-id
          (org-note--read-required-action-string "Note ID: " t))
         (description
          (org-note--read-required-action-string "Note description: "))
         (lease (org-note--read-optional-action-lease context)))
    (setq context (org-note--action-revision-context context))
    (org-note--perform-action
     context #'org-note-operation-link-note
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            (org-note--action-context-document-id context)
            purpose note-id description
            (org-note--action-revision-map context))
      (org-note--action-options
       :lease (and lease (org-note--action-lease-proof lease)))))))

(defun org-note-item-unlink-note ()
  "Unlink a Markdown note from the current Org Note item."
  (interactive)
  (let* ((context (org-note--action-base-context))
         (selection (org-note--read-note-link-to-remove context))
         (purpose (car selection))
         (note-id (cdr selection))
         (lease (org-note--read-optional-action-lease context)))
    (org-note--confirm-action "Unlink this Org Note note? ")
    (setq context (org-note--action-revision-context context))
    (org-note--assert-action-note-link context purpose note-id)
    (org-note--perform-action
     context #'org-note-operation-unlink-note
     (append
      (list (org-note--action-context-workspace-id context)
            (org-note--action-context-item-id context)
            (org-note--action-context-document-id context)
            purpose note-id (org-note--action-revision-map context))
      (org-note--action-options
       :lease (and lease (org-note--action-lease-proof lease)))))))

(defconst org-note--item-actions
  '(("claim" . org-note-item-claim)
    ("heartbeat" . org-note-item-heartbeat)
    ("release" . org-note-item-release)
    ("report progress" . org-note-item-report-progress)
    ("submit result" . org-note-item-submit-result)
    ("transition" . org-note-item-transition)
    ("retry" . org-note-item-retry)
    ("request review" . org-note-item-request-review)
    ("approve review" . org-note-item-approve-review)
    ("reject review" . org-note-item-reject-review)
    ("add dependency" . org-note-item-add-dependency)
    ("remove dependency" . org-note-item-remove-dependency)
    ("link note" . org-note-item-link-note)
    ("unlink note" . org-note-item-unlink-note))
  "Actions exposed by `org-note-item-dispatch'.")

(defun org-note-item-dispatch ()
  "Select and invoke an action for the current Org Note work item."
  (interactive)
  (org-note--action-base-context)
  (let* ((choice
          (completing-read
           "Action: " (mapcar #'car org-note--item-actions) nil t nil
           'org-note--action-choice-history))
         (command (cdr (assoc choice org-note--item-actions))))
    (call-interactively command)))

(provide 'org-note)

;;; org-note.el ends here
