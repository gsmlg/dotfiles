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

(declare-function org-note-operation-query-agenda "org-note-operation"
                  (&key workspace-ids view item-type state priority tags assignee
                        scheduled-from scheduled-to deadline-from deadline-to
                        completed-from completed-to from to include-archived
                        cursor limit))
(declare-function org-note-item-context "org-note" (workspace-id item-id))
(declare-function org-note-configure-agenda-workspaces "org-note" ())
(declare-function org-note-validation-page-cursor "org-note-validation"
                  (cursor))
(declare-function org-note-validation-bounded-pager-state "org-note-validation"
                  (&key limit max-pages max-rows max-requests max-seconds))
(declare-function org-note-validation-bounded-pager-fold "org-note-validation"
                  (state page-fetcher))
(declare-function org-agenda-goto "org-agenda" (&optional highlight))
(declare-function org-entry-get "org" (pom property &optional literal selective))
(declare-function org-get-at-bol "org" (prop))
(declare-function gsmlg-org-apply-path-settings "gsmlg-org" ())

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

(defvar gsmlg-org-note-org--agenda-command-active nil
  "Non-nil while the outermost guarded Agenda producer is running.")

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
    (advice-add #'org-capture :around #'gsmlg-org-note-org--around-capture)))

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

May `(require 'org-note)'.  Idempotent under a reentrancy guard.
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
          (gsmlg-org-note-org--install-feed-hooks)
          (setq gsmlg-org-note-org--activated t)
          (when (fboundp #'gsmlg-org-apply-path-settings)
            (gsmlg-org-apply-path-settings)))
      (setq gsmlg-org-note-org--activating nil))))

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

Phase 3 owns real capture staging; this stub only cold-starts."
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (require 'org-note)
    (gsmlg-org-note-org-activate)
    (apply orig args)))

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
  "Return non-nil when LEFT sorts before RIGHT by canonical identity."
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

Phase 1 uses a direct single-process `write-region'.  Cross-process
publication reservation locks, nonce-checked release, and acquire-
before-fetch ordering (spec blockers 69, 74, 79) are deferred to
Phase 7."
  (let ((target (or path gsmlg-org-note-org--feed-file)))
    (gsmlg-ensure-parent-directory target)
    (unless (and (file-readable-p target)
                 (with-temp-buffer
                   (insert-file-contents target)
                   (equal contents (buffer-string))))
      (write-region contents nil target nil 'silent))
    target))

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
                workspace-ids err))))))
      (setq gsmlg-org-note-org--refresh-active nil)))
  (gsmlg-org-note-org-feed-file))

(defun gsmlg-org-note-org--refresh-before-agenda (&rest _)
  "Refresh the Org Note feed before building an agenda buffer."
  (require 'org-note)
  (gsmlg-org-note-org-refresh-feed))

(defun gsmlg-org-note-org--goto (orig-fun &optional highlight)
  "Open Org Note item context instead of visiting the generated feed."
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

(provide 'gsmlg-org-note-org)
;;; gsmlg-org-note-org.el ends here
