;;; gsmlg-org-note-agenda.el --- Org Note data in Org agenda -*- lexical-binding: t; -*-

;;; Commentary:
;; Expose Org Note scheduled and deadline items through the standard Org
;; agenda by maintaining a generated feed file in the cache directory.

;;; Code:

(require 'cl-lib)
(require 'gsmlg-paths)

(declare-function org-note-operation-query-agenda "org-note-operation"
                  (&key workspace-ids view item-type state priority tags assignee
                        scheduled-from scheduled-to deadline-from deadline-to
                        completed-from completed-to from to include-archived
                        cursor limit))
(declare-function org-note-item-context "org-note" (workspace-id item-id))
(declare-function org-agenda-goto "org-agenda" (&optional highlight))
(declare-function org-entry-get "org" (pom property &optional literal selective))
(declare-function org-get-at-bol "org" (prop))
(declare-function gsmlg-org-apply-path-settings "gsmlg-org" ())

(defgroup gsmlg-org-note-agenda nil
  "Integrate Org Note agenda data into Org agenda."
  :group 'gsmlg-org)

(defconst gsmlg-org-note-agenda--feed-tag "ORGNOTE"
  "Tag marking generated Org Note feed headings.")

(defvar gsmlg-org-note-agenda--feed-file
  (gsmlg-cache-file "org-note-agenda-feed.org")
  "Generated Org file exposing Org Note scheduled and deadline items.")

(defvar gsmlg-org-note-agenda--last-workspace-ids nil
  "Workspace IDs used for the current feed snapshot.")

(defvar gsmlg-org-note-agenda--refresh-active nil
  "Non-nil while `gsmlg-org-note-agenda-refresh-feed' is running.")

(defvar gsmlg-org-note-agenda--activated nil
  "Non-nil after `gsmlg-org-note-agenda-activate' has run.")

(defun gsmlg-org-note-agenda-feed-file ()
  "Return the generated Org Note agenda feed path."
  gsmlg-org-note-agenda--feed-file)

(defun gsmlg-org-note-agenda--symbol-alist-p (value)
  "Return non-nil when VALUE is a nonempty alist."
  (and (listp value) (cl-every #'consp value)))

(defun gsmlg-org-note-agenda--string-or-nil (value)
  "Return VALUE when it is a nonempty string."
  (and (stringp value) (not (string-empty-p value)) value))

(defun gsmlg-org-note-agenda--workspace-ids ()
  "Return configured Org Note agenda workspace IDs, or nil."
  (when (boundp 'org-note-agenda-workspace-ids)
    (let ((ids org-note-agenda-workspace-ids))
      (when (and (listp ids) ids)
        (let ((copy (copy-sequence ids)))
          (when (and copy (cl-every #'gsmlg-org-note-agenda--string-or-nil copy))
            copy))))))

(defun gsmlg-org-note-agenda--timestamp-raw (timestamp)
  "Return the Org timestamp text from Org Note TIMESTAMP, or nil."
  (when (gsmlg-org-note-agenda--symbol-alist-p timestamp)
    (gsmlg-org-note-agenda--string-or-nil (alist-get 'raw timestamp))))

(defun gsmlg-org-note-agenda--item-id (item)
  "Return the validated item ID from ITEM."
  (gsmlg-org-note-agenda--string-or-nil (alist-get 'id item)))

(defun gsmlg-org-note-agenda--item-key (item)
  "Return a deduplication key for ITEM."
  (let ((workspace (gsmlg-org-note-agenda--string-or-nil
                    (alist-get 'workspace_id item)))
        (id (gsmlg-org-note-agenda--item-id item)))
    (when (and workspace id)
      (cons workspace id))))

(defun gsmlg-org-note-agenda--item-tags (item)
  "Return a `:TAG:' suffix string for ITEM."
  (let ((tags (alist-get 'tags item)))
    (when (and (listp tags) tags)
      (mapconcat #'identity
                 (cl-remove-if-not #'gsmlg-org-note-agenda--string-or-nil tags)
                 ":"))))

(defun gsmlg-org-note-agenda--item-headline (item)
  "Return an Org headline line for ITEM."
  (let* ((state (or (gsmlg-org-note-agenda--string-or-nil
                     (alist-get 'state item))
                    "TODO"))
         (title (or (gsmlg-org-note-agenda--string-or-nil
                     (alist-get 'title item))
                    "Org Note item"))
         (priority (gsmlg-org-note-agenda--string-or-nil
                     (alist-get 'priority item)))
         (tags (gsmlg-org-note-agenda--item-tags item))
         (tag-string
          (mapconcat #'identity
                     (delq nil
                           (list gsmlg-org-note-agenda--feed-tag tags))
                     ":")))
    (format "* %s%s %s :%s:"
            state
            (if priority (format " [#%s]" priority) "")
            (replace-regexp-in-string "[\n\r\t]+" " " title)
            tag-string)))

(defun gsmlg-org-note-agenda--item-properties (item)
  "Return Org property drawer lines for ITEM."
  (let ((workspace (gsmlg-org-note-agenda--string-or-nil
                    (alist-get 'workspace_id item)))
        (id (gsmlg-org-note-agenda--item-id item)))
    (when (and workspace id)
      (format ":PROPERTIES:\n:ORG_NOTE_WORKSPACE_ID: %s\n:ORG_NOTE_ITEM_ID: %s\n:END:\n"
              workspace id))))

(defun gsmlg-org-note-agenda--item-timestamp-lines (item view)
  "Return scheduled/deadline lines for ITEM and agenda VIEW."
  (let ((scheduled (gsmlg-org-note-agenda--timestamp-raw
                    (alist-get 'scheduled item)))
        (deadline (gsmlg-org-note-agenda--timestamp-raw
                   (alist-get 'deadline item))))
    (delq nil
          (list
           (when (and scheduled (memq view '(scheduled all)))
             (format "SCHEDULED: %s" scheduled))
           (when (and deadline (memq view '(upcoming_deadline all)))
             (format "DEADLINE: %s" deadline))))))

(defun gsmlg-org-note-agenda--item-text (item view)
  "Return Org text for ITEM in agenda VIEW."
  (let ((lines (append (list (gsmlg-org-note-agenda--item-headline item))
                       (gsmlg-org-note-agenda--item-timestamp-lines item view)
                       (list (gsmlg-org-note-agenda--item-properties item)))))
    (mapconcat #'identity (delq nil lines) "\n")))

(defun gsmlg-org-note-agenda--page-items (response)
  "Return item alists from an Org Note agenda page RESPONSE."
  (unless (gsmlg-org-note-agenda--symbol-alist-p response)
    (error "Org Note agenda page is malformed"))
  (let ((raw-items (alist-get 'items response)))
    (unless (listp raw-items)
      (error "Org Note agenda page items are malformed"))
    (mapcar (lambda (row)
              (let ((item (alist-get 'item row)))
                (unless (gsmlg-org-note-agenda--symbol-alist-p item)
                  (error "Org Note agenda row item is malformed"))
                item))
            raw-items)))

(defun gsmlg-org-note-agenda--page-next-cursor (response)
  "Return the next cursor from Org Note agenda page RESPONSE."
  (let ((cursor (alist-get 'next_cursor response)))
    (unless (or (null cursor) (stringp cursor))
      (error "Org Note agenda page cursor is malformed"))
    cursor))

(defun gsmlg-org-note-agenda--fetch-view-items (workspace-ids view)
  "Fetch all agenda items for VIEW across WORKSPACE-IDS."
  (let ((seen (make-hash-table :test #'equal))
        (items nil)
        (cursor nil))
    (catch 'done
      (while t
        (let* ((response (org-note-operation-query-agenda
                          :workspace-ids workspace-ids
                          :view view
                          :cursor cursor))
               (page-items (gsmlg-org-note-agenda--page-items response)))
          (dolist (item page-items)
            (let ((key (gsmlg-org-note-agenda--item-key item)))
              (when (and key (not (gethash key seen)))
                (puthash key t seen)
                (push item items))))
          (setq cursor (gsmlg-org-note-agenda--page-next-cursor response))
          (unless cursor
            (throw 'done nil)))))
    (nreverse items)))

(defun gsmlg-org-note-agenda--write-feed (contents)
  "Write CONTENTS to the Org Note agenda feed file."
  (gsmlg-ensure-parent-directory gsmlg-org-note-agenda--feed-file)
  (write-region contents nil gsmlg-org-note-agenda--feed-file nil 'silent))

(defun gsmlg-org-note-agenda--empty-feed-contents ()
  "Return the contents of an empty Org Note agenda feed."
  (format "#+TITLE: Org Note Agenda Feed\n#+FILETAGS: %s\n"
          gsmlg-org-note-agenda--feed-tag))

(defun gsmlg-org-note-agenda--build-feed-contents (workspace-ids)
  "Return Org feed contents for WORKSPACE-IDS."
  (let* ((scheduled (gsmlg-org-note-agenda--fetch-view-items
                     workspace-ids 'scheduled))
         (deadlines (gsmlg-org-note-agenda--fetch-view-items
                     workspace-ids 'upcoming_deadline))
         (seen (make-hash-table :test #'equal))
         (sections nil))
    (dolist (item (append scheduled deadlines))
      (let ((key (gsmlg-org-note-agenda--item-key item)))
        (when (and key (not (gethash key seen)))
          (puthash key t seen)
          (push (gsmlg-org-note-agenda--item-text item 'all) sections))))
    (if sections
        (concat (gsmlg-org-note-agenda--empty-feed-contents)
                (mapconcat #'identity (nreverse sections) "\n\n")
                "\n")
      (gsmlg-org-note-agenda--empty-feed-contents))))

(defun gsmlg-org-note-agenda-refresh-feed (&optional force)
  "Refresh the generated Org Note agenda feed file.

When FORCE is nil and workspace selection is unchanged, reuse the
existing snapshot."
  (let ((workspace-ids (gsmlg-org-note-agenda--workspace-ids)))
    (unless gsmlg-org-note-agenda--refresh-active
      (setq gsmlg-org-note-agenda--refresh-active t)
      (unwind-protect
          (cond
           ((null workspace-ids)
            (setq gsmlg-org-note-agenda--last-workspace-ids nil)
            (gsmlg-org-note-agenda--write-feed
             (gsmlg-org-note-agenda--empty-feed-contents)))
           ((and (not force)
                 (equal workspace-ids gsmlg-org-note-agenda--last-workspace-ids)
                 (file-readable-p gsmlg-org-note-agenda--feed-file))
            nil)
           (t
            (gsmlg-org-note-agenda--write-feed
             (gsmlg-org-note-agenda--build-feed-contents workspace-ids))
            (setq gsmlg-org-note-agenda--last-workspace-ids
                  (copy-sequence workspace-ids))))
        (setq gsmlg-org-note-agenda--refresh-active nil)))
    gsmlg-org-note-agenda--feed-file))

(defun gsmlg-org-note-agenda-expanded-files (source)
  "Return SOURCE plus the Org Note feed file when integration is active."
  (let ((files (cond
                ((null source) nil)
                ((listp source) (copy-sequence source))
                (t (list source)))))
    (push gsmlg-org-note-agenda--feed-file files)
    files))

(defun gsmlg-org-note-agenda--refresh-before-agenda (&rest _)
  "Refresh the Org Note feed before building an agenda buffer."
  (require 'org-note)
  (condition-case-unless-debug err
      (gsmlg-org-note-agenda-refresh-feed)
    (error
     (message "Org Note agenda feed refresh failed: %s" (error-message-string err))
     (gsmlg-org-note-agenda--write-feed
      (gsmlg-org-note-agenda--empty-feed-contents)))))

(defun gsmlg-org-note-agenda--goto (orig-fun &optional highlight)
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

(defun gsmlg-org-note-agenda-activate ()
  "Install Org Note agenda integration hooks and advice."
  (unless gsmlg-org-note-agenda--activated
    (setq gsmlg-org-note-agenda--activated t)
    (gsmlg-org-note-agenda--write-feed
     (gsmlg-org-note-agenda--empty-feed-contents))
    (advice-add #'org-agenda :before #'gsmlg-org-note-agenda--refresh-before-agenda)
    (advice-add #'org-agenda-goto :around #'gsmlg-org-note-agenda--goto)
    (when (fboundp #'gsmlg-org-apply-path-settings)
      (gsmlg-org-apply-path-settings))))

(provide 'gsmlg-org-note-agenda)
;;; gsmlg-org-note-agenda.el ends here
