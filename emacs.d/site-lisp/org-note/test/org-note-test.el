;;; org-note-test.el --- Tests for Org Note browsers -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for Org Note browser and read-only operational buffers.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-note)

(defun org-note-test--workspace-row (id &optional slug revision)
  "Return a complete workspace row for ID, SLUG, and REVISION."
  `((workspace_id . ,id)
    (slug . ,(or slug "workspace-slug"))
    (workspace_revision . ,(or revision 3))
    (counts . ((ready . 4)
               (running . 2)
               (blocked . 1)
               (review . 5)))
    (extra . "complete-row-data")))

(defun org-note-test--document-row (id &optional path revision)
  "Return a complete document row for ID, PATH, and REVISION."
  `((id . ,id)
    (path . ,(or path "notes/example.org"))
    (revision . ,(or revision 7))
    (extra . "complete-row-data")))

(defun org-note-test--page (items next-cursor)
  "Return an Org Note page with ITEMS and NEXT-CURSOR."
  `((items . ,items) (next_cursor . ,next-cursor)))

(defun org-note-test--timestamp (raw epoch)
  "Return an Org timestamp with RAW text and EPOCH seconds."
  `((raw . ,raw)
    (local . "2026-08-13T09:00:00")
    (timezone . "Asia/Shanghai")
    (utc_timestamp . ,epoch)))

(defun org-note-test--item (id workspace-id document-id &optional title)
  "Return a complete work item for ID, WORKSPACE-ID, and DOCUMENT-ID.

TITLE defaults to a stable test title."
  (copy-tree
   `((id . ,id)
     (workspace_id . ,workspace-id)
     (document_id . ,document-id)
     (parent_id . nil)
     (item_type . "task")
     (title . ,(or title "Ship console"))
     (state . "RUNNING")
     (priority . "A")
     (scheduled . ,(org-note-test--timestamp
                    "<2026-08-13 Thu 09:00>" 1786582800))
     (deadline . ,(org-note-test--timestamp
                   "<2026-08-14 Fri 18:00>" 1786701600))
     (assignee . "agent-one")
     (requires_review . t)
     (created_at . 1786500000)
     (tags . ("ops" "release")))))

(defun org-note-test--lease (workspace-id item-id)
  "Return a complete lease for WORKSPACE-ID and ITEM-ID."
  `((id . "lease-a")
    (workspace_id . ,workspace-id)
    (work_item_id . ,item-id)
    (attempt_id . "attempt-a")
    (kind . "execution")
    (actor_id . "agent-one")
    (acquired_at . 1786500100)
    (last_heartbeat_at . 1786500200)
    (expires_at . 1786500800)
    (status . "active")))

(defun org-note-test--operational-row
    (id workspace-id &optional document-id title)
  "Return an operational row for ID and WORKSPACE-ID.

DOCUMENT-ID and TITLE override their stable defaults."
  `((item . ,(org-note-test--item
              id workspace-id (or document-id "document-a") title))
    (attempt_count . 2)
    (current_attempt_status . "running")
    (retry_exhausted . :json-false)
    (ready_status . "ready")
    (review_lease_status . "active")
    (lease . ,(org-note-test--lease workspace-id id))
    (completion_at . nil)
    (extra . "complete-row-data")))

(defun org-note-test--operational-page (items next-cursor)
  "Return an operational page with ITEMS and NEXT-CURSOR."
  `((items . ,items)
    (next_cursor . ,next-cursor)
    (evaluated_at . 1786500300)))

(defun org-note-test--event (id workspace-id &optional item-id)
  "Return an event named ID in WORKSPACE-ID for optional ITEM-ID."
  (copy-tree
   `((id . ,id)
     (workspace_id . ,workspace-id)
     (sequence . 12)
     (subject_kind . "work_item")
     (subject_id . ,(or item-id "item-a"))
     (actor_id . "emacs:test@example")
     (attempt_id . "attempt-a")
     (event_type . "item.transitioned")
     (occurred_at . 1786500300)
     (summary . "Moved into review")
     (metadata . ((safe . "value")))
     (previous_state . "RUNNING")
     (resulting_state . "REVIEW"))))

(defun org-note-test--context (workspace-id item-id &optional empty)
  "Return a complete context for WORKSPACE-ID and ITEM-ID.

When EMPTY is non-nil, collection sections are empty."
  (let* ((document-id "document-a")
         (item (org-note-test--item
                item-id workspace-id document-id "Ship\n* unsafe heading"))
         (parent (org-note-test--item
                  "item-parent" workspace-id document-id "Parent item"))
         (child (org-note-test--item
                 "item-child" workspace-id document-id "Child item"))
         (dependency (org-note-test--item
                      "item-dependency" workspace-id document-id
                      "Dependency item"))
         (origin-item (org-note-test--item
                       "item-origin" workspace-id document-id "Origin item"))
         (event (org-note-test--event "event-a" workspace-id item-id)))
    (setf (alist-get 'parent_id item) (unless empty "item-parent")
          (alist-get 'parent_id child) item-id)
    (copy-tree
     `((workspace . ((id . ,workspace-id)
                    (slug . "delivery")
                    (display_name . "Delivery")
                    (description . "Release coordination")
                    (timezone . "Asia/Shanghai")
                    (policy_schema_version . 1)
                    (policy . ((default_state . "TODO")))
                    (revision . 8)
                    (archived_at . nil)))
      (workspace_revision . 8)
      (document . ((id . ,document-id)
                   (path . "plans/release.org")
                   (revision . 11)))
      (item . ,item)
      (parent . ,(unless empty parent))
      (children . ,(if empty nil (list child)))
      (dependencies . ,(if empty nil
                         `(((item . ,dependency)
                            (satisfied . :json-false)))))
      (note_links . ,(if empty nil
                       '(((purpose . "reference")
                          (note_id . "note-a")
                          (description . "Release notes")
                          (available . t)))))
      (attempts . ,(if empty nil
                     '(((id . "attempt-a")
                        (workspace_id . "workspace-a")
                        (work_item_id . "item-a")
                        (attempt_number . 2)
                        (actor_id . "agent-one")
                        (status . "running")
                        (started_at . 1786500100)
                        (ended_at . nil)
                        (error . nil)
                        (result_summary . "Partial result")
                        (review_outcome . nil)
                        (note_refs . (((purpose . "result")
                                       (note_id . "note-a")
                                       (description . "Attempt note"))))
                        (artifacts . (((uri . "artifact://report")
                                      (media_type . "text/plain")
                                      (name . "Report")
                                      (description . "Build report"))))
                        (metadata . ((safe . "value")))))))
      (origin . ,(unless empty
                   `((kind . "work_item")
                     (work_item_id . "item-origin")
                     (item . ,origin-item))))
      (history_segments . ,(if empty nil
                             `(((workspace_id . ,workspace-id)
                                (events . (,event))))))
      (lease . ,(unless empty (org-note-test--lease workspace-id item-id)))
      (operational . ((classifications . ("running" "review"))
                      (readiness . "ready")
                      (blockers . ,(if empty nil '("dependency")))
                      (attempt_budget . ((execution_attempt_count . 2)
                                         (max_attempts . 4)
                                         (remaining_attempts . 2)
                                         (retry_exhausted . :json-false)))
                      (recovery . ((eligible . t)
                                   (candidate . :json-false)
                                   (blockers . ,(if empty nil '("lease")))))))))))

(defun org-note-test--context-rejection-is-atomic (malformed)
  "Assert that MALFORMED cannot replace a valid item context."
  (let ((response (org-note-test--context "workspace-a" "item-a"))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-item-context)
                   (lambda (&rest _) response)))
          (setq buffer
                (save-window-excursion
                  (org-note-item-context "workspace-a" "item-a")))
          (with-current-buffer buffer
            (goto-char (point-max))
            (let ((text (buffer-string))
                  (point-before (point))
                  (data org-note--item-context-data))
              (setq response malformed)
              (should-error (org-note-item-context-refresh)
                            :type 'org-note-error)
              (should (equal (buffer-string) text))
              (should (= (point) point-before))
              (should (eq org-note--item-context-data data)))))
      (org-note-test--kill-browser-buffers))))

(defun org-note-test--kill-browser-buffers ()
  "Kill browser buffers created by Org Note tests."
  (dolist (buffer (buffer-list))
    (when (or (equal (buffer-name buffer) "*Org Note Workspaces*")
              (string-prefix-p "*Org Note Documents: " (buffer-name buffer))
              (equal (buffer-name buffer) "*Org Note Queue*")
              (equal (buffer-name buffer) "*Org Note Agenda*")
              (equal (buffer-name buffer) "*Org Note Events*")
              (string-prefix-p "*Org Note Context: " (buffer-name buffer)))
      (kill-buffer buffer))))

(ert-deftest org-note-load-is-inert ()
  "Requiring the entry package has no runtime or global side effects."
  (let* ((emacs (concat invocation-directory invocation-name))
         (directory (file-name-directory (locate-library "org-note")))
         (output (generate-new-buffer " *org-note-load-test*"))
         (form
          (format
           (concat
            "(progn (require 'cl-lib) (require 'url) "
            "(add-to-list 'load-path %S) "
            "(when (cl-some #'featurep '(org-note-client "
            "org-note-operation org-note-document org-note)) "
            "(kill-emacs 2)) "
            "(let ((before-buffers "
            "(cl-remove-if (lambda (name) (string-prefix-p \" \" name)) "
            "(mapcar #'buffer-name (buffer-list)))) "
            "(before-map (copy-keymap (current-global-map))) "
            "(requests 0) (timers 0)) "
            "(cl-letf (((symbol-function 'url-retrieve-synchronously) "
            "(lambda (&rest _) (setq requests (1+ requests)))) "
            "((symbol-function 'url-retrieve) "
            "(lambda (&rest _) (setq requests (1+ requests)))) "
            "((symbol-function 'run-at-time) "
            "(lambda (&rest _) (setq timers (1+ timers)))) "
            "((symbol-function 'run-with-timer) "
            "(lambda (&rest _) (setq timers (1+ timers))))) "
            "(require 'org-note)) "
            "(unless (and (= requests 0) (= timers 0) "
            "(cl-every #'featurep '(org-note-client org-note-operation "
            "org-note-document org-note)) "
            "(equal before-buffers "
            "(cl-remove-if (lambda (name) (string-prefix-p \" \" name)) "
            "(mapcar #'buffer-name (buffer-list)))) "
            "(equal before-map (current-global-map))) (kill-emacs 1))))")
           directory)))
    (unwind-protect
        (should (zerop (call-process emacs nil output nil
                                     "-Q" "--batch" "-L" directory
                                     "--eval" form)))
      (kill-buffer output))))

(ert-deftest org-note-browser-modes-have-exact-columns-and-local-keys ()
  "Browser modes expose their specified columns and local bindings only."
  (with-temp-buffer
    (org-note-workspace-list-mode)
    (should
     (equal (mapcar (lambda (column) (car column))
                    (append tabulated-list-format nil))
            '("Workspace" "Slug" "Revision" "Ready" "Running"
              "Blocked" "Review")))
    (should (eq (keymap-lookup org-note-workspace-list-mode-map "RET")
                #'org-note-workspace-open)))
  (with-temp-buffer
    (org-note-document-list-mode)
    (should
     (equal (mapcar (lambda (column) (car column))
                    (append tabulated-list-format nil))
            '("Path" "Revision")))
    (should (eq (keymap-lookup org-note-document-list-mode-map "RET")
                #'org-note-document-list-open)))
  (dolist (map (list org-note-workspace-list-mode-map
                     org-note-document-list-mode-map))
    (should (eq (keymap-lookup map "g") #'org-note-browser-refresh))
    (should (eq (keymap-lookup map "n") #'org-note-browser-next-page))
    (should (eq (keymap-lookup map "p") #'org-note-browser-previous-page))
    (should (eq (keymap-lookup map "q") #'quit-window))))

(ert-deftest org-note-browser-entry-and-open-functions-are-commands ()
  "All public browser entry and open functions are interactive commands."
  (dolist (command '(org-note-workspaces org-note-documents
                     org-note-workspace-open org-note-document-list-open))
    (should (commandp command)))
  (unwind-protect
      (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                 (lambda (&rest _) (org-note-test--page nil nil))))
        (should (bufferp
                 (save-window-excursion
                   (call-interactively #'org-note-workspaces)))))
    (org-note-test--kill-browser-buffers)))

(ert-deftest org-note-workspaces-render-and-store-complete-rows ()
  "Workspace pages render validated values and retain the complete rows."
  (let ((row (org-note-test--workspace-row "workspace-a" "alpha" 9))
        calls buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                   (lambda (&rest arguments)
                     (push arguments calls)
                     (org-note-test--page (list row) "next-page"))))
          (setq buffer (save-window-excursion (org-note-workspaces)))
          (with-current-buffer buffer
            (should (derived-mode-p 'org-note-workspace-list-mode))
            (should (equal calls '((:cursor nil))))
            (should
             (equal tabulated-list-entries
                    '(("workspace-a"
                       ["workspace-a" "alpha" "9" "4" "2" "1" "5"]))))
            (should (equal (gethash "workspace-a" org-note--browser-row-data)
                           row))
            (should (equal org-note--browser-next-cursor "next-page"))
            (should-not org-note--browser-current-cursor)
            (should-not org-note--browser-cursor-history)))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-workspaces-accept-json-array-items ()
  "Workspace pages accept JSON array vectors from the HTTP client."
  (let ((row (org-note-test--workspace-row "workspace-a" "alpha" 9))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                   (lambda (&rest _)
                     `((items . ,(vector row))
                       (next_cursor . nil)))))
          (setq buffer (save-window-excursion (org-note-workspaces)))
          (with-current-buffer buffer
            (should
             (equal tabulated-list-entries
                    '(("workspace-a"
                       ["workspace-a" "alpha" "9" "4" "2" "1" "5"]))))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-workspace-row-accepts-revision-alias ()
  "Workspace rows still accept the older revision alias."
  (should
   (equal
    (org-note--workspace-row
     '((workspace_id . "workspace-a")
       (slug . "alpha")
       (revision . 4)
       (counts . ((ready . 1) (running . 0) (blocked . 0) (review . 0)))))
    '("workspace-a" . ["workspace-a" "alpha" "4" "1" "0" "0" "0"]))))

(ert-deftest org-note-documents-render-and-retain-workspace-context ()
  "Document pages retain their workspace and complete source rows."
  (let ((row (org-note-test--document-row "document-a" "notes/a.org" 11))
        calls buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-documents)
                   (lambda (workspace-id &rest arguments)
                     (push (cons workspace-id arguments) calls)
                     (org-note-test--page (list row) nil))))
          (setq buffer
                (save-window-excursion (org-note-documents "workspace-a")))
          (with-current-buffer buffer
            (should (derived-mode-p 'org-note-document-list-mode))
            (should (equal calls '(("workspace-a" :cursor nil))))
            (should (equal org-note--browser-workspace-id "workspace-a"))
            (should
             (equal tabulated-list-entries
                    '(("document-a" ["notes/a.org" "11"]))))
            (should (equal (gethash "document-a" org-note--browser-row-data)
                           row))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-browser-open-commands-dispatch-from-complete-rows ()
  "Open commands use stored identifiers and document workspace context."
  (let (workspace-buffer document-buffer opened-workspace opened-document)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                     (lambda (&rest _)
                       (org-note-test--page
                        (list (org-note-test--workspace-row "workspace-a"))
                        nil))))
            (setq workspace-buffer
                  (save-window-excursion (org-note-workspaces))))
          (with-current-buffer workspace-buffer
            (org-note--goto-row-id "workspace-a")
            (cl-letf (((symbol-function 'org-note-documents)
                       (lambda (workspace-id)
                         (setq opened-workspace workspace-id))))
              (call-interactively #'org-note-workspace-open)))
          (should (equal opened-workspace "workspace-a"))
          (cl-letf (((symbol-function 'org-note-operation-list-documents)
                     (lambda (&rest _)
                       (org-note-test--page
                        (list (org-note-test--document-row "document-a")) nil))))
            (setq document-buffer
                  (save-window-excursion (org-note-documents "workspace-a"))))
          (with-current-buffer document-buffer
            (org-note--goto-row-id "document-a")
            (cl-letf (((symbol-function 'org-note-document-open)
                       (lambda (workspace-id document-id)
                         (setq opened-document
                               (list workspace-id document-id)))))
              (call-interactively #'org-note-document-list-open)))
          (should (equal opened-document '("workspace-a" "document-a"))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-browser-treats-cursors-as-opaque-and-tracks-history ()
  "Next and previous pass opaque cursor objects through unchanged."
  (let ((cursor (vector "opaque" '(token . 7)))
        calls buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                   (lambda (&rest arguments)
                     (let ((received (plist-get arguments :cursor)))
                       (push received calls)
                       (if received
                           (org-note-test--page
                            (list (org-note-test--workspace-row
                                   "workspace-a" "second")) nil)
                         (org-note-test--page
                          (list (org-note-test--workspace-row
                                 "workspace-a" "first")) cursor))))))
          (setq buffer (save-window-excursion (org-note-workspaces)))
          (with-current-buffer buffer
            (org-note--goto-row-id "workspace-a")
            (org-note-browser-next-page)
            (should (eq org-note--browser-current-cursor cursor))
            (should (equal org-note--browser-cursor-history '(nil)))
            (should (equal (tabulated-list-get-id) "workspace-a"))
            (org-note-browser-previous-page)
            (should-not org-note--browser-current-cursor)
            (should-not org-note--browser-cursor-history)
            (should (equal (tabulated-list-get-id) "workspace-a")))
          (should (= (length calls) 3))
          (setq calls (nreverse calls))
          (should-not (nth 0 calls))
          (should (eq (nth 1 calls) cursor))
          (should-not (nth 2 calls)))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-browser-refresh-preserves-selection ()
  "Refreshing preserves the selected row when that row still exists."
  (let ((reverse-order nil)
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                   (lambda (&rest _)
                     (let ((rows
                            (list (org-note-test--workspace-row "workspace-a")
                                  (org-note-test--workspace-row "workspace-b"))))
                       (org-note-test--page
                        (if reverse-order (reverse rows) rows) nil)))))
          (setq buffer (save-window-excursion (org-note-workspaces)))
          (with-current-buffer buffer
            (org-note--goto-row-id "workspace-b")
            (setq reverse-order t)
            (org-note-browser-refresh)
            (should (equal (tabulated-list-get-id) "workspace-b"))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-browser-page-boundaries-do-not-fetch ()
  "Missing next pages and first-page previous commands do not fetch."
  (let ((calls 0)
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                   (lambda (&rest _)
                     (setq calls (1+ calls))
                     (org-note-test--page
                      (list (org-note-test--workspace-row "workspace-a"))
                      nil))))
          (setq buffer (save-window-excursion (org-note-workspaces)))
          (with-current-buffer buffer
            (org-note-browser-next-page)
            (org-note-browser-previous-page))
          (should (= calls 1)))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-browser-rejects-malformed-pages-atomically ()
  "Malformed page and row data leave the previous browser state intact."
  (let ((response
         (org-note-test--page
          (list (org-note-test--workspace-row "workspace-a")) "next"))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                   (lambda (&rest _) response)))
          (setq buffer (save-window-excursion (org-note-workspaces)))
          (with-current-buffer buffer
            (let ((entries tabulated-list-entries)
                  (rows org-note--browser-row-data)
                  (current org-note--browser-current-cursor)
                  (next org-note--browser-next-cursor)
                  (history org-note--browser-cursor-history))
              (dolist
                  (malformed
                   (list
                    '((items . nil))
                    (org-note-test--page
                     (list (org-note-test--workspace-row "")) nil)
                    (org-note-test--page
                     (list '((workspace_id . "workspace-b")
                             (slug . 9)
                             (workspace_revision . 1)
                             (counts . ((ready . 1) (running . 1)
                                        (blocked . 1) (review . 1))))) nil)
                    (org-note-test--page
                     (list '((workspace_id . "workspace-b")
                             (slug . "beta")
                             (workspace_revision . 1)
                             (counts . ((ready . -1) (running . 1)
                                        (blocked . 1) (review . 1))))) nil)))
                (setq response malformed)
                (should-error (org-note-browser-refresh)
                              :type 'org-note-error)
                (should (eq tabulated-list-entries entries))
                (should (eq org-note--browser-row-data rows))
                (should (eq org-note--browser-current-cursor current))
                (should (eq org-note--browser-next-cursor next))
                (should (eq org-note--browser-cursor-history history))))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-document-browser-rejects-malformed-rows-atomically ()
  "Invalid document display fields cannot replace a valid document page."
  (let ((response
         (org-note-test--page
          (list (org-note-test--document-row "document-a")) nil))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-documents)
                   (lambda (&rest _) response)))
          (setq buffer
                (save-window-excursion (org-note-documents "workspace-a")))
          (with-current-buffer buffer
            (let ((entries tabulated-list-entries)
                  (rows org-note--browser-row-data))
              (setq response
                    (org-note-test--page
                     (list '((id . "document-b") (path . nil)
                             (revision . 2))) nil))
              (should-error (org-note-browser-refresh)
                            :type 'org-note-error)
              (should (eq tabulated-list-entries entries))
              (should (eq org-note--browser-row-data rows)))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-browser-operation-errors-preserve-page-state ()
  "Operation errors propagate without changing the current page state."
  (let ((fail nil)
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                   (lambda (&rest _)
                     (if fail
                         (signal 'org-note-transport-error
                                 '((:message "Request failed")))
                       (org-note-test--page
                        (list (org-note-test--workspace-row "workspace-a"))
                        "next")))))
          (setq buffer (save-window-excursion (org-note-workspaces)))
          (with-current-buffer buffer
            (let ((entries tabulated-list-entries)
                  (rows org-note--browser-row-data)
                  (current org-note--browser-current-cursor)
                  (next org-note--browser-next-cursor)
                  (history org-note--browser-cursor-history))
              (setq fail t)
              (should-error (org-note-browser-refresh)
                            :type 'org-note-transport-error)
              (should (eq tabulated-list-entries entries))
              (should (eq org-note--browser-row-data rows))
              (should (eq org-note--browser-current-cursor current))
              (should (eq org-note--browser-next-cursor next))
              (should (eq org-note--browser-cursor-history history)))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-browser-render-errors-roll-back-text-and-state ()
  "A render error restores text, page state, point, and selection exactly."
  (let ((response
         (org-note-test--page
          (list (org-note-test--workspace-row "workspace-a")
                (org-note-test--workspace-row "workspace-b"))
          "old-next"))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                   (lambda (&rest _) response)))
          (setq buffer (save-window-excursion (org-note-workspaces)))
          (with-current-buffer buffer
            (org-note--goto-row-id "workspace-b")
            (let ((text (buffer-substring (point-min) (point-max)))
                  (entries tabulated-list-entries)
                  (rows org-note--browser-row-data)
                  (current org-note--browser-current-cursor)
                  (next org-note--browser-next-cursor)
                  (history org-note--browser-cursor-history)
                  (selected (tabulated-list-get-id))
                  (old-point (point))
                  caught)
              (setq response
                    (org-note-test--page
                     (list (org-note-test--workspace-row "workspace-c"))
                     "new-next"))
              (cl-letf (((symbol-function 'tabulated-list-print)
                         (lambda (&rest _)
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert "partially rendered"))
                           (error "injected render failure"))))
                (condition-case error-data
                    (org-note-browser-refresh)
                  (error (setq caught error-data))))
              (should (equal caught '(error "injected render failure")))
              (should (equal (buffer-substring (point-min) (point-max)) text))
              (should (eq tabulated-list-entries entries))
              (should (eq org-note--browser-row-data rows))
              (should (eq org-note--browser-current-cursor current))
              (should (eq org-note--browser-next-cursor next))
              (should (eq org-note--browser-cursor-history history))
              (should (= (point) old-point))
              (should (equal (tabulated-list-get-id) selected)))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-browser-commands-reuse-context-buffers ()
  "Repeated entry commands reuse workspace-specific named buffers."
  (let (first second document-first document-second)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-workspaces)
                   (lambda (&rest _) (org-note-test--page nil nil)))
                  ((symbol-function 'org-note-operation-list-documents)
                   (lambda (&rest _) (org-note-test--page nil nil))))
          (setq first (save-window-excursion (org-note-workspaces))
                second (save-window-excursion (org-note-workspaces))
                document-first
                (save-window-excursion (org-note-documents "workspace-a"))
                document-second
                (save-window-excursion (org-note-documents "workspace-a")))
          (should (eq first second))
          (should (eq document-first document-second))
          (should-not
           (eq document-first
               (save-window-excursion (org-note-documents "workspace-b")))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-documents-is-interactive-and-programmatic ()
  "Document browsing accepts an argument or reads it interactively."
  (let (workspace buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "workspace-interactive"))
                  ((symbol-function 'org-note-operation-list-documents)
                   (lambda (workspace-id &rest _)
                     (setq workspace workspace-id)
                     (org-note-test--page nil nil))))
          (setq buffer
                (save-window-excursion
                  (call-interactively #'org-note-documents)))
          (should (equal workspace "workspace-interactive"))
          (with-current-buffer buffer
            (should (equal org-note--browser-workspace-id
                           "workspace-interactive"))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-operational-modes-have-exact-columns-and-local-keys ()
  "Operational modes expose approved columns and contextual local keys."
  (dolist (mode '(org-note-queue-mode org-note-agenda-mode))
    (with-temp-buffer
      (funcall mode)
      (should
       (equal (mapcar (lambda (column) (car column))
                      (append tabulated-list-format nil))
              '("Type" "Title" "State" "Priority" "Assignee"
                "When" "Attempt" "Ready" "Lease")))
      (should (eq (keymap-lookup (current-local-map) "RET")
                  #'org-note-operational-open))
      (should (eq (keymap-lookup (current-local-map) "a")
                  #'org-note-item-dispatch))))
  (with-temp-buffer
    (org-note-event-list-mode)
    (should
     (equal (mapcar (lambda (column) (car column))
                    (append tabulated-list-format nil))
            '("Sequence" "Time" "Type" "Subject" "Actor"
              "Previous" "Result" "Summary")))
    (should-not (keymap-lookup (current-local-map) "RET"))
    (should-not (keymap-lookup (current-local-map) "a")))
  (dolist (map (list org-note-queue-mode-map org-note-agenda-mode-map
                     org-note-event-list-mode-map))
    (should (eq (keymap-lookup map "g") #'org-note-browser-refresh))
    (should (eq (keymap-lookup map "n") #'org-note-browser-next-page))
    (should (eq (keymap-lookup map "p") #'org-note-browser-previous-page))
    (should (eq (keymap-lookup map "q") #'quit-window)))
  (with-temp-buffer
    (org-note-item-context-mode)
    (should (derived-mode-p 'org-mode))
    (should buffer-read-only)
    (should (eq (keymap-lookup (current-local-map) "g")
                #'org-note-item-context-refresh))
    (should (eq (keymap-lookup (current-local-map) "q") #'quit-window))
    (should (eq (keymap-lookup (current-local-map) "a")
                #'org-note-item-dispatch))))

(ert-deftest org-note-operational-public-functions-are-commands ()
  "Operational entry, open, and refresh functions are interactive commands."
  (dolist (command '(org-note-queue org-note-agenda
                     org-note-operational-open org-note-item-context
                     org-note-item-context-refresh org-note-events))
    (should (commandp command))))

(ert-deftest org-note-queue-fetches-exact-context-and-renders-complete-row ()
  "Queue pages pass exact arguments, render fields, and retain source rows."
  (let ((row (org-note-test--operational-row "item-a" "workspace-a"))
        calls buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest arguments)
                     (push arguments calls)
                     (org-note-test--operational-page (list row) "next"))))
          (setq buffer
                (save-window-excursion
                  (org-note-queue '("workspace-a" "workspace-b") 'running)))
          (with-current-buffer buffer
            (should (derived-mode-p 'org-note-queue-mode))
            (should (equal calls
                           '((:workspace-ids ("workspace-a" "workspace-b")
                              :view running :cursor nil))))
            (should
             (equal tabulated-list-entries
                    '(("item-a"
                       ["task" "Ship console" "RUNNING" "A" "agent-one"
                        "<2026-08-13 Thu 09:00>" "running / 2"
                        "ready" "execution / active / agent-one"]))))
            (should (equal (gethash "item-a" org-note--browser-row-data) row))
            (should (equal org-note--operational-workspace-ids
                           '("workspace-a" "workspace-b")))
            (should (eq org-note--operational-view 'running))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-agenda-selects-deadline-and-fetches-exact-view ()
  "Agenda pages use agenda operations and the view-relevant timestamp."
  (let ((row (org-note-test--operational-row "item-a" "workspace-a"))
        calls buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-agenda)
                   (lambda (&rest arguments)
                     (push arguments calls)
                     (org-note-test--operational-page (list row) nil))))
          (setq buffer
                (save-window-excursion
                  (org-note-agenda '("workspace-a") 'upcoming_deadline)))
          (with-current-buffer buffer
            (should (derived-mode-p 'org-note-agenda-mode))
            (should (equal calls
                           '((:workspace-ids ("workspace-a")
                              :view upcoming_deadline :cursor nil))))
            (should
             (equal (aref (cadr (car tabulated-list-entries)) 5)
                    "<2026-08-14 Fri 18:00>"))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-operational-open-uses-stored-nested-identities ()
  "Operational RET uses complete row data rather than rendered text."
  (let (buffer opened)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest _)
                     (org-note-test--operational-page
                      (list (org-note-test--operational-row
                             "item-real" "workspace-real")) nil))))
          (setq buffer
                (save-window-excursion
                  (org-note-queue '("workspace-real") 'ready)))
          (with-current-buffer buffer
            (org-note--goto-row-id "item-real")
            (aset (cadr (car tabulated-list-entries)) 1 "item-display-only")
            (cl-letf (((symbol-function 'org-note-item-context)
                       (lambda (workspace-id item-id)
                         (setq opened (list workspace-id item-id)))))
              (call-interactively #'org-note-operational-open)))
          (should (equal opened '("workspace-real" "item-real"))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-operational-pagination-is-opaque-and-has-nil-boundaries ()
  "Operational pagination preserves opaque cursors, rows, and boundaries."
  (let ((cursor (vector "opaque" '(server . token)))
        calls buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest arguments)
                     (let ((received (plist-get arguments :cursor)))
                       (push received calls)
                       (org-note-test--operational-page
                        (list (org-note-test--operational-row
                               "item-a" "workspace-a"))
                        (unless received cursor))))))
          (setq buffer
                (save-window-excursion
                  (org-note-queue '("workspace-a") 'ready)))
          (with-current-buffer buffer
            (org-note--goto-row-id "item-a")
            (org-note-browser-next-page)
            (should (eq org-note--browser-current-cursor cursor))
            (should (equal (tabulated-list-get-id) "item-a"))
            (org-note-browser-next-page)
            (org-note-browser-previous-page)
            (org-note-browser-previous-page))
          (should (= (length calls) 3))
          (setq calls (nreverse calls))
          (should-not (nth 0 calls))
          (should (eq (nth 1 calls) cursor))
          (should-not (nth 2 calls)))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-operational-malformed-pages-are-atomic ()
  "Malformed, duplicate, and mismatched rows preserve an operational page."
  (let ((response
         (org-note-test--operational-page
          (list (org-note-test--operational-row "item-a" "workspace-a"))
          "old-next"))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest _) response)))
          (setq buffer
                (save-window-excursion
                  (org-note-queue '("workspace-a") 'ready)))
          (with-current-buffer buffer
            (let ((text (buffer-string))
                  (entries tabulated-list-entries)
                  (rows org-note--browser-row-data)
                  (next org-note--browser-next-cursor))
              (dolist
                  (malformed
                   (list
                    (org-note-test--page nil nil)
                    (org-note-test--operational-page
                     (list (org-note-test--operational-row
                            "item-b" "workspace-a")
                           (org-note-test--operational-row
                            "item-b" "workspace-a")) nil)
                    (org-note-test--operational-page
                     '(((item . ((workspace_id . "workspace-a")
                                 (document_id . "document-a")
                                 (item_type . "task")
                                 (title . "Missing ID"))))) nil)
                    (org-note-test--operational-page
                     (list (org-note-test--operational-row
                            "item-b" "workspace-other")) nil)))
                (setq response malformed)
                (should-error (org-note-browser-refresh)
                              :type 'org-note-error)
                (should (equal (buffer-string) text))
                (should (eq tabulated-list-entries entries))
                (should (eq org-note--browser-row-data rows))
                (should (eq org-note--browser-next-cursor next))))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-operational-operation-and-render-errors-are-atomic ()
  "Operational request and table errors preserve text and browser state."
  (let ((failure nil)
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest _)
                     (if failure
                         (signal 'org-note-transport-error
                                 '((:message "Request failed")))
                       (org-note-test--operational-page
                        (list (org-note-test--operational-row
                               "item-a" "workspace-a")) "next")))))
          (setq buffer
                (save-window-excursion
                  (org-note-queue '("workspace-a") 'ready)))
          (with-current-buffer buffer
            (org-note--goto-row-id "item-a")
            (let ((text (buffer-string))
                  (entries tabulated-list-entries)
                  (rows org-note--browser-row-data)
                  (selected (tabulated-list-get-id)))
              (setq failure t)
              (should-error (org-note-browser-refresh)
                            :type 'org-note-transport-error)
              (setq failure nil)
              (cl-letf (((symbol-function 'tabulated-list-print)
                         (lambda (&rest _)
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert "partial"))
                           (error "render failed"))))
                (should-error (org-note-browser-refresh) :type 'error))
              (should (equal (buffer-string) text))
              (should (eq tabulated-list-entries entries))
              (should (eq org-note--browser-row-data rows))
              (should (equal (tabulated-list-get-id) selected)))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-operational-reuse-resets-cross-workspace-state ()
  "A reused operational buffer replaces cursor, fetcher, and workspace state."
  (let (calls first second)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest arguments)
                     (push arguments calls)
                     (let ((workspace-ids
                            (plist-get arguments :workspace-ids)))
                       (org-note-test--operational-page
                        (list (org-note-test--operational-row
                               (if (equal workspace-ids '("workspace-a"))
                                   "item-a"
                                 "item-b")
                               (car workspace-ids)))
                        (and (equal workspace-ids '("workspace-a"))
                             "workspace-a-next"))))))
          (setq first
                (save-window-excursion
                  (org-note-queue '("workspace-a") 'ready))
                second
                (save-window-excursion
                  (org-note-queue '("workspace-b") 'failed)))
          (should (eq first second))
          (with-current-buffer second
            (should-not org-note--browser-current-cursor)
            (should-not org-note--browser-cursor-history)
            (should-not org-note--browser-next-cursor)
            (should (equal org-note--operational-workspace-ids
                           '("workspace-b")))
            (should (eq org-note--operational-view 'failed))
            (should (gethash "item-b" org-note--browser-row-data))
            (should-not (gethash "item-a" org-note--browser-row-data))
            (org-note-browser-refresh))
          (should (equal (plist-get (car calls) :workspace-ids)
                         '("workspace-b")))
          (should-not (plist-get (car calls) :cursor)))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-item-context-fetches-renders-and-reuses-read-only-buffer ()
  "Item context validates identities and renders every required section."
  (let ((context (org-note-test--context "workspace-a" "item-a"))
        calls first second saved-point)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-item-context)
                   (lambda (workspace-id item-id)
                     (push (list workspace-id item-id) calls)
                     context)))
          (setq first
                (save-window-excursion
                  (org-note-item-context "workspace-a" "item-a")))
          (with-current-buffer first
            (should (derived-mode-p 'org-note-item-context-mode))
            (should buffer-read-only)
            (should-not buffer-file-name)
            (should (equal org-note--item-context-workspace-id "workspace-a"))
            (should (equal org-note--item-context-item-id "item-a"))
            (should (eq org-note--item-context-data context))
            (dolist (heading '("* Identity" "* Item" "* Hierarchy"
                               "* Dependencies and readiness blockers"
                               "* Linked notes"
                               "* Attempts, results, and recovery"
                               "* Lease and operational classifications"
                               "* Origin" "* History"))
              (goto-char (point-min))
              (should (search-forward heading nil t)))
            (goto-char (point-min))
            (should-not (re-search-forward "^\\* unsafe heading$" nil t))
            (goto-char (point-min))
            (search-forward "Ship * unsafe heading")
            (setq saved-point (point)))
          (setq second
                (save-window-excursion
                  (org-note-item-context "workspace-a" "item-a")))
          (should (eq first second))
          (with-current-buffer second
            (should (= (point) saved-point))
            (call-interactively #'org-note-item-context-refresh))
          (should (equal calls
                         '(("workspace-a" "item-a")
                           ("workspace-a" "item-a")
                           ("workspace-a" "item-a")))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-item-context-renders-empty-collections-explicitly ()
  "Every empty context collection retains its deterministic Org section."
  (let ((context (org-note-test--context "workspace-a" "item-a" t))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-item-context)
                   (lambda (&rest _) context)))
          (setq buffer
                (save-window-excursion
                  (org-note-item-context "workspace-a" "item-a")))
          (with-current-buffer buffer
            (dolist (empty-text '("No parent" "No children" "No dependencies"
                                  "No readiness blockers" "No linked notes"
                                  "No attempts" "No current lease"
                                  "No origin" "No history segments"))
              (goto-char (point-min))
              (should (search-forward empty-text nil t)))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-item-context-errors-preserve-text-point-and-identity ()
  "Context operation, identity, and rendering errors leave a reused buffer intact."
  (let ((response (org-note-test--context "workspace-a" "item-a"))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-item-context)
                   (lambda (&rest _)
                     (if (eq response 'transport-error)
                         (signal 'org-note-transport-error
                                 '((:message "Request failed")))
                       response))))
          (setq buffer
                (save-window-excursion
                  (org-note-item-context "workspace-a" "item-a")))
          (with-current-buffer buffer
            (goto-char (point-max))
            (let ((text (buffer-string))
                  (point-before (point))
                  (data org-note--item-context-data))
              (dolist (failure
                       (list 'transport-error
                             (org-note-test--context
                              "workspace-other" "item-a")))
                (setq response failure)
                (should-error (org-note-item-context-refresh))
                (should (equal (buffer-string) text))
                (should (= (point) point-before))
                (should (eq org-note--item-context-data data)))
              (setq response data)
              (cl-letf (((symbol-function 'org-note--render-item-context)
                         (lambda (&rest _) (error "render failed"))))
                (should-error (org-note-item-context-refresh) :type 'error))
              (should (equal (buffer-string) text))
              (should (= (point) point-before))
              (should (eq org-note--item-context-data data)))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-events-fetches-filters-and-renders-complete-rows ()
  "Event views pass exact filters, render audit fields, and retain source rows."
  (let ((row (org-note-test--event "event-a" "workspace-a" "item-a"))
        calls buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-events)
                   (lambda (workspace-id &rest arguments)
                     (push (cons workspace-id arguments) calls)
                     (org-note-test--page (list row) "next"))))
          (setq buffer
                (save-window-excursion
                  (org-note-events "workspace-a" "work_item" "item-a")))
          (with-current-buffer buffer
            (should (derived-mode-p 'org-note-event-list-mode))
            (should (equal calls
                           '(("workspace-a" :subject-kind "work_item"
                              :subject-id "item-a" :cursor nil))))
            (should
             (equal tabulated-list-entries
                    '(("event-a"
                       ["12" "1786500300" "item.transitioned"
                        "work_item / item-a" "emacs:test@example"
                        "RUNNING" "REVIEW" "Moved into review"]))))
            (should (equal (gethash "event-a" org-note--browser-row-data) row))
            (should (equal org-note--event-workspace-id "workspace-a"))
            (should (equal org-note--event-subject-kind "work_item"))
            (should (equal org-note--event-subject-id "item-a"))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-events-paginate-and-reset-cross-workspace-filters ()
  "Event reuse preserves opaque paging and replaces every request context."
  (let ((cursor (vector "opaque-event" 9))
        calls first second)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-events)
                   (lambda (workspace-id &rest arguments)
                     (push (cons workspace-id arguments) calls)
                     (let ((event
                            (org-note-test--event
                             (if (equal workspace-id "workspace-a")
                                 "event-a"
                               "event-b")
                             workspace-id)))
                       (when (equal workspace-id "workspace-b")
                         (setf (alist-get 'subject_kind event) "document"
                               (alist-get 'subject_id event) "document-b"))
                       (org-note-test--page
                        (list event)
                        (and (equal workspace-id "workspace-a")
                             (not (plist-get arguments :cursor))
                             cursor))))))
          (setq first
                (save-window-excursion
                  (org-note-events "workspace-a" "work_item" "item-a")))
          (with-current-buffer first
            (org-note-browser-next-page)
            (should (eq org-note--browser-current-cursor cursor)))
          (setq second
                (save-window-excursion
                  (org-note-events "workspace-b" "document" "document-b")))
          (should (eq first second))
          (with-current-buffer second
            (should-not org-note--browser-current-cursor)
            (should-not org-note--browser-cursor-history)
            (should (equal org-note--event-workspace-id "workspace-b"))
            (should (equal org-note--event-subject-kind "document"))
            (should (equal org-note--event-subject-id "document-b"))
            (should (gethash "event-b" org-note--browser-row-data))
            (should-not (gethash "event-a" org-note--browser-row-data)))
          (should (equal (car calls)
                         '("workspace-b" :subject-kind "document"
                           :subject-id "document-b" :cursor nil))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-events-reject-malformed-rows-atomically ()
  "Event identity and display validation cannot replace a valid event page."
  (let ((response
         (org-note-test--page
          (list (org-note-test--event "event-a" "workspace-a")) nil))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-events)
                   (lambda (&rest _) response)))
          (setq buffer
                (save-window-excursion (org-note-events "workspace-a")))
          (with-current-buffer buffer
            (let ((text (buffer-string))
                  (entries tabulated-list-entries)
                  (rows org-note--browser-row-data))
              (dolist (malformed
                       (list
                        (org-note-test--page
                         (list (org-note-test--event
                                "event-b" "workspace-other")) nil)
                        (org-note-test--page
                         '(((workspace_id . "workspace-a")
                            (sequence . 1))) nil)
                        (org-note-test--page
                         (list (org-note-test--event
                                "event-b" "workspace-a")
                               (org-note-test--event
                                "event-b" "workspace-a")) nil)))
                (setq response malformed)
                (should-error (org-note-browser-refresh)
                              :type 'org-note-error)
                (should (equal (buffer-string) text))
                (should (eq tabulated-list-entries entries))
                (should (eq org-note--browser-row-data rows))))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-operational-killed-target-cannot-mutate-current-buffer ()
  "A response for a killed browser target cannot alter another current buffer."
  (let ((other (generate-new-buffer " *org-note-other-target*"))
        target)
    (unwind-protect
        (progn
          (with-current-buffer other (insert "other buffer sentinel"))
          (cl-letf (((symbol-function 'org-note-operation-query-queue)
                     (lambda (&rest _)
                       (setq target (get-buffer "*Org Note Queue*"))
                       (kill-buffer target)
                       (set-buffer other)
                       (org-note-test--operational-page
                        (list (org-note-test--operational-row
                               "item-a" "workspace-a")) nil))))
            (should-error
             (save-window-excursion
               (org-note-queue '("workspace-a") 'ready))
             :type 'org-note-error))
          (should-not (buffer-live-p target))
          (with-current-buffer other
            (should (equal (buffer-string) "other buffer sentinel"))
            (should-not (local-variable-p 'org-note--browser-row-data))))
      (when (buffer-live-p other) (kill-buffer other))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-operational-old-response-cannot-overwrite-reentrant-page ()
  "An older shared-buffer response cannot replace a newer request context."
  (let ((outer t)
        calls buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest arguments)
                     (let ((workspace-ids
                            (plist-get arguments :workspace-ids)))
                       (push workspace-ids calls)
                       (when (and outer
                                  (equal workspace-ids '("workspace-a")))
                         (setq outer nil)
                         (save-window-excursion
                           (org-note-queue '("workspace-b") 'failed)))
                       (org-note-test--operational-page
                        (list (org-note-test--operational-row
                               (if (equal workspace-ids '("workspace-a"))
                                   "item-old"
                                 "item-new")
                               (car workspace-ids))) nil)))))
          (should-error
           (save-window-excursion
             (org-note-queue '("workspace-a") 'ready))
           :type 'org-note-error)
          (setq buffer (get-buffer "*Org Note Queue*"))
          (with-current-buffer buffer
            (should (equal org-note--operational-workspace-ids
                           '("workspace-b")))
            (should (eq org-note--operational-view 'failed))
            (should (gethash "item-new" org-note--browser-row-data))
            (should-not (gethash "item-old" org-note--browser-row-data))
            (org-note-browser-refresh))
          (should (equal (car calls) '("workspace-b"))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-operational-error-rollback-preserves-reentrant-page ()
  "An older render error cannot roll back a newer shared-buffer page."
  (let ((original (symbol-function 'tabulated-list-print))
        (outer t)
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest arguments)
                     (let ((workspace-id
                            (car (plist-get arguments :workspace-ids))))
                       (org-note-test--operational-page
                        (list (org-note-test--operational-row
                               (if (equal workspace-id "workspace-a")
                                   "item-old"
                                 "item-new")
                               workspace-id)) nil))))
                  ((symbol-function 'tabulated-list-print)
                   (lambda (&rest arguments)
                     (if (not outer)
                         (apply original arguments)
                       (setq outer nil)
                       (save-window-excursion
                         (org-note-queue '("workspace-b") 'failed))
                       (error "outer render failed")))))
          (should-error
           (save-window-excursion
             (org-note-queue '("workspace-a") 'ready))
           :type 'error)
          (setq buffer (get-buffer "*Org Note Queue*"))
          (with-current-buffer buffer
            (should (equal org-note--operational-workspace-ids
                           '("workspace-b")))
            (should (eq org-note--operational-view 'failed))
            (should (gethash "item-new" org-note--browser-row-data))
            (should-not (gethash "item-old" org-note--browser-row-data))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-item-context-killed-target-cannot-mutate-current-buffer ()
  "A context response for a killed target cannot alter the current buffer."
  (let ((other (generate-new-buffer " *org-note-other-context*"))
        target)
    (unwind-protect
        (progn
          (with-current-buffer other (insert "context sentinel"))
          (cl-letf (((symbol-function 'org-note-operation-get-item-context)
                     (lambda (&rest _)
                       (setq target
                             (get-buffer
                              "*Org Note Context: workspace-a/item-a*"))
                       (kill-buffer target)
                       (set-buffer other)
                       (org-note-test--context "workspace-a" "item-a"))))
            (should-error
             (save-window-excursion
               (org-note-item-context "workspace-a" "item-a"))
             :type 'org-note-error))
          (should-not (buffer-live-p target))
          (with-current-buffer other
            (should (equal (buffer-string) "context sentinel"))
            (should-not (local-variable-p 'org-note--item-context-data))))
      (when (buffer-live-p other) (kill-buffer other))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-item-context-old-response-cannot-overwrite-reentrant-refresh ()
  "An older item response cannot replace a newer re-entrant item response."
  (let* ((old (org-note-test--context "workspace-a" "item-a"))
         (new (org-note-test--context "workspace-a" "item-a"))
         (outer t)
         buffer)
    (setf (alist-get 'title (alist-get 'item old)) "Old response"
          (alist-get 'title (alist-get 'item new)) "New response")
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-item-context)
                   (lambda (&rest _)
                     (if outer
                         (progn
                           (setq outer nil)
                           (save-window-excursion
                             (org-note-item-context
                              "workspace-a" "item-a"))
                           old)
                       new))))
          (should-error
           (save-window-excursion
             (org-note-item-context "workspace-a" "item-a"))
           :type 'org-note-error)
          (setq buffer
                (get-buffer "*Org Note Context: workspace-a/item-a*"))
          (with-current-buffer buffer
            (should (eq org-note--item-context-data new))
            (goto-char (point-min))
            (should (search-forward "New response" nil t))
            (goto-char (point-min))
            (should-not (search-forward "Old response" nil t))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-tabulated-cells-sanitize-hostile-server-text ()
  "Queue and event cells strip properties and collapse control characters."
  (let* ((queue-row
          (org-note-test--operational-row "item-a" "workspace-a"))
         (event-row (org-note-test--event "event-a" "workspace-a"))
         queue-buffer event-buffer)
    (setf (alist-get 'title (alist-get 'item queue-row))
          (propertize "Hostile\nqueue\ttitle" 'face 'error)
          (alist-get 'summary event-row)
          (propertize "Hostile\nevent\rsummary" 'face 'warning))
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest _)
                     (org-note-test--operational-page
                      (list queue-row) nil)))
                  ((symbol-function 'org-note-operation-list-events)
                   (lambda (&rest _)
                     (org-note-test--page (list event-row) nil))))
          (setq queue-buffer
                (save-window-excursion
                  (org-note-queue '("workspace-a") 'ready))
                event-buffer
                (save-window-excursion (org-note-events "workspace-a")))
          (with-current-buffer queue-buffer
            (let ((cell (aref (cadr (car tabulated-list-entries)) 1)))
              (should (equal cell "Hostile queue title"))
              (should-not (text-properties-at 0 cell)))
            (should (= (count-lines (point-min) (point-max)) 1)))
          (with-current-buffer event-buffer
            (let ((cell (aref (cadr (car tabulated-list-entries)) 7)))
              (should (equal cell "Hostile event summary"))
              (should-not (text-properties-at 0 cell)))
            (should (= (count-lines (point-min) (point-max)) 1))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-events-subject-kind-mismatch-is-atomic ()
  "An active event subject-kind filter rejects mismatched rows atomically."
  (let ((response
         (org-note-test--page
          (list (org-note-test--event "event-a" "workspace-a")) nil))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-events)
                   (lambda (&rest _) response)))
          (setq buffer
                (save-window-excursion
                  (org-note-events "workspace-a" "work_item" nil)))
          (with-current-buffer buffer
            (let ((text (buffer-string))
                  (entries tabulated-list-entries)
                  (rows org-note--browser-row-data))
              (setq response
                    (org-note-test--page
                     (list (org-note-test--event
                            "event-b" "workspace-a")) nil))
              (setf (alist-get 'subject_kind (car (alist-get 'items response)))
                    "document")
              (should-error (org-note-browser-refresh)
                            :type 'org-note-error)
              (should (equal (buffer-string) text))
              (should (eq tabulated-list-entries entries))
              (should (eq org-note--browser-row-data rows)))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-events-subject-id-mismatch-is-atomic ()
  "An active event subject-id filter rejects mismatched rows atomically."
  (let ((response
         (org-note-test--page
          (list (org-note-test--event "event-a" "workspace-a" "item-a"))
          nil))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-list-events)
                   (lambda (&rest _) response)))
          (setq buffer
                (save-window-excursion
                  (org-note-events "workspace-a" nil "item-a")))
          (with-current-buffer buffer
            (let ((text (buffer-string))
                  (entries tabulated-list-entries)
                  (rows org-note--browser-row-data))
              (setq response
                    (org-note-test--page
                     (list (org-note-test--event
                            "event-b" "workspace-a" "item-other")) nil))
              (should-error (org-note-browser-refresh)
                            :type 'org-note-error)
              (should (equal (buffer-string) text))
              (should (eq tabulated-list-entries entries))
              (should (eq org-note--browser-row-data rows)))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-item-context-rejects-inconsistent-parent-atomically ()
  "Context parent presence and identity must match the item's parent ID."
  (let ((malformed (org-note-test--context "workspace-a" "item-a")))
    (setf (alist-get 'parent_id (alist-get 'item malformed)) "item-other")
    (org-note-test--context-rejection-is-atomic malformed)))

(ert-deftest org-note-item-context-rejects-invalid-child-backlink-atomically ()
  "Every context child must point back to the current item."
  (let ((malformed (org-note-test--context "workspace-a" "item-a")))
    (setf (alist-get 'parent_id (car (alist-get 'children malformed)))
          "item-other")
    (org-note-test--context-rejection-is-atomic malformed)))

(ert-deftest org-note-item-context-rejects-cross-workspace-dependency-atomically ()
  "Every dependency item must belong to the context workspace."
  (let ((malformed (org-note-test--context "workspace-a" "item-a")))
    (setf (alist-get
           'workspace_id
           (alist-get 'item (car (alist-get 'dependencies malformed))))
          "workspace-other")
    (org-note-test--context-rejection-is-atomic malformed)))

(ert-deftest org-note-item-context-rejects-mismatched-origin-identity-atomically ()
  "A context origin declaration must match its nested object identity."
  (let ((malformed (org-note-test--context "workspace-a" "item-a")))
    (setf (alist-get 'work_item_id (alist-get 'origin malformed))
          "item-other")
    (org-note-test--context-rejection-is-atomic malformed)))

(ert-deftest org-note-completed-view-renders-completion-time-only ()
  "The completed queue uses completion_at rather than item timestamps."
  (let ((row (org-note-test--operational-row "item-a" "workspace-a"))
        buffer)
    (setf (alist-get 'completion_at row) 1786999999)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest _)
                     (org-note-test--operational-page (list row) nil))))
          (setq buffer
                (save-window-excursion
                  (org-note-queue '("workspace-a") 'completed)))
          (with-current-buffer buffer
            (should (equal (aref (cadr (car tabulated-list-entries)) 5)
                           "1786999999"))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-expired-lease-view-renders-lease-expiry-only ()
  "The expired-lease queue uses the server lease expiry timestamp."
  (let ((row (org-note-test--operational-row "item-a" "workspace-a"))
        buffer)
    (setf (alist-get 'expires_at (alist-get 'lease row)) 1786888888)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest _)
                     (org-note-test--operational-page (list row) nil))))
          (setq buffer
                (save-window-excursion
                  (org-note-queue '("workspace-a") 'expired_lease)))
          (with-current-buffer buffer
            (should (equal (aref (cadr (car tabulated-list-entries)) 5)
                           "1786888888"))))
      (org-note-test--kill-browser-buffers))))

(ert-deftest org-note-browser-off-target-sort-commits-final-entries ()
  "Sorted off-target text and committed entries retain every row in order."
  (let ((rows
         (list (org-note-test--operational-row
                "item-b" "workspace-a" nil "Zulu")
               (org-note-test--operational-row
                "item-c" "workspace-a" nil "Middle")
               (org-note-test--operational-row
                "item-a" "workspace-a" nil "Alpha")))
        (original-printer (symbol-function 'tabulated-list-print))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-query-queue)
                   (lambda (&rest _)
                     (org-note-test--operational-page rows nil)))
                  ((symbol-function 'tabulated-list-print)
                   (lambda (&rest arguments)
                     (let ((stale-head tabulated-list-entries))
                       (prog1 (apply original-printer arguments)
                         (setq tabulated-list-entries
                               (copy-tree tabulated-list-entries))
                         (when (consp stale-head)
                           (setcdr stale-head nil)))))))
          (setq buffer
                (save-window-excursion
                  (org-note-queue '("workspace-a") 'ready)))
          (with-current-buffer buffer
            (setq-local tabulated-list-sort-key '("Title" . nil))
            (org-note-browser-refresh)
            (should (equal (mapcar #'car tabulated-list-entries)
                           '("item-a" "item-c" "item-b")))
            (goto-char (point-min))
            (should (equal (tabulated-list-get-id) "item-a"))
            (forward-line 1)
            (should (equal (tabulated-list-get-id) "item-c"))
            (forward-line 1)
            (should (equal (tabulated-list-get-id) "item-b"))
            (tabulated-list-print t)
            (should (equal (mapcar #'car tabulated-list-entries)
                           '("item-a" "item-c" "item-b")))
            (goto-char (point-min))
            (should (equal (tabulated-list-get-id) "item-a"))
            (forward-line 1)
            (should (equal (tabulated-list-get-id) "item-c"))
            (forward-line 1)
            (should (equal (tabulated-list-get-id) "item-b"))))
      (org-note-test--kill-browser-buffers))))

(defun org-note-test--action-lease (kind)
  "Return an in-memory action lease of KIND."
  (org-note-operation--make-lease
   :workspace-id "workspace-a"
   :item-id "item-a"
   :document-id "document-a"
   :kind kind
   :lease-id (concat "lease-" kind)
   :fencing-token (concat "secret-" kind "-token")
   :expires-at (+ (float-time) 600)
   :heartbeat-p nil))

(defmacro org-note-test--with-action-context (&rest body)
  "Evaluate BODY in a validated item-context action buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (org-note-item-context-mode)
     (setq-local org-note--item-context-workspace-id "workspace-a"
                 org-note--item-context-item-id "item-a"
                 org-note--item-context-data
                 (org-note-test--context "workspace-a" "item-a")
                 org-note--item-context-request-generation 7)
     ,@body))

(defmacro org-note-test--with-action-row (&rest body)
  "Evaluate BODY on a selected validated operational row."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (org-note-queue-mode)
     (let* ((row (org-note-test--operational-row
                  "item-a" "workspace-a" "document-a"))
            (rendered
             (org-note--operational-row '("workspace-a") 'ready row)))
       (puthash "item-a" row org-note--browser-row-data)
       (setq-local org-note--operational-workspace-ids '("workspace-a")
                   org-note--operational-view 'ready
                   org-note--browser-context-key
                   '(queue ("workspace-a") ready)
                   org-note--browser-request-generation 9
                   tabulated-list-entries
                   (list (list (car rendered) (cdr rendered))))
       (tabulated-list-print t)
       (goto-char (point-min))
       ,@body)))

(defun org-note-test--stub-lease-finder (leases)
  "Return a registry finder backed by LEASES keyed by kind."
  (lambda (workspace-id item-id kind)
    (and (equal workspace-id "workspace-a")
         (equal item-id "item-a")
         (alist-get kind leases nil nil #'equal))))

(ert-deftest org-note-action-claim-dispatches-exact-context ()
  "Claim uses stored context identity and lets the operation register its lease."
  (org-note-test--with-action-context
    (let (call (refreshes 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "review"))
                ((symbol-function 'org-note-operation-claim)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((lease_id . "lease-review"))))
                ((symbol-function 'org-note-item-context-refresh)
                 (lambda () (cl-incf refreshes))))
        (call-interactively #'org-note-item-claim))
      (should (equal call
                     '("workspace-a" "item-a" "document-a" 11 "review")))
      (should (= refreshes 1)))))

(ert-deftest org-note-action-heartbeat-dispatches-registered-proof ()
  "Heartbeat uses one registered lease and never fetches item context."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           call (context-fetches 0) (refreshes 0))
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'org-note-operation-get-item-context)
                 (lambda (&rest _)
                   (cl-incf context-fetches)
                   (error "unexpected context fetch")))
                ((symbol-function 'org-note-operation-heartbeat)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 (lambda () (cl-incf refreshes))))
        (call-interactively #'org-note-item-heartbeat))
      (should (equal call
                     '("workspace-a" "item-a" "lease-execution"
                       "execution" "secret-execution-token")))
      (should (= context-fetches 0))
      (should (= refreshes 1)))))

(ert-deftest org-note-action-release-dispatches-target-state ()
  "Release forwards its registered proof and optional target state exactly."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           call (refreshes 0))
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (&rest _) "BLOCKED"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-release)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 (lambda () (cl-incf refreshes))))
        (call-interactively #'org-note-item-release))
      (should (equal call
                     '("workspace-a" "item-a" "document-a" 11
                       "lease-execution" "execution"
                       "secret-execution-token" :target-state "BLOCKED")))
      (should (= refreshes 1)))))

(ert-deftest org-note-action-progress-dispatches-summary-and-metadata ()
  "Progress parses object metadata and omits no supplied field."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           (answers '("Half complete" "{\"percent\":50}"))
           call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (&rest _) (pop answers)))
                ((symbol-function 'org-note-operation-report-progress)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-report-progress))
      (should (equal (seq-take call 6)
                     '("workspace-a" "item-a" "lease-execution"
                       "execution" "secret-execution-token" "Half complete")))
      (should (eq (nth 6 call) :metadata))
      (should (= (gethash "percent" (nth 7 call)) 50)))))

(ert-deftest org-note-action-submit-result-dispatches-json-arrays ()
  "Result submission passes parsed arrays and object metadata exactly once."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           (answers
            '("Shipped"
              "[{\"purpose\":\"result\",\"note_id\":\"note-a\"}]"
              "[{\"uri\":\"artifact://one\"}]"
              "{\"stage\":\"done\"}"))
           call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (&rest _) (pop answers)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-submit-result)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-submit-result))
      (should (equal (seq-take call 7)
                     '("workspace-a" "item-a" "document-a" 11
                       "lease-execution" "secret-execution-token" "Shipped")))
      (should (vectorp (plist-get (nthcdr 7 call) :note-refs)))
      (should (vectorp (plist-get (nthcdr 7 call) :artifacts)))
      (should (hash-table-p (plist-get (nthcdr 7 call) :metadata))))))

(ert-deftest org-note-action-transition-dispatches-optional-fields ()
  "Transition passes a selected optional lease, error, and metadata."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           (answers '("BLOCKED" "waiting" "{\"reason\":\"dependency\"}"))
           call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "execution"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) (pop answers)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-transition)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-transition))
      (should (equal (seq-take call 5)
                     '("workspace-a" "item-a" "document-a" 11 "BLOCKED")))
      (should (equal (plist-get (nthcdr 5 call) :lease)
                     '((lease_id . "lease-execution")
                       (kind . "execution")
                       (fencing_token . "secret-execution-token"))))
      (should (equal (plist-get (nthcdr 5 call) :error) "waiting"))
      (should (hash-table-p (plist-get (nthcdr 5 call) :metadata))))))

(ert-deftest org-note-action-retry-dispatches-exact-context ()
  "Retry forwards the stored document revision once after confirmation."
  (org-note-test--with-action-context
    (let (call)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-retry)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((lease_id . "lease-execution"))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-retry))
      (should (equal call
                     '("workspace-a" "item-a" "document-a" 11))))))

(ert-deftest org-note-action-request-review-dispatches-execution-proof ()
  "Review request sends execution proof and supplied optional fields."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           (answers '("Ready" "[]" "[]" "{\"stage\":\"review\"}"))
           call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (&rest _) (pop answers)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-request-review)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-request-review))
      (should (equal (seq-take call 6)
                     '("workspace-a" "item-a" "document-a" 11
                       "lease-execution" "secret-execution-token")))
      (should (equal (plist-get (nthcdr 6 call) :result-summary) "Ready"))
      (should (vectorp (plist-get (nthcdr 6 call) :note-refs)))
      (should (vectorp (plist-get (nthcdr 6 call) :artifacts)))
      (should (hash-table-p (plist-get (nthcdr 6 call) :metadata))))))

(ert-deftest org-note-action-approve-review-dispatches-review-proof ()
  "Review approval sends the registered review proof and metadata."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "review"))
           (finder (org-note-test--stub-lease-finder `(("review" . ,lease))))
           call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (&rest _) "{\"verdict\":\"ok\"}"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-approve-review)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-approve-review))
      (should (equal (seq-take call 6)
                     '("workspace-a" "item-a" "document-a" 11
                       "lease-review" "secret-review-token")))
      (should (hash-table-p (plist-get (nthcdr 6 call) :metadata))))))

(ert-deftest org-note-action-reject-review-dispatches-reason ()
  "Review rejection sends its required reason and registered review proof."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "review"))
           (finder (org-note-test--stub-lease-finder `(("review" . ,lease))))
           (answers '("Needs tests" ""))
           call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (&rest _) (pop answers)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-reject-review)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-reject-review))
      (should (equal call
                     '("workspace-a" "item-a" "document-a" 11
                       "lease-review" "secret-review-token" "Needs tests"))))))

(ert-deftest org-note-action-add-dependency-dispatches-revision-map ()
  "Adding a dependency sends one exact current-document revision map."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (&rest _) "item-new-dependency"))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "execution"))
                ((symbol-function 'org-note-operation-add-dependency)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-add-dependency))
      (should (equal (seq-take call 4)
                     '("workspace-a" "item-a" "item-new-dependency"
                       "document-a")))
      (let ((revisions (nth 4 call)))
        (should (= (hash-table-count revisions) 1))
        (should (= (gethash "document-a" revisions) 11)))
      (should (equal (plist-get (nthcdr 5 call) :lease)
                     '((lease_id . "lease-execution")
                       (kind . "execution")
                       (fencing_token . "secret-execution-token")))))))

(ert-deftest org-note-action-remove-dependency-selects-stored-relation ()
  "Removing a dependency selects from the validated stored context."
  (org-note-test--with-action-context
    (let (call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "item-dependency"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-remove-dependency)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-remove-dependency))
      (should (equal (seq-take call 4)
                     '("workspace-a" "item-a" "item-dependency"
                       "document-a")))
      (should (= (gethash "document-a" (nth 4 call)) 11))
      (should (= (length call) 5)))))

(ert-deftest org-note-action-link-note-dispatches-stored-revision ()
  "Linking a note forwards required strings and the current revision map."
  (org-note-test--with-action-context
    (let ((answers '("reference" "note-new" "Design reference")) call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) #'ignore)
                ((symbol-function 'read-string)
                 (lambda (&rest _) (pop answers)))
                ((symbol-function 'org-note-operation-link-note)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-link-note))
      (should (equal (seq-take call 6)
                     '("workspace-a" "item-a" "document-a" "reference"
                       "note-new" "Design reference")))
      (should (= (gethash "document-a" (nth 6 call)) 11)))))

(ert-deftest org-note-action-unlink-note-selects-stored-relation ()
  "Unlinking selects purpose and note ID from the complete context DTO."
  (org-note-test--with-action-context
    (let (call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "reference / note-a"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-unlink-note)
                 (lambda (&rest arguments)
                   (setq call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh)
                 #'ignore))
        (call-interactively #'org-note-item-unlink-note))
      (should (equal (seq-take call 5)
                     '("workspace-a" "item-a" "document-a"
                       "reference" "note-a")))
      (should (= (gethash "document-a" (nth 5 call)) 11))
      (should (= (length call) 6)))))

(ert-deftest org-note-action-list-context-fetches-only-when-revision-bound ()
  "List actions fetch validated revision context, except heartbeat and progress."
  (org-note-test--with-action-row
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           (context (org-note-test--context "workspace-a" "item-a"))
           context-calls claim-call heartbeat-call progress-call
           (progress-answers '("Working" "")) (refreshes 0))
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'org-note-operation-get-item-context)
                 (lambda (&rest arguments)
                   (push arguments context-calls)
                   context))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "execution"))
                ((symbol-function 'org-note-operation-claim)
                 (lambda (&rest arguments)
                   (setq claim-call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-operation-heartbeat)
                 (lambda (&rest arguments)
                   (setq heartbeat-call arguments)
                   '((ok . t))))
                ((symbol-function 'read-string)
                 (lambda (&rest _) (pop progress-answers)))
                ((symbol-function 'org-note-operation-report-progress)
                 (lambda (&rest arguments)
                   (setq progress-call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-browser-refresh)
                 (lambda () (cl-incf refreshes))))
        (call-interactively #'org-note-item-claim)
        (call-interactively #'org-note-item-heartbeat)
        (call-interactively #'org-note-item-report-progress))
      (should (equal context-calls '(("workspace-a" "item-a"))))
      (should (equal claim-call
                     '("workspace-a" "item-a" "document-a" 11 "execution")))
      (should (equal heartbeat-call
                     '("workspace-a" "item-a" "lease-execution"
                       "execution" "secret-execution-token")))
      (should (equal progress-call
                     '("workspace-a" "item-a" "lease-execution"
                       "execution" "secret-execution-token" "Working")))
      (should (= refreshes 3)))))

(ert-deftest org-note-action-confirmation-aborts-before-list-context-fetch ()
  "A rejected terminal confirmation performs no read or mutation operation."
  (org-note-test--with-action-row
    (let (calls)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                ((symbol-function 'org-note-operation-get-item-context)
                 (lambda (&rest _) (push 'context calls)))
                ((symbol-function 'org-note-operation-retry)
                 (lambda (&rest _) (push 'retry calls)))
                ((symbol-function 'org-note-browser-refresh)
                 (lambda () (push 'refresh calls))))
        (should-error (call-interactively #'org-note-item-retry)
                      :type 'user-error))
      (should-not calls))))

(ert-deftest org-note-action-json-input-is-strict-and-precedes-network ()
  "Wrong JSON top-level types and blank required text fail before operations."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           calls)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (&rest _) ""))
                ((symbol-function 'org-note-operation-report-progress)
                 (lambda (&rest _) (push 'progress calls))))
        (should-error (call-interactively #'org-note-item-report-progress)
                      :type 'user-error))
      (let ((answers '("Summary" "{}" "" "")))
        (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                  ((symbol-function 'read-string)
                   (lambda (&rest _) (pop answers)))
                  ((symbol-function 'org-note-operation-submit-result)
                   (lambda (&rest _) (push 'result calls))))
          (should-error (call-interactively #'org-note-item-submit-result)
                        :type 'user-error)))
      (should-not calls))))

(ert-deftest org-note-action-blank-optional-fields-are-omitted ()
  "Blank optional inputs do not fabricate operation keyword values."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           (progress-answers '("Working" ""))
           progress-call release-call)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (&rest _) (pop progress-answers)))
                ((symbol-function 'org-note-operation-report-progress)
                 (lambda (&rest arguments)
                   (setq progress-call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh) #'ignore))
        (call-interactively #'org-note-item-report-progress))
      (should (equal progress-call
                     '("workspace-a" "item-a" "lease-execution"
                       "execution" "secret-execution-token" "Working")))
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'org-note-operation-release)
                 (lambda (&rest arguments)
                   (setq release-call arguments)
                   '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh) #'ignore))
        (call-interactively #'org-note-item-release))
      (should (equal release-call
                     '("workspace-a" "item-a" "document-a" 11
                       "lease-execution" "execution"
                       "secret-execution-token"))))))

(ert-deftest org-note-action-failure-retains-ui-and-lease ()
  "Mutation failure does not refresh the origin or alter its registered lease."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           (refreshes 0))
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string) (lambda (&rest _) "Working"))
                ((symbol-function 'org-note-operation-report-progress)
                 (lambda (&rest _) (error "mutation failed")))
                ((symbol-function 'org-note-item-context-refresh)
                 (lambda () (cl-incf refreshes))))
        (should-error (call-interactively #'org-note-item-report-progress)
                      :type 'error))
      (should (= refreshes 0))
      (should (eq (funcall finder "workspace-a" "item-a" "execution")
                  lease)))))

(ert-deftest org-note-action-killed-or-superseded-origin-is-not-refreshed ()
  "Successful mutations cannot refresh killed or newer context origins."
  (dolist (behavior '(kill supersede))
    (let ((buffer (generate-new-buffer " *org-note-action-origin*"))
          (refreshes 0))
      (unwind-protect
          (with-current-buffer buffer
            (org-note-item-context-mode)
            (setq-local org-note--item-context-workspace-id "workspace-a"
                        org-note--item-context-item-id "item-a"
                        org-note--item-context-data
                        (org-note-test--context "workspace-a" "item-a")
                        org-note--item-context-request-generation 3)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "execution"))
                      ((symbol-function 'org-note-operation-claim)
                       (lambda (&rest _)
                         (if (eq behavior 'kill)
                             (kill-buffer buffer)
                           (cl-incf org-note--item-context-request-generation))
                         '((ok . t))))
                      ((symbol-function 'org-note-item-context-refresh)
                       (lambda () (cl-incf refreshes))))
              (call-interactively #'org-note-item-claim)))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (should (= refreshes 0)))))

(ert-deftest org-note-action-dispatch-selects-interactive-command ()
  "The dispatcher validates context and calls the selected action interactively."
  (org-note-test--with-action-context
    (let (called offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered collection)
                   "report progress"))
                ((symbol-function 'org-note-item-report-progress)
                 (lambda () (interactive) (setq called t))))
        (call-interactively #'org-note-item-dispatch))
      (should called)
      (should
       (equal offered
              '("claim" "heartbeat" "release" "report progress"
                "submit result" "transition" "retry" "request review"
                "approve review" "reject review" "add dependency"
                "remove dependency" "link note" "unlink note"))))))

(ert-deftest org-note-action-invalid-origin-precedes-prompts-and-operations ()
  "Actions outside validated operational buffers do no interactive work."
  (with-temp-buffer
    (let (calls)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (push 'read calls)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (push 'complete calls)))
                ((symbol-function 'org-note-operation-get-item-context)
                 (lambda (&rest _) (push 'context calls)))
                ((symbol-function 'org-note-operation-claim)
                 (lambda (&rest _) (push 'claim calls))))
        (should-error (call-interactively #'org-note-item-claim)
                      :type 'user-error))
      (should-not calls))))

(ert-deftest org-note-action-fencing-token-never-enters-prompts-or-history ()
  "Lease fencing tokens remain absent from minibuffer prompts and histories."
  (org-note-test--with-action-context
    (let* ((lease (org-note-test--action-lease "execution"))
           (finder (org-note-test--stub-lease-finder
                    `(("execution" . ,lease))))
           (answers '("Working" ""))
           prompts)
      (setq org-note--action-text-history nil
            org-note--action-id-history nil
            org-note--action-json-history nil
            org-note--action-choice-history nil)
      (cl-letf (((symbol-function 'org-note-operation-find-lease) finder)
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (push prompt prompts)
                   (pop answers)))
                ((symbol-function 'org-note-operation-report-progress)
                 (lambda (&rest _) '((ok . t))))
                ((symbol-function 'org-note-item-context-refresh) #'ignore))
        (call-interactively #'org-note-item-report-progress))
      (let ((visible
             (prin1-to-string
              (list prompts org-note--action-text-history
                    org-note--action-id-history org-note--action-json-history
                    org-note--action-choice-history))))
        (should-not (string-match-p "secret-execution-token" visible))))))

(ert-deftest org-note-action-refresh-quit-preserves-confirmed-success ()
  "A cancelled refresh cannot turn one confirmed mutation into a failure."
  (org-note-test--with-action-context
    (let ((result '((confirmed . t)))
          (mutations 0)
          messages)
      (let ((text (buffer-string)))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "execution"))
                  ((symbol-function 'org-note-operation-claim)
                   (lambda (&rest _)
                     (cl-incf mutations)
                     result))
                  ((symbol-function 'org-note-item-context-refresh)
                   (lambda () (signal 'quit nil)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest arguments)
                     (push (apply #'format format-string arguments) messages))))
          (should (eq (call-interactively #'org-note-item-claim) result)))
        (should (= mutations 1))
        (should (equal (buffer-string) text))
        (should (equal messages
                       '("Org Note action succeeded, but its view refresh was cancelled")))
        (should-not
         (string-match-p
          "secret-execution-token" (mapconcat #'identity messages " ")))))))

(ert-deftest org-note-action-commands-are-interactive-and-locally-bound ()
  "All actions are commands and dispatch is bound only in operational maps."
  (dolist (command
           '(org-note-item-claim org-note-item-heartbeat org-note-item-release
             org-note-item-report-progress org-note-item-submit-result
             org-note-item-transition org-note-item-retry
             org-note-item-request-review org-note-item-approve-review
             org-note-item-reject-review org-note-item-add-dependency
             org-note-item-remove-dependency org-note-item-link-note
             org-note-item-unlink-note org-note-item-dispatch))
    (should (commandp command)))
  (dolist (map (list org-note-queue-mode-map org-note-agenda-mode-map
                     org-note-item-context-mode-map))
    (should (eq (keymap-lookup map "a") #'org-note-item-dispatch)))
  (dolist (map (list org-note-workspace-list-mode-map
                     org-note-document-list-mode-map
                     org-note-event-list-mode-map))
    (should-not (eq (keymap-lookup map "a") #'org-note-item-dispatch)))
  (should-not (eq (keymap-lookup global-map "a") #'org-note-item-dispatch)))

(provide 'org-note-test)

;;; org-note-test.el ends here
