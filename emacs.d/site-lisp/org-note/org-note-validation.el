;;; org-note-validation.el --- Dependency-neutral Org Note validation -*- lexical-binding: t; -*-

;;; Commentary:
;; Endpoint, page cursor, and bounded pagination validation for Org Note.

;;; Code:

(require 'cl-lib)
(require 'url-parse)

(unless (get 'org-note-error 'error-conditions)
  (define-error 'org-note-error "Org Note error"))

(defconst org-note-validation--control-char-regexp "[\0-\37\177]"
  "Regexp matching ASCII control characters in endpoint strings.")

(cl-defstruct org-note-validation--pager-state
  limit max-pages max-rows max-requests max-seconds
  (cursor nil)
  (page-count 0)
  (row-count 0)
  (request-count 0)
  (started-at (float-time))
  (seen-cursors (make-hash-table :test #'equal))
  (seen-row-ids (make-hash-table :test #'equal))
  (done nil))

(defun org-note-validation--signal (message)
  "Signal `org-note-error' with MESSAGE."
  (signal 'org-note-error (list message)))

(defun org-note-validation--reject-control-characters (value reason)
  "Signal when VALUE contains control characters for REASON."
  (when (and (stringp value)
             (string-match-p org-note-validation--control-char-regexp value))
    (org-note-validation--signal reason)))

(defun org-note-validation--default-port (type)
  "Return the default port for endpoint scheme TYPE."
  (pcase type
    ("https" 443)
    ("http" 80)
    (_ nil)))

(defun org-note-validation--normalize-path (path)
  "Normalize endpoint base PATH by trimming trailing slashes."
  (let ((normalized (or path "")))
    (while (and (> (length normalized) 0)
                (eq (aref normalized (1- (length normalized))) ?/))
      (setq normalized (substring normalized 0 -1)))
    normalized))

(defun org-note-validation--canonical-endpoint-from-parsed (url)
  "Return canonical endpoint string for parsed URL object URL."
  (let* ((type (url-type url))
         (host (url-host url))
         (port (url-port url))
         (path (org-note-validation--normalize-path (url-filename url)))
         (default-port (org-note-validation--default-port type))
         (port-part
          (when (and port (not (equal port default-port)))
            (format ":%s" port))))
    (concat type "://" host port-part path)))

(defun org-note-validation-canonical-endpoint (url-or-string)
  "Return canonical endpoint identity for URL-OR-STRING.

Only `http' and `https' endpoints with a nonempty host, no userinfo,
query, fragment, or control characters are accepted.  Trailing slashes on
the base path are normalized."
  (unless (stringp url-or-string)
    (org-note-validation--signal "Org Note endpoint must be a string"))
  (org-note-validation--reject-control-characters
   url-or-string "Org Note endpoint contains control characters")
  (let ((url (url-generic-parse-url url-or-string)))
    (unless (member (url-type url) '("http" "https"))
      (org-note-validation--signal "Org Note endpoint scheme must be http or https"))
    (when (or (url-user url) (url-password url))
      (org-note-validation--signal "Org Note endpoint must not contain userinfo"))
    (when (string-match-p "?" url-or-string)
      (org-note-validation--signal "Org Note endpoint must not contain a query"))
    (when (string-match-p "#" url-or-string)
      (org-note-validation--signal "Org Note endpoint must not contain a fragment"))
    (unless (and (url-host url) (> (length (url-host url)) 0))
      (org-note-validation--signal "Org Note endpoint host must be nonempty"))
    (org-note-validation--reject-control-characters
     (url-host url) "Org Note endpoint host contains control characters")
    (org-note-validation--canonical-endpoint-from-parsed url)))

(defun org-note-validation--query-value (value)
  "Convert query VALUE to its wire representation."
  (cond
   ((eq value t) "true")
   ((eq value :json-false) "false")
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun org-note-validation--url-encode (value)
  "Percent-encode VALUE as UTF-8 URL data."
  (require 'url-util)
  (url-hexify-string value))

(defun org-note-validation--build-url (endpoint route query)
  "Build an absolute URL from ENDPOINT, ROUTE, and QUERY alist."
  (let* ((path (replace-regexp-in-string "\\`/*" "" (or route "")))
         (pairs
          (delq nil
                (mapcar
                 (lambda (entry)
                   (let ((key (car entry))
                         (value (cdr entry)))
                     (when value
                       (format "%s=%s"
                               (org-note-validation--url-encode
                                (if (symbolp key) (symbol-name key) key))
                               (org-note-validation--url-encode
                                (org-note-validation--query-value value))))))
                 query)))
         (url (if (string-empty-p path)
                  endpoint
                (concat endpoint "/" path))))
    (if pairs
        (concat url "?" (mapconcat #'identity pairs "&"))
      url)))

(defun org-note-validation-endpoint-bound-read-context (endpoint)
  "Return endpoint-bound read context for canonical ENDPOINT.

The returned alist contains `endpoint' and `url-builder'.  The builder
accepts ROUTE and optional QUERY alist and returns an absolute URL."
  (let ((canonical (org-note-validation-canonical-endpoint endpoint)))
    (list (cons 'endpoint canonical)
          (cons 'url-builder
                (lambda (route query)
                  (org-note-validation--build-url canonical route query))))))

(defun org-note-validation-page-cursor (cursor)
  "Return CURSOR when it is nil or a nonempty opaque string."
  (cond
   ((null cursor) nil)
   ((and (stringp cursor) (> (length cursor) 0)) cursor)
   (t (org-note-validation--signal "Org Note page cursor must be nil or a nonempty string"))))

(defun org-note-validation--page-value (page key)
  "Return KEY from PAGE plist/alist PAGE."
  (or (plist-get page key)
      (alist-get key page)))

(defun org-note-validation--row-id (row)
  "Return row identity from ROW when present."
  (let ((entry
         (cond
          ((and (consp row) (atom (cdr row))
                (member (car row) '(id "id")))
           row)
          ((listp row)
           (or (assoc 'id row) (assoc "id" row))))))
    (if entry (cdr entry) row)))

(defun org-note-validation--check-pager-budgets (state)
  "Signal when bounded pager STATE exceeds configured budgets."
  (when (>= (org-note-validation--pager-state-page-count state)
            (org-note-validation--pager-state-max-pages state))
    (org-note-validation--signal "Org Note pagination exceeded the page budget"))
  (when (>= (org-note-validation--pager-state-row-count state)
            (org-note-validation--pager-state-max-rows state))
    (org-note-validation--signal "Org Note pagination exceeded the row budget"))
  (when (>= (org-note-validation--pager-state-request-count state)
            (org-note-validation--pager-state-max-requests state))
    (org-note-validation--signal "Org Note pagination exceeded the request budget"))
  (let ((elapsed
         (- (float-time)
            (org-note-validation--pager-state-started-at state))))
    (when (>= elapsed (org-note-validation--pager-state-max-seconds state))
      (org-note-validation--signal "Org Note pagination exceeded the time budget"))))

(cl-defun org-note-validation-bounded-pager-state (&key limit (max-pages 1000) (max-rows 100000)
                                                 (max-requests 1000) (max-seconds 300))
  "Create mutable bounded pager state.

LIMIT is passed to PAGE-FETCHER on each request.  MAX-PAGES, MAX-ROWS,
MAX-REQUESTS, and MAX-SECONDS enforce fail-closed pagination budgets."
  (unless (and (integerp limit) (> limit 0))
    (org-note-validation--signal "Org Note pagination limit must be a positive integer"))
  (make-org-note-validation--pager-state
   :limit limit
   :max-pages max-pages
   :max-rows max-rows
   :max-requests max-requests
   :max-seconds max-seconds))

(defun org-note-validation--track-next-cursor (state request-cursor next-cursor)
  "Record NEXT-CURSOR in STATE or signal on stuck or repeated cursors.

REQUEST-CURSOR is the cursor sent to PAGE-FETCHER; it may already appear in
STATE because the previous page returned it as `next-cursor'.  Only fail when
NEXT-CURSOR equals REQUEST-CURSOR, or when NEXT-CURSOR was already recorded."
  (when (equal next-cursor request-cursor)
    (org-note-validation--signal "Org Note pagination cursor is stuck"))
  (let ((seen (org-note-validation--pager-state-seen-cursors state)))
    (when (gethash next-cursor seen)
      (org-note-validation--signal "Org Note pagination repeated an opaque cursor"))
    (puthash next-cursor t seen)))

(defun org-note-validation--track-rows (state rows)
  "Record ROWS in STATE or signal when an identity repeats."
  (let ((seen (org-note-validation--pager-state-seen-row-ids state)))
    (dolist (row rows)
      (let ((id (org-note-validation--row-id row)))
        (when (gethash id seen)
          (org-note-validation--signal "Org Note pagination repeated a row identity"))
        (puthash id t seen)
        (cl-incf (org-note-validation--pager-state-row-count state))))))

(cl-defun org-note-validation-bounded-pager-step (state page-fetcher)
  "Fetch one page with PAGE-FETCHER using bounded pager STATE.

PAGE-FETCHER receives the current cursor and should return a plist or
alist with `rows' and optional `next-cursor'.  Returns (ROWS DONE-P)."
  (when (org-note-validation--pager-state-done state)
    (org-note-validation--signal "Org Note pagination is already complete"))
  (org-note-validation--check-pager-budgets state)
  (let ((cursor (org-note-validation--pager-state-cursor state)))
    (org-note-validation-page-cursor cursor)
    (cl-incf (org-note-validation--pager-state-request-count state))
    (let* ((page (funcall page-fetcher cursor))
           (rows (or (org-note-validation--page-value page :rows) '()))
           (next-cursor (org-note-validation--page-value page :next-cursor)))
      (unless (listp rows)
        (org-note-validation--signal "Org Note pagination page rows must be a list"))
      (org-note-validation-page-cursor next-cursor)
      (org-note-validation--track-rows state rows)
      (cl-incf (org-note-validation--pager-state-page-count state))
      (if next-cursor
          (progn
            (org-note-validation--track-next-cursor state cursor next-cursor)
            (setf (org-note-validation--pager-state-cursor state) next-cursor)
            (cl-values rows nil))
        (progn
          (setf (org-note-validation--pager-state-done state) t)
          (cl-values rows t))))))

(defun org-note-validation-bounded-pager-fold (state page-fetcher)
  "Exhaustively paginate STATE with PAGE-FETCHER, returning all ROWS."
  (cl-block org-note-validation-bounded-pager-fold
    (let (all-rows)
      (while (not (org-note-validation--pager-state-done state))
        (cl-multiple-value-bind (rows done-p)
            (org-note-validation-bounded-pager-step state page-fetcher)
          (setq all-rows (append all-rows rows))
          (when done-p
            (cl-return-from org-note-validation-bounded-pager-fold all-rows))))
      all-rows)))

(provide 'org-note-validation)
;;; org-note-validation.el ends here
