;;; emacs-agent-search.el --- Workspace discovery and search -*- lexical-binding: t; -*-

;;; Commentary:

;; Bounded project file discovery and ripgrep-backed text search.  Opaque
;; cursors retain an immutable result snapshot for a short period.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'emacs-agent-workspace)
(require 'emacs-agent-policy)

(defcustom emacs-agent-search-default-results 100
  "Default maximum number of file or search results per page."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-search-hard-limit 1000
  "Hard maximum number of retained search results."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-search-cursor-lifetime 300
  "Number of seconds an opaque search cursor remains valid."
  :type 'integer
  :group 'emacs-agent-editor)

(define-error 'emacs-agent-search-error "Emacs Agent search error")
(define-error 'emacs-agent-invalid-cursor "Search cursor is invalid"
  'emacs-agent-search-error)

(cl-defstruct (emacs-agent-search-cursor
               (:constructor emacs-agent-search-cursor--make))
  id workspace-id kind fingerprint items position expires-at)

(defvar emacs-agent-search-cursors (make-hash-table :test #'equal))
(defvar emacs-agent-search-processes (make-hash-table :test #'eq))

(defun emacs-agent-search--limit (value)
  "Validate and normalize requested page limit VALUE."
  (let ((value (or value emacs-agent-search-default-results)))
    (unless (and (integerp value) (> value 0))
      (signal 'wrong-type-argument (list 'positive-integer value)))
    (min value emacs-agent-search-hard-limit)))

(defun emacs-agent-search--matches-glob-p (path globs)
  "Return non-nil when PATH matches one of GLOBS, or when GLOBS is nil."
  (or (null globs)
      (seq-some
       (lambda (glob)
         (string-match-p
          (concat "\\`" (wildcard-to-regexp glob) "\\'")
          path))
       globs)))

(defun emacs-agent-search--included-p (path includes excludes)
  "Return whether PATH passes INCLUDES and EXCLUDES."
  (and (emacs-agent-search--matches-glob-p path includes)
       (not (and excludes
                 (emacs-agent-search--matches-glob-p path excludes)))))

(defun emacs-agent-search--new-cursor
    (workspace kind fingerprint items position)
  "Store a WORKSPACE cursor of KIND over ITEMS at POSITION.
FINGERPRINT binds the cursor to the originating request."
  (let* ((id (emacs-agent-workspace--random-id "cursor"))
         (cursor
          (emacs-agent-search-cursor--make
           :id id
           :workspace-id (emacs-agent-workspace-workspace-id workspace)
           :kind kind :fingerprint fingerprint :items items
           :position position
           :expires-at (+ (float-time)
                          emacs-agent-search-cursor-lifetime))))
    (puthash id cursor emacs-agent-search-cursors)
    id))

(defun emacs-agent-search--resume-cursor
    (workspace id kind fingerprint)
  "Resolve cursor ID for WORKSPACE, KIND, and FINGERPRINT."
  (let ((cursor (gethash id emacs-agent-search-cursors)))
    (unless (and cursor
                 (> (emacs-agent-search-cursor-expires-at cursor)
                    (float-time))
                 (equal (emacs-agent-search-cursor-workspace-id cursor)
                        (emacs-agent-workspace-workspace-id workspace))
                 (eq kind (emacs-agent-search-cursor-kind cursor))
                 (equal fingerprint
                        (emacs-agent-search-cursor-fingerprint cursor)))
      (remhash id emacs-agent-search-cursors)
      (signal 'emacs-agent-invalid-cursor
              (list "Cursor is expired or does not match the request")))
    cursor))

(defun emacs-agent-search--page
    (workspace kind fingerprint items start limit &optional old-cursor)
  "Page ITEMS from START to LIMIT for WORKSPACE and KIND.
FINGERPRINT binds a new cursor; OLD-CURSOR is consumed when supplied."
  (let* ((end (min (length items) (+ start limit)))
         (page (cl-subseq items start end))
         (next
          (when (< end (length items))
            (emacs-agent-search--new-cursor
             workspace kind fingerprint items end))))
    (when old-cursor
      (remhash old-cursor emacs-agent-search-cursors))
    (list :results page :next_cursor next
          :result_count (length page))))

(defun emacs-agent-search--project-files (workspace)
  "Return canonical relative project files for WORKSPACE."
  (let* ((root (emacs-agent-workspace-root workspace))
         (project (or (emacs-agent-workspace-project workspace)
                      (let ((default-directory root))
                        (project-current nil root))))
         (files
          (if project
              (project-files project)
            (directory-files-recursively root "." nil nil t))))
    (sort
     (delete-dups
      (seq-filter
       (lambda (path)
         (condition-case nil
             (progn
               (emacs-agent-policy-assert-document workspace path)
               t)
           (emacs-agent-error nil)))
       (mapcar
        (lambda (file)
          (file-relative-name
           (if (file-name-absolute-p file)
               file
             (expand-file-name file root))
           root))
        files)))
     #'string<)))

(cl-defun emacs-agent-workspace-files
    (workspace &key include-globs exclude-globs max-results cursor)
  "List files in WORKSPACE with pagination.
INCLUDE-GLOBS and EXCLUDE-GLOBS filter paths, MAX-RESULTS bounds the page, and
CURSOR resumes a matching prior request."
  (let* ((limit (emacs-agent-search--limit max-results))
         (fingerprint
          (secure-hash 'sha256
                       (prin1-to-string
                        (list include-globs exclude-globs)))))
    (if cursor
        (let ((saved (emacs-agent-search--resume-cursor
                      workspace cursor 'files fingerprint)))
          (emacs-agent-search--page
           workspace 'files fingerprint
           (emacs-agent-search-cursor-items saved)
           (emacs-agent-search-cursor-position saved)
           limit cursor))
      (let ((files
             (seq-filter
              (lambda (path)
                (emacs-agent-search--included-p
                 path include-globs exclude-globs))
              (emacs-agent-search--project-files workspace))))
        (emacs-agent-search--page
         workspace 'files fingerprint files 0 limit)))))

(defun emacs-agent-search--rg-arguments
    (query regexp include-globs exclude-globs)
  "Build ripgrep arguments for QUERY.
REGEXP selects regular-expression matching.  INCLUDE-GLOBS and EXCLUDE-GLOBS
constrain paths."
  (append
   (list "--json" "--line-number" "--column" "--color=never"
         "--no-heading" "--hidden"
         "--glob=!.git/**" "--glob=!.env" "--glob=!.env.*")
   (cl-mapcan
    (lambda (basename) (list "--glob" (concat "!" basename)
                             "--glob" (concat "!**/" basename)))
    emacs-agent-policy-denied-basenames)
   (cl-mapcan
    (lambda (extension) (list "--glob" (concat "!*." extension)
                              "--glob" (concat "!**/*." extension)))
    emacs-agent-policy-denied-extensions)
   (unless regexp (list "--fixed-strings"))
   (cl-mapcan (lambda (glob) (list "--glob" glob)) include-globs)
   (cl-mapcan (lambda (glob) (list "--glob" (concat "!" glob)))
              exclude-globs)
   (list "--" query ".")))

(defun emacs-agent-search--json-text (object)
  "Extract a ripgrep JSON text field from OBJECT."
  (or (alist-get 'text object)
      (let ((bytes (alist-get 'bytes object)))
        (when bytes
          (decode-coding-string (base64-decode-string bytes) 'utf-8)))))

(defun emacs-agent-search--byte-column (line byte-offset)
  "Convert zero-based BYTE-OFFSET in UTF-8 LINE to a character column."
  (length
   (decode-coding-string
    (substring (encode-coding-string line 'utf-8) 0 byte-offset)
    'utf-8)))

(defun emacs-agent-search--parse-rg-buffer (buffer)
  "Parse ripgrep JSON records in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (results)
      (while (and (< (point) (point-max))
                  (< (length results) emacs-agent-search-hard-limit))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (string-empty-p line)
            (condition-case nil
                (let* ((record
                        (json-parse-string
                         line :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil))
                       (data (alist-get 'data record)))
                  (when (equal (alist-get 'type record) "match")
                    (let* ((path (emacs-agent-search--json-text
                                  (alist-get 'path data)))
                           (preview (string-trim-right
                                     (or (emacs-agent-search--json-text
                                          (alist-get 'lines data))
                                         "")
                                     "[\r\n]+"))
                           (submatch (car (alist-get 'submatches data)))
                           (byte-column (or (alist-get 'start submatch) 0)))
                      (push
                       (list :path (string-remove-prefix "./" path)
                             :line (alist-get 'line_number data)
                             :column
                             (emacs-agent-search--byte-column
                              preview byte-column)
                             :preview preview)
                       results))))
              (error nil))))
        (forward-line 1))
      (sort
       results
       (lambda (left right)
         (let ((left-path (plist-get left :path))
               (right-path (plist-get right :path)))
           (if (equal left-path right-path)
               (let ((left-line (plist-get left :line))
                     (right-line (plist-get right :line)))
                 (if (= left-line right-line)
                     (< (plist-get left :column)
                        (plist-get right :column))
                   (< left-line right-line)))
             (string< left-path right-path))))))))

(defun emacs-agent-search--fallback
    (workspace query regexp include-globs exclude-globs)
  "Search WORKSPACE for QUERY in Emacs when ripgrep is unavailable.
REGEXP selects regular-expression matching.  INCLUDE-GLOBS and EXCLUDE-GLOBS
constrain paths."
  (let ((matcher (if regexp query (regexp-quote query)))
        results)
    (dolist (path (emacs-agent-search--project-files workspace))
      (when (and (< (length results) emacs-agent-search-hard-limit)
                 (emacs-agent-search--included-p
                  path include-globs exclude-globs))
        (let ((absolute (expand-file-name
                         path (emacs-agent-workspace-root workspace))))
          (when (file-regular-p absolute)
            (with-temp-buffer
              (condition-case nil
                  (progn
                    (insert-file-contents absolute)
                    (goto-char (point-min))
                    (while (and (< (length results)
                                   emacs-agent-search-hard-limit)
                                (re-search-forward matcher nil t))
                      (let ((match (match-beginning 0)))
                        (push
                         (list :path path
                               :line (line-number-at-pos match)
                               :column (- match
                                          (line-beginning-position))
                               :preview
                               (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
                         results))))
                (error nil)))))))
    (nreverse results)))

(defun emacs-agent-search--run-rg
    (workspace query regexp include-globs exclude-globs)
  "Search WORKSPACE synchronously with ripgrep for QUERY.
REGEXP, INCLUDE-GLOBS, and EXCLUDE-GLOBS control matching."
  (let ((buffer (generate-new-buffer " *emacs-agent-rg*"))
        (default-directory (emacs-agent-workspace-root workspace)))
    (unwind-protect
        (let ((status
               (apply #'process-file
                      (executable-find "rg") nil buffer nil
                      (emacs-agent-search--rg-arguments
                       query regexp include-globs exclude-globs))))
          (if (memq status '(0 1))
              (seq-filter
               (lambda (item)
                 (condition-case nil
                     (progn
                       (emacs-agent-policy-assert-document
                        workspace (plist-get item :path))
                       t)
                   (emacs-agent-error nil)))
               (emacs-agent-search--parse-rg-buffer buffer))
            (signal 'emacs-agent-search-error
                    (list (format "ripgrep exited with status %s" status)))))
      (kill-buffer buffer))))

(defun emacs-agent-search--finish-async
    (process workspace fingerprint limit callback)
  "Complete PROCESS for WORKSPACE and invoke CALLBACK.
FINGERPRINT binds pagination and LIMIT bounds the returned page."
  (let ((buffer (process-buffer process))
        (status (process-exit-status process)))
    (remhash process emacs-agent-search-processes)
    (unwind-protect
        (if (memq status '(0 1))
            (funcall
             callback
             (emacs-agent-search--page
              workspace 'search fingerprint
              (seq-filter
               (lambda (item)
                 (condition-case nil
                     (progn
                       (emacs-agent-policy-assert-document
                        workspace (plist-get item :path))
                       t)
                   (emacs-agent-error nil)))
               (emacs-agent-search--parse-rg-buffer buffer))
              0 limit)
             nil)
          (funcall callback nil
                   (list 'emacs-agent-search-error
                         (format "ripgrep exited with status %s" status))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun emacs-agent-search--start-rg
    (workspace query regexp include-globs exclude-globs fingerprint
               limit callback)
  "Search WORKSPACE asynchronously for QUERY and return the process.
REGEXP, INCLUDE-GLOBS, and EXCLUDE-GLOBS control matching.  FINGERPRINT binds
pagination, LIMIT bounds the page, and CALLBACK receives completion."
  (let* ((default-directory (emacs-agent-workspace-root workspace))
         (buffer (generate-new-buffer " *emacs-agent-rg*"))
         (process
          (make-process
           :name "emacs-agent-rg"
           :buffer buffer
           :command
           (cons (executable-find "rg")
                 (emacs-agent-search--rg-arguments
                  query regexp include-globs exclude-globs))
           :connection-type 'pipe :noquery t
           :sentinel
           (lambda (proc _event)
             (when (and (gethash proc emacs-agent-search-processes)
                        (memq (process-status proc) '(exit signal)))
               (emacs-agent-search--finish-async
                proc workspace fingerprint limit callback))))))
    (puthash process t emacs-agent-search-processes)
    process))

(cl-defun emacs-agent-workspace-search
    (workspace query &key regexp include-globs exclude-globs max-results
               cursor callback)
  "Search WORKSPACE for QUERY.

With CALLBACK and ripgrep available, return a process and invoke CALLBACK with
RESULT and ERROR-DATA.  Otherwise return a result page synchronously.  REGEXP,
INCLUDE-GLOBS, and EXCLUDE-GLOBS control matching; MAX-RESULTS bounds the page
and CURSOR resumes a prior request."
  (unless (and (stringp query) (not (string-empty-p query)))
    (signal 'wrong-type-argument (list 'non-empty-string query)))
  (let* ((limit (emacs-agent-search--limit max-results))
         (fingerprint
          (secure-hash
           'sha256
           (prin1-to-string
            (list query (and regexp t) include-globs exclude-globs)))))
    (cond
     (cursor
      (let ((saved (emacs-agent-search--resume-cursor
                    workspace cursor 'search fingerprint)))
        (emacs-agent-search--page
         workspace 'search fingerprint
         (emacs-agent-search-cursor-items saved)
         (emacs-agent-search-cursor-position saved)
         limit cursor)))
     ((and callback (executable-find "rg"))
      (emacs-agent-search--start-rg
       workspace query regexp include-globs exclude-globs
       fingerprint limit callback))
     (t
      (let ((items
             (if (executable-find "rg")
                 (emacs-agent-search--run-rg
                  workspace query regexp include-globs exclude-globs)
               (emacs-agent-search--fallback
                workspace query regexp include-globs exclude-globs))))
        (emacs-agent-search--page
         workspace 'search fingerprint items 0 limit))))))

(defun emacs-agent-search-cancel (process)
  "Cancel a pending search PROCESS."
  (when (gethash process emacs-agent-search-processes)
    (remhash process emacs-agent-search-processes)
    (when (process-live-p process)
      (delete-process process))
    (when (buffer-live-p (process-buffer process))
      (kill-buffer (process-buffer process)))
    t))

(provide 'emacs-agent-search)
;;; emacs-agent-search.el ends here
