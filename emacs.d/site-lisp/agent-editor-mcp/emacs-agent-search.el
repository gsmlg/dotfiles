;;; emacs-agent-search.el --- Explicit project discovery and search -*- lexical-binding: t; -*-

;;; Commentary:

;; Bounded project file discovery and ripgrep-backed text search.  Every
;; operation names a registered project; opaque cursors retain an immutable
;; result snapshot for a short period.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'emacs-agent-document)
(require 'emacs-agent-policy)
(require 'emacs-agent-project)
(require 'emacs-agent-runtime)

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
  id runtime-id project-id kind fingerprint items position expires-at)

(defvar emacs-agent-search-cursors (make-hash-table :test #'equal)
  "Opaque project search cursor registry.")

(defvar emacs-agent-search-processes (make-hash-table :test #'eq)
  "Map active asynchronous ripgrep processes to their editor runtimes.")

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
    (runtime project kind fingerprint items position)
  "Store a RUNTIME cursor for PROJECT and KIND over ITEMS at POSITION.
FINGERPRINT binds the cursor to the originating request."
  (let* ((id (emacs-agent-runtime--random-id "cursor"))
         (cursor
          (emacs-agent-search-cursor--make
           :id id
           :runtime-id (emacs-agent-runtime-instance-id runtime)
           :project-id (emacs-agent-project-project-id project)
           :kind kind
           :fingerprint fingerprint
           :items items
           :position position
           :expires-at (+ (float-time)
                          emacs-agent-search-cursor-lifetime))))
    (puthash id cursor emacs-agent-search-cursors)
    id))

(defun emacs-agent-search--resume-cursor
    (runtime project id kind fingerprint)
  "Resolve cursor ID for RUNTIME, PROJECT, KIND, and FINGERPRINT."
  (let ((cursor (gethash id emacs-agent-search-cursors)))
    (unless
        (and cursor
             (> (emacs-agent-search-cursor-expires-at cursor)
                (float-time))
             (equal
              (emacs-agent-search-cursor-runtime-id cursor)
              (emacs-agent-runtime-instance-id runtime))
             (equal
              (emacs-agent-search-cursor-project-id cursor)
              (emacs-agent-project-project-id project))
             (eq kind (emacs-agent-search-cursor-kind cursor))
             (equal fingerprint
                    (emacs-agent-search-cursor-fingerprint cursor)))
      (remhash id emacs-agent-search-cursors)
      (signal 'emacs-agent-invalid-cursor
              (list "Cursor is expired or does not match the request")))
    cursor))

(defun emacs-agent-search--page
    (runtime project kind fingerprint items start limit &optional old-cursor)
  "Page ITEMS from START to LIMIT for RUNTIME, PROJECT, and KIND.
FINGERPRINT binds a new cursor; OLD-CURSOR is consumed when supplied."
  (let* ((end (min (length items) (+ start limit)))
         (page (cl-subseq items start end))
         (next
          (when (< end (length items))
            (emacs-agent-search--new-cursor
             runtime project kind fingerprint items end))))
    (when old-cursor
      (remhash old-cursor emacs-agent-search-cursors))
    (list :results page
          :next_cursor next
          :result_count (length page))))

(defun emacs-agent-search--project-file-names (project)
  "Return file names discovered from PROJECT's object and root."
  (let ((root (emacs-agent-project-canonical-root project)))
    (if (emacs-agent-project-native-p project)
        (condition-case nil
            (project-files
             (emacs-agent-project-project-object project))
          (error
           (directory-files-recursively root "." nil nil nil)))
      (directory-files-recursively root "." nil nil nil))))

(defun emacs-agent-search--project-files (runtime project)
  "Return authorized path metadata for files in PROJECT under RUNTIME."
  (let ((root (emacs-agent-project-canonical-root project))
        (project-id (emacs-agent-project-project-id project))
        (seen (make-hash-table :test #'equal))
        results)
    (dolist (file (emacs-agent-search--project-file-names project))
      (condition-case nil
          (let* ((absolute
                  (if (file-name-absolute-p file)
                      file
                    (expand-file-name file root)))
                 (target
                  (emacs-agent-project-resolve-target
                   runtime absolute :project-id project-id))
                 (canonical
                  (emacs-agent-resolved-target-canonical-path target)))
            (when (file-regular-p canonical)
              (emacs-agent-policy-assert-document-target runtime target)
              (unless (gethash canonical seen)
                (puthash canonical t seen)
                (push (emacs-agent-policy-target-fields target)
                      results))))
        (emacs-agent-error nil)
        (file-error nil)))
    (sort
     results
     (lambda (left right)
       (string<
        (plist-get left :relative_path)
        (plist-get right :relative_path))))))

;;;###autoload
(cl-defun emacs-agent-project-files
    (runtime project-id
             &key include-globs exclude-globs max-results cursor)
  "List files for PROJECT-ID registered in RUNTIME.
INCLUDE-GLOBS and EXCLUDE-GLOBS filter project-relative paths.  MAX-RESULTS
bounds the page, and CURSOR resumes a matching prior request."
  (let* ((project (emacs-agent-project-get runtime project-id))
         (limit (emacs-agent-search--limit max-results))
         (fingerprint
          (secure-hash
           'sha256
           (prin1-to-string
            (list include-globs exclude-globs)))))
    (if cursor
        (let ((saved
               (emacs-agent-search--resume-cursor
                runtime project cursor 'files fingerprint)))
          (emacs-agent-search--page
           runtime project 'files fingerprint
           (emacs-agent-search-cursor-items saved)
           (emacs-agent-search-cursor-position saved)
           limit cursor))
      (let ((files
             (seq-filter
              (lambda (item)
                (emacs-agent-search--included-p
                 (plist-get item :relative_path)
                 include-globs exclude-globs))
              (emacs-agent-search--project-files runtime project))))
        (emacs-agent-search--page
         runtime project 'files fingerprint files 0 limit)))))

(defun emacs-agent-search--rg-arguments
    (query regexp include-globs exclude-globs)
  "Build ripgrep arguments for QUERY.
REGEXP selects regular-expression matching.  INCLUDE-GLOBS and EXCLUDE-GLOBS
constrain project-relative paths."
  (append
   (list "--json" "--line-number" "--column" "--color=never"
         "--no-heading" "--hidden"
         "--glob=!.git/**" "--glob=!.env" "--glob=!.env.*")
   (cl-mapcan
    (lambda (basename)
      (list "--glob" (concat "!" basename)
            "--glob" (concat "!**/" basename)))
    emacs-agent-policy-denied-basenames)
   (cl-mapcan
    (lambda (extension)
      (list "--glob" (concat "!*." extension)
            "--glob" (concat "!**/*." extension)))
    emacs-agent-policy-denied-extensions)
   (unless regexp (list "--fixed-strings"))
   (cl-mapcan (lambda (glob) (list "--glob" glob)) include-globs)
   (cl-mapcan
    (lambda (glob) (list "--glob" (concat "!" glob)))
    exclude-globs)
   (list "--" query ".")))

(defun emacs-agent-search--json-text (object)
  "Extract a ripgrep JSON text field from OBJECT."
  (or (alist-get 'text object)
      (let ((bytes (alist-get 'bytes object)))
        (when bytes
          (decode-coding-string
           (base64-decode-string bytes) 'utf-8)))))

(defun emacs-agent-search--byte-column (line byte-offset)
  "Convert zero-based BYTE-OFFSET in UTF-8 LINE to a character column."
  (length
   (decode-coding-string
    (substring
     (encode-coding-string line 'utf-8)
     0 byte-offset)
    'utf-8)))

(defun emacs-agent-search--parse-rg-buffer (buffer)
  "Parse ripgrep JSON records in BUFFER into relative search results."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (results)
      (while (and (< (point) (point-max))
                  (< (length results)
                     emacs-agent-search-hard-limit))
        (let ((line
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (unless (string-empty-p line)
            (condition-case nil
                (let* ((record
                        (json-parse-string
                         line
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil))
                       (data (alist-get 'data record)))
                  (when (equal (alist-get 'type record) "match")
                    (let* ((path
                            (emacs-agent-search--json-text
                             (alist-get 'path data)))
                           (preview
                            (string-trim-right
                             (or
                              (emacs-agent-search--json-text
                               (alist-get 'lines data))
                              "")
                             "[\r\n]+"))
                           (submatch
                            (car (alist-get 'submatches data)))
                           (byte-column
                            (or (alist-get 'start submatch) 0)))
                      (push
                       (list
                        :path (string-remove-prefix "./" path)
                        :line (alist-get 'line_number data)
                        :column
                        (emacs-agent-search--byte-column
                         preview byte-column)
                        :match
                        (when submatch
                          (let ((start
                                 (emacs-agent-search--byte-column
                                  preview
                                  (alist-get 'start submatch)))
                                (end
                                 (emacs-agent-search--byte-column
                                  preview
                                  (alist-get 'end submatch))))
                            (substring preview start end)))
                        :context preview
                        :preview preview
                        :source "disk"
                        :modified :false)
                       results))))
              (error nil))))
        (forward-line 1))
      (nreverse results))))

(defun emacs-agent-search--add-project-path
    (runtime project item)
  "Authorize ITEM's path for PROJECT in RUNTIME and add path metadata."
  (let* ((target
          (emacs-agent-project-resolve-target
           runtime
           (plist-get item :path)
           :project-id
           (emacs-agent-project-project-id project)))
         (result (copy-sequence item)))
    (emacs-agent-policy-assert-document-target runtime target)
    (setq result
          (plist-put
           result :path
           (emacs-agent-resolved-target-canonical-path target)))
    (setq result
          (plist-put
           result :project_id
           (emacs-agent-resolved-target-project-id target)))
    (plist-put
     result :relative_path
     (emacs-agent-resolved-target-relative-path target))))

(defun emacs-agent-search--authorize-results
    (runtime project results)
  "Authorize RESULTS for PROJECT in RUNTIME and add path metadata."
  (delq
   nil
   (mapcar
    (lambda (item)
      (condition-case nil
          (emacs-agent-search--add-project-path
           runtime project item)
        (emacs-agent-error nil)
        (file-error nil)))
    results)))

(defun emacs-agent-search--result-less-p (left right)
  "Return non-nil if search result LEFT should sort before RIGHT."
  (let ((left-path (plist-get left :path))
        (right-path (plist-get right :path)))
    (if (equal left-path right-path)
        (let ((left-line (plist-get left :line))
              (right-line (plist-get right :line)))
          (if (= left-line right-line)
              (< (plist-get left :column)
                 (plist-get right :column))
            (< left-line right-line)))
      (string< left-path right-path))))

(defun emacs-agent-search--fallback
    (runtime project query regexp include-globs exclude-globs)
  "Search PROJECT in RUNTIME for QUERY without ripgrep.
REGEXP selects regular-expression matching.  INCLUDE-GLOBS and EXCLUDE-GLOBS
constrain project-relative paths."
  (let ((matcher (if regexp query (regexp-quote query)))
        results)
    (dolist (file (emacs-agent-search--project-files runtime project))
      (let ((relative (plist-get file :relative_path))
            (absolute (plist-get file :path)))
        (when
            (and
             (< (length results)
                emacs-agent-search-hard-limit)
             (emacs-agent-search--included-p
              relative include-globs exclude-globs))
          (with-temp-buffer
            (condition-case nil
                (progn
                  (insert-file-contents absolute)
                  (goto-char (point-min))
                  (while
                      (and
                       (< (length results)
                          emacs-agent-search-hard-limit)
                       (re-search-forward matcher nil t))
                    (let* ((match-start (match-beginning 0))
                           (line-start
                            (line-beginning-position))
                           (line-end
                            (line-end-position)))
                      (push
                       (append
                        file
                        (list
                         :line
                         (line-number-at-pos match-start)
                         :column (- match-start line-start)
                         :match
                         (match-string-no-properties 0)
                         :context
                         (buffer-substring-no-properties
                          line-start line-end)
                         :preview
                         (buffer-substring-no-properties
                          line-start line-end)
                         :source "disk"
                         :modified :false))
                       results))))
              (error nil))))))
    (nreverse results)))

(defun emacs-agent-search--dirty-buffer-target
    (runtime project buffer)
  "Return BUFFER's authorized target in PROJECT under RUNTIME, or nil."
  (when-let* ((file (buffer-file-name buffer)))
    (when
        (and
         (with-current-buffer buffer
           (buffer-modified-p))
         (condition-case nil
             (emacs-agent-policy--inside-root-p
              (file-truename file)
              (emacs-agent-project-canonical-root project))
           (file-error nil)))
      (condition-case nil
          (let ((target
                 (emacs-agent-project-resolve-target
                  runtime file
                  :project-id
                  (emacs-agent-project-project-id project))))
            (emacs-agent-policy-assert-document-target
             runtime target)
            target)
        (emacs-agent-error nil)
        (file-error nil)))))

(defun emacs-agent-search--dirty-buffer-matches
    (document target buffer matcher)
  "Return MATCHER results for dirty BUFFER represented by DOCUMENT and TARGET."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let (results)
          (while
              (and
               (< (length results)
                  emacs-agent-search-hard-limit)
               (condition-case error-data
                   (re-search-forward matcher nil t)
                 (invalid-regexp
                  (signal
                   'emacs-agent-search-error
                   (list
                    (error-message-string error-data))))))
            (let* ((start (match-beginning 0))
                   (matched (match-string-no-properties 0))
                   (line-start
                    (save-excursion
                      (goto-char start)
                      (line-beginning-position)))
                   (line-end
                    (save-excursion
                      (goto-char start)
                      (line-end-position))))
              (push
               (append
                (emacs-agent-policy-target-fields target)
                (list
                 :line (line-number-at-pos start)
                 :column (- start line-start)
                 :match matched
                 :context
                 (buffer-substring-no-properties
                  line-start line-end)
                 :preview
                 (buffer-substring-no-properties
                  line-start line-end)
                 :source "buffer"
                 :modified t
                 :revision
                 (emacs-agent-document-revision document)))
               results)))
          (nreverse results))))))

(defun emacs-agent-search--dirty-buffer-results
    (runtime project query regexp include-globs exclude-globs)
  "Search dirty PROJECT buffers in RUNTIME for QUERY.
REGEXP, INCLUDE-GLOBS, and EXCLUDE-GLOBS have their public meanings.  Return
the canonical dirty paths together with normalized search results."
  (let ((matcher (if regexp query (regexp-quote query)))
        paths
        results)
    (dolist (buffer (buffer-list))
      (when-let* ((target
                   (emacs-agent-search--dirty-buffer-target
                    runtime project buffer))
                  (relative
                   (emacs-agent-resolved-target-relative-path
                    target)))
        (when
            (emacs-agent-search--included-p
             relative include-globs exclude-globs)
          (let* ((document
                  (emacs-agent-document-open runtime target))
                 (canonical
                  (emacs-agent-resolved-target-canonical-path
                   target)))
            (push canonical paths)
            (setq
             results
             (nconc
              results
              (emacs-agent-search--dirty-buffer-matches
               document target buffer matcher)))))))
    (list :paths (delete-dups paths)
          :results results)))

(defun emacs-agent-search--merge-authoritative
    (disk-results dirty-paths buffer-results)
  "Merge DISK-RESULTS with authoritative BUFFER-RESULTS.
DIRTY-PATHS are removed from disk output before deterministic sorting."
  (sort
   (append
    (seq-remove
     (lambda (item)
       (member (plist-get item :path) dirty-paths))
     disk-results)
    buffer-results)
   #'emacs-agent-search--result-less-p))

(defun emacs-agent-search--run-rg
    (runtime project query regexp include-globs exclude-globs)
  "Search PROJECT synchronously in RUNTIME with ripgrep for QUERY.
REGEXP, INCLUDE-GLOBS, and EXCLUDE-GLOBS control matching."
  (let ((buffer (generate-new-buffer " *emacs-agent-rg*"))
        (default-directory
         (emacs-agent-project-canonical-root project)))
    (unwind-protect
        (let ((status
               (apply
                #'process-file
                (executable-find "rg")
                nil buffer nil
                (emacs-agent-search--rg-arguments
                 query regexp include-globs exclude-globs))))
          (if (memq status '(0 1))
              (emacs-agent-search--authorize-results
               runtime project
               (emacs-agent-search--parse-rg-buffer buffer))
            (signal
             'emacs-agent-search-error
             (list
              (format
               "ripgrep exited with status %s" status)))))
      (kill-buffer buffer))))

(defun emacs-agent-search--finish-async
    (process runtime project fingerprint limit callback
             dirty-paths buffer-results)
  "Complete PROCESS for RUNTIME and PROJECT, then invoke CALLBACK.
FINGERPRINT binds pagination and LIMIT bounds the returned page.
DIRTY-PATHS and BUFFER-RESULTS preserve unsaved-buffer authority."
  (let ((buffer (process-buffer process))
        (status (process-exit-status process)))
    (remhash process emacs-agent-search-processes)
    (unwind-protect
        (if (memq status '(0 1))
            (funcall
             callback
             (emacs-agent-search--page
              runtime project 'search fingerprint
              (emacs-agent-search--merge-authoritative
               (emacs-agent-search--authorize-results
                runtime project
                (emacs-agent-search--parse-rg-buffer buffer))
               dirty-paths buffer-results)
              0 limit)
             nil)
          (funcall
           callback nil
           (list
            'emacs-agent-search-error
            (format
             "ripgrep exited with status %s" status))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun emacs-agent-search--start-rg
    (runtime project query regexp include-globs exclude-globs
             fingerprint limit callback dirty-paths buffer-results)
  "Search PROJECT asynchronously in RUNTIME for QUERY and return its process.
REGEXP, INCLUDE-GLOBS, and EXCLUDE-GLOBS control matching.  FINGERPRINT binds
pagination, LIMIT bounds the page, and CALLBACK receives completion.
DIRTY-PATHS shadow disk hits with authoritative BUFFER-RESULTS."
  (let* ((default-directory
          (emacs-agent-project-canonical-root project))
         (buffer (generate-new-buffer " *emacs-agent-rg*"))
         (process
          (make-process
           :name "emacs-agent-rg"
           :buffer buffer
           :command
           (cons
            (executable-find "rg")
            (emacs-agent-search--rg-arguments
             query regexp include-globs exclude-globs))
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (proc _event)
             (when
                 (and
                  (gethash proc emacs-agent-search-processes)
                  (memq
                   (process-status proc)
                   '(exit signal)))
               (emacs-agent-search--finish-async
                proc runtime project fingerprint limit callback
                dirty-paths buffer-results))))))
    (puthash process runtime emacs-agent-search-processes)
    (when (memq (process-status process) '(exit signal))
      (emacs-agent-search--finish-async
       process runtime project fingerprint limit callback
       dirty-paths buffer-results))
    process))

;;;###autoload
(cl-defun emacs-agent-project-search
    (runtime project-id query
             &key regexp include-globs exclude-globs max-results
             cursor callback)
  "Search PROJECT-ID registered in RUNTIME for QUERY.

With CALLBACK and ripgrep available, return a process and invoke CALLBACK with
RESULT and ERROR-DATA.  Otherwise return a result page synchronously.  REGEXP,
INCLUDE-GLOBS, and EXCLUDE-GLOBS control matching; MAX-RESULTS bounds the page
and CURSOR resumes a prior request."
  (unless (and (stringp query)
               (not (string-empty-p query)))
    (signal
     'wrong-type-argument
     (list 'non-empty-string query)))
  (let* ((project
          (emacs-agent-project-get runtime project-id))
         (limit
          (emacs-agent-search--limit max-results))
         (fingerprint
          (secure-hash
           'sha256
           (prin1-to-string
            (list
             query
             (and regexp t)
             include-globs
             exclude-globs)))))
    (if cursor
        (let ((saved
               (emacs-agent-search--resume-cursor
                runtime project cursor 'search fingerprint)))
          (emacs-agent-search--page
           runtime project 'search fingerprint
           (emacs-agent-search-cursor-items saved)
           (emacs-agent-search-cursor-position saved)
           limit cursor))
      (let* ((dirty
              (emacs-agent-search--dirty-buffer-results
               runtime project query regexp
               include-globs exclude-globs))
             (dirty-paths (plist-get dirty :paths))
             (buffer-results (plist-get dirty :results)))
        (cond
         ((and callback (executable-find "rg"))
          (emacs-agent-search--start-rg
           runtime project query regexp
           include-globs exclude-globs
           fingerprint limit callback
           dirty-paths buffer-results))
         (t
          (let ((items
                 (emacs-agent-search--merge-authoritative
                  (if (executable-find "rg")
                      (emacs-agent-search--run-rg
                       runtime project query regexp
                       include-globs exclude-globs)
                    (emacs-agent-search--fallback
                     runtime project query regexp
                     include-globs exclude-globs))
                  dirty-paths buffer-results)))
            (emacs-agent-search--page
             runtime project 'search fingerprint
             items 0 limit))))))))

(defun emacs-agent-search-cancel (process)
  "Cancel a pending search PROCESS."
  (when (gethash process emacs-agent-search-processes)
    (remhash process emacs-agent-search-processes)
    (when (process-live-p process)
      (delete-process process))
    (when (buffer-live-p (process-buffer process))
      (kill-buffer (process-buffer process)))
    t))

;;;###autoload
(defun emacs-agent-search-clear (&optional runtime)
  "Cancel active asynchronous search jobs belonging to RUNTIME.
When RUNTIME is nil, cancel every active asynchronous search."
  (let (processes)
    (maphash
     (lambda (process owner)
       (when (or (null runtime)
                 (eq owner runtime))
         (push process processes)))
     emacs-agent-search-processes)
    (dolist (process processes)
      (emacs-agent-search-cancel process)))
  t)

(provide 'emacs-agent-search)
;;; emacs-agent-search.el ends here
