;;; gsmlg-ai-tools.el --- Request-scoped AI edit tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Tools bound to one edit session.  Mutations touch only proposed content.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'gsmlg-ai)
(require 'gsmlg-ai-context)
(require 'gsmlg-ai-session)

(declare-function gptel-make-tool "gptel-request" (&rest slots))

(defvar gsmlg-ai-tools--registry (make-hash-table :test #'equal)
  "Opaque token -> session registry for request-scoped tools.")

(defun gsmlg-ai-tools-register (session)
  "Register SESSION and return an opaque routing token."
  (let ((token (gsmlg-ai-context--new-id "tok")))
    (puthash token session gsmlg-ai-tools--registry)
    (setf (gsmlg-ai-session-tool-token session) token)
    token))

(defun gsmlg-ai-tools-unregister (session)
  "Remove SESSION from the tool registry."
  (when-let* ((token (gsmlg-ai-session-tool-token session)))
    (remhash token gsmlg-ai-tools--registry)
    (setf (gsmlg-ai-session-tool-token session) nil)))

(defun gsmlg-ai-tools--session (token)
  "Resolve SESSION from opaque TOKEN."
  (or (gethash token gsmlg-ai-tools--registry)
      (error "Unknown or expired AI tool session")))

(defun gsmlg-ai-tools--bump (session)
  "Increment SESSION tool-call count or signal budget exhaustion."
  (let ((count (1+ (gsmlg-ai-session-tool-call-count session))))
    (setf (gsmlg-ai-session-tool-call-count session) count)
    (when (> count gsmlg-ai-max-tool-calls)
      (error "Tool budget exceeded (%d)" gsmlg-ai-max-tool-calls))
    count))

(defun gsmlg-ai-tools--file (session file-id)
  "Return the session file for FILE-ID in SESSION."
  (or (cl-find file-id (gsmlg-ai-session-files session)
               :key #'gsmlg-ai-snapshot-id
               :test #'equal)
      (error "Unauthorized file id: %s" file-id)))

(defun gsmlg-ai-tools--json (value)
  "Serialize VALUE as a JSON string."
  (let ((json-encoding-pretty-print nil))
    (json-encode value)))

(defun gsmlg-ai-tools--line-count (content)
  "Return the number of lines in CONTENT."
  (if (string-empty-p content)
      0
    (1+ (cl-count ?\n content))))

(defun gsmlg-ai-tools--content-lines (content)
  "Split CONTENT into lines without dropping a trailing empty line oddly."
  (split-string content "\n"))

(defun gsmlg-ai-tools-list-context-files (token)
  "List authorized files for the session identified by TOKEN."
  (let ((session (gsmlg-ai-tools--session token)))
    (gsmlg-ai-tools--bump session)
    (gsmlg-ai-tools--json
     (mapcar
      (lambda (file)
        `((id . ,(gsmlg-ai-snapshot-id file))
          (path . ,(gsmlg-ai-snapshot-display-path file))
          (status . ,(symbol-name (gsmlg-ai-snapshot-operation file)))
          (revision . ,(gsmlg-ai-snapshot-proposal-revision file))
          (bytes . ,(string-bytes (gsmlg-ai-snapshot-proposed-content file)))
          (lines . ,(gsmlg-ai-tools--line-count
                     (gsmlg-ai-snapshot-proposed-content file)))
          (editable . ,(gsmlg-ai-snapshot-editable-p file))))
      (gsmlg-ai-session-files session)))))

(defun gsmlg-ai-tools-read-file (token file-id &optional start-line end-line)
  "Read proposed content for FILE-ID in TOKEN's session."
  (let* ((session (gsmlg-ai-tools--session token))
         (_ (gsmlg-ai-tools--bump session))
         (file (gsmlg-ai-tools--file session file-id))
         (content (gsmlg-ai-snapshot-proposed-content file))
         (lines (gsmlg-ai-tools--content-lines content))
         (total (length lines))
         (start (max 1 (or start-line 1)))
         (end (min total (or end-line total)))
         (slice
          (if (zerop total)
              ""
            (string-join (cl-subseq lines (1- start) end) "\n")))
         (bounded
          (if (<= (string-bytes slice) gsmlg-ai-max-read-bytes)
              slice
            (let ((cut slice)
                  (bytes 0)
                  (chars 0))
              (while (and (< chars (length cut))
                          (<= (+ bytes (string-bytes (substring cut chars (1+ chars))))
                              gsmlg-ai-max-read-bytes))
                (setq bytes (+ bytes (string-bytes (substring cut chars (1+ chars))))
                      chars (1+ chars)))
              (substring cut 0 chars)))))
    (gsmlg-ai-tools--json
     `((id . ,(gsmlg-ai-snapshot-id file))
       (path . ,(gsmlg-ai-snapshot-display-path file))
       (revision . ,(gsmlg-ai-snapshot-proposal-revision file))
       (start_line . ,start)
       (end_line . ,end)
       (total_lines . ,total)
       (total_bytes . ,(string-bytes content))
       (returned_bytes . ,(string-bytes bounded))
       (content . ,bounded)))))

(defun gsmlg-ai-tools-search-files (token query &optional regexp file-ids limit)
  "Search proposed content in TOKEN's session for QUERY.
When REGEXP is non-nil, treat QUERY as a regular expression.  Optional
FILE-IDS limits the search, and LIMIT caps the number of hits."
  (let* ((session (gsmlg-ai-tools--session token))
         (_ (gsmlg-ai-tools--bump session))
         (limit (min (or limit gsmlg-ai-max-search-results)
                     gsmlg-ai-max-search-results))
         (files
          (if file-ids
              (mapcar (lambda (id) (gsmlg-ai-tools--file session id)) file-ids)
            (gsmlg-ai-session-files session)))
         (pattern
          (condition-case err
              (if regexp query (regexp-quote query))
            (error (error "Invalid search pattern: %s"
                          (error-message-string err)))))
         (hits nil))
    (catch 'done
      (dolist (file files)
        (let ((line-no 0))
          (dolist (line (gsmlg-ai-tools--content-lines
                         (gsmlg-ai-snapshot-proposed-content file)))
            (setq line-no (1+ line-no))
            (when (string-match-p pattern line)
              (push
               `((id . ,(gsmlg-ai-snapshot-id file))
                 (path . ,(gsmlg-ai-snapshot-display-path file))
                 (revision . ,(gsmlg-ai-snapshot-proposal-revision file))
                 (line . ,line-no)
                 (excerpt . ,(truncate-string-to-width line 200)))
               hits)
              (when (>= (length hits) limit)
                (throw 'done t)))))))
    (gsmlg-ai-tools--json (nreverse hits))))

(defun gsmlg-ai-tools--require-editable (file)
  "Signal an error when FILE is not editable."
  (unless (gsmlg-ai-snapshot-editable-p file)
    (error "File is read-only in this session: %s"
           (gsmlg-ai-snapshot-display-path file))))

(defun gsmlg-ai-tools--require-revision (file expected)
  "Signal an error when FILE revision differs from EXPECTED."
  (unless (eql (gsmlg-ai-snapshot-proposal-revision file) expected)
    (error "Stale proposal revision for %s: got %s expected %s"
           (gsmlg-ai-snapshot-display-path file)
           (gsmlg-ai-snapshot-proposal-revision file)
           expected)))

(defun gsmlg-ai-tools-replace-text (token file-id revision old-text new-text
                                          &optional expected-count)
  "Replace OLD-TEXT with NEW-TEXT in FILE-ID for TOKEN's session."
  (let* ((session (gsmlg-ai-tools--session token))
         (_ (gsmlg-ai-tools--bump session))
         (file (gsmlg-ai-tools--file session file-id))
         (expected (or expected-count 1))
         (content (gsmlg-ai-snapshot-proposed-content file)))
    (gsmlg-ai-tools--require-editable file)
    (gsmlg-ai-tools--require-revision file revision)
    (let ((count 0)
          (start 0)
          positions)
      (while (and (< start (length content))
                  (string-match (regexp-quote old-text) content start))
        (push (match-beginning 0) positions)
        (setq count (1+ count)
              start (match-end 0)))
      (cond
       ((zerop count)
        (error "Replace_text found no matches"))
       ((/= count expected)
        (error "Replace_text expected %d matches, found %d" expected count))
       (t
        (let ((updated content))
          (dolist (pos (sort positions #'>))
            (setq updated
                  (concat (substring updated 0 pos)
                          new-text
                          (substring updated (+ pos (length old-text))))))
          (when (> (string-bytes updated) gsmlg-ai-max-file-bytes)
            (error "Replacement exceeds gsmlg-ai-max-file-bytes"))
          (setf (gsmlg-ai-snapshot-proposed-content file) updated
                (gsmlg-ai-snapshot-proposal-revision file) (1+ revision)
                (gsmlg-ai-snapshot-operation file)
                (if (eq (gsmlg-ai-snapshot-source-kind file) 'staged-new)
                    'create
                  'modify))
          (gsmlg-ai-tools--json
           `((id . ,file-id)
             (revision . ,(gsmlg-ai-snapshot-proposal-revision file))
             (replacements . ,count)
             (bytes . ,(string-bytes updated))))))))))

(defun gsmlg-ai-tools-set-file-content (token file-id revision content)
  "Replace whole proposed CONTENT for FILE-ID in TOKEN's session."
  (let* ((session (gsmlg-ai-tools--session token))
         (_ (gsmlg-ai-tools--bump session))
         (file (gsmlg-ai-tools--file session file-id)))
    (gsmlg-ai-tools--require-editable file)
    (gsmlg-ai-tools--require-revision file revision)
    (when (> (string-bytes content) gsmlg-ai-max-file-bytes)
      (error "Content exceeds gsmlg-ai-max-file-bytes"))
    (when (string-search "\0" content)
      (error "Binary content is not allowed"))
    (setf (gsmlg-ai-snapshot-proposed-content file) content
          (gsmlg-ai-snapshot-proposal-revision file) (1+ revision)
          (gsmlg-ai-snapshot-operation file)
          (if (eq (gsmlg-ai-snapshot-source-kind file) 'staged-new)
              'create
            'modify))
    (gsmlg-ai-tools--json
     `((id . ,file-id)
       (revision . ,(gsmlg-ai-snapshot-proposal-revision file))
       (bytes . ,(string-bytes content))))))

(defun gsmlg-ai-tools--validate-new-path (session relative)
  "Validate RELATIVE path under SESSION creation root and return absolute path."
  (when (or (not (stringp relative))
            (string-empty-p relative)
            (file-name-absolute-p relative)
            (string-match-p "\\(?:\\`\\|/\\)\\.\\.\\(?:/\\|\\'\\)" relative))
    (error "Invalid new-file path: %s" relative))
  (let* ((root (gsmlg-ai-session-creation-root session))
         (_ (when (file-remote-p root)
              (error "Remote create_file is out of scope for version 1")))
         (absolute (expand-file-name relative root))
         (true-root (file-name-as-directory (file-truename root)))
         (true-abs (file-truename absolute)))
    (unless (string-prefix-p true-root true-abs)
      (error "Path escapes creation root: %s" relative))
    (when (file-exists-p absolute)
      (error "Path already exists on disk: %s" absolute))
    (when (find-buffer-visiting absolute)
      (error "Path already visited: %s" absolute))
    (when (cl-find absolute (gsmlg-ai-session-files session)
                   :key #'gsmlg-ai-snapshot-canonical-file
                   :test #'equal)
      (error "Path already staged: %s" absolute))
    absolute))

(defun gsmlg-ai-tools-create-file (token relative content)
  "Stage a new file at RELATIVE under TOKEN's creation root."
  (let* ((session (gsmlg-ai-tools--session token))
         (_ (gsmlg-ai-tools--bump session))
         (absolute (gsmlg-ai-tools--validate-new-path session relative)))
    (when (> (string-bytes content) gsmlg-ai-max-file-bytes)
      (error "New file exceeds gsmlg-ai-max-file-bytes"))
    (when (string-search "\0" content)
      (error "Binary content is not allowed"))
    (let ((file
           (gsmlg-ai-snapshot--create
            :id (gsmlg-ai-context--new-id "new")
            :display-path absolute
            :canonical-file absolute
            :source-kind 'staged-new
            :source-buffer nil
            :source-buffer-tick nil
            :source-content-hash (gsmlg-ai-context--hash "")
            :source-file-attributes nil
            :original-content ""
            :proposed-content content
            :proposal-revision 1
            :operation 'create
            :editable-p t
            :remote-p nil
            :apply-status 'pending
            :conflict-reason nil)))
      (setf (gsmlg-ai-session-files session)
            (append (gsmlg-ai-session-files session) (list file)))
      (gsmlg-ai-tools--json
       `((id . ,(gsmlg-ai-snapshot-id file))
         (path . ,absolute)
         (revision . 1)
         (bytes . ,(string-bytes content)))))))

(defun gsmlg-ai-tools-finish-proposal (token summary &optional warnings)
  "Mark TOKEN's session ready for review with SUMMARY.
Optional WARNINGS is a list of unresolved concerns."
  (let ((session (gsmlg-ai-tools--session token)))
    (gsmlg-ai-tools--bump session)
    (setf (gsmlg-ai-session-model-summary session) summary
          (gsmlg-ai-session-warnings session)
          (append (gsmlg-ai-session-warnings session)
                  (if (listp warnings)
                      warnings
                    (and warnings (list warnings))))
          (gsmlg-ai-session-state session) 'ready)
    (gsmlg-ai-tools--json
     `((status . "ready")
       (summary . ,summary)))))

(defun gsmlg-ai-tools-make-gptel-tools (session)
  "Build request-scoped gptel tools closed over SESSION."
  (gsmlg-ai--ensure-gptel)
  (unless (fboundp #'gptel-make-tool)
    (user-error "Gptel tool support is unavailable"))
  (let ((token (or (gsmlg-ai-session-tool-token session)
                   (gsmlg-ai-tools-register session))))
    (list
     (gptel-make-tool
      :name "list_context_files"
      :function (lambda () (gsmlg-ai-tools-list-context-files token))
      :description "List authorized context files for this edit session."
      :args nil
      :category "gsmlg-ai")
     (gptel-make-tool
      :name "read_file"
      :function (lambda (file_id &optional start_line end_line)
                  (gsmlg-ai-tools-read-file
                   token file_id start_line end_line))
      :description "Read current proposed content for an authorized file id."
      :args '((:name "file_id" :type string :description "Opaque file id")
              (:name "start_line" :type integer :optional t
                     :description "1-based start line")
              (:name "end_line" :type integer :optional t
                     :description "1-based end line"))
      :category "gsmlg-ai")
     (gptel-make-tool
      :name "search_files"
      :function (lambda (query &optional regexp file_ids limit)
                  (gsmlg-ai-tools-search-files
                   token query regexp file_ids limit))
      :description "Search proposed content within authorized files only."
      :args '((:name "query" :type string :description "Search query")
              (:name "regexp" :type boolean :optional t
                     :description "Treat query as regexp")
              (:name "file_ids" :type array :optional t
                     :items (:type string)
                     :description "Optional subset of file ids")
              (:name "limit" :type integer :optional t
                     :description "Result limit"))
      :category "gsmlg-ai")
     (gptel-make-tool
      :name "replace_text"
      :function (lambda (file_id revision old_text new_text
                               &optional expected_count)
                  (gsmlg-ai-tools-replace-text
                   token file_id revision old_text new_text expected_count))
      :description "Exact text replacement in proposed content."
      :args '((:name "file_id" :type string :description "Opaque file id")
              (:name "revision" :type integer :description "Expected revision")
              (:name "old_text" :type string :description "Exact text to replace")
              (:name "new_text" :type string :description "Replacement text")
              (:name "expected_count" :type integer :optional t
                     :description "Expected match count, default 1"))
      :category "gsmlg-ai")
     (gptel-make-tool
      :name "set_file_content"
      :function (lambda (file_id revision content)
                  (gsmlg-ai-tools-set-file-content
                   token file_id revision content))
      :description "Replace entire proposed file content."
      :args '((:name "file_id" :type string :description "Opaque file id")
              (:name "revision" :type integer :description "Expected revision")
              (:name "content" :type string :description "New full content"))
      :category "gsmlg-ai")
     (gptel-make-tool
      :name "create_file"
      :function (lambda (path content)
                  (gsmlg-ai-tools-create-file token path content))
      :description "Stage a new file under the session creation root."
      :args '((:name "path" :type string
                     :description "Relative path under creation root")
              (:name "content" :type string :description "Initial content"))
      :category "gsmlg-ai")
     (gptel-make-tool
      :name "finish_proposal"
      :function (lambda (summary &optional warnings)
                  (gsmlg-ai-tools-finish-proposal token summary warnings))
      :description "Mark the proposal ready for user review."
      :args '((:name "summary" :type string :description "Change summary")
              (:name "warnings" :type array :optional t
                     :items (:type string)
                     :description "Optional warnings"))
      :category "gsmlg-ai"))))

(provide 'gsmlg-ai-tools)
;;; gsmlg-ai-tools.el ends here
