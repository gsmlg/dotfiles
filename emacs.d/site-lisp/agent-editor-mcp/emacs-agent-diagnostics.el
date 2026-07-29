;;; emacs-agent-diagnostics.el --- Safe buffer diagnostics -*- lexical-binding: t; -*-

;;; Commentary:

;; Revision-bound parser and editor diagnostics for authoritative buffers.
;; This module never saves a buffer or starts an external diagnostic process.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'emacs-agent-document)
(require 'emacs-agent-policy)
(require 'emacs-agent-workspace)

(declare-function flymake-diagnostic-backend "flymake" (diagnostic))
(declare-function flymake-diagnostic-beg "flymake" (diagnostic))
(declare-function flymake-diagnostic-end "flymake" (diagnostic))
(declare-function flymake-diagnostic-text "flymake" (diagnostic))
(declare-function flymake-diagnostic-type "flymake" (diagnostic))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-is-running "flymake" ())
(declare-function treesit-language-available-p "treesit" (language &optional quiet))
(declare-function treesit-node-child "treesit" (node n &optional named))
(declare-function treesit-node-child-count "treesit" (node &optional named))
(declare-function treesit-node-check "treesit" (node property))
(declare-function treesit-node-end "treesit" (node))
(declare-function treesit-node-start "treesit" (node))
(declare-function treesit-node-type "treesit" (node))
(declare-function treesit-parser-create "treesit" (language &optional buffer no-reuse))
(declare-function treesit-parser-root-node "treesit" (parser))

(defvar flymake-mode)

(defcustom emacs-agent-diagnostics-default-wait-ms 1000
  "Default maximum wait for an already-running diagnostic provider."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-diagnostics-maximum-wait-ms 5000
  "Hard maximum wait for an already-running diagnostic provider."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-diagnostics-default-limit 50
  "Default workspace diagnostics document count per page."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-diagnostics-maximum-limit 200
  "Hard workspace diagnostics document count per page."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-diagnostics-cursor-lifetime 300
  "Seconds for which a workspace diagnostics cursor remains valid."
  :type 'integer
  :group 'emacs-agent-editor)

(defvar emacs-agent-diagnostics-cursors (make-hash-table :test #'equal)
  "Opaque cursor state for paged workspace diagnostics.")

(defun emacs-agent-diagnostics--position ()
  "Return the current buffer position in the public position format."
  (list :line (line-number-at-pos nil t)
        :column (- (point) (line-beginning-position))))

(defun emacs-agent-diagnostics--point-range ()
  "Return a one-character public range around point."
  (let ((start (emacs-agent-diagnostics--position)))
    (save-excursion
      (unless (eobp)
        (forward-char 1))
      (list :start start
            :end (emacs-agent-diagnostics--position)))))

(defun emacs-agent-diagnostics--range (beg end)
  "Return a public range spanning buffer positions BEG through END."
  (save-excursion
    (goto-char beg)
    (let ((start (emacs-agent-diagnostics--position)))
      (goto-char end)
      (list :start start :end (emacs-agent-diagnostics--position)))))

(defun emacs-agent-diagnostics--make
    (source severity message &optional code range related-information action-id)
  "Construct a diagnostic from SOURCE, SEVERITY, MESSAGE, and optional fields.
CODE, RANGE, RELATED-INFORMATION, and ACTION-ID are included when non-nil."
  (list :source source
        :severity severity
        :code code
        :message message
        :range range
        :related_information related-information
        :action_id action-id))

(defun emacs-agent-diagnostics--elisp-parser ()
  "Return safe parser diagnostics for the current Emacs Lisp buffer."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (condition-case error-data
          (let ((inhibit-message t))
            (check-parens)
            nil)
        (error
         (list
          (emacs-agent-diagnostics--make
           "parser" "error" (error-message-string error-data)
           "unbalanced_expression"
           (emacs-agent-diagnostics--point-range))))))))

(defun emacs-agent-diagnostics--json-parser ()
  "Return safe parser diagnostics for the current JSON buffer."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (condition-case error-data
          (progn
            (json-parse-buffer
             :object-type 'alist :array-type 'list
             :null-object :null :false-object :false)
            nil)
        (json-parse-error
         (list
          (emacs-agent-diagnostics--make
           "parser" "error" (error-message-string error-data)
           "invalid_json" (emacs-agent-diagnostics--point-range))))))))

(defun emacs-agent-diagnostics--treesit-errors (node)
  "Return parser diagnostics for error nodes below tree-sitter NODE."
  (let (diagnostics)
    (when (or (equal (treesit-node-type node) "ERROR")
              (treesit-node-check node 'missing))
      (push
       (emacs-agent-diagnostics--make
        "parser" "error"
        (format "Invalid syntax near %s" (treesit-node-type node))
        "invalid_syntax"
        (emacs-agent-diagnostics--range
         (treesit-node-start node) (treesit-node-end node)))
       diagnostics))
    (dotimes (index (treesit-node-child-count node))
      (setq diagnostics
            (nconc
             diagnostics
             (emacs-agent-diagnostics--treesit-errors
              (treesit-node-child node index)))))
    diagnostics))

(defun emacs-agent-diagnostics--treesit-parser (language)
  "Return safe parser diagnostics using tree-sitter LANGUAGE."
  (unless (and (fboundp 'treesit-language-available-p)
               (treesit-language-available-p language t))
    (emacs-agent-signal
     'capability_unavailable :source "parser"
     :language language :reason 'grammar_unavailable))
  (let ((parser (treesit-parser-create language (current-buffer))))
    (emacs-agent-diagnostics--treesit-errors
     (treesit-parser-root-node parser))))

(defun emacs-agent-diagnostics--assert-revision
    (document expected-revision current-revision)
  "Validate EXPECTED-REVISION for DOCUMENT against CURRENT-REVISION."
  (when (and expected-revision
             (not (equal expected-revision current-revision)))
    (emacs-agent-signal
     'revision_conflict
     :path (emacs-agent-document-relative-path document)
     :expected_revision expected-revision
     :current_revision current-revision
     :modified_by 'buffer
     :requires_reread t)))

(defun emacs-agent-diagnostics--wait-seconds (wait-ms)
  "Validate and bound WAIT-MS, returning seconds."
  (let ((wait-ms (or wait-ms emacs-agent-diagnostics-default-wait-ms)))
    (unless (and (integerp wait-ms) (>= wait-ms 0))
      (emacs-agent-signal 'invalid_position :field 'wait_ms
                          :value wait-ms))
    (/ (float (min wait-ms emacs-agent-diagnostics-maximum-wait-ms))
       1000.0)))

(defun emacs-agent-diagnostics--wait-while (predicate wait-seconds)
  "Wait at most WAIT-SECONDS while PREDICATE remains non-nil.
Return non-nil if the provider is still pending."
  (let ((deadline (+ (float-time) wait-seconds)))
    (while (and (funcall predicate)
                (< (float-time) deadline))
      (accept-process-output
       nil (min 0.05 (max 0.0 (- deadline (float-time))))))
    (and (funcall predicate) t)))

(defun emacs-agent-diagnostics--flymake-severity (type)
  "Map a Flymake diagnostic TYPE to a public severity."
  (pcase type
    (:error "error")
    (:warning "warning")
    (_ "info")))

(defun emacs-agent-diagnostics--flymake (wait-seconds)
  "Return Flymake diagnostics after a bounded WAIT-SECONDS.
The return value is a cons of diagnostics and the pending state."
  (unless (and (featurep 'flymake)
               (bound-and-true-p flymake-mode)
               (fboundp 'flymake-diagnostics)
               (fboundp 'flymake-is-running))
    (emacs-agent-signal 'capability_unavailable
                        :source "flymake" :reason 'not_enabled))
  (let ((pending
         (emacs-agent-diagnostics--wait-while
          #'flymake-is-running wait-seconds))
        diagnostics)
    (dolist (diagnostic (flymake-diagnostics (point-min) (point-max)))
      (let* ((beg (flymake-diagnostic-beg diagnostic))
             (end (flymake-diagnostic-end diagnostic))
             (backend (flymake-diagnostic-backend diagnostic)))
        (push
         (emacs-agent-diagnostics--make
          "flymake"
          (emacs-agent-diagnostics--flymake-severity
           (flymake-diagnostic-type diagnostic))
          (flymake-diagnostic-text diagnostic)
          (and backend (format "%s" backend))
          (emacs-agent-diagnostics--range beg end))
         diagnostics)))
    (cons (nreverse diagnostics) pending)))

;;;###autoload
(cl-defun emacs-agent-document-diagnostics
    (workspace path &key expected-revision sources wait-ms)
  "Return revision-bound diagnostics for PATH in WORKSPACE.
EXPECTED-REVISION, when non-nil, guards the read.  SOURCES defaults to the
safe parser.  WAIT-MS is accepted for provider parity and is bounded."
  (let* ((document (emacs-agent-document-open workspace path))
         (_ (emacs-agent-document-reconcile document))
         (revision (emacs-agent-document-revision document))
         (sources (or sources '("parser")))
         (wait-seconds (emacs-agent-diagnostics--wait-seconds wait-ms))
         diagnostics
         pending)
    (emacs-agent-diagnostics--assert-revision
     document expected-revision revision)
    (with-current-buffer (emacs-agent-document-buffer document)
      (dolist (source sources)
        (pcase source
          ("parser"
           (cond
            ((derived-mode-p 'emacs-lisp-mode)
             (setq diagnostics
                   (nconc diagnostics
                          (emacs-agent-diagnostics--elisp-parser))))
            ((let ((extension
                    (downcase
                     (or (file-name-extension
                          (emacs-agent-document-relative-path document))
                         ""))))
               (cond
                ((equal extension "json")
                 (setq diagnostics
                       (nconc diagnostics
                              (emacs-agent-diagnostics--json-parser)))
                 t)
                ((equal extension "py")
                 (setq diagnostics
                       (nconc diagnostics
                              (emacs-agent-diagnostics--treesit-parser
                               'python)))
                 t)
                ((member extension '("yaml" "yml"))
                 (setq diagnostics
                       (nconc diagnostics
                              (emacs-agent-diagnostics--treesit-parser
                               'yaml)))
                 t)))
             nil)
            (t
             (emacs-agent-signal
              'capability_unavailable :source source
              :path (emacs-agent-document-relative-path document)))))
          ("flymake"
           (pcase-let ((`(,found . ,still-running)
                        (emacs-agent-diagnostics--flymake wait-seconds)))
             (setq diagnostics (nconc diagnostics found)
                   pending (or pending still-running))))
          (_
           (emacs-agent-signal 'capability_unavailable :source source)))))
    (let ((current (emacs-agent-document-revision document)))
      (setf (emacs-agent-document-diagnostics-revision document) revision)
      (list :path (emacs-agent-document-relative-path document)
            :document_revision current
            :diagnostics_revision revision
            :providers (vconcat sources)
            :pending (and pending t)
            :stale (not (equal current revision))
            :diagnostics diagnostics))))

(defun emacs-agent-diagnostics--increment-summary (summary severity)
  "Increment SEVERITY in SUMMARY and return SUMMARY."
  (let ((key (intern (concat ":" severity))))
    (plist-put summary key (1+ (or (plist-get summary key) 0)))))

(defun emacs-agent-diagnostics--matches-globs-p (path globs)
  "Return non-nil when PATH matches GLOBS, or when GLOBS is nil."
  (or (null globs)
      (seq-some
       (lambda (glob)
         (string-match-p
          (concat "\\`" (wildcard-to-regexp glob) "\\'") path))
       globs)))

(defun emacs-agent-diagnostics--filter-paths (paths includes excludes)
  "Filter PATHS through INCLUDES and EXCLUDES globs."
  (seq-filter
   (lambda (path)
     (and (emacs-agent-diagnostics--matches-globs-p path includes)
          (not (and excludes
                    (emacs-agent-diagnostics--matches-globs-p
                     path excludes)))))
   paths))

(defun emacs-agent-diagnostics--workspace-paths (workspace)
  "Return safe parser-supported project paths in WORKSPACE."
  (let* ((root (emacs-agent-workspace-root workspace))
         (default-directory root)
         (project (or (emacs-agent-workspace-project workspace)
                      (project-current nil root)))
         (files
          (if project
              (project-files project)
            (directory-files-recursively root "." nil nil t)))
         paths)
    (dolist (file files)
      (let* ((absolute
              (if (file-name-absolute-p file)
                  file
                (expand-file-name file root)))
             (relative (file-relative-name absolute root))
             (extension (downcase (or (file-name-extension relative) ""))))
        (when (and (member extension '("el" "json" "py" "yaml" "yml"))
                   (condition-case nil
                       (progn
                         (emacs-agent-policy-assert-document
                          workspace relative)
                         t)
                     (emacs-agent-error nil)))
          (push relative paths))))
    (sort (delete-dups paths) #'string<)))

(defun emacs-agent-diagnostics--limit (limit)
  "Validate and bound workspace diagnostics LIMIT."
  (let ((limit (or limit emacs-agent-diagnostics-default-limit)))
    (unless (and (integerp limit) (> limit 0))
      (emacs-agent-signal 'invalid_position :field 'limit :value limit))
    (min limit emacs-agent-diagnostics-maximum-limit)))

(defun emacs-agent-diagnostics--cursor-page
    (workspace paths position limit fingerprint old-cursor)
  "Page PATHS at POSITION for WORKSPACE using LIMIT and FINGERPRINT.
OLD-CURSOR is consumed when non-nil.  Return (PAGE . NEXT-CURSOR)."
  (let* ((end (min (length paths) (+ position limit)))
         (page (cl-subseq paths position end))
         next)
    (when old-cursor
      (remhash old-cursor emacs-agent-diagnostics-cursors))
    (when (< end (length paths))
      (setq next (emacs-agent-workspace--random-id "diagnostics"))
      (puthash
       next
       (list
        :workspace-id (emacs-agent-workspace-workspace-id workspace)
        :paths paths :position end :fingerprint fingerprint
        :expires (+ (float-time) emacs-agent-diagnostics-cursor-lifetime))
       emacs-agent-diagnostics-cursors))
    (cons page next)))

(defun emacs-agent-diagnostics--resume-cursor
    (workspace cursor fingerprint)
  "Resolve CURSOR for WORKSPACE and FINGERPRINT."
  (let ((state (and (stringp cursor)
                    (gethash cursor emacs-agent-diagnostics-cursors))))
    (unless (and state
                 (> (plist-get state :expires) (float-time))
                 (equal (plist-get state :workspace-id)
                        (emacs-agent-workspace-workspace-id workspace))
                 (equal (plist-get state :fingerprint) fingerprint))
      (remhash cursor emacs-agent-diagnostics-cursors)
      (emacs-agent-signal 'revision_conflict
                          :reason 'invalid_cursor
                          :requires_reread t))
    state))

;;;###autoload
(cl-defun emacs-agent-workspace-diagnostics
    (workspace &key paths include-globs exclude-globs sources severities
               wait-ms limit cursor)
  "Aggregate diagnostics for selected PATHS in WORKSPACE.
SOURCES and WAIT-MS have the same meaning as for
`emacs-agent-document-diagnostics'.  SEVERITIES filters diagnostics by the
public strings \"error\", \"warning\", and \"info\".  INCLUDE-GLOBS and
EXCLUDE-GLOBS filter paths.  LIMIT and CURSOR paginate an immutable path
snapshot.  The total wait across all documents is bounded by WAIT-MS."
  (when (and paths
             (not (and (listp paths) (cl-every #'stringp paths))))
    (emacs-agent-signal 'invalid_position :field 'paths :value paths))
  (when (and severities
             (not
              (and (listp severities)
                   (cl-every
                    (lambda (severity)
                      (member severity '("error" "warning" "info")))
                    severities))))
    (emacs-agent-signal 'invalid_position
                        :field 'severities :value severities))
  (let* ((limit (emacs-agent-diagnostics--limit limit))
         (fingerprint
          (secure-hash
           'sha256
           (prin1-to-string
            (list paths include-globs exclude-globs sources severities))))
         (cursor-state
          (and cursor
               (emacs-agent-diagnostics--resume-cursor
                workspace cursor fingerprint)))
         (all-paths
          (if cursor-state
              (plist-get cursor-state :paths)
            (emacs-agent-diagnostics--filter-paths
             (or paths
                 (emacs-agent-diagnostics--workspace-paths workspace))
             include-globs exclude-globs)))
         (page
          (emacs-agent-diagnostics--cursor-page
           workspace all-paths
           (or (plist-get cursor-state :position) 0)
           limit fingerprint cursor))
         (paths (car page))
         (next-cursor (cdr page))
         (wait-seconds (emacs-agent-diagnostics--wait-seconds wait-ms))
         (deadline (+ (float-time) wait-seconds))
         (summary (list :error 0 :warning 0 :info 0))
         documents
         diagnostics
         pending
         stale)
    (dolist (path paths)
      (let* ((remaining-ms
              (max 0 (floor (* 1000.0 (- deadline (float-time))))))
             (result
              (emacs-agent-document-diagnostics
               workspace path
               :sources sources
               :wait-ms remaining-ms))
             (selected
              (seq-filter
               (lambda (diagnostic)
                 (or (null severities)
                     (member (plist-get diagnostic :severity)
                             severities)))
               (plist-get result :diagnostics))))
        (setq pending (or pending (plist-get result :pending))
              stale (or stale (plist-get result :stale)))
        (dolist (diagnostic selected)
          (setq summary
                (emacs-agent-diagnostics--increment-summary
                 summary (plist-get diagnostic :severity)))
          (push (append
                 (list :path (plist-get result :path)
                       :revision
                       (plist-get result :diagnostics_revision)
                       :stale (plist-get result :stale))
                 diagnostic)
                diagnostics))
        (push (plist-put (copy-sequence result)
                         :diagnostics selected)
              documents)))
    (list :document_count (length paths)
          :diagnostic_count (length diagnostics)
          :pending (and pending t)
          :stale (and stale t)
          :next_cursor next-cursor
          :summary summary
          :documents (nreverse documents)
          :diagnostics (nreverse diagnostics))))

(provide 'emacs-agent-diagnostics)
;;; emacs-agent-diagnostics.el ends here
