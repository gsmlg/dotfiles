;;; emacs-agent-semantic.el --- Native semantic editor services -*- lexical-binding: t; -*-

;;; Commentary:

;; Small adapters around Emacs' native imenu, xref, project, and buffer APIs.
;; This module does not implement language semantics itself.  When the active
;; major mode does not provide a native backend, callers receive a structured
;; `capability_unavailable' error.

;;; Code:

(require 'cl-lib)
(require 'imenu)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'eglot nil t)
(require 'emacs-agent-changeset)
(require 'emacs-agent-document)
(require 'emacs-agent-edit)
(require 'emacs-agent-policy)
(require 'emacs-agent-transaction)

(declare-function eglot--lsp-position-to-point "eglot")
(declare-function eglot--pos-to-lsp-position "eglot")
(declare-function eglot--request "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-path-to-uri "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot-uri-to-path "eglot")
(defvar eglot-move-to-linepos-function)

(defvar-local emacs-agent-semantic-sensitive-buffer nil
  "Non-nil means Agent Editor must not expose metadata about this buffer.")

(defcustom emacs-agent-semantic-format-function nil
  "Trusted function used to format document text, or nil.

The function receives CONTENT and MAJOR-MODE and must return formatted text.
It is configured by the Emacs user and is never selected or supplied by an
MCP client.  Agent Editor calls it without visiting another file and verifies
its returned value before previewing or applying it."
  :type '(choice (const :tag "Formatting unavailable" nil) function)
  :group 'emacs-agent-editor)

(defcustom emacs-agent-semantic-preview-ttl 300
  "Seconds a semantic mutation preview remains applicable."
  :type 'integer
  :group 'emacs-agent-editor)

(defvar emacs-agent-semantic--previews (make-hash-table :test #'equal)
  "Frozen semantic mutation plans indexed by opaque preview IDs.")

(defvar emacs-agent-semantic--actions (make-hash-table :test #'equal)
  "Code actions indexed by opaque action IDs.")

(defconst emacs-agent-semantic-supported-tools
  '("emacs_agent_document_symbols"
    "emacs_agent_workspace_symbols"
    "emacs_agent_symbol_definition"
    "emacs_agent_symbol_references"
    "emacs_agent_editor_context_get"
    "emacs_agent_format_document"
    "emacs_agent_symbol_rename"
    "emacs_agent_code_actions"
    "emacs_agent_format_range")
  "Semantic MCP tools implemented by this module, in stable display order.")

(defun emacs-agent-semantic--unavailable (capability &optional reason)
  "Signal that CAPABILITY is unavailable, optionally because of REASON."
  (emacs-agent-signal
   'capability_unavailable
   :capability capability
   :reason (or reason 'no_native_backend)))

(defun emacs-agent-semantic--json-boolean (value)
  "Return JSON-compatible true or false for VALUE."
  (if value t :false))

(defun emacs-agent-semantic--provider-name (provider fallback)
  "Return a deterministic public name for PROVIDER or FALLBACK."
  (cond
   ((symbolp provider) (symbol-name provider))
   (provider fallback)
   (t :false)))

(defun emacs-agent-semantic--imenu-provider ()
  "Return the configured imenu provider name, or nil."
  (cond
   ((and imenu-create-index-function
         (not (eq imenu-create-index-function
                  #'imenu-default-create-index-function)))
    (emacs-agent-semantic--provider-name
     imenu-create-index-function "configured"))
   (imenu-generic-expression "generic")
   (t nil)))

(defun emacs-agent-semantic--xref-provider ()
  "Return the active xref provider name, or nil."
  (let ((backend
         (condition-case nil
             (xref-find-backend)
           (error nil))))
    (and backend
         (emacs-agent-semantic--provider-name backend "configured"))))

(defun emacs-agent-semantic--eglot-runtime ()
  "Return deterministic Eglot provider capability metadata."
  (let ((server
         (and (featurep 'eglot)
              (condition-case nil
                  (eglot-current-server)
                (error nil)))))
    (cl-labels
        ((capable (capability)
           (and server
                (condition-case nil
                    (eglot-server-capable capability)
                  (error nil)))))
      `((available
         . ,(emacs-agent-semantic--json-boolean server))
        (rename
         . ,(emacs-agent-semantic--json-boolean
             (capable :renameProvider)))
        (code_actions
         . ,(emacs-agent-semantic--json-boolean
             (capable :codeActionProvider)))
        (format_document
         . ,(emacs-agent-semantic--json-boolean
             (capable :documentFormattingProvider)))
        (format_range
         . ,(emacs-agent-semantic--json-boolean
             (capable :documentRangeFormattingProvider)))))))

(defun emacs-agent-semantic--tool-availability
    (tool available provider)
  "Return public availability metadata for supported TOOL.
AVAILABLE is converted to a JSON boolean and PROVIDER names its adapter."
  `((tool . ,tool)
    (supported . t)
    (available . ,(emacs-agent-semantic--json-boolean available))
    (provider . ,(if available provider :false))))

;;;###autoload
(defun emacs-agent-semantic-runtime-capabilities (&optional buffer)
  "Report supported semantic tools and providers active for BUFFER.

BUFFER may be one buffer or a list of workspace buffers.  It defaults to the
current buffer; the sentinel `:none' explicitly probes no buffer.
`supported_tools' is the deterministic module surface and does not imply a
provider is currently active.
`providers' reports runtime imenu, xref, Eglot, trusted formatter, and editor
adapters.  `tool_availability' maps each supported tool to a provider active
in at least one supplied buffer.  This function sends no language-server
requests."
  (let ((buffers
         (cond
          ((eq buffer :none) nil)
          ((bufferp buffer) (list buffer))
          ((listp buffer) buffer)
          (t (list (current-buffer)))))
        imenu-provider xref-provider eglot-runtime)
    (dolist (candidate buffers)
      (when (buffer-live-p candidate)
        (with-current-buffer candidate
          (setq imenu-provider
                (or imenu-provider
                    (emacs-agent-semantic--imenu-provider))
                xref-provider
                (or xref-provider
                    (emacs-agent-semantic--xref-provider)))
          (let ((runtime (emacs-agent-semantic--eglot-runtime)))
            (dolist (key '(available rename code_actions
                                     format_document format_range))
              (when (eq (alist-get key runtime) t)
                (setf (alist-get key eglot-runtime) t)))))))
    (setq eglot-runtime
          (or eglot-runtime
              '((available . :false)
                (rename . :false)
                (code_actions . :false)
                (format_document . :false)
                (format_range . :false))))
    (let* ((trusted-formatter
            (functionp emacs-agent-semantic-format-function))
           (eglot-available
            (eq (alist-get 'available eglot-runtime) t))
           (rename-available
            (eq (alist-get 'rename eglot-runtime) t))
           (code-actions-available
            (eq (alist-get 'code_actions eglot-runtime) t))
           (format-range-available
            (eq (alist-get 'format_range eglot-runtime) t)))
      `((supported_tools
         . ,(copy-sequence emacs-agent-semantic-supported-tools))
        (providers
         . ((imenu
             . ((available
                 . ,(emacs-agent-semantic--json-boolean
                     imenu-provider))
                (provider . ,(or imenu-provider :false))))
            (xref
             . ((available
                 . ,(emacs-agent-semantic--json-boolean
                     xref-provider))
                (provider . ,(or xref-provider :false))))
            (eglot . ,eglot-runtime)
            (trusted_formatter
             . ((available
                 . ,(emacs-agent-semantic--json-boolean
                     trusted-formatter))
                (provider
                 . ,(if trusted-formatter
                        "trusted_formatter" :false))))
            (editor
             . ((available . t)
                (provider . "emacs")))))
        (tool_availability
         . ,(list
             (emacs-agent-semantic--tool-availability
              "emacs_agent_document_symbols"
              imenu-provider "imenu")
             (emacs-agent-semantic--tool-availability
              "emacs_agent_workspace_symbols"
              xref-provider "xref")
             (emacs-agent-semantic--tool-availability
              "emacs_agent_symbol_definition"
              xref-provider "xref")
             (emacs-agent-semantic--tool-availability
              "emacs_agent_symbol_references"
              xref-provider "xref")
             (emacs-agent-semantic--tool-availability
              "emacs_agent_editor_context_get" t "editor")
             (emacs-agent-semantic--tool-availability
              "emacs_agent_format_document"
              trusted-formatter "trusted_formatter")
             (emacs-agent-semantic--tool-availability
              "emacs_agent_symbol_rename"
              (and eglot-available rename-available) "eglot")
             (emacs-agent-semantic--tool-availability
              "emacs_agent_code_actions"
              (and eglot-available code-actions-available) "eglot")
             (emacs-agent-semantic--tool-availability
              "emacs_agent_format_range"
              (and eglot-available format-range-available) "eglot")))))))

(defun emacs-agent-semantic--field (object key)
  "Return KEY from protocol OBJECT."
  (emacs-agent-document--field object key))

(defun emacs-agent-semantic--id (prefix)
  "Return an opaque identifier beginning with PREFIX."
  (concat
   prefix
   (substring
    (secure-hash
     'sha256
     (format "%s:%s:%s" (current-time) (random) (make-temp-name prefix)))
    0 24)))

(defun emacs-agent-semantic--eglot-server (capability)
  "Return the Eglot server for CAPABILITY or fail closed."
  (unless (require 'eglot nil t)
    (emacs-agent-semantic--unavailable capability 'eglot_unavailable))
  (let ((server
         (condition-case nil
             (eglot-current-server)
           (error nil))))
    (or server
        (emacs-agent-semantic--unavailable capability 'no_eglot_server))))

(defun emacs-agent-semantic--eglot-request
    (server method params capability)
  "Send METHOD with PARAMS to SERVER for CAPABILITY."
  (condition-case error-data
      (eglot--request server method params)
    ((error quit)
     (emacs-agent-semantic--unavailable
      capability (error-message-string error-data)))))

(defun emacs-agent-semantic--assert-revision
    (workspace path expected-revision)
  "Return current document for PATH in WORKSPACE after revision validation.
EXPECTED-REVISION must identify its authoritative content."
  (let* ((document (emacs-agent-document-open workspace path))
         (_ (emacs-agent-document-reconcile document))
         (current (emacs-agent-document-revision document)))
    (unless (equal current expected-revision)
      (emacs-agent-signal
       'revision_conflict :path path
       :expected_revision expected-revision
       :current_revision current :requires_reread t))
    document))

(defun emacs-agent-semantic--uri-path (workspace uri)
  "Return workspace-relative path for LSP URI in WORKSPACE."
  (unless (stringp uri)
    (emacs-agent-semantic--unavailable
     'workspace_edit 'invalid_document_uri))
  (let* (uri-error
         (absolute
          (condition-case error-data
              (eglot-uri-to-path uri)
            (error
             (setq uri-error (error-message-string error-data))
             nil)))
         (root (emacs-agent-policy--root workspace)))
    (unless (and absolute (file-name-absolute-p absolute))
      (emacs-agent-semantic--unavailable
       'workspace_edit
       (list 'unsupported_document_uri :uri uri :error uri-error)))
    (let ((relative (file-relative-name (file-truename absolute) root)))
      (emacs-agent-policy-resolve workspace relative)
      relative)))

(defun emacs-agent-semantic--workspace-edit-entries (workspace workspace-edit)
  "Convert LSP WORKSPACE-EDIT into path/edit entries for WORKSPACE."
  (let ((document-changes
         (emacs-agent-semantic--field workspace-edit 'documentChanges))
        (changes (emacs-agent-semantic--field workspace-edit 'changes))
        entries)
    (if (and document-changes (> (length document-changes) 0))
        (dolist (change (append document-changes nil))
          (let* ((text-document
                  (emacs-agent-semantic--field change 'textDocument))
                 (uri (and text-document
                           (emacs-agent-semantic--field
                            text-document 'uri)))
                 (edits (emacs-agent-semantic--field change 'edits)))
            (unless (and text-document edits)
              (emacs-agent-semantic--unavailable
               'workspace_edit 'resource_operations_unsupported))
            (push
             (list :path
                   (emacs-agent-semantic--uri-path workspace uri)
                   :edits (append edits nil))
             entries)))
      (let ((pairs changes))
        (while pairs
          (let* ((uri-key (pop pairs))
                 (uri
                  (cond
                   ((keywordp uri-key)
                    (substring (symbol-name uri-key) 1))
                   ((stringp uri-key) uri-key)
                   (t nil)))
                 (edits (pop pairs)))
            (unless uri
              (emacs-agent-semantic--unavailable
               'workspace_edit 'invalid_changes_map))
            (push
             (list :path
                   (emacs-agent-semantic--uri-path workspace uri)
                   :edits (append edits nil))
             entries)))))
    (nreverse entries)))

(defun emacs-agent-semantic--text-edit-region (edit)
  "Return buffer start and end points for LSP text EDIT."
  (let* ((range (emacs-agent-semantic--field edit 'range))
         (start (and range
                     (emacs-agent-semantic--field range 'start)))
         (end (and range
                   (emacs-agent-semantic--field range 'end))))
    (unless (and start end)
      (emacs-agent-semantic--unavailable
       'workspace_edit 'insert_replace_edits_unsupported))
    (cons
     (eglot--lsp-position-to-point start)
     (eglot--lsp-position-to-point end))))

(defun emacs-agent-semantic--apply-lsp-text-edits
    (buffer edits linepos-function)
  "Return BUFFER content after LSP EDITS using LINEPOS-FUNCTION."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let ((eglot-move-to-linepos-function linepos-function)
            normalized
            previous-start)
        (dolist (edit edits)
          (let* ((region (emacs-agent-semantic--text-edit-region edit))
                 (new-text
                  (emacs-agent-semantic--field edit 'newText)))
            (unless (and (stringp new-text)
                         (<= (car region) (cdr region)))
              (emacs-agent-semantic--unavailable
               'workspace_edit 'invalid_text_edit))
            (push (list (car region) (cdr region) new-text) normalized)))
        (setq normalized
              (sort normalized
                    (lambda (left right)
                      (> (car left) (car right)))))
        (dolist (edit normalized)
          (when (and previous-start (> (nth 1 edit) previous-start))
            (emacs-agent-signal 'overlapping_edits))
          (setq previous-start (car edit)))
        (with-temp-buffer
          (insert-buffer-substring buffer)
          (dolist (edit normalized)
            (delete-region (nth 0 edit) (nth 1 edit))
            (goto-char (nth 0 edit))
            (insert (nth 2 edit)))
          (buffer-string))))))

(defun emacs-agent-semantic--workspace-edit-plan
    (workspace workspace-edit operation linepos-function)
  "Create an atomic transaction plan for LSP WORKSPACE-EDIT in WORKSPACE.
OPERATION names the recorded operation and LINEPOS-FUNCTION decodes LSP
character positions."
  (let (items)
    (dolist
        (entry
         (emacs-agent-semantic--workspace-edit-entries
          workspace workspace-edit))
      (let* ((path (plist-get entry :path))
             (document (emacs-agent-document-open workspace path))
             (_ (emacs-agent-document-reconcile document))
             (revision (emacs-agent-document-revision document))
             (buffer (emacs-agent-document-buffer document))
             (before
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (buffer-substring-no-properties
                   (point-min) (point-max)))))
             (after
              (emacs-agent-semantic--apply-lsp-text-edits
               buffer (plist-get entry :edits) linepos-function)))
        (push
         (emacs-agent-transaction-item--make
          :path (emacs-agent-document-relative-path document)
          :document document :expected-revision revision
          :before before :after after :operation operation
          :diff (emacs-agent-changeset--diff-text path before after))
         items)))
    (emacs-agent-transaction-plan--make
     :workspace workspace :items (nreverse items))))

(defun emacs-agent-semantic--cache-plan (workspace kind plan)
  "Cache PLAN for WORKSPACE under KIND and return its ID."
  (let ((id (emacs-agent-semantic--id "semantic:")))
    (puthash
     id
     (list :workspace (emacs-agent-policy--root workspace)
           :kind kind :plan plan
           :expires
           (+ (float-time) emacs-agent-semantic-preview-ttl))
     emacs-agent-semantic--previews)
    id))

(defun emacs-agent-semantic--take-plan (workspace preview-id kind)
  "Consume PREVIEW-ID for WORKSPACE and KIND."
  (let ((state
         (and (stringp preview-id)
              (gethash preview-id emacs-agent-semantic--previews))))
    (remhash preview-id emacs-agent-semantic--previews)
    (unless (and state
                 (equal (plist-get state :workspace)
                        (emacs-agent-policy--root workspace))
                 (eq (plist-get state :kind) kind)
                 (> (plist-get state :expires) (float-time)))
      (emacs-agent-signal
       'revision_conflict :reason 'invalid_preview
       :requires_reread t))
    (plist-get state :plan)))

(defun emacs-agent-semantic--plan-preview (preview-id operation plan)
  "Return public preview for PREVIEW-ID, OPERATION, and PLAN."
  (let ((preview (emacs-agent-transaction-apply plan t)))
    `((preview_id . ,preview-id)
      (operation . ,(symbol-name operation))
      (applied . :false)
      (modified . ,(if (plist-get preview :modified) t :false))
      (documents . ,(plist-get preview :documents)))))

(defun emacs-agent-semantic--public-position (position)
  "Return one-based line and zero-based column for buffer POSITION."
  (save-excursion
    (goto-char position)
    `((line . ,(line-number-at-pos nil t))
      (column . ,(- (point) (line-beginning-position))))))

(defun emacs-agent-semantic--range (start end)
  "Return a public range spanning buffer positions START through END."
  `((start . ,(emacs-agent-semantic--public-position start))
    (end . ,(emacs-agent-semantic--public-position end))))

(defun emacs-agent-semantic--imenu-position (value)
  "Return a buffer position represented by imenu VALUE, or nil."
  (cond
   ((markerp value) (and (marker-buffer value) (marker-position value)))
   ((overlayp value) (overlay-start value))
   ((integer-or-marker-p value) value)))

(defun emacs-agent-semantic--kind (name container position)
  "Infer a stable symbol kind from imenu NAME, CONTAINER, and POSITION."
  (let ((label (downcase (or container name ""))))
    (cond
     ((string-match-p "\\(?:function\\|method\\|procedure\\)" label)
      "function")
     ((string-match-p "\\(?:variable\\|constant\\|field\\)" label)
      "variable")
     ((string-match-p "\\(?:class\\|struct\\|interface\\)" label)
      "class")
     ((string-match-p "\\(?:module\\|namespace\\|package\\)" label)
      "module")
     ((and position
           (save-excursion
             (goto-char position)
             (looking-at-p
              "[[:space:]]*(\\(?:cl-\\)?def\\(?:un\\|macro\\|subst\\)\\_>")))
      "function")
     ((and position
           (save-excursion
             (goto-char position)
             (looking-at-p
              "[[:space:]]*(def\\(?:var\\|const\\|custom\\)\\_>")))
      "variable")
     (t "symbol"))))

(defun emacs-agent-semantic--selection-range (name position)
  "Return a best-effort selection range for NAME at POSITION."
  (save-excursion
    (goto-char position)
    (let ((line-end (line-end-position))
          (case-fold-search nil))
      (if (search-forward name line-end t)
          (emacs-agent-semantic--range (- (point) (length name)) (point))
        (emacs-agent-semantic--range position position)))))

(defun emacs-agent-semantic--imenu-items (items &optional container)
  "Convert imenu ITEMS into public symbol objects under CONTAINER."
  (delq
   nil
   (mapcar
    (lambda (item)
      (let ((name (car-safe item))
            (value (cdr-safe item)))
        (unless (or (not (stringp name))
                    (string-prefix-p "*" name))
          (let ((position (emacs-agent-semantic--imenu-position value)))
            (if position
                (save-excursion
                  (goto-char position)
                  (let ((line-start (line-beginning-position))
                        (line-end (line-end-position)))
                    `((name . ,name)
                      (kind . ,(emacs-agent-semantic--kind
                                name container position))
                      (range . ,(emacs-agent-semantic--range
                                 line-start line-end))
                      (selection_range
                       . ,(emacs-agent-semantic--selection-range
                           name position))
                      (container . ,container)
                      (source . "imenu"))))
              (when (listp value)
                (let ((children
                       (emacs-agent-semantic--imenu-items value name)))
                  (when children
                    `((name . ,name)
                      (kind . "namespace")
                      (container . ,container)
                      (source . "imenu")
                      (children . ,children))))))))))
    items)))

(defun emacs-agent-semantic--xref-backend (capability)
  "Return the current native xref backend for CAPABILITY."
  (or (xref-find-backend)
      (emacs-agent-semantic--unavailable capability)))

(defun emacs-agent-semantic--xref-identifier
    (backend capability &optional explicit)
  "Return EXPLICIT or an identifier from BACKEND for CAPABILITY."
  (or (and (stringp explicit)
           (not (string-empty-p explicit))
           explicit)
      (condition-case error-data
          (xref-backend-identifier-at-point backend)
        ((cl-no-applicable-method error)
         (emacs-agent-semantic--unavailable
          capability (error-message-string error-data))))
      (emacs-agent-semantic--unavailable capability 'no_identifier)))

(defun emacs-agent-semantic--xref-location-source (location)
  "Return whether LOCATION was already represented by a live buffer."
  (cond
   ((and (fboundp 'xref-file-location-p)
         (xref-file-location-p location))
    (if (get-file-buffer (xref-file-location-file location))
        "buffer"
      "disk"))
   (t "buffer")))

(defun emacs-agent-semantic--xref-item
    (workspace item identifier &optional relation)
  "Convert xref ITEM for IDENTIFIER in WORKSPACE.

RELATION, when non-nil, describes the reference classification.  Return nil
for locations outside the bound workspace."
  (catch 'outside-workspace
    (let* ((location (xref-item-location item))
           (source (emacs-agent-semantic--xref-location-source location))
           (marker
            (condition-case nil
                (xref-location-marker location)
              (error nil)))
           (buffer (and (markerp marker) (marker-buffer marker))))
      (unless (buffer-live-p buffer)
        (throw 'outside-workspace nil))
      (with-current-buffer buffer
        (let* ((file (or buffer-file-name
                         (throw 'outside-workspace nil)))
               (canonical
                (condition-case nil
                    (file-truename file)
                  (file-error (throw 'outside-workspace nil))))
               (root (emacs-agent-policy--root workspace)))
          (unless (file-in-directory-p canonical root)
            (throw 'outside-workspace nil))
          (let* ((path (file-relative-name canonical root))
                 (document (emacs-agent-document-open workspace path))
                 (_ (emacs-agent-document-reconcile document))
                 (position (marker-position marker))
                 (end
                  (min (+ position (length identifier))
                       (save-excursion
                         (goto-char position)
                         (line-end-position)))))
            (save-restriction
              (widen)
              (save-excursion
                (goto-char position)
                `((path . ,path)
                  (range . ,(emacs-agent-semantic--range position end))
                  (preview
                   . ,(buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))
                  (summary . ,(xref-item-summary item))
                  (kind
                   . ,(emacs-agent-semantic--kind
                       (xref-item-summary item) nil position))
                  (relation . ,relation)
                  (source . ,source)
                  (revision
                   . ,(emacs-agent-document-revision document)))))))))))

(defun emacs-agent-semantic--xref-items
    (workspace items identifier &optional relation)
  "Convert xref ITEMS for IDENTIFIER in WORKSPACE and optional RELATION."
  (delq nil
        (mapcar
         (lambda (item)
           (emacs-agent-semantic--xref-item
            workspace item identifier relation))
         items)))

;;;###autoload
(defun emacs-agent-semantic-document-symbols (workspace path)
  "Return the native imenu symbol tree for PATH in WORKSPACE."
  (let* ((document (emacs-agent-document-open workspace path))
         (_ (emacs-agent-document-reconcile document))
         (buffer (emacs-agent-document-buffer document)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (condition-case error-data
            (emacs-agent-semantic--imenu-items
             (imenu--make-index-alist t))
          ((error quit)
           (emacs-agent-semantic--unavailable
            'document_symbols
            (error-message-string error-data))))))))

;;;###autoload
(defun emacs-agent-semantic-definition
    (workspace path position &optional identifier)
  "Return native xref definitions from PATH at POSITION in WORKSPACE.

IDENTIFIER, when non-nil, overrides the identifier at POSITION."
  (let* ((document (emacs-agent-document-open workspace path))
         (_ (emacs-agent-document-reconcile document))
         (buffer (emacs-agent-document-buffer document)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (emacs-agent-document-position document position))
          (let* ((backend
                  (emacs-agent-semantic--xref-backend 'symbol_definition))
                 (identifier
                  (emacs-agent-semantic--xref-identifier
                   backend 'symbol_definition identifier))
                 (items
                  (condition-case error-data
                      (xref-backend-definitions backend identifier)
                    ((cl-no-applicable-method error)
                     (emacs-agent-semantic--unavailable
                      'symbol_definition
                      (error-message-string error-data))))))
            (emacs-agent-semantic--xref-items
             workspace items identifier "definition")))))))

;;;###autoload
(defun emacs-agent-semantic-references
    (workspace path position &optional identifier)
  "Return native xref references from PATH at POSITION in WORKSPACE.

IDENTIFIER, when non-nil, overrides the identifier at POSITION.  Xref does not
provide portable read/write classification or completeness guarantees, so
results are explicitly marked as possibly incomplete and use the neutral
`reference' relation."
  (let* ((document (emacs-agent-document-open workspace path))
         (_ (emacs-agent-document-reconcile document))
         (buffer (emacs-agent-document-buffer document)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (emacs-agent-document-position document position))
          (let* ((backend
                  (emacs-agent-semantic--xref-backend 'symbol_references))
                 (identifier
                  (emacs-agent-semantic--xref-identifier
                   backend 'symbol_references identifier))
                 (items
                  (condition-case error-data
                      (xref-backend-references backend identifier)
                    ((cl-no-applicable-method error)
                     (emacs-agent-semantic--unavailable
                      'symbol_references
                      (error-message-string error-data))))))
            `((references
               . ,(emacs-agent-semantic--xref-items
                   workspace items identifier "reference"))
              (possibly_incomplete . t)
              (source . "xref"))))))))

;;;###autoload
(defun emacs-agent-semantic-workspace-symbols
    (workspace path query &optional kind path-prefix limit)
  "Search native xref symbols in WORKSPACE using PATH as backend anchor.

QUERY is passed to the active xref backend.  KIND and PATH-PREFIX restrict the
converted results, and LIMIT defaults to 100 and is capped at 500.  Xref does
not expose portable completeness guarantees, so the result is marked possibly
incomplete."
  (let* ((document (emacs-agent-document-open workspace path))
         (_ (emacs-agent-document-reconcile document))
         (buffer (emacs-agent-document-buffer document))
         (limit (min 500 (max 0 (or limit 100)))))
    (unless (and (stringp query) (not (string-empty-p query)))
      (emacs-agent-semantic--unavailable 'workspace_symbols 'invalid_query))
    (with-current-buffer buffer
      (let* ((backend
              (emacs-agent-semantic--xref-backend 'workspace_symbols))
             (items
              (condition-case error-data
                  (xref-backend-apropos backend query)
                ((cl-no-applicable-method error)
                 (emacs-agent-semantic--unavailable
                  'workspace_symbols
                  (error-message-string error-data)))))
             (symbols
              (emacs-agent-semantic--xref-items
               workspace items query "symbol"))
             (filtered
              (seq-filter
               (lambda (symbol)
                 (and
                  (or (null kind)
                      (equal kind (alist-get 'kind symbol)))
                  (or (null path-prefix)
                      (string-prefix-p
                       path-prefix (alist-get 'path symbol)))))
               symbols)))
        `((symbols . ,(seq-take filtered limit))
          (possibly_incomplete . t)
          (source . "xref"))))))

;;;###autoload
(defun emacs-agent-semantic-rename-preview
    (workspace path position new-name expected-revision)
  "Preview a native semantic rename in WORKSPACE.

PATH and POSITION identify the symbol.  NEW-NAME is sent only to the active
Eglot server, and EXPECTED-REVISION guards the source document.  The returned
preview ID freezes the resulting workspace edit for a later atomic apply."
  (unless (and (stringp new-name) (not (string-empty-p new-name)))
    (emacs-agent-signal 'invalid_argument :field 'new_name))
  (let* ((document
          (emacs-agent-semantic--assert-revision
           workspace path expected-revision))
         (buffer (emacs-agent-document-buffer document)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (emacs-agent-document-position document position))
          (let* ((server
                  (emacs-agent-semantic--eglot-server 'symbol_rename))
                 (linepos-function eglot-move-to-linepos-function)
                 (workspace-edit
                  (emacs-agent-semantic--eglot-request
                   server :textDocument/rename
                   (list
                    :textDocument
                    (list :uri (eglot-path-to-uri buffer-file-name))
                    :position (eglot--pos-to-lsp-position)
                    :newName new-name)
                   'symbol_rename))
                 (plan
                  (emacs-agent-semantic--workspace-edit-plan
                   workspace workspace-edit 'symbol-rename
                   linepos-function))
                 (preview-id
                  (emacs-agent-semantic--cache-plan
                   workspace 'symbol-rename plan)))
            (emacs-agent-semantic--plan-preview
             preview-id 'symbol-rename plan)))))))

;;;###autoload
(defun emacs-agent-semantic-rename-apply
    (workspace preview-id &optional checkpoint request-context)
  "Atomically apply a frozen semantic rename PREVIEW-ID in WORKSPACE.

CHECKPOINT and REQUEST-CONTEXT are forwarded to the workspace transaction.
No new language-server request is made during application."
  (emacs-agent-transaction-apply
   (emacs-agent-semantic--take-plan
   workspace preview-id 'symbol-rename)
   nil checkpoint request-context))

(defun emacs-agent-semantic--code-action-classification (action)
  "Return the safe execution class for LSP code ACTION."
  (let ((edit (emacs-agent-semantic--field action 'edit))
        (command (emacs-agent-semantic--field action 'command)))
    (cond
     ((and edit command) 'edit-and-command)
     (edit 'edit)
     (command 'command)
     (t 'unresolved))))

(defun emacs-agent-semantic--code-action-command (action)
  "Return the command identifier advertised by ACTION, or nil."
  (let ((command (emacs-agent-semantic--field action 'command)))
    (cond
     ((stringp command) command)
     (command (emacs-agent-semantic--field command 'command)))))

(defun emacs-agent-semantic--cache-action
    (workspace classification plan command disabled)
  "Cache a code action for WORKSPACE and return its opaque identifier.
CLASSIFICATION, PLAN, COMMAND, and DISABLED describe its safe execution."
  (let ((id (emacs-agent-semantic--id "action:")))
    (puthash
     id
     (list :workspace (emacs-agent-policy--root workspace)
           :classification classification
           :plan plan :command command :disabled disabled
           :expires
           (+ (float-time) emacs-agent-semantic-preview-ttl))
     emacs-agent-semantic--actions)
    id))

(defun emacs-agent-semantic--public-code-action
    (workspace action linepos-function)
  "Return safe metadata for LSP ACTION in WORKSPACE."
  (let* ((classification
          (emacs-agent-semantic--code-action-classification action))
         (edit (emacs-agent-semantic--field action 'edit))
         (command (emacs-agent-semantic--code-action-command action))
         (disabled (emacs-agent-semantic--field action 'disabled))
         (plan
          (and edit
               (emacs-agent-semantic--workspace-edit-plan
                workspace edit 'code-action linepos-function)))
         (action-id
          (emacs-agent-semantic--cache-action
           workspace classification plan command disabled))
         (preview (and plan (emacs-agent-transaction-apply plan t))))
    `((action_id . ,action-id)
      (title . ,(or (emacs-agent-semantic--field action 'title) ""))
      (kind . ,(emacs-agent-semantic--field action 'kind))
      (classification
       . ,(replace-regexp-in-string
           "-" "_" (symbol-name classification)))
      (requires_approval
       . ,(if (memq classification '(command edit-and-command))
              t :false))
      (preferred
       . ,(if (eq (emacs-agent-semantic--field action 'isPreferred) t)
              t :false))
      (disabled . ,(if disabled t :false))
      (command . ,command)
      (documents . ,(and preview (plist-get preview :documents))))))

;;;###autoload
(defun emacs-agent-semantic-code-actions
    (workspace path range expected-revision &optional kind)
  "Return safe native code actions for RANGE in PATH and WORKSPACE.

EXPECTED-REVISION guards the query.  KIND optionally restricts the LSP action
kind.  Results are classified without executing commands; only workspace
edits are converted into frozen atomic transaction plans."
  (let* ((document
          (emacs-agent-semantic--assert-revision
           workspace path expected-revision))
         (buffer (emacs-agent-document-buffer document))
         (start-position
          (emacs-agent-semantic--field range 'start))
         (end-position
          (emacs-agent-semantic--field range 'end)))
    (unless (and start-position end-position)
      (emacs-agent-signal 'invalid_argument :field 'range))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let* ((start
                (emacs-agent-document-position
                 document start-position))
               (end
                (emacs-agent-document-position
                 document end-position))
               (server
                (emacs-agent-semantic--eglot-server 'code_actions))
               (linepos-function eglot-move-to-linepos-function)
               (actions
                (emacs-agent-semantic--eglot-request
                 server :textDocument/codeAction
                 (list
                  :textDocument
                  (list :uri (eglot-path-to-uri buffer-file-name))
                  :range
                  (list
                   :start (eglot--pos-to-lsp-position start)
                   :end (eglot--pos-to-lsp-position end))
                  :context
                  (append
                   (list :diagnostics [])
                   (and kind (list :only (vector kind)))))
                 'code_actions)))
          `((revision . ,expected-revision)
            (actions
             . ,(mapcar
                 (lambda (action)
                   (emacs-agent-semantic--public-code-action
                    workspace action linepos-function))
                 (append actions nil)))))))))

;;;###autoload
(defun emacs-agent-semantic-code-action-apply
    (workspace action-id &optional checkpoint request-context)
  "Atomically apply pure-edit code ACTION-ID in WORKSPACE.

Actions containing commands are never executed here and instead signal
`approval_required'.  CHECKPOINT and REQUEST-CONTEXT are passed to the
workspace transaction for pure edits."
  (let ((state
         (and (stringp action-id)
              (gethash action-id emacs-agent-semantic--actions))))
    (unless (and state
                 (equal (plist-get state :workspace)
                        (emacs-agent-policy--root workspace))
                 (> (plist-get state :expires) (float-time)))
      (emacs-agent-signal
       'revision_conflict :reason 'invalid_action
       :requires_reread t))
    (when (plist-get state :disabled)
      (emacs-agent-signal 'invalid_argument
                          :action_id action-id :reason 'disabled))
    (pcase (plist-get state :classification)
      ('edit
       (remhash action-id emacs-agent-semantic--actions)
       (emacs-agent-transaction-apply
        (plist-get state :plan) nil checkpoint request-context))
      ((or 'command 'edit-and-command)
       (emacs-agent-signal
        'approval_required :action_id action-id
        :classification (plist-get state :classification)
        :command (plist-get state :command)))
      (_
       (emacs-agent-semantic--unavailable
        'code_action_apply 'unresolved_action)))))

;;;###autoload
(defun emacs-agent-semantic-format-range-preview
    (workspace path range expected-revision)
  "Preview native Eglot formatting for RANGE in PATH and WORKSPACE.

EXPECTED-REVISION guards the source.  Formatter selection and options come
only from the active Emacs/Eglot configuration; callers cannot provide a
program or command.  The returned preview ID freezes the text edits."
  (let* ((document
          (emacs-agent-semantic--assert-revision
           workspace path expected-revision))
         (buffer (emacs-agent-document-buffer document))
         (start-position
          (emacs-agent-semantic--field range 'start))
         (end-position
          (emacs-agent-semantic--field range 'end)))
    (unless (and start-position end-position)
      (emacs-agent-signal 'invalid_argument :field 'range))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let* ((start
                (emacs-agent-document-position
                 document start-position))
               (end
                (emacs-agent-document-position
                 document end-position))
               (server
                (emacs-agent-semantic--eglot-server 'format_range))
               (linepos-function eglot-move-to-linepos-function)
               (uri (eglot-path-to-uri buffer-file-name))
               (edits
                (emacs-agent-semantic--eglot-request
                 server :textDocument/rangeFormatting
                 (list
                  :textDocument (list :uri uri)
                  :range
                  (list
                   :start (eglot--pos-to-lsp-position start)
                   :end (eglot--pos-to-lsp-position end))
                  :options
                  (list
                   :tabSize tab-width
                   :insertSpaces (if indent-tabs-mode :json-false t)
                   :insertFinalNewline
                   (if require-final-newline t :json-false)
                   :trimFinalNewlines
                   (if delete-trailing-lines t :json-false)))
                 'format_range))
               (workspace-edit
                (list
                 :documentChanges
                 (vector
                  (list :textDocument (list :uri uri)
                        :edits edits))))
               (plan
                (emacs-agent-semantic--workspace-edit-plan
                 workspace workspace-edit 'format-range
                 linepos-function))
               (preview-id
                (emacs-agent-semantic--cache-plan
                 workspace 'format-range plan)))
          (emacs-agent-semantic--plan-preview
           preview-id 'format-range plan))))))

;;;###autoload
(defun emacs-agent-semantic-format-range-apply
    (workspace preview-id &optional checkpoint request-context)
  "Atomically apply a frozen range-format PREVIEW-ID in WORKSPACE.

CHECKPOINT and REQUEST-CONTEXT are forwarded to the workspace transaction.
The formatter is not invoked again during application."
  (emacs-agent-transaction-apply
   (emacs-agent-semantic--take-plan
    workspace preview-id 'format-range)
   nil checkpoint request-context))

(defun emacs-agent-semantic--context-redaction-reason (workspace buffer)
  "Return a reason to redact BUFFER in WORKSPACE, or nil."
  (with-current-buffer buffer
    (cond
     ((or (minibufferp buffer)
          (string-prefix-p " *Minibuf-" (buffer-name buffer)))
      "minibuffer")
     (emacs-agent-semantic-sensitive-buffer "sensitive_buffer")
     (buffer-file-name
      (let* ((root (emacs-agent-policy--root workspace))
             (canonical
              (condition-case nil
                  (file-truename buffer-file-name)
                (file-error nil))))
        (cond
         ((or (null canonical)
              (not (file-in-directory-p canonical root)))
          "outside_workspace")
         ((condition-case nil
              (progn
                (emacs-agent-policy-resolve
                 workspace (file-relative-name canonical root))
                nil)
            (emacs-agent-error t))
          "sensitive_path")))))))

(defun emacs-agent-semantic--context-project (workspace)
  "Return the current project root when it is contained in WORKSPACE."
  (when-let* ((project (condition-case nil
                           (project-current nil default-directory)
                         (error nil)))
              (project-root
               (condition-case nil
                   (file-truename (project-root project))
                 (error nil)))
              (workspace-root (emacs-agent-policy--root workspace)))
    (when (or (equal (directory-file-name project-root)
                     (directory-file-name workspace-root))
              (file-in-directory-p project-root workspace-root))
      project-root)))

;;;###autoload
(defun emacs-agent-semantic-editor-context (workspace &optional buffer)
  "Return safe editor metadata for BUFFER in WORKSPACE.

BUFFER defaults to the current buffer.  No buffer text is returned.  Sensitive,
minibuffer, denied-path, and out-of-workspace buffers return only workspace and
redaction metadata."
  (setq buffer (or buffer (current-buffer)))
  (let* ((root (emacs-agent-policy--root workspace))
         (reason
          (and (buffer-live-p buffer)
               (emacs-agent-semantic--context-redaction-reason
                workspace buffer))))
    (cond
     ((not (buffer-live-p buffer))
      `((workspace . ,root)
        (redacted . t)
        (redaction_reason . "dead_buffer")))
     (reason
      `((workspace . ,root)
        (redacted . t)
        (redaction_reason . ,reason)))
     (t
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (let* ((canonical
                  (and buffer-file-name
                       (file-truename buffer-file-name)))
                 (path
                  (and canonical (file-relative-name canonical root)))
                 (window (get-buffer-window buffer t))
                 (visible
                  (and (window-live-p window)
                       (emacs-agent-semantic--range
                        (window-start window)
                        (window-end window t))))
                 (region
                  (and mark-active (mark t) (/= (point) (mark t))
                       (emacs-agent-semantic--range
                        (region-beginning) (region-end)))))
            `((workspace . ,root)
              (redacted . :false)
              (buffer
               . ((name . ,(buffer-name buffer))
                  (path . ,path)
                  (modified . ,(if (buffer-modified-p) t :false))))
              (point . ,(emacs-agent-semantic--public-position (point)))
              (active_region . ,region)
              (visible_range . ,visible)
              (major_mode . ,(symbol-name major-mode))
              (project . ,(emacs-agent-semantic--context-project
                            workspace))))))))))

(defun emacs-agent-semantic--format-state
    (workspace path expected-revision)
  "Return validated formatting state for PATH in WORKSPACE.
EXPECTED-REVISION must identify the authoritative buffer."
  (unless (functionp emacs-agent-semantic-format-function)
    (emacs-agent-semantic--unavailable 'format_document
                                       'no_trusted_formatter))
  (let* ((document (emacs-agent-document-open workspace path))
         (_ (emacs-agent-document-reconcile document))
         (revision (emacs-agent-document-revision document))
         (buffer (emacs-agent-document-buffer document)))
    (unless (equal revision expected-revision)
      (emacs-agent-signal
       'revision_conflict
       :path path
       :expected_revision expected-revision
       :current_revision revision
       :requires_reread t))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let* ((before
                (buffer-substring-no-properties (point-min) (point-max)))
               (after
                (condition-case error-data
                    (funcall emacs-agent-semantic-format-function
                             before major-mode)
                  ((error quit)
                   (emacs-agent-semantic--unavailable
                    'format_document
                    (error-message-string error-data))))))
          (unless (stringp after)
            (emacs-agent-semantic--unavailable
             'format_document 'invalid_formatter_result))
          (when (string-search (string 0) after)
            (emacs-agent-signal
             'unsupported_document_type :path path :reason 'binary))
          (when (> (string-bytes after)
                   emacs-agent-policy-maximum-document-bytes)
            (emacs-agent-signal 'document_too_large :path path))
          (list :document document
                :revision revision
                :before before
                :after after
                :end
                (emacs-agent-semantic--public-position (point-max))))))))

;;;###autoload
(defun emacs-agent-semantic-format-preview
    (workspace path expected-revision)
  "Preview trusted formatting of PATH at EXPECTED-REVISION in WORKSPACE.

The configured formatter receives an immutable string and cannot be selected
by a client.  This function never mutates the document."
  (let* ((state
          (emacs-agent-semantic--format-state
           workspace path expected-revision))
         (before (plist-get state :before))
         (after (plist-get state :after)))
    `((path . ,path)
      (revision . ,(plist-get state :revision))
      (changed . ,(if (equal before after) :false t))
      (diff
       . ,(emacs-agent-changeset--diff-text path before after)))))

;;;###autoload
(defun emacs-agent-semantic-format-apply
    (workspace path expected-revision &optional checkpoint)
  "Apply trusted formatting to PATH at EXPECTED-REVISION in WORKSPACE.

CHECKPOINT has the same meaning as in `emacs-agent-edit-apply'.  The write uses
the normal guarded edit path, so revision checks, undo, saving, and change-set
recording remain centralized."
  (let* ((state
          (emacs-agent-semantic--format-state
           workspace path expected-revision))
         (before (plist-get state :before))
         (after (plist-get state :after))
         (buffer
          (emacs-agent-document-buffer (plist-get state :document)))
         (restriction
          (with-current-buffer buffer
            (cons (point-min) (point-max))))
         (fully-widened
          (with-current-buffer buffer
            (let ((old-min (point-min))
                  (old-max (point-max)))
              (save-restriction
                (widen)
                (and (= old-min (point-min))
                     (= old-max (point-max))))))))
    (if (equal before after)
        (list :path path
              :previous_revision expected-revision
              :new_revision expected-revision
              :changed :false
              :diff "")
      (let ((result
             (emacs-agent-edit-apply
              workspace path expected-revision
              `(((start . ((line . 1) (column . 0)))
                 (end . ,(plist-get state :end))
                 (new_text . ,after)
                 (expected_text . ,before)))
              checkpoint)))
        ;; Full-buffer replacement collapses ordinary restriction markers.
        ;; Restore the caller-visible restriction from integer bounds.
        (with-current-buffer buffer
          (widen)
          (unless fully-widened
            (narrow-to-region
             (min (car restriction) (point-max))
             (min (cdr restriction) (point-max)))))
        (append
         result
         (list :changed t
               :diff
               (emacs-agent-changeset--diff-text path before after)))))))

(provide 'emacs-agent-semantic)
;;; emacs-agent-semantic.el ends here
