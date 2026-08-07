;;; emacs-agent-semantic-test.el --- Semantic service tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for project-optional native semantic and editor-context adapters.

;;; Code:

(require 'ert)
(require 'etags)
(require 'emacs-agent-project)
(require 'emacs-agent-request)
(require 'emacs-agent-semantic)

(define-derived-mode emacs-agent-semantic-test-gfm-mode text-mode "GFM-Test"
  "Minimal GFM-like mode used to exercise the default etags backend.")

(defvar emacs-agent-semantic-xref-timeout)

(defun emacs-agent-semantic-test--configure-no-tags-etags (document)
  "Configure DOCUMENT like a GFM buffer with fallback etags and no TAGS."
  (with-current-buffer (emacs-agent-document-buffer document)
    (emacs-agent-semantic-test-gfm-mode)
    (setq-local tags-file-name nil)
    (setq-local tags-table-list nil)
    (setq-local default-tags-table-function nil)
    (setq-local xref-backend-functions
                (list #'etags--xref-backend))))

(cl-defmethod xref-backend-identifier-at-point
  ((_backend (eql emacs-agent-semantic-test-backend)))
  "Return the stable semantic test identifier."
  "sample-function")

(cl-defmethod xref-backend-definitions
  ((_backend (eql emacs-agent-semantic-test-backend)) identifier)
  "Return test definitions for IDENTIFIER."
  (when (equal identifier "sample-function")
    (list
     (xref-make
     "sample-function"
      (xref-make-file-location
       (expand-file-name "sample.el" default-directory)
       1 7)))))

(cl-defmethod xref-backend-references
  ((_backend (eql emacs-agent-semantic-test-backend)) identifier)
  "Return test references for IDENTIFIER."
  (when (equal identifier "sample-function")
    (list
     (xref-make
     "sample-function call"
      (xref-make-file-location
       (expand-file-name "sample.el" default-directory)
       4 1)))))

(cl-defmethod xref-backend-apropos
  ((_backend (eql emacs-agent-semantic-test-backend)) pattern)
  "Return test symbols matching PATTERN."
  (when (string-match-p pattern "sample-function")
    (list
     (xref-make
     "sample-function"
      (xref-make-file-location
       (expand-file-name "sample.el" default-directory)
       1 7)))))

(cl-defmethod xref-backend-definitions
  ((_backend (eql emacs-agent-semantic-interaction-test-backend))
   _identifier)
  "Assert that Agent Editor disabled interactive provider behavior."
  (unless inhibit-interaction
    (error "Xref provider interaction was not inhibited"))
  nil)

(cl-defmethod xref-backend-definitions
  ((_backend (eql emacs-agent-semantic-slow-test-backend)) _identifier)
  "Wait long enough for the semantic provider deadline to expire."
  (accept-process-output nil 1)
  nil)

(defmacro emacs-agent-semantic-test--with-runtime (&rest body)
  "Run BODY with a temporary ROOT and RUNTIME, then clean up."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "emacs-agent-semantic-" t))
          (runtime (emacs-agent-runtime-create)))
     (unwind-protect
         (progn
           (clrhash emacs-agent-semantic--previews)
           (clrhash emacs-agent-semantic--actions)
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file root)
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (clrhash emacs-agent-semantic--previews)
       (clrhash emacs-agent-semantic--actions)
       (delete-directory root t))))

(defun emacs-agent-semantic-test--direct-target (runtime path)
  "Resolve absolute PATH as a direct target in RUNTIME."
  (emacs-agent-project-resolve-target runtime path))

(defun emacs-agent-semantic-test--project-target
    (runtime project-id path)
  "Resolve PATH under PROJECT-ID in RUNTIME."
  (emacs-agent-project-resolve-target
   runtime path :project-id project-id))

(defun emacs-agent-semantic-test--add-xref-backend (document)
  "Install the semantic test xref backend in DOCUMENT."
  (with-current-buffer (emacs-agent-document-buffer document)
    (add-hook 'xref-backend-functions
              (lambda () 'emacs-agent-semantic-test-backend)
              nil t)))

(defun emacs-agent-semantic-test--workspace-edit (path old-end new-text)
  "Return one LSP WorkspaceEdit for PATH ending at OLD-END with NEW-TEXT."
  `(:documentChanges
    [(:textDocument (:uri ,(concat "file://" path))
      :edits
      [(:range (:start (:line 0 :character 0)
                :end (:line 0 :character ,old-end))
        :newText ,new-text)])]))

(ert-deftest emacs-agent-semantic-document-symbols-supports-direct-target ()
  (emacs-agent-semantic-test--with-runtime
    (let ((path (expand-file-name "sample.el" root)))
      (write-region "(defun sample-function () t)\n" nil path)
      (let* ((target
              (emacs-agent-semantic-test--direct-target runtime path))
             (symbols
              (emacs-agent-semantic-document-symbols runtime target))
             (function
              (seq-find
               (lambda (symbol)
                 (equal (alist-get 'name symbol) "sample-function"))
               symbols)))
        (should function)
        (should (equal (alist-get 'kind function) "function"))
        (should (equal (alist-get 'source function) "imenu"))))))

(ert-deftest emacs-agent-semantic-document-symbols-supports-project-target ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "(defvar sample-variable 1)\n" nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (target
            (emacs-agent-semantic-test--project-target
             runtime project-id "sample.el"))
           (symbols
            (emacs-agent-semantic-document-symbols runtime target)))
      (should
       (seq-find
        (lambda (symbol)
          (or (equal (alist-get 'name symbol) "sample-variable")
              (seq-find
               (lambda (child)
                 (equal (alist-get 'name child) "sample-variable"))
               (alist-get 'children symbol))))
        symbols)))))

(ert-deftest emacs-agent-semantic-definition-emits-direct-target-fields ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "(defun sample-function () t)\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target)))
      (emacs-agent-semantic-test--add-xref-backend document)
      (let ((location
             (car
              (emacs-agent-semantic-definition
               runtime target '((line . 1) (column . 8))))))
        (should (equal (alist-get 'path location) (file-truename path)))
        (should-not (alist-get 'project_id location))
        (should-not (alist-get 'relative_path location))
        (should (equal (alist-get 'source location) "buffer"))
        (should (string-prefix-p "rev:" (alist-get 'revision location)))))))

(ert-deftest emacs-agent-semantic-definition-preserves-explicit-project ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "(defun sample-function () t)\n" nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (target
            (emacs-agent-semantic-test--project-target
             runtime project-id "sample.el"))
           (document (emacs-agent-document-open runtime target)))
      (emacs-agent-semantic-test--add-xref-backend document)
      (let ((location
             (car
              (emacs-agent-semantic-definition
               runtime target '((line . 1) (column . 8))))))
        (should (equal (alist-get 'path location) (file-truename path)))
        (should (equal (alist-get 'project_id location) project-id))
        (should (equal (alist-get 'relative_path location) "sample.el"))))))

(ert-deftest emacs-agent-semantic-xref-fails-closed-without-backend ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.txt" root))
           (_ (write-region "plain text\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (setq-local xref-backend-functions nil))
      (condition-case error-data
          (progn
            (emacs-agent-semantic-definition
             runtime target '((line . 1) (column . 0)))
            (ert-fail "Expected capability_unavailable"))
        (emacs-agent-error
         (should
          (eq (emacs-agent-error-code error-data)
              'capability_unavailable)))))))

(ert-deftest emacs-agent-semantic-definition-rejects-etags-without-tags ()
  "Definitions must fail before invoking unconfigured fallback etags."
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "README.md" root))
           (_ (write-region "# Agent Editor\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           provider-invoked)
      (emacs-agent-semantic-test--configure-no-tags-etags document)
      (cl-letf (((symbol-function 'xref-backend-definitions)
                 (lambda (&rest _arguments)
                   (setq provider-invoked t)
                   (ert-fail "Unconfigured etags must not be invoked"))))
        (condition-case error-data
            (progn
              (emacs-agent-semantic-definition
               runtime target '((line . 1) (column . 2)) "Agent")
              (ert-fail "Expected capability_unavailable"))
          (emacs-agent-error
           (should
            (eq (emacs-agent-error-code error-data)
                'capability_unavailable)))))
      (should-not provider-invoked))))

(ert-deftest emacs-agent-semantic-definition-inhibits-provider-interaction ()
  "Definitions must invoke usable Xref providers non-interactively."
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.txt" root))
           (_ (write-region "sample\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (add-hook
         'xref-backend-functions
         (lambda () 'emacs-agent-semantic-interaction-test-backend)
         nil t))
      (should-not
       (emacs-agent-semantic-definition
       runtime target '((line . 1) (column . 1)) "sample")))))

(ert-deftest emacs-agent-semantic-definition-enforces-provider-deadline ()
  "Definitions must stop a yielding Xref provider at the server deadline."
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.txt" root))
           (_ (write-region "sample\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (emacs-agent-semantic-xref-timeout 0.02)
           (started-at (float-time)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (add-hook
         'xref-backend-functions
         (lambda () 'emacs-agent-semantic-slow-test-backend)
         nil t))
      (condition-case error-data
          (progn
            (emacs-agent-semantic-definition
             runtime target '((line . 1) (column . 1)) "sample")
            (ert-fail "Expected operation_timeout"))
        (emacs-agent-error
         (should
          (eq (emacs-agent-error-code error-data)
              'operation_timeout))))
      (should (< (- (float-time) started-at) 0.5)))))

(ert-deftest emacs-agent-semantic-definition-honors-request-cancellation ()
  "Definitions must stop a yielding provider when its request is cancelled."
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.txt" root))
           (_ (write-region "sample\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (emacs-agent-semantic-xref-timeout 1)
           (request
            (emacs-agent-request-register
             (emacs-agent-request-create
              :id 77 :operation "emacs_agent_symbol_definition")))
           (timer
            (run-at-time 0.02 nil
                         #'emacs-agent-request-cancel request))
           (started-at (float-time)))
      (unwind-protect
          (progn
            (with-current-buffer (emacs-agent-document-buffer document)
              (add-hook
               'xref-backend-functions
               (lambda () 'emacs-agent-semantic-slow-test-backend)
               nil t))
            (condition-case error-data
                (progn
                  (emacs-agent-semantic-definition
                   runtime target '((line . 1) (column . 1))
                   "sample" request)
                  (ert-fail "Expected operation_cancelled"))
              (emacs-agent-error
               (should
                (eq (emacs-agent-error-code error-data)
                    'operation_cancelled))))
            (should (< (- (float-time) started-at) 0.5))
            (should
             (eq (emacs-agent-request-state request) 'cancelled))
            (should-not (emacs-agent-request-find 77)))
        (when (timerp timer)
          (cancel-timer timer))
        (when (eq (emacs-agent-request-state request) 'pending)
          (emacs-agent-request-finish request 'completed))))))

(ert-deftest emacs-agent-semantic-references-supports-direct-target ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region
               "(defun sample-function () t)\n\n\n(sample-function)\n"
               nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target)))
      (emacs-agent-semantic-test--add-xref-backend document)
      (let* ((result
              (emacs-agent-semantic-references
               runtime target '((line . 4) (column . 2))))
             (reference (car (alist-get 'references result))))
        (should (eq (alist-get 'possibly_incomplete result) t))
        (should (equal (alist-get 'path reference) (file-truename path)))
        (should (equal (alist-get 'relation reference) "reference"))))))

(ert-deftest emacs-agent-semantic-references-reject-etags-without-tags ()
  "References must fail before invoking unconfigured fallback etags."
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "README.md" root))
           (_ (write-region "# Agent Editor\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           provider-invoked)
      (emacs-agent-semantic-test--configure-no-tags-etags document)
      (cl-letf (((symbol-function 'xref-backend-references)
                 (lambda (&rest _arguments)
                   (setq provider-invoked t)
                   (ert-fail "Unconfigured etags must not be invoked"))))
        (condition-case error-data
            (progn
              (emacs-agent-semantic-references
               runtime target '((line . 1) (column . 2)) "Agent")
              (ert-fail "Expected capability_unavailable"))
          (emacs-agent-error
           (should
            (eq (emacs-agent-error-code error-data)
                'capability_unavailable)))))
      (should-not provider-invoked))))

(ert-deftest emacs-agent-semantic-references-preserve-explicit-project ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region
               "(defun sample-function () t)\n\n\n(sample-function)\n"
               nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (target
            (emacs-agent-semantic-test--project-target
             runtime project-id "sample.el"))
           (document (emacs-agent-document-open runtime target)))
      (emacs-agent-semantic-test--add-xref-backend document)
      (let ((reference
             (car
              (alist-get
               'references
               (emacs-agent-semantic-references
                runtime target '((line . 4) (column . 2)))))))
        (should (equal (alist-get 'project_id reference) project-id))
        (should
         (equal (alist-get 'relative_path reference) "sample.el"))))))

(ert-deftest emacs-agent-project-symbols-requires-explicit-project-context ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "(defun sample-function () t)\n" nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (target
            (emacs-agent-semantic-test--project-target
             runtime project-id "sample.el"))
           (document (emacs-agent-document-open runtime target)))
      (emacs-agent-semantic-test--add-xref-backend document)
      (let* ((result
              (emacs-agent-project-symbols
               runtime project-id "sample.el" "sample"
               "function" "sample" 1))
             (symbol (car (alist-get 'symbols result))))
        (should (equal (alist-get 'path symbol) (file-truename path)))
        (should (equal (alist-get 'project_id symbol) project-id))
        (should (equal (alist-get 'relative_path symbol) "sample.el")))
      (condition-case error-data
          (progn
            (emacs-agent-project-symbols
             runtime "missing" "sample.el" "sample")
            (ert-fail "Expected project_not_found"))
        (emacs-agent-error
         (should
          (eq (emacs-agent-error-code error-data) 'project_not_found)))))))

(ert-deftest emacs-agent-project-symbols-does-not-use-current-directory ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((other (make-temp-file "emacs-agent-other-project-" t))
           (path (expand-file-name "sample.el" root))
           (_ (write-region "(defun sample-function () t)\n" nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (target
            (emacs-agent-semantic-test--project-target
             runtime project-id "sample.el"))
           (document (emacs-agent-document-open runtime target))
           (default-directory other))
      (unwind-protect
          (progn
            (emacs-agent-semantic-test--add-xref-backend document)
            (let ((symbol
                   (car
                    (alist-get
                     'symbols
                     (emacs-agent-project-symbols
                      runtime project-id "sample.el" "sample")))))
              (should
               (equal (alist-get 'path symbol) (file-truename path)))))
        (delete-directory other t)))))

(ert-deftest emacs-agent-project-symbols-rejects-etags-without-tags ()
  "Project symbols must fail before invoking unconfigured fallback etags."
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "README.md" root))
           (_ (write-region "# Agent Editor\n" nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (target
            (emacs-agent-semantic-test--project-target
             runtime project-id "README.md"))
           (document (emacs-agent-document-open runtime target))
           provider-invoked)
      (emacs-agent-semantic-test--configure-no-tags-etags document)
      (cl-letf (((symbol-function 'xref-backend-apropos)
                 (lambda (&rest _arguments)
                   (setq provider-invoked t)
                   (ert-fail "Unconfigured etags must not be invoked"))))
        (condition-case error-data
            (progn
              (emacs-agent-project-symbols
               runtime project-id "README.md" "Agent")
              (ert-fail "Expected capability_unavailable"))
          (emacs-agent-error
           (should
            (eq (emacs-agent-error-code error-data)
                'capability_unavailable)))))
      (should-not provider-invoked))))

(ert-deftest emacs-agent-semantic-rename-direct-preview-is-runtime-scoped ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "target();\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (workspace-edit
            (emacs-agent-semantic-test--workspace-edit path 6 "renamed"))
           (request-count 0))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (_server method _params &rest _)
                   (should (eq method :textDocument/rename))
                   (setq request-count (1+ request-count))
                   workspace-edit)))
        (let* ((preview
                (emacs-agent-semantic-rename-preview
                 runtime target '((line . 1) (column . 1))
                 "renamed" revision))
               (preview-id (alist-get 'preview_id preview))
               (other-runtime (emacs-agent-runtime-create)))
          (should (= request-count 1))
          (condition-case error-data
              (progn
                (emacs-agent-semantic-rename-apply
                 other-runtime preview-id)
                (ert-fail "Preview must not cross runtimes"))
            (emacs-agent-error
             (should
              (eq (emacs-agent-error-code error-data)
                  'revision_conflict))))
          (let ((result
                 (emacs-agent-semantic-rename-apply runtime preview-id)))
            (should (= request-count 1))
            (should (plist-get result :applied))
            (should
             (equal
              (with-current-buffer
                  (emacs-agent-document-buffer document)
                (buffer-string))
              "renamed();\n"))))))))

(ert-deftest emacs-agent-semantic-empty-workspace-edit-is-a-true-noop ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "target\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (workspace-edit '(:documentChanges [])))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _) workspace-edit)))
        (let* ((preview
                (emacs-agent-semantic-rename-preview
                 runtime target '((line . 1) (column . 1))
                 "renamed" revision))
               (result
                (emacs-agent-semantic-rename-apply
                 runtime (alist-get 'preview_id preview))))
          (should (eq (alist-get 'modified preview) :false))
          (should-not (alist-get 'documents preview))
          (should (plist-get result :applied))
          (should-not (plist-get result :modified))
          (should-not (plist-get result :changeset_id))
          (should (equal (plist-get result :diff) ""))
          (should-not (plist-get result :documents))
          (should
           (zerop
            (hash-table-count
             (emacs-agent-runtime-changeset-registry runtime))))
          (with-current-buffer (emacs-agent-document-buffer document)
            (should (equal (buffer-string) "target\n"))))))))

(ert-deftest emacs-agent-semantic-workspace-edit-rejects-nul-result ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "target\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (workspace-edit
            (emacs-agent-semantic-test--workspace-edit
             path 6 (concat "bad" (string 0)))))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _) workspace-edit)))
        (condition-case error-data
            (progn
              (emacs-agent-semantic-rename-preview
               runtime target '((line . 1) (column . 1))
               "bad" revision)
              (ert-fail "Expected unsafe binary result rejection"))
          (emacs-agent-error
           (should
            (eq (emacs-agent-error-code error-data)
                'unsupported_document_type))))
        (should (zerop (hash-table-count emacs-agent-semantic--previews)))
        (with-current-buffer (emacs-agent-document-buffer document)
          (should (equal (buffer-string) "target\n")))))))

(ert-deftest emacs-agent-semantic-workspace-edit-rejects-oversize-result ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "x\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (workspace-edit
            (emacs-agent-semantic-test--workspace-edit
             path 1 "abcdef"))
           (emacs-agent-policy-maximum-document-bytes 4))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _) workspace-edit)))
        (condition-case error-data
            (progn
              (emacs-agent-semantic-rename-preview
               runtime target '((line . 1) (column . 1))
               "abcdef" revision)
              (ert-fail "Expected oversized result rejection"))
          (emacs-agent-error
           (should
            (eq (emacs-agent-error-code error-data)
                'document_too_large))))
        (should (zerop (hash-table-count emacs-agent-semantic--previews)))
        (with-current-buffer (emacs-agent-document-buffer document)
          (should (equal (buffer-string) "x\n")))))))

(ert-deftest emacs-agent-semantic-rename-stale-document-aborts-all-edits ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((one-path (expand-file-name "one.el" root))
           (two-path (expand-file-name "two.el" root))
           (_ (write-region "target();\n" nil one-path))
           (_ (write-region "(target)\n" nil two-path))
           (one-target
            (emacs-agent-semantic-test--direct-target runtime one-path))
           (two-target
            (emacs-agent-semantic-test--direct-target runtime two-path))
           (one-document (emacs-agent-document-open runtime one-target))
           (two-document (emacs-agent-document-open runtime two-target))
           (revision (emacs-agent-document-revision one-document))
           (workspace-edit
            `(:documentChanges
              [(:textDocument (:uri ,(concat "file://" one-path))
                :edits
                [(:range (:start (:line 0 :character 0)
                          :end (:line 0 :character 6))
                  :newText "renamed")])
               (:textDocument (:uri ,(concat "file://" two-path))
                :edits
                [(:range (:start (:line 0 :character 1)
                          :end (:line 0 :character 7))
                  :newText "renamed")])])))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _) workspace-edit)))
        (let* ((preview
                (emacs-agent-semantic-rename-preview
                 runtime one-target '((line . 1) (column . 1))
                 "renamed" revision))
               (preview-id (alist-get 'preview_id preview)))
          (with-current-buffer
              (emacs-agent-document-buffer two-document)
            (erase-buffer)
            (insert "human\n"))
          (condition-case error-data
              (progn
                (emacs-agent-semantic-rename-apply runtime preview-id)
                (ert-fail "Expected revision_conflict"))
            (emacs-agent-error
             (should
              (eq (emacs-agent-error-code error-data)
                  'revision_conflict))))
          (with-current-buffer
              (emacs-agent-document-buffer one-document)
            (should (equal (buffer-string) "target();\n")))
          (with-current-buffer
              (emacs-agent-document-buffer two-document)
            (should (equal (buffer-string) "human\n"))))))))

(ert-deftest emacs-agent-semantic-rename-project-preview-keeps-context ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "target();\n" nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (target
            (emacs-agent-semantic-test--project-target
             runtime project-id "sample.el"))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (workspace-edit
            (emacs-agent-semantic-test--workspace-edit path 6 "renamed")))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _) workspace-edit)))
        (let* ((preview
                (emacs-agent-semantic-rename-preview
                 runtime target '((line . 1) (column . 1))
                 "renamed" revision))
               (entry (car (alist-get 'documents preview))))
          (should (equal (plist-get entry :path) (file-truename path)))
          (should (equal (plist-get entry :project_id) project-id))
          (should (equal (plist-get entry :relative_path) "sample.el")))))))

(ert-deftest emacs-agent-semantic-lsp-external-path-is-policy-checked ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((outside (make-temp-file "emacs-agent-semantic-outside-" nil ".el"))
           (path (expand-file-name "sample.el" root))
           (_ (write-region "target();\n" nil path))
           (_ (write-region "target();\n" nil outside))
           (_ (setf
               (emacs-agent-runtime-filesystem-policy runtime) 'allowlist
               (emacs-agent-runtime-allowed-roots runtime) (list root)))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (workspace-edit
            (emacs-agent-semantic-test--workspace-edit
             outside 6 "renamed")))
      (unwind-protect
          (cl-letf (((symbol-function 'eglot-current-server)
                     (lambda (&optional _prompt) 'server))
                    ((symbol-function 'eglot-uri-to-path)
                     (lambda (uri) (substring uri 7)))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _) workspace-edit)))
            (condition-case error-data
                (progn
                  (emacs-agent-semantic-rename-preview
                   runtime target '((line . 1) (column . 1))
                   "renamed" revision)
                  (ert-fail "Expected policy rejection"))
              (emacs-agent-error
               (should
                (eq (emacs-agent-error-code error-data)
                    'path_not_allowed)))))
        (delete-file outside)))))

(ert-deftest emacs-agent-semantic-code-actions-are-runtime-scoped ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "bad\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (edit
            (emacs-agent-semantic-test--workspace-edit path 3 "good"))
           (server-actions
            `[
              (:title "Fix text" :kind "quickfix" :isPreferred t
               :edit ,edit)
              (:title "Run generator" :kind "source"
               :command (:title "Run" :command "danger.generate"))
              (:title "Fix and run" :kind "quickfix"
               :edit ,edit
               :command (:title "Run" :command "danger.generate"))
              ]))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (_server method _params &rest _)
                   (should (eq method :textDocument/codeAction))
                   server-actions)))
        (let* ((result
                (emacs-agent-semantic-code-actions
                 runtime target
                 '((start . ((line . 1) (column . 0)))
                   (end . ((line . 1) (column . 3))))
                 revision))
               (actions (alist-get 'actions result))
               (pure (car actions))
               (command (cadr actions))
               (mixed (caddr actions))
               (other-runtime (emacs-agent-runtime-create)))
          (should (equal (alist-get 'classification pure) "edit"))
          (should (eq (alist-get 'requires_approval pure) :false))
          (should (equal
                   (alist-get 'classification command) "command"))
          (should
           (equal
            (alist-get 'classification mixed) "edit_and_command"))
          (condition-case error-data
              (progn
                (emacs-agent-semantic-code-action-apply
                 other-runtime (alist-get 'action_id pure))
                (ert-fail "Action must not cross runtimes"))
            (emacs-agent-error
             (should
              (eq (emacs-agent-error-code error-data)
                  'revision_conflict))))
          (condition-case error-data
              (progn
                (emacs-agent-semantic-code-action-apply
                 runtime (alist-get 'action_id command))
                (ert-fail "Commands must never execute"))
            (emacs-agent-error
             (should
              (eq (emacs-agent-error-code error-data)
                  'approval_required))))
          (emacs-agent-semantic-code-action-apply
           runtime (alist-get 'action_id pure))
          (with-current-buffer (emacs-agent-document-buffer document)
            (should (equal (buffer-string) "good\n"))))))))

(ert-deftest emacs-agent-semantic-code-action-project-preview-keeps-context ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "bad\n" nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (target
            (emacs-agent-semantic-test--project-target
             runtime project-id "sample.el"))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (edit
            (emacs-agent-semantic-test--workspace-edit path 3 "good")))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _)
                   (vector
                    (list :title "Fix text" :kind "quickfix"
                          :edit edit)))))
        (let* ((result
                (emacs-agent-semantic-code-actions
                 runtime target
                 '((start . ((line . 1) (column . 0)))
                   (end . ((line . 1) (column . 3))))
                 revision))
               (entry
                (car
                 (alist-get
                  'documents
                  (car (alist-get 'actions result))))))
          (should (equal (plist-get entry :project_id) project-id))
          (should
           (equal (plist-get entry :relative_path) "sample.el")))))))

(ert-deftest emacs-agent-semantic-format-supports-direct-and-project-targets ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.txt" root))
           (_ (write-region "alpha  beta\n" nil path))
           (direct
            (emacs-agent-semantic-test--direct-target runtime path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (project
            (emacs-agent-semantic-test--project-target
             runtime project-id "sample.txt"))
           (document (emacs-agent-document-open runtime direct))
           (revision (emacs-agent-document-revision document))
           (emacs-agent-semantic-format-function
            (lambda (content _major-mode)
              (string-replace "  " " " content)))
           (direct-preview
            (emacs-agent-semantic-format-preview
             runtime direct revision))
           (project-preview
            (emacs-agent-semantic-format-preview
             runtime project revision)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "alpha  beta\n")))
      (should (equal (alist-get 'path direct-preview)
                     (file-truename path)))
      (should-not (alist-get 'project_id direct-preview))
      (should (equal (alist-get 'project_id project-preview) project-id))
      (should (equal
               (alist-get 'relative_path project-preview) "sample.txt"))
      (emacs-agent-semantic-format-apply runtime project revision)
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "alpha beta\n"))))))

(ert-deftest emacs-agent-semantic-format-range-supports-project-target ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "x= 1\n" nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (target
            (emacs-agent-semantic-test--project-target
             runtime project-id "sample.el"))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (server-edits
            [(:range (:start (:line 0 :character 0)
                      :end (:line 0 :character 4))
              :newText "x = 1")]))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (_server method _params &rest _)
                   (should (eq method :textDocument/rangeFormatting))
                   server-edits)))
        (let* ((preview
                (emacs-agent-semantic-format-range-preview
                 runtime target
                 '((start . ((line . 1) (column . 0)))
                   (end . ((line . 1) (column . 4))))
                 revision))
               (entry (car (alist-get 'documents preview)))
               (preview-id (alist-get 'preview_id preview)))
          (should (equal (plist-get entry :project_id) project-id))
          (should (equal (plist-get entry :relative_path) "sample.el"))
          (with-current-buffer (emacs-agent-document-buffer document)
            (should (equal (buffer-string) "x= 1\n")))
          (emacs-agent-semantic-format-range-apply runtime preview-id)
          (with-current-buffer (emacs-agent-document-buffer document)
            (should (equal (buffer-string) "x = 1\n"))))))))

(ert-deftest emacs-agent-semantic-format-range-supports-direct-target ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "x= 1\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (server-edits
            [(:range (:start (:line 0 :character 0)
                      :end (:line 0 :character 4))
              :newText "x = 1")]))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _) server-edits)))
        (let* ((preview
                (emacs-agent-semantic-format-range-preview
                 runtime target
                 '((start . ((line . 1) (column . 0)))
                   (end . ((line . 1) (column . 4))))
                 revision))
               (entry (car (alist-get 'documents preview))))
          (should (equal (plist-get entry :path) (file-truename path)))
          (should-not (plist-get entry :project_id))
          (should-not (plist-get entry :relative_path)))))))

(ert-deftest emacs-agent-semantic-preview-rechecks-policy-before-apply ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "target();\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (workspace-edit
            (emacs-agent-semantic-test--workspace-edit path 6 "renamed")))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _) workspace-edit)))
        (let* ((preview
                (emacs-agent-semantic-rename-preview
                 runtime target '((line . 1) (column . 1))
                 "renamed" revision))
               (preview-id (alist-get 'preview_id preview)))
          (setf
           (emacs-agent-runtime-denied-paths runtime)
           (list
            (emacs-agent-resolved-target-canonical-path target)))
          (condition-case error-data
              (progn
                (emacs-agent-semantic-rename-apply runtime preview-id)
                (ert-fail "Apply must recheck the path policy"))
            (emacs-agent-error
             (should
              (eq (emacs-agent-error-code error-data)
                  'path_denied)))))))))

(ert-deftest emacs-agent-semantic-mutations-fail-closed-without-eglot ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.txt" root))
           (_ (write-region "target\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document)))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) nil)))
        (condition-case error-data
            (progn
              (emacs-agent-semantic-rename-preview
               runtime target '((line . 1) (column . 0))
               "renamed" revision)
              (ert-fail "Expected capability_unavailable"))
          (emacs-agent-error
           (should
            (eq (emacs-agent-error-code error-data)
                'capability_unavailable))
           (should
            (eq
             (plist-get
              (emacs-agent-error-details error-data) :capability)
             'symbol_rename))))))))

(ert-deftest emacs-agent-semantic-editor-context-never-infers-project ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.txt" root))
           (_ (write-region "first line\n" nil path))
           (project-id
            (plist-get (emacs-agent-project-open runtime root) :project_id))
           (buffer (find-file-noselect path)))
      (let* ((direct
              (emacs-agent-semantic-editor-context runtime buffer))
             (direct-buffer (alist-get 'buffer direct))
             (project
              (emacs-agent-semantic-editor-context
               runtime buffer project-id))
             (project-buffer (alist-get 'buffer project)))
        (should (equal (alist-get 'path direct-buffer)
                       (file-truename path)))
        (should-not (alist-get 'project_id direct-buffer))
        (should-not (alist-get 'relative_path direct-buffer))
        (should (equal (alist-get 'project_id project-buffer) project-id))
        (should (equal
                 (alist-get 'relative_path project-buffer) "sample.txt"))))))

(ert-deftest emacs-agent-semantic-editor-context-redacts-sensitive-buffers ()
  (emacs-agent-semantic-test--with-runtime
    (dolist (case '(("secret" . "sensitive_buffer")
                    (" *Minibuf-test*" . "minibuffer")))
      (let ((buffer (generate-new-buffer (car case))))
        (unwind-protect
            (with-current-buffer buffer
              (unless (string-prefix-p " *Minibuf-" (buffer-name))
                (setq-local emacs-agent-semantic-sensitive-buffer t))
              (insert "must not be exposed")
              (let ((context
                     (emacs-agent-semantic-editor-context
                      runtime buffer)))
                (should (eq (alist-get 'redacted context) t))
                (should
                 (equal
                  (alist-get 'redaction_reason context) (cdr case)))
                (should-not (assoc 'buffer context))
                (should-not (assoc 'point context))))
          (kill-buffer buffer))))))

(ert-deftest emacs-agent-semantic-clear-invalidates-runtime-state ()
  (emacs-agent-semantic-test--with-runtime
    (let* ((path (expand-file-name "sample.el" root))
           (_ (write-region "target();\n" nil path))
           (target
            (emacs-agent-semantic-test--direct-target runtime path))
           (document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (workspace-edit
            (emacs-agent-semantic-test--workspace-edit path 6 "renamed")))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _) workspace-edit)))
        (let* ((preview
                (emacs-agent-semantic-rename-preview
                 runtime target '((line . 1) (column . 1))
                 "renamed" revision))
               (preview-id (alist-get 'preview_id preview)))
          (emacs-agent-semantic-clear runtime)
          (condition-case error-data
              (progn
                (emacs-agent-semantic-rename-apply runtime preview-id)
                (ert-fail "Cleared preview must be invalid"))
            (emacs-agent-error
             (should
              (eq (emacs-agent-error-code error-data)
                  'revision_conflict)))))))))

(ert-deftest emacs-agent-semantic-runtime-capabilities-use-project-tool-name ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (add-hook 'xref-backend-functions
              (lambda () 'emacs-agent-semantic-test-backend)
              nil t)
    (let* ((report
            (emacs-agent-semantic-runtime-capabilities (current-buffer)))
           (supported (alist-get 'supported_tools report)))
      (should
       (equal
        supported
        '("emacs_agent_document_symbols"
          "emacs_agent_project_symbols"
          "emacs_agent_symbol_definition"
          "emacs_agent_symbol_references"
          "emacs_agent_editor_context_get"
          "emacs_agent_format_document"
          "emacs_agent_symbol_rename"
          "emacs_agent_code_actions"
          "emacs_agent_format_range"))))))

(ert-deftest emacs-agent-semantic-runtime-capabilities-reject-etags-without-tags ()
  "A README buffer must not advertise unusable fallback etags support."
  (let ((root (make-temp-file "emacs-agent-semantic-gfm-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "README.md" root)
                default-directory (file-name-as-directory root))
          (emacs-agent-semantic-test-gfm-mode)
          (setq-local tags-file-name nil)
          (setq-local tags-table-list nil)
          (setq-local default-tags-table-function nil)
          (setq-local xref-backend-functions
                      (list #'etags--xref-backend))
          (let* ((report
                  (emacs-agent-semantic-runtime-capabilities
                   (current-buffer)))
                 (providers (alist-get 'providers report))
                 (xref (alist-get 'xref providers))
                 (availability (alist-get 'tool_availability report)))
            (should (eq (alist-get 'backend_present xref) t))
            (should (equal (alist-get 'provider xref) "etags"))
            (should (eq (alist-get 'noninteractive_ready xref) :false))
            (should (eq (alist-get 'available xref) :false))
            (dolist (tool '("emacs_agent_project_symbols"
                            "emacs_agent_symbol_definition"
                            "emacs_agent_symbol_references"))
              (should
               (eq
                (alist-get
                 'available
                 (seq-find
                  (lambda (entry)
                    (equal (alist-get 'tool entry) tool))
                  availability))
                :false)))))
      (delete-directory root t))))

(ert-deftest emacs-agent-semantic-runtime-capabilities-keep-hooked-etags-on-nil-backend ()
  "Emacs 31+ etags returns nil without TAGS; hooked etags stays present."
  (let ((root (make-temp-file "emacs-agent-semantic-gfm-nil-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "README.md" root)
                default-directory (file-name-as-directory root))
          (emacs-agent-semantic-test-gfm-mode)
          (setq-local tags-file-name nil)
          (setq-local tags-table-list nil)
          (setq-local default-tags-table-function nil)
          (setq-local xref-backend-functions
                      (list #'etags--xref-backend))
          (cl-letf (((symbol-function #'etags--xref-backend)
                     (lambda () nil))
                    ((symbol-function #'xref-find-backend)
                     (lambda ()
                       (run-hook-with-args-until-success
                        'xref-backend-functions))))
            (let* ((report
                    (emacs-agent-semantic-runtime-capabilities
                     (current-buffer)))
                   (xref (alist-get 'xref (alist-get 'providers report))))
              (should (eq (alist-get 'backend_present xref) t))
              (should (equal (alist-get 'provider xref) "etags"))
              (should (eq (alist-get 'available xref) :false))
              (should
               (eq (alist-get 'noninteractive_ready xref) :false)))))
      (delete-directory root t))))

(ert-deftest emacs-agent-semantic-runtime-capabilities-do-not-query-eglot ()
  (with-temp-buffer
    (fundamental-mode)
    (let ((emacs-agent-semantic-format-function nil))
      (cl-letf (((symbol-function 'xref-find-backend)
                 (lambda () nil))
                ((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) nil))
                ((symbol-function 'eglot--request)
                 (lambda (&rest _)
                   (ert-fail "Capability reporting must not request LSP"))))
        (let* ((report
                (emacs-agent-semantic-runtime-capabilities
                 (current-buffer)))
               (providers (alist-get 'providers report)))
          (dolist (provider '(imenu xref eglot trusted_formatter))
            (should
             (eq
              (alist-get
               'available (alist-get provider providers))
              :false))))))))

(provide 'emacs-agent-semantic-test)
;;; emacs-agent-semantic-test.el ends here
