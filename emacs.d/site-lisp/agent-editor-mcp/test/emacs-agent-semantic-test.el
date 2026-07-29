;;; emacs-agent-semantic-test.el --- Semantic service tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for native Emacs semantic and editor-context adapters.

;;; Code:

(require 'ert)
(require 'emacs-agent-semantic)

(cl-defmethod xref-backend-identifier-at-point
  ((_backend (eql emacs-agent-semantic-test-backend)))
  "sample-function")

(cl-defmethod xref-backend-definitions
  ((_backend (eql emacs-agent-semantic-test-backend)) identifier)
  (when (equal identifier "sample-function")
    (list
     (xref-make
      "sample-function"
      (xref-make-file-location
       (expand-file-name "sample.el" default-directory) 1 7)))))

(cl-defmethod xref-backend-references
  ((_backend (eql emacs-agent-semantic-test-backend)) identifier)
  (when (equal identifier "sample-function")
    (list
     (xref-make
      "sample-function call"
      (xref-make-file-location
       (expand-file-name "sample.el" default-directory) 4 1)))))

(cl-defmethod xref-backend-apropos
  ((_backend (eql emacs-agent-semantic-test-backend)) pattern)
  (when (string-match-p pattern "sample-function")
    (list
     (xref-make
      "sample-function"
      (xref-make-file-location
       (expand-file-name "sample.el" default-directory) 1 7)))))

(defmacro emacs-agent-semantic-test--with-workspace (&rest body)
  "Run BODY in a temporary workspace and clean up visiting buffers."
  (declare (indent 0) (debug t))
  `(let ((root (make-temp-file "emacs-agent-semantic-" t)))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file root)
             (kill-buffer buffer))))
       (delete-directory root t))))

(ert-deftest emacs-agent-semantic-document-symbols-uses-imenu ()
  (emacs-agent-semantic-test--with-workspace
    (write-region
     "(defun sample-function (argument)\n  argument)\n\n(defvar sample-variable 1)\n"
     nil (expand-file-name "sample.el" root))
    (let* ((symbols
            (emacs-agent-semantic-document-symbols root "sample.el"))
           (function
            (seq-find
             (lambda (symbol)
               (equal (alist-get 'name symbol) "sample-function"))
             symbols))
           (variables
            (seq-find
             (lambda (symbol)
               (equal (alist-get 'name symbol) "Variables"))
             symbols))
           (variable
            (seq-find
             (lambda (symbol)
               (equal (alist-get 'name symbol) "sample-variable"))
             (alist-get 'children variables))))
      (should function)
      (should variable)
      (should (equal (alist-get 'kind function) "function"))
      (should (equal (alist-get 'container variable) "Variables"))
      (should (equal (alist-get 'source function) "imenu"))
      (should (equal
               (alist-get 'start (alist-get 'selection_range function))
               '((line . 1) (column . 7)))))))

(ert-deftest emacs-agent-semantic-definition-uses-native-xref ()
  (emacs-agent-semantic-test--with-workspace
    (write-region
     "(defun sample-function (argument)\n  argument)\n"
     nil (expand-file-name "sample.el" root))
    (let* ((document (emacs-agent-document-open root "sample.el"))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (add-hook 'xref-backend-functions
                  (lambda () 'emacs-agent-semantic-test-backend)
                  nil t))
      (let* ((locations
              (emacs-agent-semantic-definition
               root "sample.el" '((line . 1) (column . 8))))
             (location (car locations)))
        (should (= (length locations) 1))
        (should (equal (alist-get 'path location) "sample.el"))
        (should (equal (alist-get 'preview location)
                       "(defun sample-function (argument)"))
        (should (equal (alist-get 'source location) "buffer"))
        (should (string-prefix-p "rev:" (alist-get 'revision location)))
        (should
         (equal
          (alist-get 'start (alist-get 'range location))
          '((line . 1) (column . 7))))))))

(ert-deftest emacs-agent-semantic-references-mark-xref-results-incomplete ()
  (emacs-agent-semantic-test--with-workspace
    (write-region
     "(defun sample-function (argument)\n  argument)\n\n(sample-function 1)\n"
     nil (expand-file-name "sample.el" root))
    (let* ((document (emacs-agent-document-open root "sample.el"))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (add-hook 'xref-backend-functions
                  (lambda () 'emacs-agent-semantic-test-backend)
                  nil t))
      (let* ((result
              (emacs-agent-semantic-references
               root "sample.el" '((line . 4) (column . 2))))
             (reference (car (alist-get 'references result))))
        (should (eq (alist-get 'possibly_incomplete result) t))
        (should (equal (alist-get 'relation reference) "reference"))
        (should (equal (alist-get 'source reference) "buffer"))
        (should (equal
                 (alist-get 'start (alist-get 'range reference))
                 '((line . 4) (column . 1))))))))

(ert-deftest emacs-agent-semantic-workspace-symbols-filters-native-results ()
  (emacs-agent-semantic-test--with-workspace
    (write-region
     "(defun sample-function (argument)\n  argument)\n"
     nil (expand-file-name "sample.el" root))
    (let* ((document (emacs-agent-document-open root "sample.el"))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (add-hook 'xref-backend-functions
                  (lambda () 'emacs-agent-semantic-test-backend)
                  nil t))
      (let* ((result
              (emacs-agent-semantic-workspace-symbols
               root "sample.el" "sample" "function" "sample" 1))
             (symbols (alist-get 'symbols result))
             (symbol (car symbols)))
        (should (= (length symbols) 1))
        (should (equal (alist-get 'summary symbol) "sample-function"))
        (should (equal (alist-get 'kind symbol) "function"))
        (should (eq (alist-get 'possibly_incomplete result) t))))))

(ert-deftest emacs-agent-semantic-xref-fails-closed-without-backend ()
  (emacs-agent-semantic-test--with-workspace
    (write-region "plain text\n" nil (expand-file-name "sample.txt" root))
    (let* ((document (emacs-agent-document-open root "sample.txt"))
           (buffer (emacs-agent-document-buffer document)))
      (with-current-buffer buffer
        (setq-local xref-backend-functions nil))
      (condition-case error-data
          (progn
            (emacs-agent-semantic-definition
             root "sample.txt" '((line . 1) (column . 0)))
            (ert-fail "Expected capability_unavailable"))
        (emacs-agent-error
          (should
          (eq (emacs-agent-error-code error-data)
              'capability_unavailable)))))))

(ert-deftest emacs-agent-semantic-editor-context-reports-metadata-not-content ()
  (emacs-agent-semantic-test--with-workspace
    (write-region "first line\nsecond line\n"
                  nil (expand-file-name "sample.txt" root))
    (let ((buffer (find-file-noselect (expand-file-name "sample.txt" root))))
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (line-beginning-position))
        (goto-char (+ (line-beginning-position) 6))
        (setq mark-active t)
        (let ((context (emacs-agent-semantic-editor-context root buffer)))
          (should (eq (alist-get 'redacted context) :false))
          (should (equal (alist-get 'path (alist-get 'buffer context))
                         "sample.txt"))
          (should (equal (alist-get 'point context)
                         '((line . 2) (column . 6))))
          (should (equal
                   (alist-get 'start (alist-get 'active_region context))
                   '((line . 2) (column . 0))))
          (should-not (assoc 'content context)))))))

(ert-deftest emacs-agent-semantic-editor-context-redacts-sensitive-buffers ()
  (emacs-agent-semantic-test--with-workspace
    (dolist (case '(("secret" . "sensitive_buffer")
                    (" *Minibuf-test*" . "minibuffer")))
      (let ((buffer (generate-new-buffer (car case))))
        (unwind-protect
            (with-current-buffer buffer
              (unless (string-prefix-p " *Minibuf-" (buffer-name))
                (setq-local emacs-agent-semantic-sensitive-buffer t))
              (insert "must not be exposed")
              (let ((context
                     (emacs-agent-semantic-editor-context root buffer)))
                (should (eq (alist-get 'redacted context) t))
                (should (equal (alist-get 'redaction_reason context)
                               (cdr case)))
                (should-not (assoc 'buffer context))
                (should-not (assoc 'point context))))
          (kill-buffer buffer))))))

(ert-deftest emacs-agent-semantic-format-previews-then-applies-trusted-output ()
  (emacs-agent-semantic-test--with-workspace
    (write-region "alpha  beta\n" nil (expand-file-name "sample.txt" root))
    (let* ((document (emacs-agent-document-open root "sample.txt"))
           (buffer (emacs-agent-document-buffer document))
           (revision (emacs-agent-document-revision document))
           (emacs-agent-semantic-format-function
            (lambda (content _major-mode)
              (string-replace "  " " " content)))
           (preview
            (emacs-agent-semantic-format-preview
             root "sample.txt" revision)))
      (should (eq (alist-get 'changed preview) t))
      (should (string-match-p "alpha beta" (alist-get 'diff preview)))
      (with-current-buffer buffer
        (should (equal (buffer-string) "alpha  beta\n")))
      (emacs-agent-semantic-format-apply root "sample.txt" revision)
      (with-current-buffer buffer
        (should (equal (buffer-string) "alpha beta\n"))))))

(ert-deftest emacs-agent-semantic-rename-requires-preview-and-applies-atomically ()
  (emacs-agent-semantic-test--with-workspace
    (write-region "target();\n" nil (expand-file-name "one.el" root))
    (write-region "(target)\n" nil (expand-file-name "two.el" root))
    (let* ((workspace (emacs-agent-workspace-create root))
           (document (emacs-agent-document-open workspace "one.el"))
           (second-document
            (emacs-agent-document-open workspace "two.el"))
           (revision (emacs-agent-document-revision document))
           (one-uri
            (concat "file://" (expand-file-name "one.el" root)))
           (two-uri
            (concat "file://" (expand-file-name "two.el" root)))
           (workspace-edit
            `(:documentChanges
              [(:textDocument (:uri ,one-uri)
                :edits
                [(:range (:start (:line 0 :character 0)
                          :end (:line 0 :character 6))
                  :newText "renamed")])
               (:textDocument (:uri ,two-uri)
                :edits
                [(:range (:start (:line 0 :character 1)
                          :end (:line 0 :character 7))
                  :newText "renamed")])])))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri) (substring uri 7)))
                ((symbol-function 'eglot--request)
                 (lambda (_server method _params &rest _)
                   (should (eq method :textDocument/rename))
                   workspace-edit)))
        (let* ((preview
                (emacs-agent-semantic-rename-preview
                 workspace "one.el" '((line . 1) (column . 1))
                 "renamed" revision))
               (preview-id (alist-get 'preview_id preview)))
          (should (stringp preview-id))
          (should (= (length (alist-get 'documents preview)) 2))
          (should (eq (alist-get 'applied preview) :false))
          (should (equal
                   (with-current-buffer
                       (emacs-agent-document-buffer document)
                     (buffer-string))
                   "target();\n"))
          (let ((result
                 (emacs-agent-semantic-rename-apply
                  workspace preview-id)))
            (should (eq (plist-get result :applied) t))
            (should (= (length (plist-get result :documents)) 2))
            (should (equal
                     (with-current-buffer
                         (emacs-agent-document-buffer document)
                       (buffer-string))
                     "renamed();\n"))
            (should (equal
                     (with-current-buffer
                         (emacs-agent-document-buffer second-document)
                       (buffer-string))
                     "(renamed)\n"))))))))

(ert-deftest emacs-agent-semantic-code-actions-classify-and-never-run-commands ()
  (emacs-agent-semantic-test--with-workspace
    (write-region "bad\n" nil (expand-file-name "sample.el" root))
    (let* ((workspace (emacs-agent-workspace-create root))
           (document (emacs-agent-document-open workspace "sample.el"))
           (revision (emacs-agent-document-revision document))
           (uri (concat "file://" (expand-file-name "sample.el" root)))
           (edit
            `(:documentChanges
              [(:textDocument (:uri ,uri)
                :edits
                [(:range (:start (:line 0 :character 0)
                          :end (:line 0 :character 3))
                  :newText "good")])]))
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
                 (lambda (value) (substring value 7)))
                ((symbol-function 'eglot--request)
                 (lambda (_server method _params &rest _)
                   (should (eq method :textDocument/codeAction))
                   server-actions)))
        (let* ((result
                (emacs-agent-semantic-code-actions
                 workspace "sample.el"
                 '((start . ((line . 1) (column . 0)))
                   (end . ((line . 1) (column . 3))))
                 revision))
               (actions (alist-get 'actions result))
               (pure (nth 0 actions))
               (command (nth 1 actions))
               (mixed (nth 2 actions)))
          (should (equal (alist-get 'classification pure) "edit"))
          (should (eq (alist-get 'requires_approval pure) :false))
          (should (equal
                   (alist-get 'classification command) "command"))
          (should (eq (alist-get 'requires_approval command) t))
          (should (equal
                   (alist-get 'classification mixed)
                   "edit_and_command"))
          (condition-case error-data
              (progn
                (emacs-agent-semantic-code-action-apply
                 workspace (alist-get 'action_id command))
                (ert-fail "Commands must never execute"))
            (emacs-agent-error
             (should
              (eq (emacs-agent-error-code error-data)
                  'approval_required))))
          (emacs-agent-semantic-code-action-apply
           workspace (alist-get 'action_id pure))
          (with-current-buffer (emacs-agent-document-buffer document)
            (should (equal (buffer-string) "good\n"))))))))

(ert-deftest emacs-agent-semantic-format-range-previews-native-eglot-edits ()
  (emacs-agent-semantic-test--with-workspace
    (write-region "x= 1\n" nil (expand-file-name "sample.el" root))
    (let* ((workspace (emacs-agent-workspace-create root))
           (document (emacs-agent-document-open workspace "sample.el"))
           (revision (emacs-agent-document-revision document))
           (server-edits
            [(:range (:start (:line 0 :character 0)
                      :end (:line 0 :character 4))
              :newText "x = 1")]))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) 'server))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (value) (substring value 7)))
                ((symbol-function 'eglot--request)
                 (lambda (_server method params &rest _)
                   (should (eq method :textDocument/rangeFormatting))
                   (should (equal
                            (plist-get
                             (plist-get params :range) :start)
                            '(:line 0 :character 0)))
                   server-edits)))
        (let* ((preview
                (emacs-agent-semantic-format-range-preview
                 workspace "sample.el"
                 '((start . ((line . 1) (column . 0)))
                   (end . ((line . 1) (column . 4))))
                 revision))
               (preview-id (alist-get 'preview_id preview)))
          (should (stringp preview-id))
          (should (string-match-p
                   "x = 1"
                   (plist-get
                    (car (alist-get 'documents preview)) :diff)))
          (with-current-buffer (emacs-agent-document-buffer document)
            (should (equal (buffer-string) "x= 1\n")))
          (emacs-agent-semantic-format-range-apply
           workspace preview-id)
          (with-current-buffer (emacs-agent-document-buffer document)
            (should (equal (buffer-string) "x = 1\n"))))))))

(ert-deftest emacs-agent-semantic-mutations-fail-closed-without-eglot ()
  (emacs-agent-semantic-test--with-workspace
    (write-region "target\n" nil (expand-file-name "sample.txt" root))
    (let* ((workspace (emacs-agent-workspace-create root))
           (document (emacs-agent-document-open workspace "sample.txt"))
           (revision (emacs-agent-document-revision document)))
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&optional _prompt) nil)))
        (condition-case error-data
            (progn
              (emacs-agent-semantic-rename-preview
               workspace "sample.txt"
               '((line . 1) (column . 0)) "renamed" revision)
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

(provide 'emacs-agent-semantic-test)
;;; emacs-agent-semantic-test.el ends here
