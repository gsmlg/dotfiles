;;; emacs-agent-policy.el --- Workspace path policy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Canonical, fail-closed path resolution for Emacs Agent Editor.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function emacs-agent-workspace-root "emacs-agent-workspace")

(define-error 'emacs-agent-error "Emacs Agent Editor error")

(defgroup emacs-agent-editor nil
  "Buffer-first editor services for software agents."
  :group 'tools)

(defcustom emacs-agent-policy-maximum-document-bytes (* 4 1024 1024)
  "Largest on-disk document accepted by the editor."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-policy-denied-basenames
  '(".authinfo" ".authinfo.gpg" ".netrc"
    "credentials" "credentials.json"
    "id_dsa" "id_ecdsa" "id_ed25519" "id_rsa")
  "File basenames denied independently of their directory."
  :type '(repeat string)
  :group 'emacs-agent-editor)

(defcustom emacs-agent-policy-denied-extensions
  '("key" "p12" "pfx" "pem")
  "Credential-bearing file extensions denied by default."
  :type '(repeat string)
  :group 'emacs-agent-editor)

(defun emacs-agent-signal (code &rest details)
  "Signal an editor error identified by CODE with DETAILS."
  (signal 'emacs-agent-error (list code details)))

(defun emacs-agent-error-code (error-data)
  "Return the stable error code from ERROR-DATA."
  (cadr error-data))

(defun emacs-agent-error-details (error-data)
  "Return structured details from ERROR-DATA."
  (caddr error-data))

(defun emacs-agent-policy--root (workspace)
  "Extract and canonicalize the root represented by WORKSPACE."
  (let ((root
         (cond
          ((stringp workspace) workspace)
          ((and (fboundp 'emacs-agent-workspace-p)
                (emacs-agent-workspace-p workspace))
           (or (and (fboundp 'emacs-agent-workspace-canonical-root)
                    (emacs-agent-workspace-canonical-root workspace))
               (emacs-agent-workspace-root workspace)))
          (t
           (emacs-agent-signal 'workspace_not_bound
                               :message "No workspace is bound")))))
    (when (file-remote-p root)
      (emacs-agent-signal 'path_denied :path root :reason 'remote))
    (unless (file-directory-p root)
      (emacs-agent-signal 'workspace_not_bound :root root))
    (file-name-as-directory (file-truename root))))

(defun emacs-agent-policy--nearest-existing-parent (path)
  "Return the nearest existing parent of PATH."
  (let ((candidate (directory-file-name path))
        parent)
    (while (and (not (file-exists-p candidate))
                (setq parent (file-name-directory candidate))
                (not (equal candidate (directory-file-name parent))))
      (setq candidate (directory-file-name parent)))
    (and (file-exists-p candidate) candidate)))

(defun emacs-agent-policy--canonical-missing-path (absolute)
  "Canonicalize ABSOLUTE through its nearest existing ancestor."
  (let* ((existing (emacs-agent-policy--nearest-existing-parent absolute))
         (existing (or existing
                       (emacs-agent-signal 'path_outside_root
                                           :path absolute)))
         (relative (file-relative-name absolute
                                       (if (file-directory-p existing)
                                           existing
                                         (file-name-directory existing))))
         (base (if (file-directory-p existing)
                   (file-truename existing)
                 (file-name-directory (file-truename existing)))))
    (expand-file-name relative base)))

(defun emacs-agent-policy--inside-root-p (path root)
  "Return non-nil when PATH is strictly inside ROOT."
  (and (not (equal (directory-file-name path)
                   (directory-file-name root)))
       (file-in-directory-p path root)))

(defun emacs-agent-policy--denied-relative-p (relative)
  "Return a denial reason for workspace-relative RELATIVE, or nil."
  (let* ((components (split-string relative "/" t))
         (basename (car (last components)))
         (extension (and basename (file-name-extension basename))))
    (cond
     ((member ".git" components) 'git-metadata)
     ((and basename
           (or (string= basename ".env")
               (string-prefix-p ".env." basename)))
      'environment-secret)
     ((member basename emacs-agent-policy-denied-basenames)
      'credential-file)
     ((and extension
           (member (downcase extension)
                   emacs-agent-policy-denied-extensions))
      'credential-file))))

(defun emacs-agent-policy--workspace-patterns (workspace accessor)
  "Return path patterns from WORKSPACE using ACCESSOR, when available."
  (and (not (stringp workspace))
       (fboundp accessor)
       (funcall accessor workspace)))

(defun emacs-agent-policy--matches-pattern-p (relative pattern)
  "Return non-nil when RELATIVE matches glob or regexp PATTERN."
  (cond
   ((stringp pattern)
    (string-match-p (wildcard-to-regexp pattern) relative))
   ((functionp pattern) (funcall pattern relative))))

;;;###autoload
(defun emacs-agent-policy-resolve (workspace path &optional for-create)
  "Resolve PATH inside WORKSPACE and enforce boundary and deny rules.

PATH must be relative.  When FOR-CREATE is non-nil, resolve a missing leaf
through its nearest existing parent."
  (unless (and (stringp path) (not (string-empty-p path)))
    (emacs-agent-signal 'path_denied :path path :reason 'invalid))
  (when (or (file-name-absolute-p path) (file-remote-p path))
    (emacs-agent-signal 'path_outside_root :path path))
  (when (string-match-p "\\(?:\\`\\|/\\)\\.\\.\\(?:/\\|\\'\\)" path)
    (emacs-agent-signal 'path_outside_root :path path))
  (let* ((root (emacs-agent-policy--root workspace))
         (expanded (expand-file-name path root))
         (canonical
          (cond
           ((file-exists-p expanded) (file-truename expanded))
           (for-create (emacs-agent-policy--canonical-missing-path expanded))
           (t expanded)))
         (relative (file-relative-name canonical root))
         (denial (emacs-agent-policy--denied-relative-p relative))
         (denied
          (emacs-agent-policy--workspace-patterns
           workspace 'emacs-agent-workspace-denied-paths))
         (allowed
          (emacs-agent-policy--workspace-patterns
           workspace 'emacs-agent-workspace-allowed-paths)))
    (unless (emacs-agent-policy--inside-root-p canonical root)
      (emacs-agent-signal 'path_outside_root :path path))
    (when denial
      (emacs-agent-signal 'path_denied :path path :reason denial))
    (when (cl-some (lambda (pattern)
                     (emacs-agent-policy--matches-pattern-p relative pattern))
                   denied)
      (emacs-agent-signal 'path_denied :path path :reason 'configured-deny))
    (when (and allowed
               (not (cl-some
                     (lambda (pattern)
                       (emacs-agent-policy--matches-pattern-p
                        relative pattern))
                     allowed)))
      (emacs-agent-signal 'path_denied :path path :reason 'not-allowed))
    canonical))

(defun emacs-agent-policy--binary-file-p (path)
  "Return non-nil when the prefix of PATH has a NUL byte."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 (min 8192
                                                   (file-attribute-size
                                                    (file-attributes path))))
    (search-forward (string 0) nil t)))

;;;###autoload
(defun emacs-agent-policy-assert-document
    (workspace path &optional for-create max-bytes)
  "Resolve PATH and assert it is an allowed regular text document.

WORKSPACE and FOR-CREATE have the meaning used by
`emacs-agent-policy-resolve'.  MAX-BYTES defaults to
`emacs-agent-policy-maximum-document-bytes'."
  (let* ((canonical (emacs-agent-policy-resolve workspace path for-create))
         (attributes (and (file-exists-p canonical)
                          (file-attributes canonical 'integer))))
    (when (and attributes (not (file-regular-p canonical)))
      (emacs-agent-signal 'unsupported_document_type
                          :path path :reason 'special-file))
    (when (and attributes
               (> (file-attribute-size attributes)
                  (or max-bytes emacs-agent-policy-maximum-document-bytes)))
      (emacs-agent-signal 'document_too_large :path path))
    (when (and attributes (emacs-agent-policy--binary-file-p canonical))
      (emacs-agent-signal 'unsupported_document_type
                          :path path :reason 'binary))
    canonical))

(provide 'emacs-agent-policy)
;;; emacs-agent-policy.el ends here
