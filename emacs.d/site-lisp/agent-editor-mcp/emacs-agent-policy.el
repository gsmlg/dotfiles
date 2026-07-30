;;; emacs-agent-policy.el --- Runtime path policy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Canonical, fail-closed path resolution for Emacs Agent Editor.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-agent-runtime)

(define-error 'emacs-agent-error "Emacs Agent Editor error")

(defgroup emacs-agent-editor nil
  "Buffer-first editor services for software agents."
  :group 'tools)

(defcustom emacs-agent-policy-maximum-document-bytes (* 4 1024 1024)
  "Largest on-disk document accepted by the editor."
  :type 'integer
  :group 'emacs-agent-editor)

(defcustom emacs-agent-policy-filesystem-scope 'unrestricted
  "Default filesystem scope for a new editor runtime.
`unrestricted' permits local files subject to denial rules.  `allowlist'
requires canonical paths to be inside `emacs-agent-policy-allowed-roots'."
  :type '(choice (const unrestricted) (const allowlist))
  :group 'emacs-agent-editor)

(defcustom emacs-agent-policy-allowed-roots nil
  "Canonical root directories permitted in `allowlist' scope."
  :type '(repeat directory)
  :group 'emacs-agent-editor)

(defcustom emacs-agent-policy-denied-paths nil
  "Additional absolute path globs or predicate functions to deny."
  :type '(repeat
          (choice string
                  (function :tag "Predicate")))
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

(cl-defstruct (emacs-agent-resolved-target
               (:constructor emacs-agent-resolved-target--make))
  input-path canonical-path project-id relative-path)

(defun emacs-agent-signal (code &rest details)
  "Signal an editor error identified by CODE with DETAILS."
  (signal 'emacs-agent-error (list code details)))

(defun emacs-agent-error-code (error-data)
  "Return the stable error code from ERROR-DATA."
  (cadr error-data))

(defun emacs-agent-error-details (error-data)
  "Return structured details from ERROR-DATA."
  (caddr error-data))

(defun emacs-agent-policy--nearest-existing-parent (path)
  "Return the nearest existing parent of PATH."
  (let ((candidate (directory-file-name path))
        parent)
    (while (and (not (file-exists-p candidate))
                (not (file-symlink-p candidate))
                (setq parent (file-name-directory candidate))
                (not (equal candidate (directory-file-name parent))))
      (setq candidate (directory-file-name parent)))
    (and (or (file-exists-p candidate)
             (file-symlink-p candidate))
         candidate)))

(defun emacs-agent-policy--truename-or-deny (path)
  "Return the canonical target of PATH, or reject an invalid symlink."
  (condition-case nil
      (file-truename path)
    (file-error
     (emacs-agent-signal
      'path_not_allowed :path path :reason 'unresolvable-symlink))))

(defun emacs-agent-policy--canonical-missing-path (absolute)
  "Canonicalize ABSOLUTE through its nearest existing ancestor."
  (let* ((existing (emacs-agent-policy--nearest-existing-parent absolute))
         (existing (or existing
                       (emacs-agent-signal 'path_not_allowed
                                           :path absolute)))
         (same-path
          (equal (directory-file-name absolute)
                 (directory-file-name existing)))
         (base (emacs-agent-policy--truename-or-deny existing)))
    (cond
     (same-path base)
     ((or (file-directory-p existing)
          (file-symlink-p existing))
      (expand-file-name
       (file-relative-name
        absolute (file-name-as-directory existing))
       (file-name-as-directory base)))
     (t
      (emacs-agent-signal
       'path_not_allowed :path absolute :reason 'non-directory-parent)))))

(defun emacs-agent-policy--inside-root-p (path root)
  "Return non-nil when PATH is strictly inside ROOT."
  (and (not (equal (directory-file-name path)
                   (directory-file-name root)))
       (file-in-directory-p path root)))

(defun emacs-agent-policy--within-root-p (path root)
  "Return non-nil when PATH is ROOT or is contained by ROOT."
  (or (equal (directory-file-name path)
             (directory-file-name root))
      (file-in-directory-p path root)))

(defun emacs-agent-policy--denied-relative-p (relative)
  "Return a denial reason for path RELATIVE, or nil."
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

(defun emacs-agent-policy--matches-pattern-p (relative pattern)
  "Return non-nil when RELATIVE matches glob or regexp PATTERN."
  (cond
   ((stringp pattern)
    (string-match-p (wildcard-to-regexp pattern) relative))
   ((functionp pattern) (funcall pattern relative))))

(defun emacs-agent-policy--runtime-scope (runtime)
  "Return the filesystem scope configured for RUNTIME."
  (or (and (emacs-agent-runtime-p runtime)
           (emacs-agent-runtime-filesystem-policy runtime))
      emacs-agent-policy-filesystem-scope))

(defun emacs-agent-policy--runtime-allowed-roots (runtime)
  "Return configured allowlist roots for RUNTIME."
  (or (and (emacs-agent-runtime-p runtime)
           (emacs-agent-runtime-allowed-roots runtime))
      emacs-agent-policy-allowed-roots))

(defun emacs-agent-policy--runtime-denied-paths (runtime)
  "Return configured denied path patterns for RUNTIME."
  (append
   (and (emacs-agent-runtime-p runtime)
        (emacs-agent-runtime-denied-paths runtime))
   emacs-agent-policy-denied-paths))

(defun emacs-agent-policy--canonicalize (path for-create)
  "Return canonical local PATH.
When FOR-CREATE is non-nil, canonicalize a missing leaf through its nearest
existing ancestor."
  (cond
   ((file-symlink-p path)
    (emacs-agent-policy--truename-or-deny path))
   ((file-exists-p path)
    (file-truename path))
   (for-create
    (emacs-agent-policy--canonical-missing-path path))
   (t
    (expand-file-name path))))

(defun emacs-agent-policy--canonical-allowed-root (root)
  "Return canonical directory form of configured allowlist ROOT."
  (file-name-as-directory
   (if (file-exists-p root)
       (file-truename (expand-file-name root))
     (emacs-agent-policy--canonical-missing-path
      (expand-file-name root)))))

(defun emacs-agent-policy--authorize-canonical-path
    (runtime canonical &optional project-relative)
  "Authorize CANONICAL against RUNTIME policy.
PROJECT-RELATIVE is also considered by configured convenience patterns."
  (let* ((scope (emacs-agent-policy--runtime-scope runtime))
         (denial
          (emacs-agent-policy--denied-relative-p canonical))
         (denied-patterns
          (emacs-agent-policy--runtime-denied-paths runtime))
         (allowed-patterns
          (and (emacs-agent-runtime-p runtime)
               (emacs-agent-runtime-allowed-paths runtime))))
    (when denial
      (emacs-agent-signal
       'path_denied :path canonical :reason denial))
    (when
        (cl-some
         (lambda (pattern)
           (or
            (emacs-agent-policy--matches-pattern-p
             canonical pattern)
            (and project-relative
                 (emacs-agent-policy--matches-pattern-p
                  project-relative pattern))))
         denied-patterns)
      (emacs-agent-signal
       'path_denied :path canonical :reason 'configured-deny))
    (when
        (and
         allowed-patterns
         (not
          (cl-some
           (lambda (pattern)
             (or
              (emacs-agent-policy--matches-pattern-p
               canonical pattern)
              (and project-relative
                   (emacs-agent-policy--matches-pattern-p
                    project-relative pattern))))
           allowed-patterns)))
      (emacs-agent-signal
       'path_not_allowed :path canonical :reason 'not-allowed))
    (pcase scope
      ('unrestricted t)
      ('allowlist
       (unless
           (cl-some
            (lambda (root)
              (emacs-agent-policy--within-root-p
               canonical
               (emacs-agent-policy--canonical-allowed-root root)))
            (emacs-agent-policy--runtime-allowed-roots runtime))
         (emacs-agent-signal 'path_not_allowed :path canonical)))
      (_
       (emacs-agent-signal
        'path_not_allowed :path canonical :reason 'invalid-policy)))
    canonical))

(defun emacs-agent-policy-authorize-project-root (runtime root)
  "Authorize local directory ROOT for registration in RUNTIME."
  (unless (and (stringp root) (not (string-empty-p root)))
    (emacs-agent-signal 'project_path_required :root root))
  (when (file-remote-p root)
    (emacs-agent-signal 'remote_path_unsupported :path root))
  (unless (file-name-absolute-p root)
    (emacs-agent-signal 'project_path_required :root root))
  (unless (file-directory-p root)
    (emacs-agent-signal 'project_path_required :root root))
  (let ((canonical
         (file-name-as-directory
          (file-truename (expand-file-name root)))))
    (emacs-agent-policy--authorize-canonical-path runtime canonical)
    canonical))

;;;###autoload
(cl-defun emacs-agent-policy-resolve-target
    (runtime path &key project-id project-root for-create)
  "Resolve PATH for RUNTIME with optional explicit project context.
PROJECT-ID and PROJECT-ROOT must be supplied together.  A relative PATH
requires that context.  FOR-CREATE permits a missing leaf."
  (unless (emacs-agent-runtime-p runtime)
    (emacs-agent-signal 'runtime_not_started))
  (unless (and (stringp path) (not (string-empty-p path)))
    (emacs-agent-signal
     'path_denied :path path :reason 'invalid))
  (when (file-remote-p path)
    (emacs-agent-signal 'remote_path_unsupported :path path))
  (unless (eq (and project-id t) (and project-root t))
    (emacs-agent-signal
     (if project-id 'project_not_found 'project_path_required)
     :project_id project-id))
  (when
      (and
       (not (file-name-absolute-p path))
       (not project-id))
    (emacs-agent-signal 'project_path_required :path path))
  (when
      (and
       project-id
       (not (file-name-absolute-p path))
       (string-match-p
        "\\(?:\\`\\|/\\)\\.\\.\\(?:/\\|\\'\\)"
        path))
    (emacs-agent-signal
     'path_outside_project :path path :project_id project-id))
  (let* ((canonical-root
          (and project-root
               (file-name-as-directory
                (file-truename
                 (expand-file-name project-root)))))
         (expanded
          (expand-file-name path canonical-root))
         (canonical
          (emacs-agent-policy--canonicalize expanded for-create))
         (relative
          (and canonical-root
               (file-relative-name canonical canonical-root))))
    (when
        (and
         canonical-root
         (not
          (emacs-agent-policy--inside-root-p
           canonical canonical-root)))
      (emacs-agent-signal
       'path_outside_project
       :path path :project_id project-id))
    (emacs-agent-policy--authorize-canonical-path
     runtime canonical relative)
    (emacs-agent-resolved-target--make
     :input-path path
     :canonical-path canonical
     :project-id project-id
     :relative-path relative)))

(defun emacs-agent-policy-target-fields (target)
  "Return public path metadata for resolved TARGET."
  (unless (emacs-agent-resolved-target-p target)
    (signal 'wrong-type-argument
            (list 'emacs-agent-resolved-target target)))
  (list
   :path (emacs-agent-resolved-target-canonical-path target)
   :project_id (emacs-agent-resolved-target-project-id target)
   :relative_path
   (emacs-agent-resolved-target-relative-path target)))

(defun emacs-agent-policy-assert-document-target
    (runtime target &optional max-bytes)
  "Assert TARGET is an allowed regular text document in RUNTIME.
MAX-BYTES defaults to `emacs-agent-policy-maximum-document-bytes'."
  (unless (and (emacs-agent-runtime-p runtime)
               (emacs-agent-resolved-target-p target))
    (signal 'wrong-type-argument
            (list 'emacs-agent-resolved-target target)))
  (let* ((canonical
          (emacs-agent-resolved-target-canonical-path target))
         (attributes
          (and (file-exists-p canonical)
               (file-attributes canonical 'integer))))
    (emacs-agent-policy--authorize-canonical-path
     runtime canonical
     (emacs-agent-resolved-target-relative-path target))
    (when (and attributes (not (file-regular-p canonical)))
      (emacs-agent-signal
       'unsupported_document_type
       :path canonical :reason 'special-file))
    (when
        (and
         attributes
         (> (file-attribute-size attributes)
            (or max-bytes
                emacs-agent-policy-maximum-document-bytes)))
      (emacs-agent-signal 'document_too_large :path canonical))
    (when
        (and
         attributes
         (emacs-agent-policy--binary-file-p canonical))
      (emacs-agent-signal
       'unsupported_document_type
       :path canonical :reason 'binary))
    target))

(defun emacs-agent-policy--binary-file-p (path)
  "Return non-nil when the prefix of PATH has a NUL byte."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 (min 8192
                                                   (file-attribute-size
                                                    (file-attributes path))))
    (search-forward (string 0) nil t)))

(provide 'emacs-agent-policy)
;;; emacs-agent-policy.el ends here
