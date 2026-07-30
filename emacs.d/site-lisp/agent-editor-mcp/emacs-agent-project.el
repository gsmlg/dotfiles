;;; emacs-agent-project.el --- Optional project registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Explicit project registration for project-scoped search and semantic
;; services.  Projects provide context only and never own documents.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'emacs-agent-policy)
(require 'emacs-agent-runtime)

(cl-defstruct (emacs-agent-project
               (:constructor emacs-agent-project--make))
  project-id
  root
  canonical-root
  project-object
  name
  type
  native-p
  opened-at)

(defun emacs-agent-project--canonical-root (runtime root)
  "Validate and canonicalize project ROOT for RUNTIME."
  (emacs-agent-policy-authorize-project-root runtime root))

(defun emacs-agent-project--name (canonical-root)
  "Return a display name for CANONICAL-ROOT."
  (let ((name
         (file-name-nondirectory
          (directory-file-name canonical-root))))
    (if (string-empty-p name)
        canonical-root
      name)))

(defun emacs-agent-project--native-object (canonical-root)
  "Return a native project object for CANONICAL-ROOT, when detected."
  (let ((default-directory canonical-root))
    (when-let* ((project (project-current nil canonical-root))
                (root (project-root project)))
      (and (file-equal-p root canonical-root)
           project))))

(defun emacs-agent-project--public (project opened)
  "Return public metadata for PROJECT with OPENED status."
  (list
   :project_id (emacs-agent-project-project-id project)
   :root (emacs-agent-project-canonical-root project)
   :name (emacs-agent-project-name project)
   :type (emacs-agent-project-type project)
   :native_project
   (and (emacs-agent-project-native-p project) t)
   :opened (and opened t)))

(defun emacs-agent-project-get (runtime project-id)
  "Return PROJECT-ID from RUNTIME or signal `project_not_found'."
  (unless (and (emacs-agent-runtime-p runtime)
               (stringp project-id)
               (not (string-empty-p project-id)))
    (emacs-agent-signal 'project_not_found :project_id project-id))
  (or
   (gethash project-id
            (emacs-agent-runtime-project-registry runtime))
   (emacs-agent-signal
    'project_not_found :project_id project-id)))

(defun emacs-agent-project-find-by-root (runtime root)
  "Return the active project registered for canonical ROOT in RUNTIME."
  (let* ((canonical
          (emacs-agent-project--canonical-root runtime root))
         (project-id
          (gethash
           canonical
           (emacs-agent-runtime-project-root-index runtime))))
    (and
     project-id
     (gethash
      project-id
      (emacs-agent-runtime-project-registry runtime)))))

;;;###autoload
(defun emacs-agent-project-open (runtime root)
  "Register absolute local directory ROOT in RUNTIME.
Repeated registration of the same canonical root is idempotent."
  (unless (emacs-agent-runtime-p runtime)
    (emacs-agent-signal 'runtime_not_started))
  (let* ((canonical
          (emacs-agent-project--canonical-root runtime root))
         (root-index
          (emacs-agent-runtime-project-root-index runtime))
         (registry
          (emacs-agent-runtime-project-registry runtime))
         (known-id (gethash canonical root-index))
         (known (and known-id (gethash known-id registry))))
    (if known
        (emacs-agent-project--public known nil)
      (let* ((native-object
              (emacs-agent-project--native-object canonical))
             (project-object
              (or native-object (cons 'transient canonical)))
             (project-id
              (or known-id
                  (emacs-agent-runtime--random-id "project")))
             (project
              (emacs-agent-project--make
               :project-id project-id
               :root canonical
               :canonical-root canonical
               :project-object project-object
               :name (emacs-agent-project--name canonical)
               :type
               (if native-object
                   (format "%s" (car-safe native-object))
                 "directory")
               :native-p (and native-object t)
               :opened-at (float-time))))
        (puthash canonical project-id root-index)
        (puthash project-id project registry)
        (emacs-agent-runtime-record-activity
         runtime
         (list
          :tool "project_open"
          :status "completed"
          :project_id project-id
          :root canonical))
        (emacs-agent-project--public project t)))))

;;;###autoload
(defun emacs-agent-project-list (&optional runtime)
  "Return active projects registered in RUNTIME."
  (let ((runtime (or runtime (emacs-agent-runtime-current)))
        projects)
    (maphash
     (lambda (_project-id project)
       (push
        (emacs-agent-project--public project nil)
        projects))
     (emacs-agent-runtime-project-registry runtime))
    (setq
     projects
     (sort
      projects
      (lambda (left right)
        (string<
         (plist-get left :root)
         (plist-get right :root)))))
    (list
     :projects projects
     :project_count (length projects))))

;;;###autoload
(defun emacs-agent-project-info (runtime project-id)
  "Return public metadata for PROJECT-ID in RUNTIME."
  (emacs-agent-project--public
   (emacs-agent-project-get runtime project-id)
   nil))

(defun emacs-agent-project--managed-document-count (runtime project)
  "Return managed document count for PROJECT in RUNTIME."
  (let ((root (emacs-agent-project-canonical-root project))
        (count 0))
    (maphash
     (lambda (path _document)
       (when (and
              (stringp path)
              (emacs-agent-policy--inside-root-p path root))
         (setq count (1+ count))))
     (emacs-agent-runtime-document-registry runtime))
    count))

;;;###autoload
(defun emacs-agent-project-close (runtime project-id)
  "Unregister PROJECT-ID context from RUNTIME without touching buffers."
  (let* ((project
          (emacs-agent-project-get runtime project-id))
         (managed-count
          (emacs-agent-project--managed-document-count runtime project)))
    (remhash
     project-id
     (emacs-agent-runtime-project-registry runtime))
    (emacs-agent-runtime-record-activity
     runtime
     (list
      :tool "project_close"
      :status "completed"
      :project_id project-id))
    (list
     :project_id project-id
     :closed t
     :managed_document_count managed-count)))

(cl-defun emacs-agent-project-resolve-target
    (runtime path &key project-id for-create)
  "Resolve PATH in RUNTIME with optional explicit PROJECT-ID.
FOR-CREATE permits a missing leaf."
  (if project-id
      (let ((project
             (emacs-agent-project-get runtime project-id)))
        (emacs-agent-policy-resolve-target
         runtime path
         :project-id project-id
         :project-root
         (emacs-agent-project-canonical-root project)
         :for-create for-create))
    (emacs-agent-policy-resolve-target
     runtime path :for-create for-create)))

(provide 'emacs-agent-project)
;;; emacs-agent-project.el ends here
