;;; gsmlg-package-lock.el --- Elpaca lock read, validate, and write -*- lexical-binding: t; -*-

;;; Commentary:
;; Own the committed exact-revision lock file.  Bootstrap loads this module so
;; warm startup validates the lock before package resolution.  Maintenance
;; commands that mutate sources live in `gsmlg-package-maintenance'.

;;; Code:

(require 'cl-lib)
(require 'gsmlg-bootstrap)
(require 'gsmlg-paths)

(defvar elpaca-lock-file)
(defvar elpaca-builds-directory)
(defvar gsmlg-elpaca-revision)
(declare-function gsmlg-bootstrap-wait "gsmlg-bootstrap" ())
(declare-function gsmlg-bootstrap--run "gsmlg-bootstrap"
                  (buffer program &rest arguments))
(declare-function elpaca--queued "elpaca" (&optional items))
(declare-function elpaca<-source-dir "elpaca" (package))
(declare-function elpaca<-recipe "elpaca" (package))
(declare-function elpaca<-id "elpaca" (package))
(declare-function elpaca-write-lock-file "elpaca" (file &optional queue))

(defun gsmlg-package-lock-read ()
  "Read and validate the committed exact-revision Elpaca lock file."
  (unless (file-readable-p elpaca-lock-file)
    (error "Required Elpaca lock file is unreadable: %s" elpaca-lock-file))
  (condition-case error-data
      (with-temp-buffer
        (insert-file-contents elpaca-lock-file)
        (let ((entries (read (current-buffer))))
          (skip-chars-forward " \t\r\n")
          (unless (eobp)
            (error "Unexpected trailing data"))
          (unless (and (listp entries) entries)
            (error "Lock file contains no package entries"))
          (dolist (entry entries)
            (let* ((id (car-safe entry))
                   (recipe (plist-get (cdr-safe entry) :recipe))
                   (revision (and (listp recipe)
                                  (plist-get recipe :ref))))
              (unless (and (symbolp id)
                           (stringp revision)
                           (string-match-p
                            "\\`[[:xdigit:]]\\{40\\}\\'" revision))
                (error "Package %S lacks an exact revision" id))))
          (let* ((elpaca-entry (assq 'elpaca entries))
                 (recipe (plist-get (cdr elpaca-entry) :recipe)))
            (unless (equal (plist-get recipe :ref) gsmlg-elpaca-revision)
              (error "Elpaca lock revision does not match bootstrap pin")))
          entries))
    (error
     (error "Invalid Elpaca lock file %s: %s"
            elpaca-lock-file
            (error-message-string error-data)))))

(defconst gsmlg-elpaca-lock-entries
  (gsmlg-package-lock-read)
  "Validated exact-revision entries from the committed Elpaca lock file.")

(defun gsmlg-package-lock--installed-package-ids ()
  "Return package IDs represented by XDG Elpaca build directories."
  (cl-loop for name in (directory-files elpaca-builds-directory nil
                                         directory-files-no-dot-files-regexp)
           for directory = (expand-file-name name elpaca-builds-directory)
           when (file-directory-p directory)
           collect (intern name)))

(defun gsmlg-package-lock--git-output (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY and return trimmed output."
  (with-temp-buffer
    (let ((default-directory directory))
      (unless (zerop (apply #'process-file "git" nil t nil arguments))
        (error "Git %s failed in %s: %s"
               (string-join arguments " ")
               directory
               (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

;; Hosted Git commit archives record their source commit in pax_global_header,
;; but Elpaca 0.12's tar ref parser misclassifies that header.  Read the
;; immutable commit directly so lock writing verifies source contents instead
;; of merely trusting the recipe.  Existing Git source directories are accepted
;; as a one-time transport migration and verified through Git.
(defun gsmlg-package-lock--archive-header-revision (source)
  "Return the exact commit recorded by the archive below SOURCE."
  (let ((header (expand-file-name "pax_global_header" source)))
    (unless (file-readable-p header)
      (error "Immutable package source lacks pax_global_header: %s" source))
    (with-temp-buffer
      (insert-file-contents-literally header)
      (goto-char (point-min))
      (unless
          (re-search-forward
           "comment=\\([[:xdigit:]]\\{40\\}\\)" nil t)
        (error "Immutable package header lacks an exact commit: %s" header))
      (match-string-no-properties 1))))

(defun gsmlg-package-lock-source-revision (package)
  "Return the verified source revision for Elpaca PACKAGE."
  (let* ((recipe (elpaca<-recipe package))
         (source (elpaca<-source-dir package))
         (type (plist-get recipe :type))
         (expected (plist-get recipe :ref))
         (actual
          (if (file-directory-p (expand-file-name ".git" source))
              (gsmlg-package-lock--git-output source "rev-parse" "HEAD")
            (if (eq type 'tar)
                (gsmlg-package-lock--archive-header-revision source)
              (error "Package %S has no verifiable source revision"
                     (elpaca<-id package))))))
    (when (and (eq type 'tar)
               (not (equal actual expected)))
      (error "Archive package %S is at %s, expected %s"
             (elpaca<-id package) actual expected))
    actual))

(defalias 'gsmlg-bootstrap-source-revision
  #'gsmlg-package-lock-source-revision)
(defalias 'gsmlg-bootstrap--archive-header-revision
  #'gsmlg-package-lock--archive-header-revision)
(defalias 'gsmlg-bootstrap--git-output
  #'gsmlg-package-lock--git-output)
(defalias 'gsmlg-bootstrap--installed-package-ids
  #'gsmlg-package-lock--installed-package-ids)
(defalias 'gsmlg-bootstrap--read-lock-file
  #'gsmlg-package-lock-read)

(defvar gsmlg-package-lock--archive-ref-installed nil
  "Non-nil when the tar `elpaca-ref' method has been installed.")

(defun gsmlg-package-lock-install-archive-ref-method ()
  "Install archive revision verification after packages are realized.

Installing the method before tar packages finish can abort Elpaca's archive
subprocess when `pax_global_header' is not yet on disk."
  (unless gsmlg-package-lock--archive-ref-installed
    (require 'elpaca-tar)
    (eval
     '(cl-defmethod elpaca-ref ((package (elpaca tar)))
        "Return the verified immutable archive revision for PACKAGE."
        (gsmlg-package-lock-source-revision package))
     t)
    (setq gsmlg-package-lock--archive-ref-installed t)))

(defun gsmlg-package-lock--installed-p (elpaca)
  "Return non-nil when ELPACA has an installed source repository."
  (file-directory-p (elpaca<-source-dir elpaca)))

;;;###autoload
(defun gsmlg-elpaca-write-lock-file ()
  "Finish queued work and write exact package recipes to the lock file."
  (interactive)
  (gsmlg-bootstrap-wait)
  (gsmlg-package-lock-install-archive-ref-method)
  ;; Normally the init queue still contains every finished declaration.  If a
  ;; caller already finalized it, recreate declarations without starting them;
  ;; the declaration menu retains explicit recipes from init.el.
  (unless (elpaca--queued)
    (let ((real-this-command nil))
      (dolist (id (gsmlg-package-lock--installed-package-ids))
        (eval `(elpaca ,id) t)))
    (gsmlg-bootstrap-wait))
  (let ((elpaca-lock-file-functions
         '(gsmlg-package-lock--installed-p)))
    (elpaca-write-lock-file elpaca-lock-file (elpaca--queued))))

(autoload 'gsmlg-elpaca-update-package "gsmlg-package-maintenance" nil t)

(provide 'gsmlg-package-lock)
;;; gsmlg-package-lock.el ends here
