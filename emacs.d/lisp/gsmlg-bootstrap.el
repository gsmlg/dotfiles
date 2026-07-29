;;; gsmlg-bootstrap.el --- Reproducible Elpaca bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstrap the pinned Elpaca revision under XDG data storage and connect it
;; to Emacs 30's built-in use-package.  The committed lock file is the first
;; recipe source, so warm startup does not need package archive metadata.

;;; Code:

(require 'cl-lib)
(require 'gsmlg-paths)

(defconst gsmlg-elpaca-revision
  "6530ffa73b18ccee858e7c471415ab7e0c0d8ce1"
  "Exact Elpaca revision used to bootstrap this configuration.")

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory
  (file-name-as-directory (gsmlg-data-file "elpaca/")))
(defvar elpaca-cache-directory
  (file-name-as-directory (gsmlg-cache-file "elpaca/")))
(defvar elpaca-builds-directory
  (file-name-as-directory (expand-file-name "builds/" elpaca-directory)))
(defvar elpaca-sources-directory
  (file-name-as-directory (expand-file-name "repos/" elpaca-directory)))
(defvar elpaca-lock-file
  (expand-file-name "elpaca-lock.el" gsmlg-config-directory))
(defvar elpaca-order
  `(elpaca
    :repo "https://github.com/progfolio/elpaca.git"
    :ref ,gsmlg-elpaca-revision
    :depth nil
    :inherit ignore
    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
    :build (:not elpaca-activate)))
(defvar elpaca-use-package-mode)
(defvar use-package-always-ensure)

(defconst gsmlg-bootstrap-elpaca-subprocess-environment-form
  '(progn
     (setq gc-cons-percentage 1.0
           print-level nil
           print-circle nil)
     (unless (fboundp #'zlib-decompress-region)
       (defalias #'zlib-decompress-region
         (lambda (start end &optional _allow-partial)
           "Decompress the gzip data between START and END with gzip."
           (unless (executable-find "gzip")
             (error
              "Emacs lacks zlib support and gzip is unavailable"))
           (let ((coding-system-for-read 'no-conversion)
                 (coding-system-for-write 'no-conversion)
                 (status
                  (call-process-region
                   start end "gzip" t t nil "-d" "-c")))
             (unless (and (integerp status) (zerop status))
               (error "Gzip decompression failed with status %s" status)))))))
  "Environment applied to Elpaca child Emacs processes.

Some supported Emacs builds omit the optional zlib function used by Elpaca's
tar transport.  Such child processes use the system gzip executable instead.")

(defun gsmlg-bootstrap--read-lock-file ()
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
  (gsmlg-bootstrap--read-lock-file)
  "Validated exact-revision entries from the committed Elpaca lock file.")

(defcustom gsmlg-elpaca-offline
  (equal (getenv "GSMLG_EMACS_OFFLINE") "1")
  "When non-nil, prohibit a first-time network bootstrap.

Already installed packages continue to load normally."
  :type 'boolean
  :group 'gsmlg)

(defun gsmlg-bootstrap--emacs-program ()
  "Return the current Emacs executable path."
  (expand-file-name invocation-name invocation-directory))

(defun gsmlg-bootstrap--run (buffer program &rest arguments)
  "Run PROGRAM with ARGUMENTS, logging to BUFFER, or signal an error."
  (let ((status (apply #'call-process program nil buffer t arguments)))
    (unless (zerop status)
      (error "%s exited with status %s; see %s"
             program status (buffer-name buffer)))))

(defun gsmlg-bootstrap--install-elpaca (repository recipe)
  "Clone and build Elpaca in REPOSITORY using RECIPE."
  (when gsmlg-elpaca-offline
    (error
     (concat "Elpaca is not bootstrapped and GSMLG_EMACS_OFFLINE=1. "
             "Run one connected startup, then offline startups are supported")))
  (make-directory repository t)
  (let ((buffer (get-buffer-create "*gsmlg-elpaca-bootstrap*")))
    (condition-case error-data
        (progn
          (with-current-buffer buffer
            (erase-buffer))
          (delete-directory repository)
          (let ((default-directory
                 (file-name-directory (directory-file-name repository))))
            (gsmlg-bootstrap--run
             buffer "git" "clone" (plist-get recipe :repo) repository))
          (let ((default-directory repository))
            (gsmlg-bootstrap--run
             buffer "git" "checkout" "--detach" (plist-get recipe :ref))
            (gsmlg-bootstrap--run
             buffer
             (gsmlg-bootstrap--emacs-program)
             "-Q" "-L" "." "--batch"
             "--eval" "(byte-recompile-directory \".\" 0 'force)")))
      (error
       (when (file-directory-p repository)
         (delete-directory repository t))
       (error
        "Unable to bootstrap pinned Elpaca revision: %s (log: %s)"
        (error-message-string error-data)
        (buffer-name buffer))))))

(let* ((repository (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (recipe (cdr elpaca-order))
       (default-directory
        (file-name-directory (directory-file-name repository))))
  (add-to-list 'load-path (if (file-directory-p build) build repository))
  (unless (file-directory-p repository)
    (gsmlg-bootstrap--install-elpaca repository recipe))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repository)
    (let ((load-source-file-function nil))
      (load (expand-file-name "elpaca-autoloads.el" repository) nil 'nomessage))))

(defun gsmlg-bootstrap--failed-package-ids ()
  "Return IDs of queued Elpaca packages whose processing failed."
  (cl-loop for (id . package) in (elpaca--queued)
           when (eq (elpaca<-status package) 'failed)
           collect id))

(defun gsmlg-bootstrap-wait ()
  "Wait for queued work and fail clearly when an Elpaca package failed."
  (elpaca-wait)
  (when-let* ((failed (gsmlg-bootstrap--failed-package-ids)))
    (error "Elpaca failed to prepare: %s; inspect *elpaca-log*"
           (mapconcat #'symbol-name failed ", "))))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(setopt elpaca-with-emacs-env-form
        gsmlg-bootstrap-elpaca-subprocess-environment-form)

(require 'use-package)
(elpaca compat)
(elpaca
    `(elpaca-use-package
      :repo "https://github.com/progfolio/elpaca.git"
      :ref ,gsmlg-elpaca-revision
      :wait nil
      :files ("extensions/elpaca-use-package.el")
      :main "extensions/elpaca-use-package.el"
      :build (:not elpaca-source elpaca-build-docs))
  (elpaca-use-package-mode 1)
  (setopt use-package-always-ensure t))

;; Package declarations and built-in Eglot must wait until Elpaca owns
;; use-package's :ensure keyword and external compat shadows Emacs's internal
;; compatibility shim.  This is the bootstrap phase boundary.
(gsmlg-bootstrap-wait)
(unless (and (bound-and-true-p elpaca-use-package-mode)
             use-package-always-ensure)
  (error "Elpaca use-package integration failed; inspect *elpaca-log*"))
(let ((compat-library (locate-library "compat")))
  (unless (and compat-library
               (file-in-directory-p
                (file-truename compat-library)
                (file-truename elpaca-builds-directory)))
    (error "External compat failed to activate from Elpaca")))

(defun gsmlg-bootstrap--installed-package-ids ()
  "Return package IDs represented by XDG Elpaca build directories."
  (cl-loop for name in (directory-files elpaca-builds-directory nil
                                         directory-files-no-dot-files-regexp)
           for directory = (expand-file-name name elpaca-builds-directory)
           when (file-directory-p directory)
           collect (intern name)))

(defun gsmlg-bootstrap--git-output (directory &rest arguments)
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
(defun gsmlg-bootstrap--archive-header-revision (source)
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

(defun gsmlg-bootstrap-source-revision (package)
  "Return the verified source revision for Elpaca PACKAGE."
  (let* ((recipe (elpaca<-recipe package))
         (source (elpaca<-source-dir package))
         (type (plist-get recipe :type))
         (expected (plist-get recipe :ref))
         (actual
          (if (file-directory-p (expand-file-name ".git" source))
              (gsmlg-bootstrap--git-output source "rev-parse" "HEAD")
            (if (eq type 'tar)
                (gsmlg-bootstrap--archive-header-revision source)
              (error "Package %S has no verifiable source revision"
                     (elpaca<-id package))))))
    (when (and (eq type 'tar)
               (not (equal actual expected)))
      (error "Archive package %S is at %s, expected %s"
             (elpaca<-id package) actual expected))
    actual))

(require 'elpaca-tar)
(cl-defmethod elpaca-ref ((package (elpaca tar)))
  "Return the verified immutable archive revision for PACKAGE."
  (gsmlg-bootstrap-source-revision package))

(defun gsmlg-bootstrap--remote-default-target (directory)
  "Return the origin default branch target for Git repository DIRECTORY."
  (condition-case nil
      (gsmlg-bootstrap--git-output
       directory "symbolic-ref" "--quiet" "--short"
       "refs/remotes/origin/HEAD")
    (error
     (let ((remote-head
            (gsmlg-bootstrap--git-output
             directory "ls-remote" "--symref" "origin" "HEAD")))
       (unless (string-match
                "\\`ref: refs/heads/\\([^[:space:]]+\\)[[:space:]]+HEAD"
                remote-head)
         (error "Unable to resolve origin's default branch in %s" directory))
       (concat "origin/" (match-string 1 remote-head))))))

(defun gsmlg-bootstrap--package-update-target (directory recipe)
  "Return the update target for RECIPE in Git repository DIRECTORY."
  (if-let* ((branch (plist-get recipe :branch))
            ((stringp branch))
            ((not (string-empty-p branch))))
      (concat "origin/" (string-remove-prefix "refs/heads/" branch))
    (gsmlg-bootstrap--remote-default-target directory)))

(defun gsmlg-elpaca-update-package (id)
  "Advance installed package ID to its configured upstream and rebuild it.

This explicit maintenance command leaves `elpaca-lock-file' unchanged so the
updated package graph can be tested before `gsmlg-elpaca-write-lock-file' is
called.  It refuses the Elpaca bootstrap repository, whose source pin and lock
entries must be changed together."
  (interactive
   (list
    (intern
     (completing-read
      "Update locked package: "
      (mapcar #'symbol-name (gsmlg-bootstrap--installed-package-ids))
      nil t))))
  (when (memq id '(elpaca elpaca-use-package))
    (user-error
     "Update the Elpaca bootstrap pin and its lock entries together"))
  (let* ((package (or (elpaca-get id)
                      (user-error "Package %S is not active" id)))
         (recipe (elpaca<-recipe package))
         (source (elpaca<-source-dir package)))
    (when (eq (plist-get recipe :type) 'tar)
      (user-error
       (concat "Package %S uses an immutable archive recipe; update its exact "
               "lock :ref, delete and re-realize it, then verify a fresh "
               "bootstrap")
       id))
    (let ((dirty (gsmlg-bootstrap--git-output
                  source "status" "--porcelain")))
      (unless (string-empty-p dirty)
        (user-error "Package source has uncommitted changes: %s" source)))
    (let ((buffer (get-buffer-create "*gsmlg-elpaca-update*"))
          (default-directory source))
      (with-current-buffer buffer
        (erase-buffer))
      (gsmlg-bootstrap--run buffer "git" "fetch" "--prune" "origin"))
    (let* ((target
            (gsmlg-bootstrap--package-update-target source recipe))
           (shared
            (cl-loop for (shared-id . shared-package) in (elpaca--queued)
                     when (equal source (elpaca<-source-dir shared-package))
                     collect shared-id)))
      (gsmlg-bootstrap--git-output
       source "rev-parse" "--verify" (concat target "^{commit}"))
      (gsmlg-bootstrap--git-output source "checkout" "--detach" target)
      (dolist (shared-id shared)
        (elpaca-rebuild shared-id))
      (elpaca-process-queues)
      (gsmlg-bootstrap-wait)
      (message "Updated and rebuilt %s at %s"
               (mapconcat #'symbol-name shared ", ")
               (gsmlg-bootstrap--git-output
                source "rev-parse" "HEAD")))))

(defun gsmlg-bootstrap--lock-installed-p (elpaca)
  "Return non-nil when ELPACA has an installed source repository."
  (file-directory-p (elpaca<-source-dir elpaca)))

(defun gsmlg-elpaca-write-lock-file ()
  "Finish queued work and write exact package recipes to the lock file."
  (interactive)
  (gsmlg-bootstrap-wait)
  ;; Normally the init queue still contains every finished declaration.  If a
  ;; caller already finalized it, recreate declarations without starting them;
  ;; the declaration menu retains explicit recipes from init.el.
  (unless (elpaca--queued)
    (let ((real-this-command nil))
      (dolist (id (gsmlg-bootstrap--installed-package-ids))
        (eval `(elpaca ,id) t)))
    (gsmlg-bootstrap-wait))
  (let ((elpaca-lock-file-functions
         '(gsmlg-bootstrap--lock-installed-p)))
    (elpaca-write-lock-file elpaca-lock-file (elpaca--queued))))

(provide 'gsmlg-bootstrap)
;;; gsmlg-bootstrap.el ends here
