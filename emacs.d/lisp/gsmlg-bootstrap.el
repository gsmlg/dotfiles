;;; gsmlg-bootstrap.el --- Reproducible Elpaca bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstrap the pinned Elpaca revision under XDG data storage and connect it
;; to Emacs 30's built-in use-package.  Lock validation lives in
;; `gsmlg-package-lock'; package update workflows live in
;; `gsmlg-package-maintenance' and are not loaded during normal startup.

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

(provide 'gsmlg-bootstrap)
;;; gsmlg-bootstrap.el ends here
