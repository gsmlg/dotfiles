;;; gsmlg-package-maintenance.el --- Explicit Elpaca package updates -*- lexical-binding: t; -*-

;;; Commentary:
;; Optional maintenance workflow for advancing installed package sources and
;; rebuilding them before rewriting the lock file.  This module is not loaded
;; on the normal startup path; invoke `gsmlg-elpaca-update-package' to load it.

;;; Code:

(require 'cl-lib)
(require 'gsmlg-bootstrap)
(require 'gsmlg-package-lock)

(declare-function elpaca-get "elpaca" (id))
(declare-function elpaca-rebuild "elpaca" (id))
(declare-function elpaca-process-queues "elpaca" (&optional queues))
(declare-function elpaca--queued "elpaca" (&optional items))
(declare-function elpaca<-recipe "elpaca" (package))
(declare-function elpaca<-source-dir "elpaca" (package))

(defun gsmlg-package-maintenance--remote-default-target (directory)
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

(defun gsmlg-package-maintenance--package-update-target (directory recipe)
  "Return the update target for RECIPE in Git repository DIRECTORY."
  (if-let* ((branch (plist-get recipe :branch))
            ((stringp branch))
            ((not (string-empty-p branch))))
      (concat "origin/" (string-remove-prefix "refs/heads/" branch))
    (gsmlg-package-maintenance--remote-default-target directory)))

(defalias 'gsmlg-bootstrap--remote-default-target
  #'gsmlg-package-maintenance--remote-default-target)
(defalias 'gsmlg-bootstrap--package-update-target
  #'gsmlg-package-maintenance--package-update-target)

;;;###autoload
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
      (mapcar #'symbol-name (gsmlg-package-lock--installed-package-ids))
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
    (let ((dirty (gsmlg-package-lock--git-output
                  source "status" "--porcelain")))
      (unless (string-empty-p dirty)
        (user-error "Package source has uncommitted changes: %s" source)))
    (let ((buffer (get-buffer-create "*gsmlg-elpaca-update*"))
          (default-directory source))
      (with-current-buffer buffer
        (erase-buffer))
      (gsmlg-bootstrap--run buffer "git" "fetch" "--prune" "origin"))
    (let* ((target
            (gsmlg-package-maintenance--package-update-target source recipe))
           (shared
            (cl-loop for (shared-id . shared-package) in (elpaca--queued)
                     when (equal source (elpaca<-source-dir shared-package))
                     collect shared-id)))
      (gsmlg-package-lock--git-output
       source "rev-parse" "--verify" (concat target "^{commit}"))
      (gsmlg-package-lock--git-output source "checkout" "--detach" target)
      (dolist (shared-id shared)
        (elpaca-rebuild shared-id))
      (elpaca-process-queues)
      (gsmlg-bootstrap-wait)
      (message "Updated and rebuilt %s at %s"
               (mapconcat #'symbol-name shared ", ")
               (gsmlg-package-lock--git-output
                source "rev-parse" "HEAD")))))

(provide 'gsmlg-package-maintenance)
;;; gsmlg-package-maintenance.el ends here
