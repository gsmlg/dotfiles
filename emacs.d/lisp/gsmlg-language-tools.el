;;; gsmlg-language-tools.el --- Project and remote executable discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared foundation for resolving executables near `default-directory',
;; including TRAMP compute-near-data lookups used by Eglot, formatters, and
;; language dispatch modules.

;;; Code:

(require 'cl-lib)
(require 'project)

(defun gsmlg-language-tools-find-executable (program)
  "Find PROGRAM in the environment nearest `default-directory'."
  (executable-find program (and (file-remote-p default-directory) t)))

(defun gsmlg-language-tools-project-executable (program root)
  "Return project-local PROGRAM below ROOT in an executable command form.

Probe the full local or TRAMP filename.  For a remote project, return a path
relative to ROOT because Eglot launches the server with the remote project
root as `default-directory'; passing a literal TRAMP filename to the remote
shell would not name an executable there."
  (let* ((relative (concat "node_modules/.bin/" program))
         (remote-prefix (file-remote-p root))
         (candidate (expand-file-name relative root)))
    (when (file-executable-p candidate)
      (if remote-prefix
          (concat "./" relative)
        candidate))))

(defun gsmlg-language-tools-project-root (&optional project)
  "Return root of PROJECT or the current project without prompting."
  (when-let* ((project (or project (project-current nil))))
    (project-root project)))

(defun gsmlg-language-tools-available-command (candidates)
  "Return the first executable command list from CANDIDATES."
  (cl-loop for candidate in candidates
           when (gsmlg-eglot-find-executable (car candidate))
           return candidate))

(defun gsmlg-language-tools-command-executable-p (program &optional root)
  "Return non-nil when PROGRAM is executable from project ROOT.

ROOT defaults to `default-directory'.  File-name handlers therefore perform
both local and TRAMP probes near the data rather than on the client host."
  (let* ((root (file-name-as-directory (or root default-directory)))
         (root-remote (file-remote-p root))
         (program-remote (file-remote-p program)))
    (cond
     (program-remote
      (and root-remote
           (equal program-remote root-remote)
           (file-executable-p program)))
     ((file-name-directory program)
      (file-executable-p
       (if (and root-remote (file-name-absolute-p program))
           (concat root-remote program)
         (expand-file-name program root))))
     (t
      (let ((default-directory root))
        (gsmlg-eglot-find-executable program))))))

(defun gsmlg-language-tools-normalize-override (override root)
  "Validate OVERRIDE from ROOT and return an Eglot-safe command.

For a command explicitly qualified with the same TRAMP connection as ROOT,
strip the TRAMP prefix before handing it to the remote shell."
  (let ((program (car override)))
    (when (gsmlg-language-tools-command-executable-p program root)
      (if (file-remote-p program)
          (cons (file-remote-p program 'localname) (cdr override))
        override))))

;; Compatibility aliases retained for existing tests and callers.
(defalias 'gsmlg-eglot-find-executable #'gsmlg-language-tools-find-executable)
(defalias 'gsmlg-eglot-project-executable
  #'gsmlg-language-tools-project-executable)
(defalias 'gsmlg-eglot--project-root #'gsmlg-language-tools-project-root)
(defalias 'gsmlg-eglot--available-command
  #'gsmlg-language-tools-available-command)
(defalias 'gsmlg-eglot-command-executable-p
  #'gsmlg-language-tools-command-executable-p)
(defalias 'gsmlg-eglot-normalize-override
  #'gsmlg-language-tools-normalize-override)

(provide 'gsmlg-language-tools)
;;; gsmlg-language-tools.el ends here
