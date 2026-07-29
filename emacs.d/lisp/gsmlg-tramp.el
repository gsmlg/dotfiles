;;; gsmlg-tramp.el --- Compute-near-data remote development -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep project operations, process execution, formatting, search, and server
;; discovery at the location of the data.  The user's SSH configuration remains
;; authoritative.

;;; Code:

(require 'gsmlg-paths)
(require 'tramp)

(defun gsmlg-tramp--set-verbosity (symbol value)
  "Set SYMBOL to VALUE and immediately apply it to TRAMP."
  (set-default symbol value)
  (setq tramp-verbose value))

(defcustom gsmlg-tramp-verbosity 1
  "TRAMP diagnostic verbosity.

Increase this temporarily when diagnosing a remote connection."
  :type 'integer
  :set #'gsmlg-tramp--set-verbosity
  :group 'gsmlg)

(defun gsmlg-process-file-near-data
    (program infile destination display &rest arguments)
  "Run PROGRAM with ARGUMENTS using file handlers near `default-directory'.

INFILE, DESTINATION, and DISPLAY have the same meaning as for `process-file'."
  (apply #'process-file program infile destination display arguments))

(defun gsmlg-start-file-process-near-data (name buffer program &rest arguments)
  "Start PROGRAM with ARGUMENTS near the data as process NAME in BUFFER."
  (apply #'start-file-process name buffer program arguments))

(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

(let ((auto-save-directory
       (gsmlg-ensure-directory (gsmlg-cache-file "auto-save/")))
      (backup-directory
       (gsmlg-ensure-directory (gsmlg-state-file "backups/"))))
  (setq tramp-auto-save-directory auto-save-directory
        tramp-backup-directory-alist `(("." . ,backup-directory))
        tramp-persistency-file-name
        (gsmlg-ensure-parent-directory (gsmlg-state-file "tramp"))))

(provide 'gsmlg-tramp)
;;; gsmlg-tramp.el ends here
