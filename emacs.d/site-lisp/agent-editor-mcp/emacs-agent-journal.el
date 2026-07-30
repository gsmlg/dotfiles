;;; emacs-agent-journal.el --- Redacted JSONL audit journal -*- lexical-binding: t; -*-

;;; Commentary:

;; Optional append-only metadata journal.  Source snapshots, bearer tokens, and
;; authorization material are recursively removed before serialization.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'emacs-agent-runtime)

(defcustom emacs-agent-journal-enabled nil
  "Whether runtime activity should be appended to a JSON Lines journal."
  :type 'boolean
  :group 'emacs-agent-editor)

(defcustom emacs-agent-journal-file-name "journal.jsonl"
  "File name used inside a runtime state directory."
  :type 'string
  :group 'emacs-agent-editor)

(defconst emacs-agent-journal-sensitive-key-regexp
  (rx string-start
      (or "authorization" "proxy-authorization" "token" "bearer"
          "credential" "secret" "password" "content" "before_snapshot"
          "before_snapshots" "source" "raw_headers")
      string-end)
  "Keys that must never be persisted in the journal.")

(defvar emacs-agent-journal-files (make-hash-table :test #'equal))

(defun emacs-agent-journal--state-root ()
  "Return the default state directory root."
  (file-name-as-directory
   (or (getenv "XDG_STATE_HOME")
       (expand-file-name ".local/state" (or (getenv "HOME") "~")))))

(defun emacs-agent-journal--directory (runtime)
  "Return RUNTIME's private state directory."
  (or (emacs-agent-runtime-state-directory runtime)
      (expand-file-name
       (format "emacs-agent-editor/%s/"
               (emacs-agent-runtime-instance-id runtime))
       (emacs-agent-journal--state-root))))

(defun emacs-agent-journal-open (&optional runtime)
  "Prepare and return the journal path for RUNTIME."
  (let* ((runtime (or runtime (emacs-agent-runtime-current)))
         (directory (emacs-agent-journal--directory runtime))
         (path (expand-file-name emacs-agent-journal-file-name directory)))
    (make-directory directory t)
    (set-file-modes directory #o700)
    (unless (file-exists-p path)
      (write-region "" nil path nil 'silent))
    (set-file-modes path #o600)
    (puthash (emacs-agent-runtime-instance-id runtime)
             path emacs-agent-journal-files)
    path))

(defun emacs-agent-journal-close (&optional runtime)
  "Forget the journal handle for RUNTIME."
  (let ((runtime (or runtime (emacs-agent-runtime-current))))
    (remhash (emacs-agent-runtime-instance-id runtime)
             emacs-agent-journal-files)
    t))

(defun emacs-agent-journal--key-name (key)
  "Convert KEY to a lowercase journal key name."
  (downcase
   (replace-regexp-in-string
    "\\`:" "" (if (symbolp key) (symbol-name key) (format "%s" key)))))

(defun emacs-agent-journal--sensitive-key-p (key)
  "Return non-nil when KEY carries secret or source material."
  (string-match-p emacs-agent-journal-sensitive-key-regexp
                  (emacs-agent-journal--key-name key)))

(defun emacs-agent-journal--redact (value)
  "Recursively remove sensitive fields from VALUE."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key item)
         (unless (emacs-agent-journal--sensitive-key-p key)
           (puthash key (emacs-agent-journal--redact item) copy)))
       value)
      copy))
   ((and (listp value) (keywordp (car value)))
    (let (copy)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (unless (emacs-agent-journal--sensitive-key-p key)
            (setq copy
                  (append copy
                          (list key
                                (emacs-agent-journal--redact item)))))))
      copy))
   ((and (listp value) (consp (car value)))
    (delq
     nil
     (mapcar
      (lambda (entry)
        (unless (emacs-agent-journal--sensitive-key-p (car entry))
          (cons (car entry)
                (emacs-agent-journal--redact (cdr entry)))))
      value)))
   ((vectorp value)
    (vconcat (mapcar #'emacs-agent-journal--redact value)))
   ((listp value)
    (vconcat (mapcar #'emacs-agent-journal--redact value)))
   (t value)))

(defun emacs-agent-journal-write (runtime event)
  "Append redacted EVENT metadata for RUNTIME.

Return the redacted event.  Nothing is written when journaling is disabled."
  (let* ((entry
          (emacs-agent-journal--redact
           (append
            (list :timestamp
                  (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t)
                  :runtime_instance_id
                  (emacs-agent-runtime-instance-id runtime))
            event)))
         (path
          (or (gethash
               (emacs-agent-runtime-instance-id runtime)
               emacs-agent-journal-files)
              (when emacs-agent-journal-enabled
                (emacs-agent-journal-open runtime)))))
    (when (and emacs-agent-journal-enabled path)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region
         (concat (json-serialize entry :null-object nil
                                 :false-object :json-false)
                 "\n")
         nil path 'append 'silent))
      (set-file-modes path #o600))
    entry))

(provide 'emacs-agent-journal)
;;; emacs-agent-journal.el ends here
