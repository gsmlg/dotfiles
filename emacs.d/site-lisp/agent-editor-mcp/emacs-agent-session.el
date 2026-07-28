;;; emacs-agent-session.el --- Legacy MCP session metadata  -*- lexical-binding: t; -*-

;;; Commentary:

;; Compatibility sessions never own buffers or workspace state.

;;; Code:

(require 'cl-lib)

(cl-defstruct (emacs-agent-session
               (:constructor emacs-agent-session--create))
  id protocol-version client-info initialized created-at last-seen)

(defvar emacs-agent--sessions (make-hash-table :test #'equal))

(defvar emacs-agent-session-id-function
  #'emacs-agent-session--secure-id
  "Function called with no arguments to mint a compatibility session ID.
Applications on platforms without `/dev/urandom' must supply an equally secure
implementation before enabling the legacy protocol profile.")

(defun emacs-agent-session--secure-id ()
  "Create a session identifier from operating-system entropy."
  (unless (executable-find "openssl")
    (error "Secure operating-system entropy is unavailable"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary)
          (coding-system-for-write 'binary))
      (unless (and (zerop (call-process "openssl" nil t nil "rand" "32"))
                   (= (buffer-size) 32))
        (error "Could not read enough operating-system entropy")))
    (secure-hash 'sha256 (current-buffer))))

(defun emacs-agent-session--id ()
  "Create an opaque visible-ASCII session identifier."
  (let ((id (funcall emacs-agent-session-id-function)))
    (unless (and (stringp id)
                 (string-match-p "\\`[!-~]+\\'" id))
      (error "Session ID function returned an invalid identifier"))
    id))

(defun emacs-agent-session-create (protocol-version client-info)
  "Create compatibility metadata for PROTOCOL-VERSION and CLIENT-INFO."
  (let* ((now (float-time))
         (session (emacs-agent-session--create
                   :id (emacs-agent-session--id)
                   :protocol-version protocol-version
                   :client-info client-info
                   :created-at now :last-seen now)))
    (puthash (emacs-agent-session-id session) session emacs-agent--sessions)
    session))

(defun emacs-agent-session-get (id)
  "Return session ID and update its last-seen time."
  (when-let* ((session (and (stringp id)
                            (gethash id emacs-agent--sessions))))
    (setf (emacs-agent-session-last-seen session) (float-time))
    session))

(defun emacs-agent-session-delete (id)
  "Delete session ID."
  (remhash id emacs-agent--sessions))

(defun emacs-agent-session-clear ()
  "Delete all compatibility sessions."
  (clrhash emacs-agent--sessions))

(provide 'emacs-agent-session)
;;; emacs-agent-session.el ends here
