;;; emacs-agent-request.el --- Protocol-neutral request state  -*- lexical-binding: t; -*-

;;; Commentary:

;; Request contexts are the only protocol data visible to editor tool handlers.

;;; Code:

(require 'cl-lib)

(cl-defstruct (emacs-agent-request
               (:constructor emacs-agent-request-create))
  id protocol-version client-info operation arguments authorization-context
  progress-context cancellation-token session-id state started-at timer
  cancel-functions)

(defvar emacs-agent-request-timeout 30
  "Normal synchronous request timeout in seconds.")

(defvar emacs-agent-request-absolute-timeout 120
  "Maximum lifetime of a pending request in seconds.")

(defvar emacs-agent--pending-requests (make-hash-table :test #'eq))

(defun emacs-agent-request-register (request)
  "Register REQUEST as pending and return it."
  (setf (emacs-agent-request-state request) 'pending
        (emacs-agent-request-started-at request) (float-time)
        (emacs-agent-request-timer request)
        (run-at-time emacs-agent-request-absolute-timeout nil
                     #'emacs-agent-request-cancel request))
  (puthash request request emacs-agent--pending-requests)
  request)

(defun emacs-agent-request-find
    (id &optional protocol-version session-id)
  "Return a pending request matching ID.
When supplied, also match PROTOCOL-VERSION and SESSION-ID."
  (let (found)
    (maphash
     (lambda (request _value)
       (when (and (not found)
                  (equal id (emacs-agent-request-id request))
                  (or (null protocol-version)
                      (equal protocol-version
                             (emacs-agent-request-protocol-version request)))
                  (or (null session-id)
                      (equal session-id
                             (emacs-agent-request-session-id request))))
         (setq found request)))
     emacs-agent--pending-requests)
    found))

(defun emacs-agent-request-finish (request state)
  "Finish REQUEST in STATE and remove it from the pending registry."
  (when-let* ((timer (emacs-agent-request-timer request)))
    (cancel-timer timer))
  (setf (emacs-agent-request-timer request) nil
        (emacs-agent-request-state request) state)
  (remhash request emacs-agent--pending-requests)
  (when-let* ((connection
               (emacs-agent-request-cancellation-token request)))
    (when (processp connection)
      (process-put connection 'emacs-agent-request nil)))
  request)

(defun emacs-agent-request-on-cancel (request function)
  "Arrange to call FUNCTION if REQUEST is cancelled."
  (push function (emacs-agent-request-cancel-functions request))
  request)

(defun emacs-agent-request-cancel (request)
  "Cancel REQUEST and its registered effects.
Return non-nil when a pending request was cancelled."
  (when (eq (emacs-agent-request-state request) 'pending)
    (setf (emacs-agent-request-state request) 'cancelled)
    (dolist (function (emacs-agent-request-cancel-functions request))
      (ignore-errors (funcall function)))
    (emacs-agent-request-finish request 'cancelled)
    t))

(defun emacs-agent-request-cancel-id
    (id &optional protocol-version session-id)
  "Cancel the pending request matching ID.
PROTOCOL-VERSION and SESSION-ID narrow legacy cancellation."
  (when-let* ((request
               (emacs-agent-request-find id protocol-version session-id)))
    (emacs-agent-request-cancel request)))

(provide 'emacs-agent-request)
;;; emacs-agent-request.el ends here
