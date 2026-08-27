;;; gsmlg-server.el --- Canonical Emacs server singleton -*- lexical-binding: t; -*-

;;; Commentary:
;; One formal interactive Emacs server per OS user on a host.  The fixed server
;; name is `main'.  Batch tests and temporary isolation processes bypass the
;; singleton policy through `gsmlg-server-testing-p'.

;;; Code:

(require 'gsmlg-paths)
(require 'server)

(defgroup gsmlg-server nil
  "Canonical Emacs server identity and lifecycle."
  :group 'gsmlg)

(defcustom gsmlg-server-name "main"
  "Canonical Emacs server name for the formal interactive daemon."
  :type 'string
  :group 'gsmlg-server)

(defcustom gsmlg-server-autostart t
  "Whether a normal interactive non-daemon session should start the server.

Daemon sessions already own a server.  Batch and testing sessions ignore this
option."
  :type 'boolean
  :group 'gsmlg-server)

(defun gsmlg-server-testing-p ()
  "Return non-nil when this process must not join the user singleton server.

Testing is detected from batch mode, `GSMLG_EMACS_TESTING', or an active
`GSMLG_EMACS_TEST_ROOT' harness."
  (or noninteractive
      (equal (getenv "GSMLG_EMACS_TESTING") "1")
      (and (getenv "GSMLG_EMACS_TEST_ROOT")
           (not (string-empty-p (getenv "GSMLG_EMACS_TEST_ROOT"))))))

(defun gsmlg-server-owned-p ()
  "Return non-nil when this Emacs process owns a live server."
  (and server-mode
       (boundp 'server-process)
       (process-live-p server-process)))

(defun gsmlg-server-identity ()
  "Return an alist describing the current Emacs server identity."
  `((server_name . ,(or server-name gsmlg-server-name))
    (daemon . ,(let ((name (daemonp)))
                 (cond
                  ((stringp name) name)
                  (name "anonymous")
                  (t :false))))
    (pid . ,(emacs-pid))
    (owned . ,(if (gsmlg-server-owned-p) t :false))
    (testing . ,(if (gsmlg-server-testing-p) t :false))
    (socket_dir . ,(or server-socket-dir server-auth-dir))))

(defun gsmlg-server--apply-canonical-name ()
  "Bind `server-name' to `gsmlg-server-name' for the formal interactive server.

Named test daemons and explicit non-main daemons keep their own names."
  (unless (gsmlg-server-testing-p)
    (let ((daemon (daemonp)))
      (cond
       ((and (stringp daemon)
             (not (equal daemon gsmlg-server-name)))
        nil)
       (t
        (setq server-name gsmlg-server-name))))))

(defun gsmlg-server-configure-socket-directory ()
  "Place the Emacs server socket under the XDG state directory."
  (let ((server-directory
         (gsmlg-ensure-directory (gsmlg-state-file "server/"))))
    (set-file-modes server-directory #o700)
    (setopt server-auth-dir server-directory
            server-socket-dir server-directory)))

;;;###autoload
(defun gsmlg-server-status ()
  "Report whether this process owns the canonical Emacs server."
  (interactive)
  (let* ((identity (gsmlg-server-identity))
         (message
          (format
           "Emacs server name=%s pid=%s owned=%s testing=%s"
           (alist-get 'server_name identity)
           (alist-get 'pid identity)
           (alist-get 'owned identity)
           (alist-get 'testing identity))))
    (when (called-interactively-p 'interactive)
      (message "%s" message))
    identity))

;;;###autoload
(defun gsmlg-server-start ()
  "Start the canonical Emacs server when this process can host one."
  (interactive)
  (when noninteractive
    (user-error "An Emacs server cannot start in batch mode"))
  (gsmlg-server--apply-canonical-name)
  (cond
   ((daemonp)
    (message "This Emacs daemon already provides an emacsclient server")
    'daemon)
   ((gsmlg-server-owned-p)
    (message "The Emacs server is already running")
    'running)
   ((and (not (gsmlg-server-testing-p))
         (server-running-p server-name))
    (user-error
     "Server %S is already owned by another Emacs process"
     server-name))
   (t
    (server-start)
    (message "Emacs server %S started" server-name)
    'started)))

(defun gsmlg-server-start-maybe ()
  "Start the canonical server when the autostart policy permits it."
  (when (and gsmlg-server-autostart
             (not noninteractive)
             (not (daemonp)))
    (condition-case error-data
        (gsmlg-server-start)
      (error
       (message "GSMLG server autostart failed: %s"
                (error-message-string error-data))))))

(gsmlg-server-configure-socket-directory)
(gsmlg-server--apply-canonical-name)
(add-hook 'emacs-startup-hook #'gsmlg-server-start-maybe 95)

(provide 'gsmlg-server)
;;; gsmlg-server.el ends here
