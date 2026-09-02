;;; gsmlg-project.el --- project.el and project environments -*- lexical-binding: t; -*-

;;; Commentary:
;; Use built-in project.el as the sole project abstraction.  All commands keep
;; the project root, including TRAMP prefixes, as their default directory.

;;; Code:

(require 'project)
(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)

(declare-function consult-ripgrep "consult" (&optional directory initial))
(declare-function envrc-global-mode "envrc" (&optional arg))
(declare-function envrc-mode "envrc" (&optional arg))
(declare-function envrc--export "envrc" (env-dir))
(declare-function exec-path-from-shell-initialize "exec-path-from-shell" ())
(defvar envrc-mode)
(defvar envrc-remote)

(defcustom gsmlg-project-local-bin-directories
  '("node_modules/.bin")
  "Project-relative executable directories made buffer-local when present."
  :type '(repeat string)
  :group 'gsmlg)

(defcustom gsmlg-envrc-enable nil
  "Whether interactive sessions should enable `envrc-global-mode'.

Defaults to off so Emacs does not invoke direnv or surface blocked
`.envrc' errors.  Set this non-nil in the external local file to opt in,
including for remote TRAMP buffers when `envrc-remote' is non-nil.
When nil, buffer-local `envrc-mode' (for example from desktop restore)
is also refused.  Blocked `.envrc' diagnostics never pop `*envrc*';
use `envrc-show-log' to inspect them.  Batch sessions ignore this option."
  :type 'boolean
  :group 'gsmlg)

(defvar gsmlg-exec-path-from-shell-initialized nil
  "Non-nil after importing the macOS login-shell environment.")

(defun gsmlg-project-root (&optional directory)
  "Return the current project root for DIRECTORY, preserving remote names."
  (when-let* ((project (project-current nil directory)))
    (project-root project)))

(defun gsmlg-project-search (&optional initial)
  "Run Consult ripgrep at the current project root with optional INITIAL text."
  (interactive)
  (unless (fboundp #'consult-ripgrep)
    (user-error "Consult ripgrep is unavailable"))
  (let ((root (or (gsmlg-project-root default-directory)
                  default-directory)))
    (consult-ripgrep root initial)))

(defun gsmlg-project-activate-local-bins ()
  "Prepend existing project-local executable directories buffer-locally."
  (when-let* ((root (gsmlg-project-root default-directory)))
    (dolist (relative gsmlg-project-local-bin-directories)
      (let ((directory (file-name-as-directory
                        (expand-file-name relative root))))
        (when (file-directory-p directory)
          (setq-local exec-path
                      (cons directory (delete directory exec-path))))))))

(defun gsmlg-project-import-macos-environment (&optional frame)
  "Import the login-shell environment once for graphical macOS FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (and (eq system-type 'darwin)
               (display-graphic-p)
               (not gsmlg-exec-path-from-shell-initialized)
               (fboundp #'exec-path-from-shell-initialize))
      (exec-path-from-shell-initialize)
      (setq gsmlg-exec-path-from-shell-initialized t))))

(setq project-list-file
      (gsmlg-ensure-parent-directory (gsmlg-state-file "projects")))

(add-hook 'find-file-hook #'gsmlg-project-activate-local-bins 80)

(use-package project
  :ensure nil
  :demand t
  :config
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-dired "Dired")
            (project-vc-dir "VC")
            (project-eshell "Eshell"))))

(defun gsmlg-project-enable-envrc-maybe ()
  "Enable envrc after local overrides when `gsmlg-envrc-enable' permits it."
  (when (and gsmlg-envrc-enable
             (not noninteractive)
             (fboundp #'envrc-global-mode)
             (not (bound-and-true-p envrc-global-mode)))
    (envrc-global-mode 1)))

(defun gsmlg-envrc--enable-arg-p (arg)
  "Return non-nil when ARG would enable `envrc-mode'."
  (cond
   ((eq arg 'toggle) (not (bound-and-true-p envrc-mode)))
   ((and (numberp arg) (< arg 1)) nil)
   ((null arg) (not (bound-and-true-p envrc-mode)))
   (t t)))

(defun gsmlg-envrc--guard-mode (orig-fun &optional arg)
  "Refuse to enable `envrc-mode' unless `gsmlg-envrc-enable' is non-nil.

Desktop restore can re-enable buffer-local `envrc-mode' even when
`envrc-global-mode' is off.  Keep that path gated by the same opt-in."
  (if (and (gsmlg-envrc--enable-arg-p arg)
           (not gsmlg-envrc-enable))
      nil
    (funcall orig-fun arg)))

(defun gsmlg-envrc--blocked-message-p (buffer)
  "Return non-nil when BUFFER ends with a blocked-direnv diagnostic."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (re-search-backward
         "\\.envrc is blocked\\. Run 'direnv allow' to approve its content"
         nil t)))))

(defun gsmlg-envrc--display-buffer-maybe (orig-fun buffer-or-name &optional action frame)
  "Skip popping `*envrc*' when direnv only reports a blocked `.envrc'."
  (let ((buffer (get-buffer buffer-or-name)))
    (if (and buffer
             (equal (buffer-name buffer) "*envrc*")
             (gsmlg-envrc--blocked-message-p buffer))
        buffer
      (funcall orig-fun buffer-or-name action frame))))

(defun gsmlg-envrc--export-quiet-blocked (orig-fun env-dir)
  "Run ORIG-FUN for ENV-DIR without surfacing blocked-direnv popups.

`envrc--export' always calls `display-buffer' on failure.  A blocked
`.envrc' is an expected local state, not a failure worth interrupting
editing; leave the log in `*envrc*' for `envrc-show-log'."
  (advice-add #'display-buffer :around #'gsmlg-envrc--display-buffer-maybe)
  (unwind-protect
      (funcall orig-fun env-dir)
    (advice-remove #'display-buffer #'gsmlg-envrc--display-buffer-maybe)))

(use-package envrc
  :demand t
  :config
  (setopt envrc-remote t)
  (advice-add #'envrc-mode :around #'gsmlg-envrc--guard-mode)
  (advice-add #'envrc--export :around #'gsmlg-envrc--export-quiet-blocked)
  ;; Defer until `emacs-startup-hook' so local.el can setopt
  ;; `gsmlg-envrc-enable' first.
  (add-hook 'emacs-startup-hook #'gsmlg-project-enable-envrc-maybe 80))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (add-hook 'after-make-frame-functions
            #'gsmlg-project-import-macos-environment)
  (gsmlg-project-import-macos-environment))

(provide 'gsmlg-project)
;;; gsmlg-project.el ends here
