;;; gsmlg-core.el --- Core built-in Emacs behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Stable editing defaults and built-in global facilities shared by all
;; platforms and session types.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)

(defcustom gsmlg-preferred-indent-width 2
  "Preferred indentation width for modes without a project-local setting."
  :type 'integer
  :group 'gsmlg)

(defcustom gsmlg-gc-cons-threshold (* 32 1024 1024)
  "Garbage collection threshold restored after startup."
  :type 'integer
  :group 'gsmlg)

(defun gsmlg-restore-startup-gc ()
  "Restore conservative garbage collection settings after startup."
  (setq gc-cons-threshold gsmlg-gc-cons-threshold
        gc-cons-percentage 0.1)
  (when (boundp 'gsmlg-early-init--gc-restored)
    (setq gsmlg-early-init--gc-restored t)))

(defun gsmlg-enable-line-numbers ()
  "Enable line numbers in a local programming buffer."
  (display-line-numbers-mode 1))

(setq-default indent-tabs-mode nil
              tab-width gsmlg-preferred-indent-width
              fill-column 80
              sentence-end-double-space nil)

(setq ring-bell-function #'ignore
      use-file-dialog nil
      use-dialog-box nil
      confirm-kill-emacs #'yes-or-no-p
      require-final-newline t
      read-process-output-max (* 1024 1024)
      enable-recursive-minibuffers t)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(setopt auto-revert-remote-files nil
        global-auto-revert-non-file-buffers t)
(repeat-mode 1)
(global-so-long-mode 1)

(add-hook 'prog-mode-hook #'gsmlg-enable-line-numbers)
(add-hook 'emacs-startup-hook #'gsmlg-restore-startup-gc 90)

(use-package which-key
  :ensure nil
  :demand t
  :config
  (setopt which-key-idle-delay 0.5
          which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

(use-package editorconfig
  :ensure nil
  :demand t
  :config
  (editorconfig-mode 1))

(provide 'gsmlg-core)
;;; gsmlg-core.el ends here
