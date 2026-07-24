;;; init-sessions.el --- Configuration for init-sessions -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-sessions.

;;; Code:

(defvar desktop-path)
(defvar desktop-auto-save-timeout)
(defvar desktop-globals-to-save)

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list gsmlg/cache-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

(defun gsmlg/time-desktop-read (orig-fn &rest args)
  "Time the execution of ORIG-FN with ARGS when reading desktop."
  (let ((start-time (current-time)))
    (prog1
        (apply orig-fn args)
      (when (fboundp 'gsmlg/time-subtract-millis)
        (message "Desktop restored in %.2fms"
                 (gsmlg/time-subtract-millis (current-time) start-time))))))

(advice-add 'desktop-read :around #'gsmlg/time-desktop-read)

(defun gsmlg/time-desktop-create-buffer (orig-fn ver &rest args)
  "Time buffer creation by ORIG-FN for VER and ARGS."
  (let ((start-time (current-time))
        (filename (nth 0 args)))
    (prog1
        (apply orig-fn ver args)
      (when (fboundp 'gsmlg/time-subtract-millis)
        (message "Desktop: %.2fms to restore %s"
                 (gsmlg/time-subtract-millis (current-time) start-time)
                 (when filename (abbreviate-file-name filename)))))))

(advice-add 'desktop-create-buffer :around #'gsmlg/time-desktop-create-buffer)

;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(setq-default history-length 1000)
(add-hook 'after-init-hook 'savehist-mode)

(use-package session
  :ensure t
  :init (setq session-save-file (expand-file-name ".session" gsmlg/cache-directory)
              session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)"
              session-save-file-coding-system 'utf-8)
  :config
  (add-hook 'after-init-hook 'session-initialize))

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((comint-input-ring        . 50)
                (compile-history          . 30)
                desktop-missing-file-warning
                (dired-regexp-history     . 20)
                (extended-command-history . 30)
                (face-name-history        . 20)
                (file-name-history        . 100)
                (grep-find-history        . 30)
                (grep-history             . 30)
                (ido-buffer-history       . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (ivy-history              . 100)
                (magit-read-rev-history   . 50)
                (minibuffer-history       . 50)
                (org-clock-history        . 50)
                (org-refile-history       . 50)
                (org-tags-history         . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                register-alist
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                tags-table-list)))

(provide 'init-sessions)
;;; init-sessions.el ends here
