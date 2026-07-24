;;; init-git.el --- Configuration for init-git -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-git.

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(defvar *is-a-mac*)

(use-package git-blamed
  :ensure t
  :commands git-blamed-mode)
(use-package git-modes
  :ensure t
  :defer t)
(use-package git-timemachine
  :ensure t
  :commands git-timemachine)
(use-package git-link
  :ensure t
  :commands git-link)

(use-package magit
  :ensure t
  :init (setq-default magit-diff-refine-hunk t)
  :bind (([(meta f12)] . magit-status)
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-status)
         :map magit-status-mode-map
         ("C-M-<up>" . magit-section-up))
  :config
  (fullframe magit-status magit-mode-quit-window)
  (when *is-a-mac*
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

;; git-commit package is currently unavailable, commenting out for now
;; (use-package git-commit
;;   :ensure t
;;   :hook (git-commit-mode . goto-address-mode))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                      :hint nil)
  "Git gutter commands."
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter-mode -1))
   :color blue))
(global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)


;; Convenient binding for vc-git-grep
(after-load 'vc
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))




;;; git-svn support

;; (when (maybe-require-package 'magit-svn)
;;   (require-package 'magit-svn)
;;   (autoload 'magit-svn-enabled "magit-svn")
;;   (defun gsmlg/maybe-enable-magit-svn-mode ()
;;     (when (magit-svn-enabled)
;;       (magit-svn-mode)))
;;   (add-hook 'magit-status-mode-hook #'gsmlg/maybe-enable-magit-svn-mode))

(after-load 'compile
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands.")
(defun git-svn--available-commands ()
  "Get available git svn commands."
  (or git-svn--available-commands
      (setq git-svn--available-commands
            (gsmlg/string-all-matches
             "^  \\([a-z\\-]+\\) +"
             (shell-command-to-string "git svn help") 1))))

(autoload 'vc-git-root "vc-git")

(defun git-svn (dir command)
  "Run a git svn subcommand COMMAND in DIR."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (_major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))



(use-package git-messenger
  :ensure t
  :init (setq git-messenger:show-detail t)
  ;; Though see also vc-annotate's "n" & "p" bindings
  :after vc
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)))

(provide 'init-git)
;;; init-git.el ends here
