;;; gsmlg-vcs.el --- Magit and diff-hl version control -*- lexical-binding: t; -*-

;;; Commentary:
;; Replace the former gutter command stack with diff-hl and a small Transient
;; interface while retaining Magit and line-commit inspection.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)

(declare-function diff-hl-mode "diff-hl" (&optional arg))
(declare-function diff-hl-hunk-overlay-at "diff-hl" (position))
(declare-function diff-hl-next-hunk "diff-hl" (&optional backward))
(declare-function diff-hl-previous-hunk "diff-hl" ())
(declare-function diff-hl-revert-hunk "diff-hl" ())
(declare-function diff-hl-set-reference-rev-in-project "diff-hl" (revision))
(declare-function diff-hl-show-hunk "diff-hl-show-hunk" ())
(declare-function diff-hl-stage-current-hunk "diff-hl" ())
(declare-function transient-quit-one "transient" ())

(defun gsmlg-diff-hl-first-hunk ()
  "Move to the first diff-hl hunk in the current buffer."
  (interactive)
  (goto-char (point-min))
  (if-let* ((hunk (diff-hl-hunk-overlay-at (point))))
      (goto-char (overlay-start hunk))
    (diff-hl-next-hunk)))

(defun gsmlg-diff-hl-last-hunk ()
  "Move to the last diff-hl hunk in the current buffer."
  (interactive)
  (goto-char (point-max))
  (diff-hl-previous-hunk))

(defun gsmlg-diff-hl-disable ()
  "Disable diff-hl in the current buffer."
  (interactive)
  (diff-hl-mode -1))

(defun gsmlg-diff-hl-transient-quit ()
  "Exit the active diff-hl Transient."
  (interactive)
  (transient-quit-one))

(defalias (intern (concat "gsmlg/git-" "gutter-first-hunk"))
  #'gsmlg-diff-hl-first-hunk)
(defalias (intern (concat "gsmlg/git-" "gutter-last-hunk"))
  #'gsmlg-diff-hl-last-hunk)
(defalias (intern (concat "gsmlg/git-" "gutter-off"))
  #'gsmlg-diff-hl-disable)

(use-package transient
  :demand t
  :config
  (transient-define-prefix gsmlg-diff-hl-transient ()
    "Navigate and operate on diff-hl hunks."
    [["Navigate"
      ("j" "Next hunk" diff-hl-next-hunk :transient t)
      ("k" "Previous hunk" diff-hl-previous-hunk :transient t)
      ("h" "First hunk" gsmlg-diff-hl-first-hunk :transient t)
      ("l" "Last hunk" gsmlg-diff-hl-last-hunk :transient t)]
     ["Hunk"
      ("p" "Show" diff-hl-show-hunk)
      ("s" "Stage" diff-hl-stage-current-hunk)
      ("r" "Revert" diff-hl-revert-hunk)
      ("R" "Reference revision" diff-hl-set-reference-rev-in-project)]
     ["Mode"
      ("q" "Quit" gsmlg-diff-hl-transient-quit)
      ("Q" "Disable here" gsmlg-diff-hl-disable)]]))

(use-package magit
  :defer t
  :config
  (setopt magit-save-repository-buffers 'dontask
          magit-diff-refine-hunk t
          magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1))

(use-package diff-hl
  :ensure
  (:type tar
   :host github
   :repo "dgutov/diff-hl")
  :demand t
  :config
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package git-link
  :defer t)

(use-package git-timemachine
  :ensure
  (:type tar
   :host github
   :repo "emacsmirror/git-timemachine")
  :defer t)

(use-package git-modes
  :defer t)

(use-package git-messenger
  :defer t
  :config
  (setopt git-messenger:show-detail t))

(provide 'gsmlg-vcs)
;;; gsmlg-vcs.el ends here
