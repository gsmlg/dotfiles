;;; project-test.el --- project.el and worktree tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Validate real Git worktree roots and project-aware search directory choices.

;;; Code:

(require 'test-helper)
(require 'project)
(require 'gsmlg-project)

(ert-deftest gsmlg-project-git-worktrees-have-independent-roots ()
  "Visited files in a repository and worktree resolve and search independently."
  (let* ((root (make-temp-file "gsmlg-project-" t))
         (worktree (concat root "-worktree"))
         (root-file (expand-file-name "root.txt" root))
         (worktree-file (expand-file-name "worktree.txt" worktree))
         root-buffer
         worktree-buffer
         root-search
         worktree-search)
    (unwind-protect
        (progn
          (gsmlg-test-git "-C" root "init")
          (gsmlg-test-git "-C" root "config" "user.email" "test@example.invalid")
          (gsmlg-test-git "-C" root "config" "user.name" "GSMLG Test")
          (write-region "root" nil root-file nil 'silent)
          (gsmlg-test-git "-C" root "add" "root.txt")
          (gsmlg-test-git "-C" root "commit" "-m" "initial")
          (gsmlg-test-git "-C" root "worktree" "add" "-b" "test-worktree"
                          worktree)
          (write-region "worktree" nil worktree-file nil 'silent)
          (setq root-buffer (find-file-noselect root-file)
                worktree-buffer (find-file-noselect worktree-file))
          (let (root-project worktree-project)
            (with-current-buffer root-buffer
              (setq root-project (project-current nil))
              (cl-letf (((symbol-function #'consult-ripgrep)
                         (lambda (&optional directory _initial)
                           (setq root-search directory))))
                (gsmlg-project-search)))
            (with-current-buffer worktree-buffer
              (setq worktree-project (project-current nil))
              (cl-letf (((symbol-function #'consult-ripgrep)
                         (lambda (&optional directory _initial)
                           (setq worktree-search directory))))
                (gsmlg-project-search)))
            (should root-project)
            (should worktree-project)
            (should
             (equal (file-truename (project-root root-project))
                    (file-name-as-directory (file-truename root))))
            (should
             (equal (file-truename (project-root worktree-project))
                    (file-name-as-directory (file-truename worktree))))
            (should-not
             (equal (file-truename (project-root root-project))
                    (file-truename (project-root worktree-project))))
            (should
             (equal (file-truename root-search)
                    (file-name-as-directory (file-truename root))))
            (should
             (equal (file-truename worktree-search)
                    (file-name-as-directory (file-truename worktree))))))
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer))
      (when (buffer-live-p worktree-buffer)
        (kill-buffer worktree-buffer))
      (when (file-directory-p worktree)
        (delete-directory worktree t))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest gsmlg-project-search-uses-current-project-root ()
  "The Consult project search wrapper should preserve the project root."
  (let* ((root (make-temp-file "gsmlg-search-project-" t))
         (default-directory root)
         called-directory)
    (unwind-protect
        (progn
          (gsmlg-test-git "-C" root "init")
          (cl-letf (((symbol-function #'consult-ripgrep)
                     (lambda (&optional directory _initial)
                       (setq called-directory directory))))
            (gsmlg-project-search)
            (should (equal called-directory
                           (file-name-as-directory root)))))
      (delete-directory root t))))

(ert-deftest gsmlg-project-envrc-respects-enable-setting ()
  "Envrc-global-mode should stay off when `gsmlg-envrc-enable' is nil."
  (let ((gsmlg-envrc-enable nil)
        (noninteractive nil)
        (envrc-global-mode nil)
        enabled)
    (cl-letf (((symbol-function #'envrc-global-mode)
               (lambda (&optional _arg)
                 (setq enabled t))))
      (gsmlg-project-enable-envrc-maybe)
      (should-not enabled)
      (let ((gsmlg-envrc-enable t))
        (gsmlg-project-enable-envrc-maybe)
        (should enabled)))))

(ert-deftest gsmlg-project-envrc-guard-blocks-mode-when-disabled ()
  "Buffer-local envrc-mode must not enable while `gsmlg-envrc-enable' is nil."
  (let ((gsmlg-envrc-enable nil)
        (envrc-mode nil)
        called)
    (cl-letf (((symbol-function #'envrc-mode)
               (lambda (&optional _arg)
                 (setq called t
                       envrc-mode t))))
      (should-not (gsmlg-envrc--guard-mode #'envrc-mode 1))
      (should-not called)
      (should-not envrc-mode)
      (let ((gsmlg-envrc-enable t))
        (gsmlg-envrc--guard-mode #'envrc-mode 1)
        (should called)
        (should envrc-mode)))))

(ert-deftest gsmlg-project-envrc-suppresses-blocked-display ()
  "Blocked direnv diagnostics must not call display-buffer."
  (let ((buffer (get-buffer-create "*envrc*"))
        (saved nil)
        displayed)
    (with-current-buffer buffer
      (setq saved (buffer-string))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "──── 2026-08-31 14:35:27 ──── /tmp/project/ ────\n\n"
                "direnv: error /tmp/project/.envrc is blocked. "
                "Run 'direnv allow' to approve its content\n"))
      (cl-letf (((symbol-function #'display-buffer)
                 (lambda (buffer-or-name &optional _action _frame)
                   (setq displayed buffer-or-name)
                   (get-buffer buffer-or-name))))
        (should (eq buffer
                    (gsmlg-envrc--display-buffer-maybe
                     #'display-buffer buffer)))
        (should-not displayed))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert saved)))))

(provide 'project-test)
;;; project-test.el ends here
