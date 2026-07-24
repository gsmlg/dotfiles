;;; init-lisp.el --- Configuration for init-lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-lisp.

;;; Code:

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))

;; Slime allows very convenient navigation to the symbol at point (using M-.),
;; and the ability to pop back to previous marks (using M-,).
(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))






;; Make C-x C-e run 'eval-region if the region is active

(defun gsmlg/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key [remap eval-expression] 'pp-eval-expression)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'gsmlg/eval-last-sexp-or-region))

;(use-package ipretty
;  :ensure t
;  :config
;  (add-hook 'after-init-hook 'ipretty-mode))




(defun gsmlg/make-pp-read-only (_expression out-buffer-name &rest _)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with \"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))

(advice-add 'pp-display-expression :after #'gsmlg/make-pp-read-only)




(defun gsmlg/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'gsmlg/maybe-set-bundled-elisp-readonly)




;; Use C-c C-z to toggle between elisp files and an ielm session
;; I might generalise this to ruby etc., or even just adopt the repl-toggle package.

(defvar gsmlg/repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(make-variable-buffer-local 'gsmlg/repl-original-buffer)

(defvar gsmlg/repl-switch-function 'switch-to-buffer-other-window)

(defun gsmlg/switch-to-ielm ()
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall gsmlg/repl-switch-function "*ielm*")
      (ielm))
    (setq gsmlg/repl-original-buffer orig-buffer)))

(defun gsmlg/repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if gsmlg/repl-original-buffer
      (funcall gsmlg/repl-switch-function gsmlg/repl-original-buffer)
    (error "No original buffer")))

(defvar ielm-map)
(defvar ert-results-mode-map)

(after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'gsmlg/switch-to-ielm))
(after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'gsmlg/repl-switch-back))




;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------
(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (add-to-list 'hippie-expand-try-functions-list 'my/try-complete-lisp-symbol-without-namespace t))


;; ----------------------------------------------------------------------------
;; Automatic byte compilation
;; ----------------------------------------------------------------------------
(use-package auto-compile
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'auto-compile-on-save-mode)
    (add-hook 'after-init-hook 'auto-compile-on-load-mode)))

;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)




(use-package immortal-scratch
  :ensure t
  :config
  (add-hook 'after-init-hook 'immortal-scratch-mode))




;;; Support byte-compilation in a sub-process, as
;;; required by highlight-cl
(defun gsmlg/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))




;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(defun gsmlg/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defun gsmlg/disable-indent-guide ()
  (when (bound-and-true-p indent-guide-mode)
    (indent-guide-mode -1)))

(defvar gsmlg/lispy-modes-hook
  '(
    ;; enable-paredit-mode
    turn-on-eldoc-mode
    gsmlg/disable-indent-guide
    gsmlg/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(use-package aggressive-indent
  :config
  (add-to-list 'gsmlg/lispy-modes-hook 'aggressive-indent-mode))

(defun gsmlg/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'gsmlg/lispy-modes-hook))

(defun gsmlg/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst gsmlg/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst gsmlg/lispy-modes
  (append gsmlg/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name gsmlg/lispy-modes))
  (add-hook hook 'gsmlg/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name gsmlg/elispy-modes))
  (add-hook hook 'gsmlg/emacs-lisp-setup))

(if (boundp 'eval-expression-minibuffer-setup-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (use-package eldoc-eval
    :config
    (add-hook 'after-init-hook 'eldoc-in-minibuffer-mode)))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

;; ----------------------------------------------------------------------------
;; Delete .elc files when reverting the .el from VC or magit
;; ----------------------------------------------------------------------------

;; When .el files are open, we can intercept when they are modified
;; by VC or magit in order to remove .elc files that are likely to
;; be out of sync.

;; This is handy while actively working on elisp files, though
;; obviously it doesn't ensure that unopened files will also have
;; their .elc counterparts removed - VC hooks would be necessary for
;; that.

(defvar gsmlg/vc-reverting nil
  "Whether or not VC or Magit is currently reverting buffers.")

(defun gsmlg/maybe-remove-elc (&rest _)
  "If reverting from VC, delete any .elc file that will now be out of sync."
  (when gsmlg/vc-reverting
    (when (and (eq 'emacs-lisp-mode major-mode)
               buffer-file-name
               (string= "el" (file-name-extension buffer-file-name)))
      (let ((elc (concat buffer-file-name "c")))
        (when (file-exists-p elc)
          (message "Removing out-of-sync elc file %s" (file-name-nondirectory elc))
          (delete-file elc))))))

(advice-add 'revert-buffer :after #'gsmlg/maybe-remove-elc)

(defun gsmlg/with-vc-reverting (orig-fn &rest args)
  (let ((gsmlg/vc-reverting t))
    (apply orig-fn args)))

(advice-add 'magit-revert-buffers :around #'gsmlg/with-vc-reverting)
(advice-add 'vc-revert-buffer-internal :around #'gsmlg/with-vc-reverting)




(use-package macrostep)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))




;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "<f1> K") 'find-function-on-key)




;; Extras for theme editing

(defvar gsmlg/theme-mode-hook nil
  "Hook triggered when editing a theme file.")

(defun gsmlg/run-theme-mode-hooks-if-theme ()
  "Run `gsmlg/theme-mode-hook' if this appears to a theme."
  (when (string-match "\\(color-theme-\\|-theme\\.el\\)" (buffer-name))
    (run-hooks 'gsmlg/theme-mode-hook)))

(add-hook 'emacs-lisp-mode-hook 'gsmlg/run-theme-mode-hooks-if-theme t)

;; (when (maybe-require-package 'rainbow-mode)
;;   (add-hook 'gsmlg/theme-mode-hook 'rainbow-mode)
;;   (add-hook 'help-mode-hook 'rainbow-mode))

;; (when (maybe-require-package 'aggressive-indent)
;;   ;; Can be prohibitively slow with very long forms
;;   (add-to-list 'gsmlg/theme-mode-hook (lambda () (aggressive-indent-mode -1)) t))




;; (when (maybe-require-package 'highlight-quoted)
;;   (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))



;; (when (maybe-require-package 'flycheck)
;;   (require-package 'flycheck-package)
;;   (after-load 'flycheck
;;     (flycheck-package-setup)))




;; ERT
(after-load 'ert
  (define-key ert-results-mode-map (kbd "g") 'ert-results-rerun-all-tests))



(defun gsmlg/cl-libify-next ()
  "Find next symbol from cl and replace it with the cl-lib equivalent."
  (interactive)
  (let ((case-fold-search nil))
    (re-search-forward
     (concat
      "("
      (regexp-opt
       ;; Not an exhaustive list
       '("loop" "incf" "plusp" "first" "decf" "minusp" "assert"
         "case" "destructuring-bind" "second" "third" "defun*"
         "defmacro*" "return-from" "labels" "cadar" "fourth"
         "cadadr") t)
      "\\_>")))
  (let ((form (match-string 1)))
    (backward-sexp)
    (cond
     ((string-match "^\\(defun\\|defmacro\\)\\*$" form)
      (kill-sexp)
      (insert (concat "cl-" (match-string 1))))
     (t
      (insert "cl-")))
    (when (fboundp 'aggressive-indent-indent-defun)
      (aggressive-indent-indent-defun))))


;; (maybe-require-package 'cask-mode)

(provide 'init-lisp)
;;; init-lisp.el ends here
