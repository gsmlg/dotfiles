(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))

;; Slime allows very convenient navigation to the symbol at point (using M-.),
;; and the ability to pop back to previous marks (using M-,).
(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(provide 'init-lisp)



;; Make C-x C-e run 'eval-region if the region is active

(defun gsmlginc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(global-set-key [remap eval-expression] 'pp-eval-expression)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'gsmlginc/eval-last-sexp-or-region))

;(use-package ipretty
;  :ensure t
;  :config
;  (add-hook 'after-init-hook 'ipretty-mode))



(defun gsmlginc/make-pp-read-only (_expression out-buffer-name &rest _)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with \"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))

(advice-add 'pp-display-expression :after #'gsmlginc/make-pp-read-only)



(defun gsmlginc/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'gsmlginc/maybe-set-bundled-elisp-readonly)



;; Use C-c C-z to toggle between elisp files and an ielm session
;; I might generalise this to ruby etc., or even just adopt the repl-toggle package.

(defvar gsmlginc/repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(make-variable-buffer-local 'gsmlginc/repl-original-buffer)

(defvar gsmlginc/repl-switch-function 'switch-to-buffer-other-window)

(defun gsmlginc/switch-to-ielm ()
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall gsmlginc/repl-switch-function "*ielm*")
      (ielm))
    (setq gsmlginc/repl-original-buffer orig-buffer)))

(defun gsmlginc/repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if gsmlginc/repl-original-buffer
      (funcall gsmlginc/repl-switch-function gsmlginc/repl-original-buffer)
    (error "No original buffer")))

(after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'gsmlginc/switch-to-ielm))
(after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'gsmlginc/repl-switch-back))



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
(defun gsmlginc/byte-compile-file-batch (filename)
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
(defun gsmlginc/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defun gsmlginc/disable-indent-guide ()
  (when (bound-and-true-p indent-guide-mode)
    (indent-guide-mode -1)))

(defvar gsmlginc/lispy-modes-hook
  '(
    ;; enable-paredit-mode
    turn-on-eldoc-mode
    gsmlginc/disable-indent-guide
    gsmlginc/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(use-package aggressive-indent
  :config
  (add-to-list 'gsmlginc/lispy-modes-hook 'aggressive-indent-mode))

(defun gsmlginc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'gsmlginc/lispy-modes-hook))

(defun gsmlginc/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst gsmlginc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst gsmlginc/lispy-modes
  (append gsmlginc/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name gsmlginc/lispy-modes))
  (add-hook hook 'gsmlginc/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name gsmlginc/elispy-modes))
  (add-hook hook 'gsmlginc/emacs-lisp-setup))

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

(defvar gsmlginc/vc-reverting nil
  "Whether or not VC or Magit is currently reverting buffers.")

(defun gsmlginc/maybe-remove-elc (&rest _)
  "If reverting from VC, delete any .elc file that will now be out of sync."
  (when gsmlginc/vc-reverting
    (when (and (eq 'emacs-lisp-mode major-mode)
               buffer-file-name
               (string= "el" (file-name-extension buffer-file-name)))
      (let ((elc (concat buffer-file-name "c")))
        (when (file-exists-p elc)
          (message "Removing out-of-sync elc file %s" (file-name-nondirectory elc))
          (delete-file elc))))))

(advice-add 'revert-buffer :after #'gsmlginc/maybe-remove-elc)

(defun gsmlginc/with-vc-reverting (orig-fn &rest args)
  (let ((gsmlginc/vc-reverting t))
    (apply orig-fn args)))

(advice-add 'magit-revert-buffers :around #'gsmlginc/with-vc-reverting)
(advice-add 'vc-revert-buffer-internal :around #'gsmlginc/with-vc-reverting)



(use-package macrostep)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))



;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "<f1> K") 'find-function-on-key)



;; Extras for theme editing

(defvar gsmlginc/theme-mode-hook nil
  "Hook triggered when editing a theme file.")

(defun gsmlginc/run-theme-mode-hooks-if-theme ()
  "Run `gsmlginc/theme-mode-hook' if this appears to a theme."
  (when (string-match "\\(color-theme-\\|-theme\\.el\\)" (buffer-name))
    (run-hooks 'gsmlginc/theme-mode-hook)))

(add-hook 'emacs-lisp-mode-hook 'gsmlginc/run-theme-mode-hooks-if-theme t)

;; (when (maybe-require-package 'rainbow-mode)
;;   (add-hook 'gsmlginc/theme-mode-hook 'rainbow-mode)
;;   (add-hook 'help-mode-hook 'rainbow-mode))

;; (when (maybe-require-package 'aggressive-indent)
;;   ;; Can be prohibitively slow with very long forms
;;   (add-to-list 'gsmlginc/theme-mode-hook (lambda () (aggressive-indent-mode -1)) t))



;; (when (maybe-require-package 'highlight-quoted)
;;   (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))


;; (when (maybe-require-package 'flycheck)
;;   (require-package 'flycheck-package)
;;   (after-load 'flycheck
;;     (flycheck-package-setup)))



;; ERT
(after-load 'ert
  (define-key ert-results-mode-map (kbd "g") 'ert-results-rerun-all-tests))


(defun gsmlginc/cl-libify-next ()
  "Find next symbol from 'cl and replace it with the 'cl-lib equivalent."
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
     ((string-match "^\\(defun\\|defmacro\\)\\*$")
      (kill-sexp)
      (insert (concat "cl-" (match-string 1))))
     (t
      (insert "cl-")))
    (when (fboundp 'aggressive-indent-indent-defun)
      (aggressive-indent-indent-defun))))


;; (maybe-require-package 'cask-mode)



(provide 'init-lisp)
