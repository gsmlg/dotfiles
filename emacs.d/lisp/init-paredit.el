(use-package paredit
  :ensure t
  :diminish "Paredit"
  :hook ((paredit-mode . maybe-map-paredit-newline)
         ;; Use paredit in the minibuffer
         ;; TODO: break out into separate package
         ;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
         (minibuffer-setup . conditionally-enable-paredit-mode))
  :config
  (progn
    (autoload 'enable-paredit-mode "paredit")
    (dolist (binding '("C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
      (define-key paredit-mode-map (read-kbd-macro binding) nil))
    ;; Compatibility with other modes
    (suspend-mode-during-cua-rect-selection 'paredit-mode)
    (defvar paredit-minibuffer-commands '(eval-expression
                                          pp-eval-expression
                                          eval-expression-with-eldoc
                                          ibuffer-do-eval
                                          ibuffer-do-view-and-eval)
      "Interactive commands for which paredit should be enabled in the minibuffer.")))

(defun maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

;; ----------------------------------------------------------------------------
;; Enable some handy paredit functions in all prog modes
;; ----------------------------------------------------------------------------
(use-package paredit-everywhere
  :ensure t
  :hook ((prog-mode . paredit-everywhere-mode)
         (css-mode . paredit-everywhere-mode))
  :config
  (define-key paredit-everywhere-mode-map (kbd "M-s") nil))

(provide 'init-paredit)
