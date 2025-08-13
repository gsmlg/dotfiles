
(defvar add-node-modules-path-debug nil
  "Enable verbose output when non nil.")

(defun add-node-modules-path ()
  "Search the current buffer's parent directories for `node_modules/.bin`.
If it's found, then add it to the `exec-path'."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                    (expand-file-name "node_modules/.bin/" root))))
    (if root
        (progn
          (make-local-variable 'exec-path)
          (add-to-list 'exec-path path)
          (when add-node-modules-path-debug
            (message (concat "added " path  " to exec-path"))))
      (when add-node-modules-path-debug
        (message (concat "node_modules not found in " root))))))


(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    ;; Disable js2 mode's syntax error highlighting by default...
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil
                  js2-skip-preprocessor-directives t)
    ;; ... but enable it if flycheck can't handle javascript
    (autoload 'flycheck-get-checker-for-buffer "flycheck")
    (defun gsmlg/disable-js2-checks-if-flycheck-active ()
      (unless (flycheck-get-checker-for-buffer)
        (set (make-local-variable 'js2-mode-show-parse-errors) t)
        (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
    (add-hook 'js2-jsx-mode-hook
	      (lambda () (setq-local sgml-basic-offset js2-basic-offset)))
    (add-hook 'js2-mode-hook
	      (lambda ()
                (add-node-modules-path)
                (gsmlg/disable-js2-checks-if-flycheck-active)
		(setq-local js-switch-indent-offset js2-basic-offset)))))

(use-package rjsx-mode
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :config
  (progn
    (add-hook 'rjsx-mode-hook
	      (lambda () (setq-local sgml-basic-offset js2-basic-offset)))
    (advice-add 'js-jsx-indent-line :after
                (lambda (&rest _)
                  "Workaround `sgml-mode' and follow airbnb component style."
                  (let* ((cur-line (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
                    (if (string-match "^\\( +\\)\/?> *$" cur-line)
                        (let* ((empty-spaces (match-string 1 cur-line)))
                          (replace-regexp empty-spaces
                                          (make-string (- (length empty-spaces) sgml-basic-offset) 32)
                                          nil
                                          (line-beginning-position) (line-end-position)))))))))

(use-package json-mode
  :ensure t)

(use-package tern
  :if (executable-find "tern")
  :ensure t
  :hook (js2-mode . tern-mode))

;; (use-package company-tern
;;   :if (executable-find "tern")
;;   :ensure t
;;   :after (company tern)
;;   :hook (js2-mode . (lambda () (add-to-list 'company-backends 'company-tern))))

(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-, r"))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

;; TODO: add indium - Javascript development environment

(provide 'init-javascript)
