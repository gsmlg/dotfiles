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
                (gsmlg/disable-js2-checks-if-flycheck-active)
		(setq-local js-switch-indent-offset js2-basic-offset)))))

(use-package rjsx-mode
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :config
  (progn
    (add-hook 'rjsx-mode-hook
	      (lambda () (setq-local sgml-basic-offset js2-basic-offset)))
    (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
      "Workaround `sgml-mode' and follow airbnb component style."
      (let* ((cur-line (buffer-substring-no-properties
			(line-beginning-position)
			(line-end-position))))
	(if (string-match "^\\( +\\)\/?> *$" cur-line)
	    (let* ((empty-spaces (match-string 1 cur-line)))
	      (replace-regexp empty-spaces
			      (make-string (- (length empty-spaces) sgml-basic-offset) 32)
			      nil
			      (line-beginning-position) (line-end-position))))))))

(use-package json-mode
  :ensure t)

(use-package tern
  :if (executable-find "tern")
  :ensure t
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :if (executable-find "tern")
  :ensure t
  :after (company tern)
  :hook (js2-mode . (lambda () (add-to-list 'company-backends 'company-tern))))

(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-, r"))

;; TODO: add indium - Javascript development environment

(provide 'init-javascript)
