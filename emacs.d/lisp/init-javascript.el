(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    (add-hook 'js2-jsx-mode-hook
	      (lambda () (setq-local sgml-basic-offset js2-basic-offset)))
    (add-hook 'js2-mode-hook
	      (lambda ()
		(setq-local js-switch-indent-offset js2-basic-offset)
		(define-key js2-mode-map "@" 'js-doc-insert-tag)))))

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


(use-package tern
  :config
  (when (executable-find "tern")
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))))


(provide 'init-javascript)
