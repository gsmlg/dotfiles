(use-package company
  :ensure t
  :init
  (setq-default company-dabbrev-other-buffers 'all
		company-tooltip-align-annotations t)
  :bind ("M-C-/" . company-complete)
  :config
  (progn
    (diminish 'company-mode "CMP")
    (define-key company-mode-map (kbd "M-/") 'company-complete)
    (define-key company-active-map (kbd "M-/") 'company-select-next)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package company-quickhelp
  :ensure t
  :config
  (add-hook 'after-init-hook 'company-quickhelp-mode))

;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(after-load 'company
  (after-load 'page-break-lines
    (defvar gsmlginc/page-break-lines-on-p nil)
    (make-variable-buffer-local 'gsmlginc/page-break-lines-on-p)

    (defun gsmlginc/page-break-lines-disable (&rest ignore)
      (when (setq gsmlginc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun gsmlginc/page-break-lines-maybe-reenable (&rest ignore)
      (when gsmlginc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'gsmlginc/page-break-lines-disable)
    (add-hook 'company-completion-finished-hook 'gsmlginc/page-break-lines-maybe-reenable)
    (add-hook 'company-completion-cancelled-hook 'gsmlginc/page-break-lines-maybe-reenable)))


(provide 'init-company)
