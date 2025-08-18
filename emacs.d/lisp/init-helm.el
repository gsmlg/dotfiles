(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)
	 ("C-c r" . helm-recentf)
	 ("C-c i" . helm-imenu)
         ("C-x b" . helm-buffers-list))
  :config
  (progn
    (when (executable-find "mdfind")
      (setq helm-locate-command "mdfind %s -name %s"))
    ;; Toggle adaptive sorting in all sources
    (helm-adaptive-mode t)
    ;; Global helm mode
    (helm-mode t)))

;; helm-swoop removed — helm-occur provides equivalent functionality

;; Preserve previous helm-swoop keybindings by mapping them to helm-occur
;; with safe fallbacks when helm-swoop isn't available.
;; M-i: invoke search (previously helm-swoop)
;; M-I: resume last helm session (approx of back-to-last-point)
;; C-c M-i and C-x M-i: try multi-buffer search, fallback to helm-occur

;; Define safe wrapper functions that prefer helm-swoop if present
(defun my/helm-swoop-or-occur ()
  "Call helm-swoop if available, otherwise helm-occur."
  (interactive)
  (if (fboundp 'helm-swoop)
      (call-interactively 'helm-swoop)
    (call-interactively 'helm-occur)))

(defun my/helm-swoop-back-or-resume ()
  "Call helm-swoop-back-to-last-point if available, otherwise helm-resume."
  (interactive)
  (cond
   ((fboundp 'helm-swoop-back-to-last-point) (call-interactively 'helm-swoop-back-to-last-point))
   ((fboundp 'helm-resume) (call-interactively 'helm-resume))
   (t (message "No helm resume/back command available"))))

(defun my/helm-multi-swoop-or-multi-occur ()
  "Call helm-multi-swoop if available, otherwise try helm-multi-occur or helm-occur."
  (interactive)
  (cond
   ((fboundp 'helm-multi-swoop) (call-interactively 'helm-multi-swoop))
   ((fboundp 'helm-multi-occur) (call-interactively 'helm-multi-occur))
   (t (call-interactively 'helm-occur))))

(defun my/helm-swoop-from-isearch-or-occur ()
  "From isearch, exit and call helm-swoop-from-isearch if available, otherwise helm-occur."
  (interactive)
  (let ((pattern (if (and (boundp 'isearch-string) isearch-mode) isearch-string nil)))
    (when isearch-mode (isearch-exit))
    (cond
     ((and pattern (fboundp 'helm-swoop-from-isearch)) (helm-swoop-from-isearch))
     ((and pattern (fboundp 'helm-occur-from-isearch)) (helm-occur-from-isearch))
     (t (call-interactively 'helm-occur)))))

;; Install keybindings
(global-set-key (kbd "M-i") 'my/helm-swoop-or-occur)
(global-set-key (kbd "M-I") 'my/helm-swoop-back-or-resume)
(global-set-key (kbd "C-c M-i") 'my/helm-multi-swoop-or-multi-occur)
(global-set-key (kbd "C-x M-i") 'my/helm-multi-swoop-or-multi-occur)

(define-key isearch-mode-map (kbd "M-i") 'my/helm-swoop-from-isearch-or-occur)

(provide 'init-helm)
