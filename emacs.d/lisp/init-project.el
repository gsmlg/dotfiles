;;--------------------------------------------------------------------
;; set projectile for manage projects
;; projectile.el at https://github.com/bbatsov/projectile
;;--------------------------------------------------------------------
(use-package projectile
  :init
  (setq projectile-enable-idle-timer t
	projectile-idle-timer-hook '()
	projectile-cache-file (expand-file-name "projectile.cache" gsmlg-cache-directory)
	projectile-file-exists-remote-cache-expire (* 10 60)
	projectile-file-exists-local-cache-expire (* 3 60)
	projectile-enable-caching t)
  :bind-keymap ("C-c p" . projectile-command-map)
  :ensure t
  :config
  (progn
    (projectile-mode)
    (add-hook 'projectile-after-switch-project-hook 'editorconfig-apply)))

(use-package helm-projectile
  :ensure t
  :config
  (progn
    (helm-projectile-on)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(provide 'init-project)
