(use-package elfeed
  :ensure t
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" gsmlg/cache-directory)))

(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure t
  :config
  (progn
    (setq rmh-elfeed-org-files '("~/.emacs.d/elfeed.org"))
    (elfeed-org)))


(provide 'init-elfeed)
