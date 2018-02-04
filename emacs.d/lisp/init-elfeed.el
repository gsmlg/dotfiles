(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory "~/.emacs.d/elfeed"))

(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure
  :config
  (progn
    (setq rmh-elfeed-org-files '("~/.emacs.d/elfeed.org"))
    (elfeed-org)))


(provide 'init-elfeed)
