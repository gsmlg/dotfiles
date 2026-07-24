;; -*- lexical-binding: t; -*-
(use-package elfeed
  :ensure t
  :commands elfeed
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" gsmlg/cache-directory)))

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory)))
  (elfeed-org))


(provide 'init-elfeed)
