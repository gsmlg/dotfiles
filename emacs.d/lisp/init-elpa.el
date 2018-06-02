(require 'package)

 (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")))
;; (setq package-archives '(("gnu" . "http://elpa.gsmlg.org:8080/gnu/")
;;                         ("melpa" . "http://elpa.gsmlg.org:8080/melpa/")
;;                         ("org" . "http://elpa.gsmlg.org:8080/org/")))


;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(provide 'init-elpa)
