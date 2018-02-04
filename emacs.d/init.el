(setq user-emacs-directory "~/.dotfiles/emacs.d")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))
(defconst *is-a-lin* (eq system-type 'gnu/linux))

(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el

;; (package-initialize)

(require 'init-elpa)
(require 'init-ui)
(require 'init-helm)

;;; programe
(require 'init-lisp)
(require 'init-javascript)


;;; applications


(when (file-exists-p custom-file)
  (load custom-file))

