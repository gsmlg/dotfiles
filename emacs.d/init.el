(setq user-emacs-directory "~/.dotfiles/emacs.d/")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))
(defconst *is-a-lin* (eq system-type 'gnu/linux))

(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el

;; (package-initialize)

(require 'init-elpa)
(require 'init-ui)
(require 'init-setting)
(require 'init-ibuffer)
(require 'init-helm)
(require 'init-company)
(require 'init-sessions)
(require 'init-flycheck)

;;; programe
(require 'init-lisp)
(require 'init-javascript)
(require 'init-web)
(require 'init-ruby)
(require 'init-rust)
(require 'init-yaml)
(require 'init-elixir)
(require 'init-markdown)
(require 'init-paredit)
(require 'init-conf)

(require 'init-git)
(require 'init-project)

;;; applications
(require 'init-org)
(require 'init-elfeed)
(require 'init-email)
(require 'init-music)


(when *is-a-mac*
  (gsmlg/mac-osx-remap-command))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(put 'set-goal-column 'disabled nil)
