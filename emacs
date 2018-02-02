(package-initialize)

(setq user-emacs-directory "~/.dotfiles/emacs.d")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)

(when (file-exists-p custom-file)
   (load custom-file))

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (progn
    (package-install 'use-package)))
(require 'use-package)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)
	 ("C-x C-r" . helm-recentf)
         ("C-x b" . helm-buffers-list)))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    (add-hook 'js2-mode-hook
	      #'(lambda ()
		  (setq js-switch-indent-offset js2-basic-offset)
		  (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
		  (define-key js2-mode-map "@" 'js-doc-insert-tag)))
    ))

;; Set the monospaced font size when mixed Chinese and English words
(defun gsmlg//set-monospaced-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

(gsmlg//set-monospaced-font  "Source Code Pro" "Hiragino Sans GB" 16 20)
