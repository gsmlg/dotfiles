;;; init-elpa.el --- Configuration for init-elpa -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-elpa.

;;; Code:

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap `use-package' (built-in in Emacs 29+)
(unless (or (locate-library "use-package") (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(provide 'init-elpa)
;;; init-elpa.el ends here
