;;; gsmlg-completion.el --- Minibuffer and in-buffer completion -*- lexical-binding: t; -*-

;;; Commentary:
;; Compose standard completion APIs with Vertico, Consult, Corfu, Cape, and
;; Yasnippet.  Mode and Eglot CAPFs stay ahead of global Cape fallbacks.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)

(defun gsmlg-corfu-terminal-update (&optional frame)
  "Enable terminal Corfu rendering when FRAME is not graphical."
  (with-selected-frame (or frame (selected-frame))
    (when (and (not (display-graphic-p))
               (fboundp #'corfu-terminal-mode))
      (corfu-terminal-mode 1))))

(defun gsmlg-enable-yasnippet ()
  "Enable Yasnippet in the current programming or text buffer."
  (yas-minor-mode 1))

(use-package savehist
  :ensure nil
  :demand t)

(use-package vertico
  :demand t
  :config
  (setopt vertico-cycle t
          vertico-resize t)
  (vertico-mode 1))

(use-package orderless
  :demand t
  :config
  (setopt completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides
          '((file (styles partial-completion basic))
            (eglot (styles orderless basic)))))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

(use-package consult
  :demand t
  :config
  (setopt consult-preview-key '(:debounce 0.25 any)
          consult-narrow-key "<")
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :demand t
  :config
  (setopt prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t)

(use-package corfu
  :demand t
  :config
  (setopt corfu-auto t
          corfu-auto-delay 0.25
          corfu-auto-prefix 2
          corfu-cycle t
          corfu-preselect 'prompt
          corfu-quit-no-match 'separator)
  (global-corfu-mode 1))

(use-package popon
  :ensure
  (:type tar
   :host codeberg
   :repo "akib/emacs-popon")
  :defer t)

(use-package corfu-terminal
  :ensure
  (:type tar
   :host codeberg
   :repo "akib/emacs-corfu-terminal")
  :after corfu
  :config
  (add-hook 'after-make-frame-functions #'gsmlg-corfu-terminal-update)
  (gsmlg-corfu-terminal-update))

(use-package cape
  :demand t
  :config
  (add-hook 'completion-at-point-functions #'cape-file 90)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 95))

(use-package yasnippet
  :demand t
  :config
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets" gsmlg-config-directory))
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'gsmlg-enable-yasnippet)
  (add-hook 'text-mode-hook #'gsmlg-enable-yasnippet))

(provide 'gsmlg-completion)
;;; gsmlg-completion.el ends here
