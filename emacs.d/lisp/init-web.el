(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
    (defun my-web-mode-hook ()
      "Hooks for Web mode."
      ;; HTML element offset indentation
      (setq web-mode-markup-indent-offset preferred-indent-level)
      ;; CSS offset indentation
      (setq web-mode-css-indent-offset preferred-indent-level)
      ;; Script/code offset indentation (for JavaScript, Java, PHP, Ruby, VBScript, Python, etc.)
      (setq web-mode-code-indent-offset preferred-indent-level)
      ;; By default, tag attributes are indented like this:
      ;; <img src="pix.png"
      ;;      class="noborder"/>
      ;; You can force a fixed indentation with web-mode-attr-indent-offset
      ;; <img src="pix.png"
      ;;   class="noborder"/>
      ;; (setq web-mode-attr-indent-offset preferred-indent-level)
      ;; Left padding
      ;; For <style> parts
      (setq web-mode-style-padding preferred-indent-levelp)
      ;; For <script> parts
      (setq web-mode-script-padding preferred-indent-level)
      ;; For multi-line blocks
      (setq web-mode-block-padding 0)
      ;; Enable / disable features
      ;; Auto-pairing
      (setq web-mode-enable-auto-pairing t)
      ;; CSS colorization
      (setq web-mode-enable-css-colorization t)
      ;; Block face: can be used to set blocks background and default foreground (see web-mode-block-face)
      (setq web-mode-enable-block-face t)
      ;; Part face: can be used to set parts background and default foreground (see web-mode-script-face and web-mode-style-face which inheritate from web-mode-part-face)
      (setq web-mode-enable-part-face t)
      ;; Comment keywords (see web-mode-comment-keyword-face)
      (setq web-mode-enable-comment-keywords t)
      ;; Heredoc (cf. PHP strings) fontification (when the identifier is <<<EOTHTML or <<<EOTJAVASCRIPT)
      (setq web-mode-enable-heredoc-fontification t))
    (add-hook 'web-mode-hook  'my-web-mode-hook)))


;;; Colourise CSS colour literals
(use-package rainbow-mode
  :ensure t
  :config
  (dolist (hook '(css-mode-hook html-mode-hook))
    (add-hook hook 'rainbow-mode)))


;;; LESS
(use-package less-css-mode
  :ensure t)
(use-package skewer-less
  :ensure t
  :hook (less-css-mode . skewer-less-mode))


;; Skewer CSS
(use-package skewer-mode
  :ensure t
  :hook (css-mode . skewer-css-mode))


;;; Use eldoc for syntax hints
(use-package css-eldoc
  :ensure t
  :hook (css-mode . turn-on-css-eldoc)
  :config
  (autoload 'turn-on-css-eldoc "css-eldoc"))



(use-package httprepl
  :ensure t)

(use-package restclient
  :ensure t
  :config
  (defun gsmlg/rest-client ()
    (interactive)
    (with-current-buffer (get-buffer-create "*rest-client*")
      (restclient-mode)
      (pop-to-buffer (current-buffer)))))


(provide 'init-web)
