;;; init-markdown.el --- Configuration for init-markdown -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-markdown.

;;; Code:

(defvar whitespace-cleanup-mode-ignore-modes)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode(("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))

(provide 'init-markdown)
;;; init-markdown.el ends here
