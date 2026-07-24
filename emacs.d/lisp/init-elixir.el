;;; init-elixir.el --- Configuration for init-elixir -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-elixir.

;;; Code:

(use-package elixir-mode
  :ensure t
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.heex\\'"))

(use-package alchemist
  :ensure t
  :defer t)

(provide 'init-elixir)
;;; init-elixir.el ends here
