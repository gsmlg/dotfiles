;; -*- lexical-binding: t; -*-
(use-package elixir-mode
  :ensure t
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.heex\\'"))

(use-package alchemist
  :ensure t
  :defer t)

(provide 'init-elixir)
