

(use-package lsp-mode
  :hook ((js2-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (ruby-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode)


(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(provide 'init-lsp)
