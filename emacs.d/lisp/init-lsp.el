

(use-package lsp-mode
  :hook ((js2-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (ruby-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode)


(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(provide 'init-lsp)
