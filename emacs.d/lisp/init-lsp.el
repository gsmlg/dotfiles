

(use-package lsp-mode
  :hook ((js2-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (ruby-mode . lsp-deferred))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)


(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(provide 'init-lsp)
