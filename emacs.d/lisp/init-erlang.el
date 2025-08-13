;;; init-erlang.el --- Erlang language support configuration -*- lexical-binding: t; -*-

;; This file configures Erlang mode using use-package.
;; It sets up file extensions, ensures the package is installed,
;; and integrates with optional LSP and tree-sitter modules if they are enabled.

(use-package erlang
  :ensure t
  :mode ("\\.erl\\'" . erlang-mode)
  :mode ("\\.hrl\\'" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)
  :config
  (when (modulep! +lsp)
    (add-hook 'erlang-mode-local-vars-hook #'lsp! 'append))
  (when (modulep! +tree-sitter)
    (add-hook 'erlang-mode-local-vars-hook #'tree-sitter! 'append)))

(provide 'init-erlang)
;;; init-erlang.el ends here
