;;; init-erlang.el --- Erlang language support configuration -*- lexical-binding: t; -*-

(use-package erlang
  :ensure t
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
         ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)))

(provide 'init-erlang)
;;; init-erlang.el ends here
