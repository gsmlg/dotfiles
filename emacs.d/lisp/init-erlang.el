;;; init-erlang.el --- Configuration for init-erlang -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-erlang.

;;; Code:

(use-package erlang
  :ensure t
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
         ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)))

(provide 'init-erlang)
;;; init-erlang.el ends here
