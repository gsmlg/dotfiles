;;; gsmlg-lang-beam.el --- Elixir, HEEx, and Erlang modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Prefer built-in tree-sitter modes while retaining maintained fallback
;; packages when grammars are supplied externally.

;;; Code:

(require 'gsmlg-eglot)
(require 'gsmlg-bootstrap)

(declare-function elixir-ts-mode "elixir-ts-mode" ())
(declare-function erlang-ts-mode "erlang-ts-mode" ())
(declare-function heex-ts-mode "heex-ts-mode" ())

(defun gsmlg-elixir-mode ()
  "Select Elixir tree-sitter mode or maintained `elixir-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'elixir #'elixir-ts-mode #'elixir-mode))

(defun gsmlg-heex-mode ()
  "Select HEEx tree-sitter mode or maintained `web-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'heex #'heex-ts-mode #'web-mode))

(defun gsmlg-erlang-mode ()
  "Select Erlang tree-sitter mode when available, else `erlang-mode'."
  (interactive)
  (gsmlg-treesit-or-fallback 'erlang #'erlang-ts-mode #'erlang-mode))

(defun gsmlg-lang-beam-register-auto-modes ()
  "Place GSMLG BEAM dispatchers ahead of package-provided entries."
  (dolist (entry '(("\\.\\(?:erl\\|hrl\\)\\'" . gsmlg-erlang-mode)
                   ("\\.heex\\'" . gsmlg-heex-mode)
                   ("\\.exs?\\'" . gsmlg-elixir-mode)))
    (gsmlg-auto-mode-prepend entry)))

(gsmlg-lang-beam-register-auto-modes)

(use-package elixir-mode
  :defer t)

(use-package erlang
  :ensure
  (:type tar
   :host github
   :repo ("erlang/otp" . "otp")
   :files ("lib/tools/emacs/*.el"
           (:exclude "lib/tools/emacs/erlang_appwiz.el")))
  :defer t)

(provide 'gsmlg-lang-beam)
;;; gsmlg-lang-beam.el ends here
