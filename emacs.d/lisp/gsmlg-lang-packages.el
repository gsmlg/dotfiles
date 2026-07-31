;;; gsmlg-lang-packages.el --- Deferred language package declarations -*- lexical-binding: t; -*-

;;; Commentary:
;; Queue language-mode packages during core startup so Elpaca activates locked
;; builds, while file-type dispatch modules remain application-deferred.

;;; Code:

(require 'gsmlg-bootstrap)

(defvar native-comp-jit-compilation-deny-list)

(defconst gsmlg-go-mode-native-compilation-deny-regexp
  "/go-mode\\.el\\'"
  "Match upstream go-mode source files with undeclared optional client APIs.")

(with-eval-after-load 'comp-run
  ;; Upstream issue dominikh/go-mode.el#446 tracks the missing declarations.
  (add-to-list 'native-comp-jit-compilation-deny-list
               gsmlg-go-mode-native-compilation-deny-regexp))

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

(use-package web-mode
  :defer t)

(use-package typescript-mode
  :defer t)

(use-package rust-mode
  :defer t)

(use-package go-mode
  :defer t)

(use-package zig-mode
  :ensure
  (:type tar
   :host github
   :repo "ziglang/zig-mode")
  :defer t)

(use-package nix-mode
  :ensure
  (:host github
   :repo "NixOS/nix-mode"
   :files (:defaults (:exclude "nix-c?mpany.el" "nix-mode-mmm.el")))
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package terraform-mode
  :defer t)

(use-package markdown-mode
  :defer t)

(use-package macrostep
  :commands (macrostep-expand))

(provide 'gsmlg-lang-packages)
;;; gsmlg-lang-packages.el ends here
