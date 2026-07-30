# Legacy keybinding migration

The executable compatibility contract is
`gsmlg-keybinding-contract` in `lisp/gsmlg-keybindings.el`.  ERT loads each
required feature and checks every applicable contract entry with `lookup-key`.
Platform-specific entries run on that platform; the optional macOS
`org-mac-link` entry is skipped only when that optional package is absent.
This document also records generated legacy prefix bindings that intentionally
have no modern equivalent.

Status meanings:

- **Exact** keeps the key and the same user-visible command.
- **Semantic replacement** keeps the key and intent with a modern command.
- **Intentional deviation** changes or removes an operation because the old
  implementation has no safe modern equivalent, conflicts with the required
  modern prefix, or belonged to a deliberately disabled module.

## Global editing

| Scope/map | Key | Old command | New command | Status | Reason |
|---|---|---|---|---|---|
| Global | `C-c j` | `join-line` | `join-line` | Exact | Unchanged. |
| Global | `C-h` | `delete-backward-char` | `delete-backward-char` | Exact | Deliberately retains the nonstandard muscle-memory binding. |
| Global | `RET` | `newline-and-indent` | `newline-and-indent` | Exact | Unchanged. |
| Global | `S-<return>` | `gsmlg/newline-at-end-of-line` | `gsmlg-newline-at-end-of-line` | Exact | The old public name remains an alias. |
| Global | `M-Z` | `zap-up-to-char` | `zap-up-to-char` | Exact | Unchanged. |
| Global | `M-Y` | `browse-kill-ring` | `consult-yank-pop` | Semantic replacement | Uses Consult over the standard kill ring. |
| Vertico | `C-g` | `browse-kill-ring-quit` | `abort-recursive-edit` | Semantic replacement | Exits the minibuffer replacement UI. |
| Vertico | `M-n` | `browse-kill-ring-forward` | `vertico-next` | Semantic replacement | Moves to the next candidate. |
| Vertico | `M-p` | `browse-kill-ring-previous` | `vertico-previous` | Semantic replacement | Moves to the previous candidate. |
| Global | `C-=` | `er/expand-region` | `er/expand-region` | Exact | Unchanged. |
| Global | `C-.` | `set-mark-command` | `set-mark-command` | Exact | Reserved for mark activation; Embark does not replace it. |
| Global | `C-x C-.` | `pop-global-mark` | `pop-global-mark` | Exact | Unchanged. |
| Global | `C-<` | `mc/mark-previous-like-this` | `mc/mark-previous-like-this` | Exact | Unchanged. |
| Global | `C->` | `mc/mark-next-like-this` | `mc/mark-next-like-this` | Exact | Unchanged. |
| Global | `C-+` | `mc/mark-next-like-this` | `mc/mark-next-like-this` | Exact | Unchanged. |
| Global | `C-c C-<` | `mc/mark-all-like-this` | `mc/mark-all-like-this` | Exact | Unchanged. |
| `C-c m` prefix | `r` | `set-rectangular-region-anchor` | `set-rectangular-region-anchor` | Exact | Unchanged. |
| `C-c m` prefix | `c` | `mc/edit-lines` | `mc/edit-lines` | Exact | Unchanged. |
| `C-c m` prefix | `e` | `mc/edit-ends-of-lines` | `mc/edit-ends-of-lines` | Exact | Unchanged. |
| `C-c m` prefix | `a` | `mc/edit-beginnings-of-lines` | `mc/edit-beginnings-of-lines` | Exact | Unchanged. |
| Global | `M-<left>` | Unbound | Unbound | Exact | Continues to fall through to no global command. |
| Global | `M-<right>` | Unbound | Unbound | Exact | Continues to fall through to no global command. |
| Global | `C-M-<backspace>` | `kill-back-to-indentation` | `gsmlg-kill-back-to-indentation` | Exact | The old generic name remains an alias. |
| Global | `M-<up>` | `md/move-lines-up` | `move-dup-move-lines-up` | Semantic replacement | Uses the current command name from the same maintained package. |
| Global | `M-<down>` | `md/move-lines-down` | `move-dup-move-lines-down` | Semantic replacement | Uses the current command name from the same maintained package. |
| Global | `M-S-<up>` | `md/move-lines-up` | `move-dup-move-lines-up` | Semantic replacement | Uses the current command name. |
| Global | `M-S-<down>` | `md/move-lines-down` | `move-dup-move-lines-down` | Semantic replacement | Uses the current command name. |
| JavaScript refactor mode, now global | `C-S-<up>` | `js2r-move-line-up` | `move-dup-move-lines-up` | Semantic replacement | Preserves line movement without the retired parser-specific package. |
| JavaScript refactor mode, now global | `C-S-<down>` | `js2r-move-line-down` | `move-dup-move-lines-down` | Semantic replacement | Preserves line movement without the retired parser-specific package. |
| Global | `C-c d` | `md/duplicate-down` | `move-dup-duplicate-down` | Semantic replacement | Uses the current command name. |
| Global | `C-c u` | `md/duplicate-up` | `move-dup-duplicate-up` | Semantic replacement | Uses the current command name. |
| Global remap | `backward-up-list` | `backward-up-sexp` | `gsmlg-backward-up-sexp` | Exact | Retains quote-aware upward movement; the old generic name remains an alias. |
| Global | `C-o` | `gsmlg/open-line-with-reindent` | `gsmlg-open-line-with-reindent` | Exact | The old public name remains an alias. |
| Global | `C-z` | `gsmlg/maybe-suspend-frame` | `gsmlg-maybe-suspend-frame` | Exact | Still avoids hiding a graphical macOS frame and suspends elsewhere. |
| Global | `C-x C-b` | `ibuffer` | `ibuffer` | Exact | Unchanged. |
| Global | `C-x u` | `undo-tree-visualize` | `vundo` | Semantic replacement | Preserves the visual undo-history entry point using the maintained native-undo frontend. |
| Dired | `C-l` | `dired-up-directory` | `dired-up-directory` | Exact | Returns to the parent directory. |
| Dired | `C-j` | `dired-find-file` | `dired-find-file` | Exact | Enters the directory at point or visits the file at point. |
| Global | `C-;` | Unassigned | `embark-act` | Intentional deviation | Embark uses a nonconflicting key because `C-.` remains mark activation. |
| Global | `C-c C-;` | Unassigned | `embark-dwim` | Intentional deviation | Adds the companion Embark action without taking a legacy key. |

## Minibuffer, navigation, search, and completion

| Scope/map | Key | Old command | New command | Status | Reason |
|---|---|---|---|---|---|
| Global | `M-x` | `helm-M-x` | `execute-extended-command` | Semantic replacement | Vertico and Orderless provide the completion UI. |
| Global | `C-x C-m` | `helm-M-x` | `execute-extended-command` | Semantic replacement | Retains the alternate command launcher. |
| Global | `C-x C-f` | `helm-find-files` | `find-file` | Semantic replacement | Uses standard completion with Vertico. |
| Global | `C-c r` | `helm-recentf` | `consult-recent-file` | Semantic replacement | Consult replacement. |
| Global | `C-c i` | `helm-imenu` | `consult-imenu` | Semantic replacement | Consult replacement. |
| Global | `C-x b` | `helm-buffers-list` | `consult-buffer` | Semantic replacement | Consult replacement. |
| Global | `M-i` | `my/helm-swoop-or-occur` | `gsmlg-consult-line` | Semantic replacement | Searches the current buffer and records public history state. |
| Global | `M-I` | `my/helm-swoop-back-or-resume` | `gsmlg-consult-line-resume` | Semantic replacement | Returns to a live origin marker and reuses the last line query. |
| Global | `C-c M-i` | `my/helm-multi-swoop-or-multi-occur` | `consult-line-multi` | Semantic replacement | Consult multi-buffer line search. |
| Global | `C-x M-i` | `my/helm-multi-swoop-or-multi-occur` | `consult-line-multi` | Semantic replacement | Consult multi-buffer line search. |
| Isearch | `M-i` | `my/helm-swoop-from-isearch-or-occur` | `gsmlg-consult-line-from-isearch` | Semantic replacement | Exits Isearch and seeds Consult from `isearch-string`. |
| Global | `M-C-/` | `company-complete` | `completion-at-point` | Semantic replacement | Uses the standard CAPF path shared by Corfu and Eglot. |
| Global, outside popup | `M-/` | `company-complete` | `completion-at-point` | Semantic replacement | Uses the standard CAPF path. |
| Corfu popup | `M-/` | `company-select-next` | `corfu-next` | Semantic replacement | Retains popup navigation. |
| Corfu popup | `C-n` | `company-select-next` | `corfu-next` | Semantic replacement | Retains popup navigation. |
| Corfu popup | `C-p` | `company-select-previous` | `corfu-previous` | Semantic replacement | Retains popup navigation. |

`M-I` is not an alias for `M-i`.  The implementation records a marker and a
dedicated minibuffer-history variable through public hooks, returns to the
marker's live buffer, and supplies the last query as Consult's initial input.

## Project prefix

`C-c p` remains a named prefix, now backed exclusively by `project.el`.

| Scope/map | Key | Old command | New command | Status | Reason |
|---|---|---|---|---|---|
| `C-c p` | `p` | `projectile-switch-project` | `project-switch-project` | Semantic replacement | Built-in project selection. |
| `C-c p` | `f` | `projectile-find-file` | `project-find-file` | Semantic replacement | Built-in project file selection. |
| `C-c p` | `b` | `projectile-switch-to-buffer` | `project-switch-to-buffer` | Semantic replacement | Built-in project buffer selection. |
| `C-c p` | `d` | `projectile-find-dir` | `project-dired` | Semantic replacement | Opens the project root in Dired. |
| `C-c p` | `D` | `projectile-dired` | `project-dired` | Semantic replacement | Direct equivalent. |
| `C-c p` | `k` | `projectile-kill-buffers` | `project-kill-buffers` | Semantic replacement | Direct built-in equivalent. |
| `C-c p` | `c` | `projectile-command-map` lifecycle prefix | `project-compile` | Semantic replacement | Preserves project compilation as a direct built-in command. |
| `C-c p` | `e` | `projectile-recentf` | `project-eshell` | Intentional deviation | The required modern prefix reserves `e` for a project-local Eshell. |
| `C-c p` | `s` | Projectile search prefix | `gsmlg-project-search` | Semantic replacement | Searches project contents with Consult Ripgrep at `project-root`. |
| `C-c p` | `S` | Unassigned | `project-eshell` | Intentional deviation | Opens Eshell at the project root. |
| `C-c p` | `!` | `projectile-run-shell-command-in-root` | `project-shell-command` | Semantic replacement | Built-in project command. |
| `C-c p` | `&` | `projectile-run-async-shell-command-in-root` | `project-async-shell-command` | Semantic replacement | Built-in project command. |
| `C-c p` | `?` | `projectile-find-references` | `xref-find-references` | Semantic replacement | Uses Xref with the active Eglot/project backend. |
| `C-c p` | `g` | `projectile-find-file-dwim` | `gsmlg-project-search` | Intentional deviation | The required modern prefix assigns `g` to Consult Ripgrep at `project-root`. |
| `C-c p` | `I` | `projectile-ibuffer` | `project-list-buffers` | Semantic replacement | Built-in project buffer listing. |
| `C-c p` | `o` | `projectile-multi-occur` | `gsmlg-project-search` | Semantic replacement | Consult Ripgrep supersedes project Occur. |
| `C-c p` | `q` | `projectile-switch-open-project` | `project-switch-project` | Semantic replacement | Built-in project selection includes remembered projects. |
| `C-c p` | `r` | `projectile-replace` | `project-query-replace-regexp` | Semantic replacement | Built-in project replacement using regular expressions. |
| `C-c p` | `v` | `projectile-vc` | `project-vc-dir` | Semantic replacement | Built-in project VC entrypoint. |

The generated Projectile map exposed many implementation-specific commands.
They were inventoried rather than silently dropped:

| Legacy `C-c p` keys | Old intent | New access | Status and reason |
|---|---|---|---|
| `4 4`, `4 a`, `4 b`, `4 C-o`, `4 d`, `4 D`, `4 f`, `4 g`, `4 j`, `4 p`, `4 t` | Force the next project result into another window | Standard `C-x 4` display prefixes plus the project commands above | Intentional deviation; no duplicate project-specific display layer. |
| `5 5`, `5 a`, `5 b`, `5 d`, `5 D`, `5 f`, `5 g`, `5 j`, `5 p`, `5 t` | Force the next project result into another frame | Standard `C-x 5` display prefixes plus the project commands above | Intentional deviation; no duplicate project-specific display layer. |
| `a` | Find a related/other file | `project-find-file`, Xref, or the language server's code actions | Intentional deviation; there is no language-neutral built-in related-file rule. |
| `A` | Add a known project | Projects are remembered by `project.el` when visited or selected | Intentional deviation; no extra direct binding. |
| `B s`, `B j`, `B d` | Project-scoped bookmarks | Built-in `bookmark-set`, `bookmark-jump`, and bookmark deletion | Intentional deviation; bookmarks are not reimplemented as project-owned state. |
| `C` | Find a changed file | `project-vc-dir` or Magit status | Semantic replacement; no duplicate direct key. |
| `E` | Edit project directory locals | Visit `.dir-locals.el` with `project-find-file` | Intentional deviation; Emacs 30.2 has no public project command for this. |
| `F` | Find a file in another known project | `project-switch-project`, then `project-find-file` | Semantic replacement; kept as a two-step built-in flow. |
| `i`, `z` | Invalidate or populate a project cache | No command | Intentional deviation; `project.el` does not maintain Projectile's file cache. |
| `j`, `J`, `t`, `T` | File-kind or implementation/test switching | Language-server code actions, Xref, or project conventions | Intentional deviation; there is no safe language-neutral mapping. |
| `l` | Find a file under a chosen directory | `project-find-file` | Semantic replacement; no duplicate direct key. |
| `m` | Open package-specific dispatcher | Built-in Which Key labels on `C-c p` | Intentional deviation; the prefix itself is the dispatcher. |
| `P` | Package-specific dashboard | `project-switch-project` | Intentional deviation; no separate dashboard. |
| `<left>`, `<right>`, `ESC` | Cycle project buffers | `project-switch-to-buffer` | Semantic replacement; no global cycling state. |
| `R`, `u` | Review or undo project replacement | `project-query-replace-regexp` and normal undo | Intentional deviation; no package-specific replacement transaction UI. |
| `s s`, `s g`, `s r`, `s a`, `s x`, `s R`, `s X`, `s t` | Search, Grep, Ripgrep, references, reviews, and TODO search | `C-c p g`, `C-c p o`, `C-c p ?`, and `M-x consult-ripgrep` | Intentional deviation; `s` is now the required project-shell key. |
| `S` | Save all project buffers | Standard `save-some-buffers` | Semantic replacement; no duplicate direct key. |
| `w s`, `w S`, `w r`, `w R`, `w f`, `w b` | Package-specific project sessions | Optional XDG-backed desktop state and `project-switch-to-buffer` | Intentional deviation; per-project session machinery is not recreated. |
| `c o`, `c c`, `c p`, `c i`, `c t`, `c .`, `c r` | Configure, compile, package, install, test, or run a project | `C-c p c`, `project-compile`, and project-owned commands from Nix/devenv | Intentional deviation; `c` is now direct compile and generic task guessing is removed. |
| `c m f`, `c m o`, `c m c`, `c m t`, `c m i`, `c m p`, `c m r` | Package-specific subproject lifecycle | Enter the nested project and use the normal project commands | Intentional deviation; nested projects remain independent roots. |
| `c x`, `c X` | Run or repeat a package-discovered task | `project-compile` or a project-local command | Intentional deviation; no package-specific task registry. |
| `x r`, `x e`, `x i`, `x t`, `x s`, `x g`, `x v`, `x 4 v`, `x x`, `x 4 x`, `x G`, `x 4 G` | Open package-specific REPLs, shells, terminals, debuggers, or AI shells | `C-c p e`, `C-c p s`, `M-x dape`, and explicit terminal commands | Intentional deviation; only portable project-local shells receive prefix keys. |
| `H` | Package diagnostics | `M-x gsmlg-treesit-report`, Eglot events, and built-in project inspection | Intentional deviation; no Projectile doctor remains. |

## Version control

| Scope/map | Key | Old command | New command | Status | Reason |
|---|---|---|---|---|---|
| Global | `M-<f12>` | `magit-status` | `magit-status` | Exact | Unchanged. |
| Global | `C-x g` | `magit-status` | `magit-status` | Exact | Unchanged. |
| Global | `C-x M-g` | `magit-status` | `magit-status` | Exact | Unchanged. |
| Magit status | `C-M-<up>` | `magit-section-up` | `magit-section-up` | Exact | Unchanged. |
| VC prefix | `C-x v f` | `vc-git-grep` | `vc-git-grep` | Exact | Unchanged. |
| VC prefix | `C-x v p` | `git-messenger:popup-message` | `git-messenger:popup-message` | Exact | Retains the line-commit popup package. |
| Global | `M-g M-g` | `hydra-git-gutter/body` | `gsmlg-diff-hl-transient` | Semantic replacement | A Transient interface over current diff-hl APIs replaces the old menu. |
| diff-hl Transient | `j` | Next gutter hunk | `diff-hl-next-hunk` | Semantic replacement | Current diff-hl API. |
| diff-hl Transient | `k` | Previous gutter hunk | `diff-hl-previous-hunk` | Semantic replacement | Current diff-hl API. |
| diff-hl Transient | `h` | First gutter hunk | `gsmlg-diff-hl-first-hunk` | Semantic replacement | Small public wrapper. |
| diff-hl Transient | `l` | Last gutter hunk | `gsmlg-diff-hl-last-hunk` | Semantic replacement | Small public wrapper. |
| diff-hl Transient | `s` | Stage gutter hunk | `diff-hl-stage-current-hunk` | Semantic replacement | Current diff-hl API. |
| diff-hl Transient | `r` | Revert gutter hunk | `diff-hl-revert-hunk` | Semantic replacement | Current diff-hl API. |
| diff-hl Transient | `p` | Pop up gutter hunk | `diff-hl-show-hunk` | Semantic replacement | Current diff-hl API. |
| diff-hl Transient | `R` | Set start revision | `diff-hl-set-reference-rev-in-project` | Semantic replacement | Current diff-hl API. |
| diff-hl Transient | `q` | Exit menu | `gsmlg-diff-hl-transient-quit` | Exact | Exits the Transient. |
| diff-hl Transient | `Q` | Disable gutter mode | `gsmlg-diff-hl-disable` | Semantic replacement | Disables diff-hl only in the current buffer. |

## Org

| Scope/map | Key | Old command | New command | Status | Reason |
|---|---|---|---|---|---|
| Global | `C-c l` | `org-store-link` | `org-store-link` | Exact | Unchanged. |
| Global | `C-c a` | `org-agenda` | `org-agenda` | Exact | Unchanged. |
| Global | `C-c c` | `org-capture` | `org-capture` | Exact | Unchanged. |
| Org | `C-M-<up>` | `org-up-element` | `org-up-element` | Exact | Unchanged. |
| Org | `C-M-<down>` | `org-down-element` | `org-down-element` | Exact | Unchanged. |
| Org agenda | `P` | `org-pomodoro` | `org-pomodoro` | Exact | Unchanged. |
| Org clock header line | Mouse 2 | `org-clock-goto` | `org-clock-goto` | Exact | Unchanged. |
| Org clock header line | Mouse 1 | `org-clock-menu` | `org-clock-menu` | Exact | Unchanged. |
| Org on macOS | `C-c g` | `org-mac-grab-link` | `org-mac-grab-link` when available | Exact | Optional package absence does not break startup. |
| Org on macOS | `M-h` | Fall through | Fall through | Exact | Allows the global macOS hide binding. |

Org speed commands are part of the same compatibility surface:

| Speed key | Old command | New command | Status |
|---|---|---|---|
| `h` | `gsmlg/org-hide-other` | `gsmlg-org-hide-other` | Exact; old name remains an alias. |
| `k` | `org-kill-note-or-show-branches` | `org-kill-note-or-show-branches` | Exact |
| `q` | `org-agenda` | `org-agenda` | Exact |
| `s` | `org-save-all-org-buffers` | `org-save-all-org-buffers` | Exact |
| `w` | `org-refile` | `org-refile` | Exact |
| `z` | `org-add-note` | `org-add-note` | Exact |
| `J` | `org-clock-goto` | `org-clock-goto` | Exact |
| `P` | `org-pomodoro` | `org-pomodoro` | Exact |
| `W` | `bh/widen` | `gsmlg-org-widen` | Exact; old name remains an alias. |

## Emacs Lisp, Paredit, and Xref

| Scope/map | Key | Old command | New command | Status | Reason |
|---|---|---|---|---|---|
| Global remap | `eval-expression` | `pp-eval-expression` | `pp-eval-expression` | Exact | Unchanged. |
| Emacs Lisp | `C-x C-e` | `gsmlg/eval-last-sexp-or-region` | `gsmlg-eval-last-sexp-or-region` | Exact | The old public name remains an alias. |
| Emacs Lisp | `C-c C-z` | `gsmlg/switch-to-ielm` | `gsmlg-switch-to-ielm` | Exact | The old public name remains an alias. |
| IELM | `C-c C-z` | `gsmlg/repl-switch-back` | `gsmlg-repl-switch-back` | Exact | The old public name remains an alias. |
| Emacs Lisp | `C-c e` | `macrostep-expand` | `macrostep-expand` | Exact | Unchanged. |
| Help prefix | `<f1> K` | `find-function-on-key` | `find-function-on-key` | Exact | F1 continues to expose normal Help. |
| ERT results | `g` | `ert-results-rerun-all-tests` | `ert-results-rerun-all-tests` | Exact | Unchanged. |
| Symbol Overlay | `M-n` | `symbol-overlay-jump-next` | `symbol-overlay-jump-next` | Exact | Unchanged. |
| Symbol Overlay | `M-p` | `symbol-overlay-jump-prev` | `symbol-overlay-jump-prev` | Exact | Unchanged. |
| Paredit | `RET` | `paredit-newline` outside REPLs/minibuffers | `paredit-newline` outside REPLs/minibuffers | Exact | Structural newline behavior is retained. |
| Paredit | `C-<left>` | Fall through | Fall through | Exact | Paredit binding remains unset. |
| Paredit | `C-<right>` | Fall through | Fall through | Exact | Paredit binding remains unset. |
| Paredit | `C-M-<left>` | Fall through | Fall through | Exact | Paredit binding remains unset. |
| Paredit | `C-M-<right>` | Fall through | Fall through | Exact | Paredit binding remains unset. |
| Paredit | `M-s` | Fall through | Fall through | Exact | Preserves the global search prefix. |
| Paredit | `M-?` | Fall through | Fall through to `xref-find-references` | Exact | Preserves the standard Xref command. |
| Paredit Everywhere | `M-s` | Fall through | Fall through | Exact | Preserves global search. |
| Global | `M-.` | `xref-find-definitions` | `xref-find-definitions` | Exact | Standard Xref behavior remains visible. |
| Global | `M-?` | `xref-find-references` | `xref-find-references` | Exact | Standard Xref behavior remains visible. |

## JavaScript refactor prefix

`C-, r` remains the refactor namespace, but it is language-server based:

| Scope/map | Key | Old command | New command | Status | Reason |
|---|---|---|---|---|---|
| `C-, r` | `r` | Prefix fragment of `rv` | `eglot-rename` | Semantic replacement | Required direct rename command. |
| `C-, r` | `a` | Prefix fragment of `ag` and `ao` | `eglot-code-actions` | Semantic replacement | Exposes server-supported refactors. |
| `C-, r` | `f` | Unassigned | `gsmlg-format-buffer` | Semantic replacement | Uses the selected Apheleia/Eglot formatting path. |
| `C-, r` | `o` | Unassigned | `gsmlg-eglot-organize-imports` | Semantic replacement | Requests source organization when supported. |

The old generated structural operations were all inventoried.  They are not
pretended to be portable LSP operations:

| Legacy key after `C-, r` | Old command | Status and replacement |
|---|---|---|
| `ee` | `js2r-expand-node-at-point` | Intentional deviation; use tree-sitter movement/selection or `er/expand-region`. |
| `cc` | `js2r-contract-node-at-point` | Intentional deviation; no safe LSP equivalent. |
| `wi` | `js2r-wrap-buffer-in-iife` | Intentional deviation; no safe LSP equivalent. |
| `ig` | `js2r-inject-global-in-iife` | Intentional deviation; no safe LSP equivalent. |
| `ev` | `js2r-extract-var` | Intentional deviation; use `C-, r a` when the server offers extraction. |
| `el` | `js2r-extract-let` | Intentional deviation; use `C-, r a` when supported. |
| `ec` | `js2r-extract-const` | Intentional deviation; use `C-, r a` when supported. |
| `iv` | `js2r-inline-var` | Intentional deviation; use `C-, r a` when supported. |
| `rv` | `js2r-rename-var` | Semantic intent moves to the shorter required `C-, r r`. |
| `vt` | `js2r-var-to-this` | Intentional deviation; no safe LSP equivalent. |
| `ag` | `js2r-add-to-globals-annotation` | Intentional deviation; configure the project linter instead. |
| `sv` | `js2r-split-var-declaration` | Intentional deviation; no safe LSP equivalent. |
| `ss` | `js2r-split-string` | Intentional deviation; no safe LSP equivalent. |
| `st` | `js2r-string-to-template` | Intentional deviation; use `C-, r a` when supported. |
| `ef` | `js2r-extract-function` | Intentional deviation; use `C-, r a` when supported. |
| `em` | `js2r-extract-method` | Intentional deviation; use `C-, r a` when supported. |
| `ip` | `js2r-introduce-parameter` | Intentional deviation; use `C-, r a` when supported. |
| `lp` | `js2r-localize-parameter` | Intentional deviation; no safe LSP equivalent. |
| `tf` | `js2r-toggle-function-expression-and-declaration` | Intentional deviation; use `C-, r a` when supported. |
| `ta` | `js2r-toggle-arrow-function-and-expression` | Intentional deviation; use `C-, r a` when supported. |
| `ts` | `js2r-toggle-function-async` | Intentional deviation; use `C-, r a` when supported. |
| `ao` | `js2r-arguments-to-object` | Intentional deviation; no safe LSP equivalent. |
| `uw` | `js2r-unwrap` | Intentional deviation; no safe LSP equivalent. |
| `wl` | `js2r-wrap-in-for-loop` | Intentional deviation; no safe LSP equivalent. |
| `3i` | `js2r-ternary-to-if` | Intentional deviation; use `C-, r a` when supported. |
| `lt` | `js2r-log-this` | Intentional deviation; no safe language-server operation. |
| `dt` | `js2r-debug-this` | Intentional deviation; use Dape and explicit breakpoints. |
| `sl` | `js2r-forward-slurp` | Intentional deviation; no maintained parser-independent equivalent. |
| `ba` | `js2r-forward-barf` | Intentional deviation; no maintained parser-independent equivalent. |
| `k` | `js2r-kill` | Intentional deviation; use standard structural editing commands. |

The generated unprefixed `C-S-<up>` and `C-S-<down>` line movements are
preserved globally through move-dup, as recorded in the global editing table.

## macOS profiles

The default macOS profile calls `gsmlg-mac-osx-remap-command`.  The optional
PC-keyboard profile remains available through
`gsmlg-mac-osx-unremap-command`.  Both old slash-style command names remain
aliases.  The default profile maps the physical Command key to Emacs Meta and
disables Option as an Emacs modifier.  The PC-keyboard profile maps Command to
Super and Option to Meta.  Both GNU Emacs NS and Emacs Mac Port modifier
variables are supported.

| Profile and map | Key | Old command | New command | Status |
|---|---|---|---|---|
| Default macOS global | `M-\`` | `ns-next-frame` | `ns-next-frame` | Exact |
| Default macOS global | `M-h` | `ns-do-hide-emacs` | `ns-do-hide-emacs` | Exact |
| Default macOS global | `M-˙`, `M-ˍ` | `ns-do-hide-others` | `ns-do-hide-others` | Exact |
| PC-keyboard macOS global | `s-\`` | `ns-next-frame` | `ns-next-frame` | Exact |
| PC-keyboard macOS global | `s-h` | `ns-do-hide-emacs` | `ns-do-hide-emacs` | Exact |
| PC-keyboard macOS global | `s-˙`, `s-ˍ` | `ns-do-hide-others` | `ns-do-hide-others` | Exact |
| Either macOS profile | wheel left/right, double-wheel left/right, triple-wheel left/right | `ignore` | `ignore` | Exact |
| nXML, default profile | `M-h` | Fall through | Fall through | Exact |
| nXML, PC-keyboard profile | `s-h` | Fall through | Fall through | Exact |
| Magit on macOS | `M-h` | Fall through | Fall through | Exact |
| Org on macOS | `M-h` | Fall through | Fall through | Exact |

No macOS-only key symbol is installed on Linux.

## Deliberately disabled mail and music modules

The legacy mail module was commented out in `init.el`, so its declared
`C-x m` and `C-c m` bindings were never part of the active keymap.  Mail and
music remain disabled:

| Legacy declaration | Current behavior | Status and reason |
|---|---|---|
| `C-x m` to `mu4e-compose-new` | Standard Emacs mail binding remains available | Intentional deviation; private mail is not activated. |
| `C-c m` to `mu4e` | `C-c m` is the preserved multiple-cursors prefix | Intentional deviation; the inactive mail declaration cannot displace an active editing prefix. |

## Compatibility aliases

Saved keyboard macros and external local configuration can continue to refer
to these old public names:

- `gsmlg/newline-at-end-of-line`
- `gsmlg/open-line-with-reindent`
- `gsmlg/eval-last-sexp-or-region`
- `gsmlg/switch-to-ielm`
- `gsmlg/repl-switch-back`
- `gsmlg/maybe-suspend-frame`
- `gsmlg/set-indent`
- `gsmlg/mac-osx-remap-command`
- `gsmlg/mac-osx-unremap-command`
- `gsmlg/byte-compile-file-batch`
- `gsmlg/cl-libify-next`
- `gsmlg/preferred-indent-level`
- `gsmlg/repl-original-buffer`
- `gsmlg/repl-switch-function`
- `ffap-vlf`
- `kill-back-to-indentation`
- `backward-up-sexp`
- `gsmlg/git-gutter-first-hunk`
- `gsmlg/git-gutter-last-hunk`
- `gsmlg/git-gutter-off`
- `gsmlg/org-hide-other`
- `bh/widen`
- `make-orgcapture-frame`
- `gsmlg/show-org-clock-in-header-line`
- `gsmlg/hide-org-clock-from-header-line`
- `gsmlg/agent-editor-mcp-autostart`
