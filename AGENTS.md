# Project Context and Agent Directives

## Repository scope

This repository manages the user's Emacs, Zsh, Vim, Git, mbsync, and msmtp
configuration. Work on the subsystem named by the request and do not reformat
or refactor adjacent dotfiles.

`install.sh` installs the Emacs configuration as:

```text
${XDG_CONFIG_HOME:-$HOME/.config}/emacs -> <repository>/emacs.d
```

It also preserves the existing non-Emacs installation behavior. Never edit a
deployed symlink under `$HOME`; edit the tracked source in this repository.

## Emacs baseline and architecture

- GNU Emacs 30.2 is the minimum supported release.
- `emacs.d/early-init.el` contains only settings that must run before normal
  initialization.
- `emacs.d/init.el` is an orchestrator. Keep its explicit dependency order and
  do not turn it into a configuration dumping ground.
- First-party modules live in `emacs.d/lisp/` and are named `gsmlg-*.el`.
- Language dispatch modules live in `emacs.d/lisp/lang/`.
- The only vendored package on `load-path` is
  `emacs.d/site-lisp/agent-editor-mcp/`; add that exact directory, never scan
  `site-lisp` recursively.
- Every first-party Emacs Lisp file must use lexical binding, a standard
  feature name and `provide` form, and the `gsmlg-` prefix for public
  variables and functions.
- Prefer named hook functions, `#'function` notation, `setopt` for user
  options, `with-eval-after-load`, and the Emacs 30 keymap APIs
  (`keymap-set`, `keymap-global-set`, `keymap-unset`, and `defvar-keymap`).
- Put adjustable behavior behind `defgroup`/`defcustom`. Avoid anonymous hook
  lambdas and unnecessary advice; document the reason for any advice that
  remains.
- Preserve the responsibility boundaries documented in
  `emacs.d/docs/architecture.md`.

Do not reintroduce the deleted `init-*.el` architecture or a parallel legacy
stack. Active configuration must not use package.el installation, Helm,
Company, Flycheck, lsp-mode, lsp-ui, helm-lsp, Projectile, git-gutter, Hydra,
Spaceline, all-the-icons, undo-tree, Alchemist, Tern, js2-mode, rjsx-mode, or
js2-refactor.

## Package and external-tool ownership

Elpaca owns third-party Emacs Lisp packages. Its bootstrap revision is pinned
in `gsmlg-bootstrap.el`, and exact package recipes and revisions are committed
in `emacs.d/elpaca-lock.el`. Declare built-in libraries with `:ensure nil`.

Never add any of the following to normal startup:

- package archive refresh or package.el installation;
- runtime `package-vc-install`;
- automatic tree-sitter grammar downloads;
- automatic language-server or formatter installation;
- unconditional network access.

Nix, devenv, direnv, or each project owns language servers, formatters,
compilers, test runners, tree-sitter grammars, and other executables. Preserve
buffer-local project executable discovery and compute-near-data behavior for
TRAMP buffers.

Update packages only as an explicit maintenance operation:

1. Run `M-x gsmlg-elpaca-update-package` for each selected package.
   For immutable archive recipes (`corfu-terminal`, `diff-hl`, `erlang`,
   `git-timemachine`, `popon`, and `zig-mode`), update an exact lock `:ref`
   intentionally, delete/re-realize that package, and require a forced fresh
   bootstrap instead.
2. Run the complete test suite against that data directory with
   `GSMLG_EMACS_TEST_DATA_HOME=/path/to/xdg-data`,
   `GSMLG_EMACS_STARTUP_MODE=reuse` and
   `GSMLG_EMACS_TEST_ALLOW_UNLOCKED=1`.
3. Run `M-x gsmlg-elpaca-write-lock-file`.
4. Review the lock-file diff.
5. Run the complete suite normally, then commit configuration and lock-file
   changes together.

## XDG and mutable-state rules

The tracked `emacs.d/` tree is configuration, tests, documentation, snippets,
the feed list, the Agent Editor MCP source, and the Elpaca lock file. Do not
write package repositories, builds, native compilation output, caches,
history, desktop data, bookmarks, project lists, customizations, TRAMP state,
server files, Agent Editor metadata, `.elc`, or `.eln` files there.

Use the path helpers from `gsmlg-paths.el`:

- `gsmlg-data-directory` for Elpaca and persistent application data;
- `gsmlg-cache-directory` for disposable caches and auto-save data;
- `gsmlg-state-directory` for mutable state and backups;
- `gsmlg-config-directory` for tracked, read-only configuration data.

Machine-specific paths and private values belong in the external local
override selected by `GSMLG_EMACS_LOCAL` or, by default,
`${XDG_CONFIG_HOME:-$HOME/.config}/gsmlg/emacs-local.el`. Do not put secrets in
`local.el.example`.

## Keybinding compatibility

Existing user muscle memory is a compatibility contract. When changing a
binding:

1. Update `gsmlg-keybinding-contract` in
   `emacs.d/lisp/gsmlg-keybindings.el`.
2. Update `emacs.d/tests/keybindings-test.el`.
3. Update `emacs.d/docs/keybindings.md`, including the old command, new
   command, status, and reason for deviations.

Do not bind Embark to `C-.`; that remains `set-mark-command`. `C-h` remains
`delete-backward-char`, while F1 continues to expose normal Emacs help.

## Project and remote-development rules

Built-in project.el is the only project abstraction. Use `project-current` and
`project-root`; do not implement project roots with ad hoc Git subprocesses.
Git worktrees must remain independent project roots, and nested projects must
not be globally collapsed.

For a remote `default-directory`, keep the full TRAMP name for project and
search roots. Discover and launch language servers, ripgrep, formatters,
compilers, and tests on the remote host. Use `process-file` or
`start-file-process` compatible APIs. Never copy a remote project locally,
strip a TRAMP prefix from a data path, or silently select a local executable.
Do not override the user's SSH control settings.

## Org, server, and Agent Editor MCP

- Preserve the Org workflow, capture templates, agenda commands, TODO states,
  clocking, speed commands, Babel policy, and macOS behavior when editing
  `gsmlg-org.el`.
- Optional Babel libraries and host-specific paths must not make startup fail.
- Batch mode must start neither an Emacs server nor Agent Editor MCP.
- Normal GUI startup must not create a server unless explicitly enabled.
- Agent Editor MCP autostart is opt-in and requires an explicit workspace.
- The bundled MCP implementation serves one workspace per Emacs process. Do
  not claim multi-workspace routing; use one named daemon per workspace.
- Run the package's own test suite after any integration or package change.

## Installer safety

The Emacs section of `install.sh` may remove only a legacy symlink that
resolves exactly to this repository's Emacs configuration or `init.el`.
Conflicting files, directories, and broken symlinks must be moved to a printed,
timestamped backup. Never recursively delete arbitrary user configuration.
The installer must remain idempotent and verify both `early-init.el` and
`init.el` through the installed link.

## Validation

After changing Emacs Lisp, run the tests scoped to the touched area, then the
complete suite before declaring the work complete:

```sh
./run-emacs-tests.sh
```

The component entrypoints are:

```sh
./emacs.d/tests/install-test.sh
./test-emacs-startup.sh
./emacs.d/tests/module-load-test.sh
./lint-emacs-config.sh
./emacs.d/site-lisp/agent-editor-mcp/run_tests.sh
```

The complete runner owns the ERT suite. First-party byte-compiler warnings are
failures. Checkdoc must pass, generated `.elc` and `.eln` files must be
removed, and `git status --short` after testing must show no generated runtime
files.

For language, project, TRAMP, Org, installer, or keybinding changes, run the
corresponding focused test file as well. Do not fix out-of-scope failures;
record them and stop.

## General engineering conventions

- Begin by checking `git status --short`, `git diff`, and
  `git diff --cached`.
- Preserve unrelated user changes.
- Make the smallest change that satisfies the request.
- Use two-space indentation, UTF-8, LF line endings, and final newlines.
- Shell scripts use `#!/usr/bin/env bash` or `#!/bin/sh -e`, fail fast, and use
  `snake_case` names.
- Do not commit secrets or hardcoded private paths.
- Do not create a commit unless the user explicitly requests one.
