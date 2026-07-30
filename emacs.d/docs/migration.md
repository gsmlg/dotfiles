# Migration from the legacy configuration

The Emacs 30.2 configuration is a one-pass replacement. The former
`init-*.el` modules and their package bootstrap are not loaded alongside the
new `gsmlg-*` modules. Mutable files that used to accumulate below
`emacs.d/` now live below the XDG data, cache, and state directories.

## Installation and startup changes

The old `~/.emacs` link is replaced by the standard XDG configuration link:

```text
${XDG_CONFIG_HOME:-$HOME/.config}/emacs -> <dotfiles>/emacs.d
```

This is what allows Emacs to discover `early-init.el` before `init.el`.
`install.sh` removes an old repository symlink, but moves every conflicting
real file, directory, or broken link to a timestamped backup and prints its
path. Repeating the installer leaves the correct link unchanged.

`init.el` is now an explicit orchestrator. The old recursive site-lisp scan is
gone; only `lisp/`, `lisp/lang/`, and the exact Agent Editor MCP directory are
added to `load-path`.

Normal interactive startup starts an Emacs server by default. Set
`gsmlg-server-autostart` to `nil` in the local override to disable it. Daemons
already provide their own server, and batch mode cannot start one.

## Package and state migration

Elpaca replaces package.el as the only third-party Emacs Lisp package manager.
The bootstrap revision is exact, and `elpaca-lock.el` commits exact package
recipes and revisions. Package sources and builds moved from tracked or
checkout-local `elpa-*` directories to:

```text
${XDG_DATA_HOME:-~/.local/share}/emacs/elpaca/
```

A connected first startup realizes the lock. Warm startup does not refresh
archives and works with `GSMLG_EMACS_OFFLINE=1`. Tree-sitter grammars,
language servers, formatters, compilers, and test tools are external
dependencies owned by Nix, devenv, direnv, or each project.

The following mutable files also moved out of the checkout:

- native compilation, URL, and auto-save data to the XDG cache root;
- customizations, backups, recent files, minibuffer history, saved places,
  bookmarks, project list, TRAMP, desktop, Transient, and Org clock state to
  the XDG state root;
- the Elfeed database to the XDG data root;
- Agent Editor MCP metadata to the XDG state root.

Do not copy the former checkout-local history, desktop, project cache, or
package directories into the new configuration. If a specific history file
must be retained, merge its data deliberately into the corresponding XDG file
after making a backup.

## Major stack replacements

| Legacy stack | Replacement |
|---|---|
| package.el bootstrap and runtime package-vc installation | Elpaca recipes and the committed lock file |
| Helm | Vertico, Orderless, Marginalia, Consult, Embark |
| Company | Corfu, Cape, completion-at-point |
| Flycheck | Flymake |
| lsp-mode, lsp-ui, helm-lsp | Eglot, ElDoc, Xref, Flymake, Consult |
| Projectile | project.el and Consult |
| git-gutter and Hydra | diff-hl and Transient |
| undo-tree | built-in undo/undo-redo and vundo |
| Spaceline and all-the-icons | the native lightweight mode line |
| js2-mode, rjsx-mode, Tern, js2-refactor | maintained tree-sitter/classic modes and Eglot refactors |
| org-bullets | org-modern |

The complete key-by-key mapping, including semantic replacements and
intentional deviations, is in `keybindings.md`.

## Language and project migration

Built-in project.el replaces Projectile. Git repositories and linked worktrees
are discovered through `project-current`/`project-root`; worktrees remain
independent roots. envrc supplies each project's environment, and
`node_modules/.bin` is buffer-local instead of globally mutating `exec-path`.

Eglot is the only LSP client. It starts only when the buffer uses a supported
mode, belongs to a project, and a server is available in the local or remote
environment. No server is downloaded by Emacs. Apheleia owns external
formatter orchestration, with active Eglot formatting retained as an
interactive fallback.

Language dispatch prefers Emacs 30 tree-sitter modes only when the matching
grammar is ready. Classic maintained modes provide startup-safe fallbacks.
`M-x gsmlg-treesit-report` replaces assumptions about locally installed
grammars.

For TRAMP buffers, project roots, executable lookup, language servers,
ripgrep, formatters, compilation, and tests stay on the remote host. The
configuration does not strip remote prefixes or create a local indexing copy.

## Org workflow

The Org workflow remains active and retains:

- `C-c l`, `C-c a`, and `C-c c`;
- the TODO, NEXT, PROJECT, WAITING, DELEGATED, HOLD, DONE, CANCELLED, and
  MEETING states and their logging behavior;
- capture targets for `todo.org`, `note.org`, and `bookmark.org`;
- the Notes and GTD custom agenda commands;
- stuck-project detection, refiling to level five, archive placement, clock
  persistence, LOGBOOK drawers, and the clock header line;
- Org protocol, Org Pomodoro, headline speed commands, and the Alfred capture
  frame entrypoint;
- the existing enabled Babel language choices, subject to availability;
- the macOS `M-h` fall-through and optional `C-c g` link capture.

The Orphaned Tasks agenda predicate now uses the valid `nottodo` condition;
the legacy `nottododo` typo is gone. The obsolete `org-show-entry` name was
replaced with `org-fold-show-entry`, and the removed
`org-export-kill-product-buffer-when-displayed` setting was dropped.
`org-log-done` now uses its current `time` value, preserving the old timestamp
behavior without an Emacs 30 customization warning.

Capture targets are expanded from `gsmlg-org-directory`, so changing that
option also changes the generated capture paths. The following machine-local
values are user options and may be set in the external local file:

```elisp
(setopt gsmlg-org-directory "~/Documents/org/"
        gsmlg-org-agenda-files "~/Documents/org/.agenda_files"
        gsmlg-org-mobile-directory "/Volumes/org.gsmlg.org/"
        gsmlg-org-plantuml-jar-path nil)
```

An absent mobile volume is harmless. An unreadable PlantUML jar is ignored so
that Org can use a `plantuml` executable supplied by the current environment.
Org Babel loads an enabled language only when its `ob-LANGUAGE` library is
available. External interpreters remain the responsibility of Nix, devenv,
direnv, or the project, and normal Org source-block confirmation remains
enabled.

Compatibility aliases preserve names that may be called by saved keyboard
macros or external automation:

- `gsmlg/org-hide-other` → `gsmlg-org-hide-other`
- `bh/widen` → `gsmlg-org-widen`
- `make-orgcapture-frame` → `gsmlg-org-capture-frame`
- the two former `gsmlg/...org-clock...header-line` hook names → their
  `gsmlg-org-*` replacements

## Elfeed

Elfeed remains enabled with the tracked `elfeed.org` subscription source.
The source is read from `gsmlg-config-directory`; its database is stored in
`gsmlg-data-directory/elfeed/`. The configuration checkout therefore no
longer receives Elfeed database files.

## Agent Editor MCP

The Agent Editor package itself remains vendored, but the old unconditional
integration is gone. Batch startup never opens its listener. Interactive
autostart is also off unless explicitly enabled, and every start requires a
workspace from `gsmlg-agent-workspace`, `EMACS_AGENT_WORKSPACE`, or the
interactive command argument. Startup `default-directory` is not captured as
a hidden workspace.

Port 9876 remains the compatibility default, with `EMACS_AGENT_PORT` as an
override, and the listener remains bound to `127.0.0.1`. The package supports
one workspace per Emacs process; use one named daemon per explicit workspace
instead of assuming multi-workspace request routing. `M-x gsmlg-agent-stop`
stops the MCP listener without stopping the Emacs daemon.

## Disabled mail and music

Mail and music were already disabled in the former orchestrator and remain
disabled. The rewrite does not install or activate Mu4e, Simple MPC, or
Mingus.

The old mail module contained machine-private account identities, Maildir
names, and mbsync/msmtp choices. Those values were not migrated into active
Emacs Lisp. Anyone restoring mail support should create an optional module,
keep account details in the external local file, and use the system-provided
Mu4e matching the installed `mu` version. The repository-level
`.mbsyncrc` and `.msmtprc` installer behavior is independent of Emacs and was
not enabled or disabled by this migration.

The inactive mail bindings are intentional deviations:

- `C-c m` is reserved for the preserved multiple-cursor prefix.
- `C-x m` retains normal Emacs mail composition behavior unless a future
  optional mail module changes it locally.

The old music modules had no custom workflow or keybindings, so removing their
inactive package declarations loses no active behavior.
