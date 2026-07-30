# Emacs architecture

## Baseline

This is a vanilla GNU Emacs configuration for Emacs 30.2 and newer. It
supports Linux and macOS, graphical and terminal frames, named daemons and
`emacsclient`, batch CI, local Git projects and worktrees, and TRAMP over SSH.
It does not depend on Doom, Spacemacs, Evil, General, or another configuration
framework.

The configuration is installed at the normal XDG Emacs location:

```text
${XDG_CONFIG_HOME:-$HOME/.config}/emacs -> <dotfiles>/emacs.d
```

This lets Emacs discover `early-init.el` and `init.el` naturally. Neither file
changes `user-emacs-directory`.

## Directory layout

```text
emacs.d/
├── early-init.el
├── init.el
├── elpaca-lock.el
├── lisp/
│   ├── gsmlg-bootstrap.el
│   ├── gsmlg-paths.el
│   ├── gsmlg-core.el
│   ├── gsmlg-ui.el
│   ├── gsmlg-completion.el
│   ├── gsmlg-editing.el
│   ├── gsmlg-keybindings.el
│   ├── gsmlg-project.el
│   ├── gsmlg-vcs.el
│   ├── gsmlg-eglot.el
│   ├── gsmlg-tramp.el
│   ├── gsmlg-session.el
│   ├── gsmlg-org.el
│   ├── gsmlg-elfeed.el
│   ├── gsmlg-agent.el
│   └── lang/
│       ├── gsmlg-lang-elisp.el
│       ├── gsmlg-lang-beam.el
│       ├── gsmlg-lang-web.el
│       ├── gsmlg-lang-systems.el
│       ├── gsmlg-lang-scripting.el
│       └── gsmlg-lang-infra.el
├── site-lisp/agent-editor-mcp/
├── tests/
├── docs/
├── README.md
└── local.el.example
```

The three load-path additions are explicit: `lisp/`, `lisp/lang/`, and the
exact Agent Editor MCP package directory. `site-lisp/` is never scanned
recursively.

## Startup phases

### Early initialization

`early-init.el` performs only work that must happen before normal
initialization:

- disables package.el activation;
- temporarily relaxes garbage collection;
- inhibits implied frame resizing;
- suppresses startup UI;
- disables menu, tool, and scroll bars without assuming a GUI frame;
- redirects native compilation output below the XDG cache directory.

It does not install packages or configure applications or languages.

### Orchestration

`init.el` first rejects Emacs versions older than 30.2, adds the three explicit
load paths, and requires modules in dependency order:

```text
paths -> bootstrap
      -> core -> session -> UI -> completion -> editing
      -> TRAMP -> project -> VCS -> Eglot
      -> language dispatch modules
      -> Org -> Elfeed -> Agent Editor MCP integration
      -> package phase boundary -> keybindings
      -> XDG custom file -> external local override
```

The orchestrator does not contain application settings. Garbage collection is
restored from `emacs-startup-hook`. The XDG-backed Customize file is loaded
when readable, followed by the optional local file as the final override.

### Package bootstrap

`gsmlg-bootstrap.el` pins Elpaca itself to revision
`6530ffa73b18ccee858e7c471415ab7e0c0d8ce1`. Elpaca repositories and builds
live below the XDG data directory. Emacs 30's built-in `use-package` is
integrated through `elpaca-use-package`.

`elpaca-lock.el` is committed configuration. It supplies exact recipes and
source revisions for the package graph. Most packages use Git sources.
`corfu-terminal`, `diff-hl`, `git-timemachine`, `popon`, `zig-mode`, and the
OTP-backed Erlang fallback use Elpaca's native hosted archive source at an exact
commit, avoiding full-history clones for those repositories. Bootstrap rejects
a missing, malformed, empty, or non-exact lock before package resolution and
reports failed package
IDs after each real phase boundary. Normal warm startup reads installed builds
and does not refresh archives. First-start network operations clone pinned
Elpaca and fetch the locked package sources. Set
`GSMLG_EMACS_OFFLINE=1` to prohibit a missing first-time bootstrap from
attempting the network; the resulting error explains that one connected
bootstrap is required.

Elpaca's archive worker normally uses Emacs's optional
`zlib-decompress-region`. The bootstrap child-process environment supplies an
external `gzip` fallback only when that function is unavailable, so minimal
Emacs builds remain reproducible without changing package transport or cloning
the full OTP history.

The configuration waits only at two real phase boundaries: after establishing
Elpaca's `use-package` integration, and after queuing declarations before maps
and commands are asserted by the keybinding module.

## Responsibility map

| Module | Responsibility |
| --- | --- |
| `gsmlg-paths` | XDG directories, path helpers, custom file, optional local file |
| `gsmlg-bootstrap` | pinned Elpaca bootstrap, use-package integration, lock writing |
| `gsmlg-core` | built-in editing defaults, UTF-8, EditorConfig, which-key, startup GC |
| `gsmlg-session` | recent files, history, places, bookmarks, desktop policy, explicit server control |
| `gsmlg-ui` | Duskmoon Moonlight, optional Nerd Font glyphs, native mode line and file breadcrumb header |
| `gsmlg-completion` | Vertico, Orderless, Marginalia, Consult, Embark, Corfu, Cape |
| `gsmlg-editing` | editing commands, Paredit behavior, vundo and editing packages, macOS remaps |
| `gsmlg-keybindings` | prefix maps, compatibility wrappers, machine-readable key contract |
| `gsmlg-project` | project.el, worktree roots, Consult search, envrc, project-local executables |
| `gsmlg-vcs` | Magit, diff-hl Transient, Git links/history/modes and line commit popup |
| `gsmlg-eglot` | Eglot server selection, Flymake, formatting, Dape, tree-sitter helpers |
| `gsmlg-tramp` | compute-near-data process helpers and remote state policy |
| `gsmlg-org` | agenda, capture, TODO, clock, Babel, Pomodoro and Org presentation |
| `gsmlg-elfeed` | tracked feed source and XDG-backed Elfeed database |
| `gsmlg-agent` | explicit Agent Editor MCP workspace, port, autostart and XDG state |
| `gsmlg-lang-*` | non-overlapping file dispatch and tree-sitter/classic fallbacks |

## XDG storage model

The repository is immutable at runtime. Defaults are:

| Class | Root | Contents |
| --- | --- | --- |
| Configuration | `${XDG_CONFIG_HOME:-~/.config}/emacs/` | tracked source, lock file, feed list |
| Package/application data | `${XDG_DATA_HOME:-~/.local/share}/emacs/` | Elpaca repositories and builds, Elfeed database |
| Disposable cache | `${XDG_CACHE_HOME:-~/.cache}/emacs/` | Elpaca cache, native compilation, URL cache, Org persistence, local and remote auto-save files |
| Mutable state | `${XDG_STATE_HOME:-~/.local/state}/emacs/` | customizations, backups, auto-save index, recentf, savehist, save-place, bookmarks, project list, TRAMP, desktop, Transient, multiple-cursors, Eshell, network security, Org clock/ID, server sockets/authentication, Agent Editor metadata |

First-party compilation tests use temporary output and clean it. `.elc`, `.eln`,
package repositories, history, customizations, caches, and application
databases must never appear in the tracked tree.

## Completion and language intelligence

The minibuffer pipeline is the standard Emacs completion API with Vertico as
the UI, Orderless matching, Marginalia annotations, Consult commands, Embark
actions, and savehist persistence. `C-.` remains `set-mark-command`; Embark
uses `C-;` and `C-c C-;`.

Corfu presents `completion-at-point` results. Cape adds only low-priority
fallback CAPFs, so a major mode or Eglot CAPF stays authoritative. Terminal
frames use corfu-terminal.  `JetBrainsMono Nerd Font Mono` adds mode-line and
breadcrumb glyphs when installed, with a text-only fallback for terminal
frames and hosts where the font is unavailable.

Automatic Eglot discovery runs from the late file-visiting hook, after envrc,
local variables, and buffer-local project executable paths are active.
Negative server lookups are cached for automatic hooks, invalidated when
envrc activates, and bypassed by a manual
`M-x gsmlg-eglot-ensure-maybe`.

Eglot is the only LSP client. It integrates with Flymake, ElDoc, Xref,
project.el, and Corfu through built-in APIs. `gsmlg-eglot-ensure-maybe`
requires a supported mode, a current project, and an available server. Missing
servers are cached per mode/project to avoid repeated prompts. Emacs never
installs a server.

Apheleia orchestrates external formatters. `gsmlg-format-buffer` uses an
available Apheleia formatter, then active Eglot as a fallback. The
configuration does not enable competing Apheleia and Eglot save hooks.

Tree-sitter grammars are selected only when `treesit-ready-p` succeeds. A
maintained classic mode remains the fallback. `M-x gsmlg-treesit-report`
reports readiness; `M-x gsmlg-treesit-install-language-grammar` is an explicit
user action and never runs during startup.

## Projects, Git worktrees, and environments

Built-in project.el is the only project abstraction. `project-current` and
`project-root` determine roots; no global override forces nested repositories
into a parent. Git's own project backend recognizes a normal checkout and each
linked worktree as separate roots.

`gsmlg-project-search` invokes Consult ripgrep with the current project root.
The `C-x p` prefix exposes project.el commands. envrc activates project
environments, while existing `node_modules/.bin` directories are prepended to
`exec-path` buffer-locally. On graphical macOS, exec-path-from-shell imports
the login-shell environment once. No project executable directory is added
globally.

## TRAMP compute-near-data behavior

The full remote file name remains the `default-directory` and project root.
Consequently:

- Eglot executable discovery asks the remote file handler and starts the
  server remotely;
- project-local TypeScript server lookup checks the remote
  `node_modules/.bin`;
- Consult ripgrep receives the remote project root and runs remotely;
- Apheleia, compilation, tests, and shell commands inherit the remote
  directory;
- helpers use `process-file` and `start-file-process`, which honor TRAMP file
  handlers.

The configuration never removes a TRAMP prefix from a data path, downloads a
remote project for indexing, pre-scans remote repositories, or substitutes a
local executable. Remote auto-saves and backups are redirected to local XDG
storage. Remote recent files are excluded, desktop restore skips remote paths,
and auto-revert does not request remote file notifications. This configuration
does not override the user's SSH settings. Emacs 30's built-in Eglot does
disable ControlMaster dynamically for the individual remote language-server
process as its own TRAMP safety workaround.

## Sessions, servers, and daemons

savehist, save-place, recentf, bookmarks, and the project list persist under
the XDG state root. Desktop persistence is on by default, restores graphical
frame size, position, and window layout, saves without an exit prompt, and is
controlled by `gsmlg-desktop-save-enabled`; its default filter excludes remote
files.

Normal interactive startup calls `server-start` by default. Set
`gsmlg-server-autostart` to `nil` in the local override to disable this for a
non-daemon process. A daemon already provides its own server, and batch mode
always refuses to start one. Server authentication files live under XDG state.

## Agent Editor MCP model

Agent Editor MCP binds to `127.0.0.1` only. Port 9876 is the compatibility
default and `EMACS_AGENT_PORT` may override it. Loading the configuration does
not start a listener. Start it with `M-x gsmlg-agent-start`, or opt into
autostart with `gsmlg-agent-autostart`/`EMACS_AGENT_AUTOSTART` after selecting
an explicit workspace through `gsmlg-agent-workspace` or
`EMACS_AGENT_WORKSPACE`.

The integration does not capture startup `default-directory`. The bundled
package binds one workspace per Emacs process and does not route requests
across multiple workspaces. Run one named daemon per explicit workspace when
isolation is needed. Connection metadata lives below
`${XDG_STATE_HOME:-~/.local/state}/emacs/agent-editor/`. Stopping MCP with
`M-x gsmlg-agent-stop` stops only the listener, never the Emacs daemon. A
listener failure is reported without aborting Emacs startup.

## Local override

The optional local file resolves in this order:

1. `GSMLG_EMACS_LOCAL`, when non-empty;
2. `${XDG_CONFIG_HOME:-~/.config}/gsmlg/emacs-local.el`;
3. no local file.

It is loaded last only when it exists. Use
[`local.el.example`](../local.el.example) as a starting point, but copy it
outside this repository. Put host paths, private identities, and executable
overrides there.

## Package maintenance and recovery

Package updates are deliberate:

1. Run `M-x gsmlg-elpaca-update-package` once for each selected package. It
   advances the package's shared source repository to its configured branch
   or upstream default, rebuilds every package using that source, and leaves
   the lock file unchanged.
   For immutable archive recipes (`corfu-terminal`, `diff-hl`, `erlang`,
   `git-timemachine`, `popon`, and `zig-mode`), select an exact upstream
   commit, remove the package with `M-x elpaca-delete`, update the corresponding
   lock `:ref`, let a connected startup re-realize it, and use a forced fresh
   bootstrap; the Git update command rejects archive sources.
2. Run the complete suite against that updated data directory with
   `GSMLG_EMACS_TEST_ALLOW_UNLOCKED=1`, as documented in
   [`README.md`](../README.md).
3. Run `M-x gsmlg-elpaca-write-lock-file` in the updated Emacs installation.
4. Review every recipe and revision change in `elpaca-lock.el`.
5. Run the complete suite normally, then commit the configuration and lock file
   together.

The maintenance command intentionally refuses the Elpaca bootstrap repository.
To update Elpaca, select an exact revision, update
`gsmlg-elpaca-revision` and the `elpaca` plus `elpaca-use-package` lock
entries together, and verify them with a forced fresh bootstrap.

If the package bootstrap is damaged:

1. Quit every Emacs process using the data directory.
2. Move `${XDG_DATA_HOME:-~/.local/share}/emacs/elpaca/` to a backup location.
3. Keep `elpaca-lock.el`; it is the reproducibility input.
4. Start Emacs once with network access to realize pinned Elpaca and the locked
   package graph.
5. Run the startup, ERT, lint, and Agent Editor MCP tests.
6. Start again with `GSMLG_EMACS_OFFLINE=1` to verify the warm installation.

Moving the Elpaca data aside is recoverable. Do not delete configuration,
state, or the lock file as part of package recovery.
