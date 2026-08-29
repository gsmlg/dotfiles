# GSMLG Emacs configuration

This directory is a reproducible vanilla GNU Emacs configuration for Emacs
30.2 and newer. It uses responsibility-based `gsmlg-*` modules, pinned Elpaca
packages, built-in project.el/Eglot/Flymake/Xref, and an explicit XDG storage
model. The former package.el and `init-*.el` stacks are not loaded.

Read the [architecture](docs/architecture.md), the complete
[keybinding migration](docs/keybindings.md), and the
[legacy migration notes](docs/migration.md) for details.

## Install

From the repository root:

```sh
./install.sh
```

This creates an absolute symlink:

```text
${XDG_CONFIG_HOME:-$HOME/.config}/emacs -> <repository>/emacs.d
```

The installer recognizes an already-correct link. Legacy `~/.emacs`,
`~/.emacs.el`, `~/.emacs.d`, and conflicting XDG configurations are removed
only when they are exact old repository links; otherwise they are moved to
printed timestamped backups.

Emacs naturally discovers both `early-init.el` and `init.el` through the XDG
link. The configuration does not rewrite `user-emacs-directory`.

## First and subsequent startup

The first startup requires Git and network access. It clones Elpaca at the
exact revision declared in `gsmlg-bootstrap.el`, then realizes the exact
recipes and revisions in `elpaca-lock.el`. Most sources are Git repositories;
`corfu-terminal`, `diff-hl`, `git-timemachine`, `popon`, `zig-mode`, and the
OTP-backed Erlang fallback use Elpaca's native exact-commit archive transport
so bootstrap does not clone their full histories. Package sources and builds are
stored under `${XDG_DATA_HOME:-~/.local/share}/emacs/elpaca/`; Elpaca's
disposable cache is under `${XDG_CACHE_HOME:-~/.cache}/emacs/elpaca/`.
Archive extraction uses Emacs's built-in zlib support when available. If the
running Emacs omits that optional feature, `gzip` must be available on `PATH`;
Elpaca child processes use it only as the decompression fallback.

After the first bootstrap, normal startup reads installed builds and does not
refresh archives or install packages. Verify this explicitly with:

```sh
GSMLG_EMACS_OFFLINE=1 emacs
```

If Elpaca has not yet been bootstrapped, offline startup fails with a clear
message instead of falling back to package.el.

## Package stack

- Minibuffer: Vertico, Orderless, Marginalia, Consult, Embark,
  embark-consult, savehist
- In-buffer completion: Corfu, corfu-terminal, Cape, standard CAPFs
- Language intelligence: built-in Eglot, Flymake, ElDoc, Xref, project.el
- Formatting and debugging: Apheleia and lazy-loaded Dape
- Version control: Magit, diff-hl, Transient, git-link, git-timemachine,
  git-modes, git-messenger
- Editing: built-in undo/undo-redo and electric modes, vundo, expand-region,
  multiple-cursors, move-dup, Paredit, paredit-everywhere, symbol-overlay,
  rainbow-delimiters, page-break-lines, VLF
- UI: Duskmoon Moonlight, built-in which-key and EditorConfig, native mode
  line, optional fonts without icon-font assumptions
- Applications: Org, org-modern, org-pomodoro, Elfeed, and the bundled Agent
  Editor MCP package

Elpaca owns Emacs Lisp packages. Nix, devenv, direnv, or the current project
owns external executables.

## XDG paths

| Purpose | Default |
| --- | --- |
| Tracked configuration | `${XDG_CONFIG_HOME:-~/.config}/emacs/` |
| Elpaca and application data | `${XDG_DATA_HOME:-~/.local/share}/emacs/` |
| Disposable caches and auto-saves | `${XDG_CACHE_HOME:-~/.cache}/emacs/` |
| Mutable state and backups | `${XDG_STATE_HOME:-~/.local/state}/emacs/` |
| Agent Editor discovery state | `${XDG_STATE_HOME:-~/.local/state}/emacs/agent-editor/` |

Customizations, native compilation output, URL data, recentf, savehist,
save-place, bookmarks, the project list, TRAMP persistence, desktop and
Transient data, Org clock state, server authentication files, Agent Editor
metadata, backups, and auto-saves stay outside this checkout. UNIX
`emacsclient` sockets use the Emacs default runtime path (see
[Server and daemon behavior](#server-and-daemon-behavior)), not XDG state.

## Local configuration

The optional local override is resolved in this order:

1. the non-empty path in `GSMLG_EMACS_LOCAL`;
2. `${XDG_CONFIG_HOME:-~/.config}/gsmlg/emacs-local.el`;
3. no local file.

A missing file is silent. Copy settings from
[`local.el.example`](local.el.example) to the external location and keep
machine paths, private identities, and secrets there. XDG-backed
`custom-file` is loaded first when present, then the local file is loaded
last, so `setopt` can override any documented `gsmlg-*` user option.

## Tree-sitter

No grammar is downloaded during startup. Language dispatch prefers a
tree-sitter major mode only when `treesit-ready-p` succeeds for that
grammar; otherwise it uses the documented built-in or maintained classic
fallback.

### Supported grammars

```text
bash  c  cpp  css  elixir  erlang  go  heex  html  javascript  json
python  ruby  rust  toml  tsx  typescript  yaml
```

Erlang still uses maintained `erlang-mode` unless the running Emacs also
supplies a compatible `erlang-ts-mode` and grammar. Terraform and HCL keep
their maintained classic modes because GNU Emacs 30.2 does not provide
corresponding tree-sitter major modes.

### Commands

| Command | Purpose |
| --- | --- |
| `M-x gsmlg-treesit-report` | Show ready vs missing for every grammar above |
| `M-x gsmlg-treesit-install-language-grammar` | Install one grammar interactively |
| `M-x gsmlg-treesit-install-all-language-grammars` | Install every missing grammar |

`gsmlg-treesit-install-all-language-grammars`:

- fills gaps in `treesit-language-source-alist` from
  `gsmlg-treesit-default-sources` without replacing existing entries;
- skips grammars that are already ready;
- continues after individual failures and reports them in
  `*GSMLG Tree-sitter Install*` when run interactively;
- returns a plist with `:succeeded`, `:skipped`, and `:failed`;
- never runs automatically at startup.

Building grammars needs a working C toolchain (`cc` / `gcc` / `clang`) and
network access to clone the recipe repositories.

### Install location and load path

Compiled grammars are written under the XDG data directory:

```text
${XDG_DATA_HOME:-~/.local/share}/emacs/tree-sitter/
```

`gsmlg-treesit` adds that directory to `treesit-extra-load-path`, so grammars
installed by these helpers are discovered without changing system Emacs
paths.

### Custom recipes

Default clone recipes live in `gsmlg-treesit-default-sources`. To override
or extend them, set `treesit-language-source-alist` in the external local
file. Existing entries win; defaults only fill missing languages:

```elisp
(setopt treesit-language-source-alist
        '((elixir "https://example.invalid/tree-sitter-elixir")))
```

## Language-server prerequisites

Eglot does not install servers. It selects the first executable available in
the project/direnv environment:

| Language | Preference order |
| --- | --- |
| Elixir and HEEx | `expert --stdio`, `elixir-ls`, `language_server.sh` |
| Erlang | `elp` |
| JavaScript and TypeScript | project-local, then PATH `typescript-language-server --stdio` |
| Rust | `rust-analyzer` |
| Go | `gopls` |
| Python | `basedpyright-langserver --stdio`, `pyright-langserver --stdio` |
| Ruby | `ruby-lsp` |
| Zig | `zls` |
| Nix | `nixd`, `nil` |
| C and C++ | `clangd` |
| YAML | `yaml-language-server --stdio` |
| Dockerfile | `docker-langserver --stdio` |
| Terraform | `terraform-ls serve` |
| Shell | `bash-language-server start` |

Set `gsmlg-eglot-command-overrides` in the external local file when a project
uses another compatible command. Missing servers are harmless. A manual
`M-x gsmlg-eglot-ensure-maybe` rechecks the current environment and explains
why no server was selected.

Install formatters used by Apheleia in the same project environment. The
interactive `gsmlg-format-buffer` command uses a configured Apheleia formatter
or active Eglot formatting; no competing format-on-save hooks are enabled.

Consult project search and Agent Editor project search benefit from
`ripgrep`. envrc integration requires `direnv`. The Agent package has an
Emacs fallback for search when ripgrep is unavailable.

## Local projects, worktrees, and TRAMP

project.el is the only project abstraction. Git repositories, nested
repositories, and real Git worktrees retain their own `project-root`.
`node_modules/.bin` is added only to a buffer-local `exec-path`, and envrc
activates each project's environment before automatic Eglot discovery when
`gsmlg-envrc-enable` is non-nil. That option defaults to off so Emacs does
not run direnv or pop blocked-`.envrc` errors; set it in the external local
file to opt in. When enabled, envrc also covers SSH TRAMP buffers so remote
direnv environments remain remote. After changing `.envrc`, run
`M-x gsmlg-envrc-reload-and-refresh-eglot` to reload direnv, clear the
negative server cache, and either reconnect or start Eglot.

Remote development follows compute-near-data:

- TRAMP project roots remain remote file names;
- Eglot resolves and starts servers on the remote host;
- Consult ripgrep, formatters, compilation, tests, and shells run remotely;
- process helpers use file-handler-aware APIs;
- no remote project is copied or indexed locally.

The user's SSH configuration remains authoritative. Remote auto-save and
backup data go to local XDG storage, remote recent files are excluded, and
remote desktop buffers are not restored by default.

## Server and daemon behavior

Each OS user runs one formal interactive Emacs server named `main`. Prefer the
OS user service templates under `services/` and the `bin/gsmlg-emacs` helper:

```sh
# macOS launchd (after substituting HOME into the plist template)
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.gsmlg.emacs.plist

# systemd user unit
systemctl --user enable --now gsmlg-emacs.service

# diagnose / open clients
gsmlg-emacs status
gsmlg-emacs gui
gsmlg-emacs tty
```

Shell aliases in `oh-my-zsh/zshrc` point `EDITOR`, `VISUAL`, and `GIT_EDITOR` at
`emacsclient -s main`. Use `emacs-solo` only for diagnosis.

A normal interactive GUI or terminal process can still call:

```text
M-x gsmlg-server-start
```

Set `gsmlg-server-autostart` to `nil` in the local file to opt out for an
interactive, non-daemon process. Batch mode and `GSMLG_EMACS_TESTING=1` never
join the user singleton. Batch mode never opens a server socket.

### Socket location

UNIX sockets stay on the stock Emacs / `emacsclient` path so clients need no
extra environment:

| Condition | Socket directory |
| --- | --- |
| `XDG_RUNTIME_DIR` is set | `$XDG_RUNTIME_DIR/emacs/` |
| otherwise | `/tmp/emacs$UID/` |

TCP authentication files remain under XDG state:

```text
${XDG_STATE_HOME:-~/.local/state}/emacs/server/
```

Do not set `EMACS_SOCKET_NAME` for the default interactive server. After
changing this policy, restart Emacs (or run `M-x server-force-delete` then
`M-x gsmlg-server-start`) so an old socket under XDG state is not reused.

### Connecting with emacsclient

```sh
emacsclient -s main -c         # new graphical frame on the formal server
emacsclient -s main -nw        # frame in the current terminal
emacsclient -s main -n FILE    # visit FILE without waiting
```

When several servers run, `-s NAME` selects the socket named `NAME` in the
directory above. A bare name is enough; a full path is unnecessary for the
default layout.

### Formal server example

```sh
emacs --daemon=main
emacsclient -s main -c
```

Desktop persistence is controlled by `gsmlg-desktop-save-enabled`. The single
desktop file is `${XDG_STATE_HOME}/emacs/desktop/desktop.el`. Frames are not
restored (`desktop-restore-frames` is nil); emacsclient creates frames for the
current display.

## GSMLG AI Workbench

The AI Workbench (`gsmlg-ai-*`) and inline completion (`gsmlg-ai-completion-*`)
are deferred applications. Normal startup does not load `gptel` or `minuet`,
and no network request runs until you invoke a command.

### What you need to do

1. **Export an API key** in the shell that starts Emacs (or your login
   environment). Never put the secret in Git or in tracked Lisp:

   ```sh
   export DEEPSEEK_API_KEY=sk-...
   ```

   GUI Emacs on macOS often does not inherit interactive shell exports. Prefer
   a login-shell env, LaunchAgent/`environment.plist`, or import via
   `exec-path-from-shell` so `getenv` sees the key.

2. **First AI use installs packages** if Elpaca has not realized `gptel` /
   `minuet` / `plz` yet (needs network once). Afterwards they load from the
   locked builds.

3. **Use the workbench** with prefix `C-c A` (Org Agenda remains `C-c a`).

4. **Inline auto-completion stays off** until you enable it. Manual one-shot
   suggestions work without enabling a mode.

Optional machine overrides go in the external local file (see
`local.el.example`), for example
`${XDG_CONFIG_HOME:-~/.config}/gsmlg/emacs-local.el`.

### Defaults

| Setting | Default |
| --- | --- |
| Workbench backend | DeepSeek via `gptel-make-deepseek` |
| Workbench model | `deepseek-v4-flash` |
| API key env var | `DEEPSEEK_API_KEY` |
| Inline completion provider | Minuet `openai-fim-compatible` (DeepSeek FIM) |
| Automatic inline completion | off |
| Send confirmation | `when-sensitive` (paths matching secret patterns) |

On first workbench command, `gsmlg-ai` registers DeepSeek as the gptel default
when `gsmlg-ai-configure-deepseek-default` is non-nil. Missing
`DEEPSEEK_API_KEY` fails at request time with a clear error.

### Commands

| Command | Purpose |
| --- | --- |
| `C-c A g` / `gsmlg-ai-chat` | gptel chat |
| `C-c A m` / `gsmlg-ai-menu` | gptel model / request menu |
| `C-c A a` / `gsmlg-ai-ask` | one-shot question over selected context |
| `C-c A v` / `gsmlg-ai-review` | read-only review |
| `C-c A r` / `gsmlg-ai-rewrite-region` | preview-based region rewrite |
| `C-c A e` / `gsmlg-ai-edit` | staged multi-file edit proposal |
| `C-c A c` / `b` / `f` / `d` | context manager / add buffer / file / Dired |
| `C-c A p` | proposal review / apply |
| `C-c A x` | cancel incomplete request (keeps a ready proposal) |
| `C-c A i` | manual inline suggestion |
| `C-c A t` | toggle buffer-local automatic completion |
| `C-c A T` | toggle global automatic completion |
| `C-c A ?` | completion diagnostics |

Region / project-file / clear context commands are available via `M-x`
(`gsmlg-ai-context-add-region`, `gsmlg-ai-context-add-project-files`,
`gsmlg-ai-context-clear`) and inside the context manager buffer.

### Inline completion

| Goal | Action |
| --- | --- |
| One suggestion at point | `C-c A i` |
| Auto-complete in this buffer | `C-c A t` |
| Auto-complete in eligible buffers | set `gsmlg-ai-completion-auto-enable` to `t`, then `C-c A T` |
| Check blockers / provider | `C-c A ?` |

Completion is independent of workbench context. It never writes the buffer
before an accept command. Remote/TRAMP buffers are blocked unless
`gsmlg-ai-completion-allow-remote` is non-nil.

### Common options

Put overrides in the local file, not in tracked modules:

```elisp
;; Model / key env name (secret still comes from the environment)
(setopt gsmlg-ai-deepseek-model 'deepseek-v4-flash
        gsmlg-ai-deepseek-api-key-env "DEEPSEEK_API_KEY")

;; Disable the built-in DeepSeek default and configure gptel yourself
;; (setopt gsmlg-ai-configure-deepseek-default nil)

;; Opt into global automatic completion after C-c A T
(setopt gsmlg-ai-completion-auto-enable t)

;; Confirm every outbound workbench request
;; (setopt gsmlg-ai-confirm-before-send 'always)
```

More examples (local OpenAI-compatible servers, alternate Minuet providers)
are in `local.el.example`. Keybinding details live in
[docs/keybindings.md](docs/keybindings.md).

### Behavior notes

- Ask / review / edit use an explicit in-memory context; add buffers or files
  before sending when you need more than the default selection.
- Multi-file edits stay staged until you apply them from the proposal UI;
  apply never saves.
- `C-c A x` cancels in-flight ask/edit requests but leaves a ready proposal;
  discard it from the proposal buffer or with the discard command.
- Batch mode does not autoload or start AI providers.
## Agent Editor MCP

Agent Editor MCP starts with the formal interactive Emacs server by default
and is always off in batch mode. No project or startup directory is required;
run:

```text
M-x gsmlg-agent-start
```

The listener is loopback-only. Port 9876 is the default; `EMACS_AGENT_PORT`
overrides it. To opt out of interactive autostart, set
`gsmlg-agent-autostart` to nil. `EMACS_AGENT_AUTOSTART=1` still enables
autostart explicitly.

The endpoint supports MCP versions `2026-07-28`, `2025-11-25`, and
Codex-compatible `2025-06-18`.

The recommended deployment is one `main` Emacs server, one MCP endpoint, and
zero or more projects registered through MCP after startup. Direct absolute
local files do not require project registration:

```sh
emacs --daemon=main
emacsclient -s main -c
```

Connection metadata is written to
`${XDG_STATE_HOME:-~/.local/state}/emacs/agent-editor/connection.json`.
Management commands: `gsmlg-agent-start`, `gsmlg-agent-stop`,
`gsmlg-agent-restart`, `gsmlg-agent-status`, and
`gsmlg-agent-show-connection`. `M-x gsmlg-agent-stop` stops only MCP and does
not terminate Emacs. Startup catches MCP failures so they cannot prevent the
editor from opening. See the bundled
[Agent Editor MCP README](site-lisp/agent-editor-mcp/README.md) for its
project registration, direct-file, protocol, and editing model.

## Package updates

Update packages only as an intentional maintenance operation:

1. Run `M-x gsmlg-elpaca-update-package` once for each package being updated.
   The command checks out the recipe's configured branch (or the upstream
   default branch), rebuilds packages that share that source repository, and
   deliberately leaves the lock unchanged.
   `corfu-terminal`, `diff-hl`, `erlang`, `git-timemachine`, `popon`, and
   `zig-mode` are immutable archive recipes: choose and verify an exact
   upstream commit, remove the old package source/build with `M-x
   elpaca-delete`, update that package's lock `:ref`, and let a connected
   startup re-realize it before exercising a fresh bootstrap. The command
   rejects these packages before attempting a Git operation.
2. Test that updated graph before writing a new lock:

   ```sh
   GSMLG_EMACS_STARTUP_MODE=reuse \
   GSMLG_EMACS_TEST_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}" \
   GSMLG_EMACS_TEST_ALLOW_UNLOCKED=1 \
   ./run-emacs-tests.sh
   ```

3. In the same updated Emacs installation, run
   `M-x gsmlg-elpaca-write-lock-file`.
4. Review every change in `elpaca-lock.el`.
5. Run `./run-emacs-tests.sh` normally, then commit the configuration and lock
   file together.

Do not change a package revision without verifying that the recipe can realize
it. `gsmlg-elpaca-update-package` refuses to update Elpaca itself:
select an exact Elpaca revision, change `gsmlg-elpaca-revision` and both
`elpaca` lock entries together, then force a fresh bootstrap and run the full
suite.

## Tests

Run the complete isolated suite from the repository root:

```sh
./run-emacs-tests.sh
```

It runs installer migration tests first, forces a fresh Elpaca bootstrap in
temporary HOME/XDG directories, repeats startup with package networking
blocked, requires every module in a separate warm/offline Emacs process, runs
every `emacs.d/tests/*-test.el` ERT test, byte-compiles
first-party Lisp with warnings as errors to temporary output, runs checkdoc and
active dependency scans, and finishes with the Agent Editor MCP test suite.
The temporary environment is removed after the run.

Use a specific Emacs executable with:

```sh
EMACS=/path/to/emacs ./run-emacs-tests.sh
```

When `emacsclient` is not next to that executable, also set
`EMACSCLIENT=/path/to/emacsclient`.

The component commands are:

```sh
./emacs.d/tests/install-test.sh
./test-emacs-startup.sh
./emacs.d/tests/module-load-test.sh
./lint-emacs-config.sh
./emacs.d/site-lisp/agent-editor-mcp/run_tests.sh
```

The standalone startup test reuses the current XDG package data by default
while keeping HOME, configuration, cache, state, and runtime data isolated:

```sh
./test-emacs-startup.sh
```

Override the reusable package data directory with
`GSMLG_EMACS_TEST_DATA_HOME=/safe/path`. Force a completely fresh package
bootstrap with `GSMLG_EMACS_STARTUP_MODE=fresh`; fresh mode uses temporary
package data unless an explicitly safe test-root path is supplied. The
offline pass sets `GSMLG_EMACS_OFFLINE=1` and blocks Emacs URL/network APIs,
package.el operations, network Git subcommands, curl, and wget. A valid
startup also verifies every realized Git repository or immutable source
archive against the non-empty exact-ref lock, including pinned Elpaca and
theme revisions, checks the enabled
Duskmoon Moonlight theme, and asserts the absence of server or MCP listeners
in batch mode. A
separate named-daemon probe verifies natural XDG discovery and shuts the daemon
down before the test exits.

After testing, `git status --short` must show no generated state, `.elc`, or
`.eln` files.

CI runs the same entrypoints on required Ubuntu/Emacs 30.2 and
macOS/Emacs 30.2 jobs. Ubuntu with the current Emacs snapshot is advisory so
forward-compatibility failures remain visible without replacing the stable
baseline.

## Recovery

If Elpaca data is damaged:

1. Quit Emacs processes using that data directory.
2. Move `${XDG_DATA_HOME:-~/.local/share}/emacs/elpaca/` to a backup path.
3. Preserve this configuration and `elpaca-lock.el`.
4. Start Emacs once with network access to reproduce the pinned package graph.
5. Run `./run-emacs-tests.sh`.
6. Verify a warm startup with `GSMLG_EMACS_OFFLINE=1 emacs`.

Do not delete mutable state or the lock file to repair package builds.
