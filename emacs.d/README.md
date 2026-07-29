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
- In-buffer completion: Corfu, corfu-terminal, Cape, Yasnippet, standard CAPFs
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

Customizations, native compilation output, URL data, recentf, savehist,
save-place, bookmarks, the project list, TRAMP persistence, desktop and
Transient data, Org clock state, server files, Agent Editor metadata, backups,
and auto-saves stay outside this checkout.

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

## Tree-sitter prerequisites

No grammar is downloaded during startup. The configuration prefers a
tree-sitter mode only when its grammar is installed and ready; otherwise it
uses the documented built-in or maintained fallback.

Externally provide the grammars needed by your languages:

```text
bash  c  cpp  css  elixir  erlang  heex  html  javascript  json
go  python  ruby  rust  toml  tsx  typescript  yaml
```

Use:

```text
M-x gsmlg-treesit-report
```

to see current readiness. If an explicit Emacs-side install is desired, first
define the relevant entries in `treesit-language-source-alist` in the local
file, then invoke:

```text
M-x gsmlg-treesit-install-language-grammar
```

This helper is never called automatically. Erlang uses maintained
`erlang-mode` unless the running Emacs also supplies a compatible
`erlang-ts-mode` and grammar. Terraform and HCL use their maintained classic
modes because GNU Emacs 30.2 does not provide corresponding tree-sitter major
modes.

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

Consult project search and Agent Editor workspace search benefit from
`ripgrep`. envrc integration requires `direnv`. The Agent package has an
Emacs fallback for search when ripgrep is unavailable.

## Local projects, worktrees, and TRAMP

project.el is the only project abstraction. Git repositories, nested
repositories, and real Git worktrees retain their own `project-root`.
`node_modules/.bin` is added only to a buffer-local `exec-path`, and envrc
activates each project's environment before automatic Eglot discovery. envrc
is enabled for SSH TRAMP buffers, so remote direnv environments remain remote.

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

A normal GUI or terminal process does not start an Emacs server by default.
Start one explicitly:

```text
M-x gsmlg-server-start
```

Set `gsmlg-server-autostart` in the local file or
`GSMLG_EMACS_SERVER=1` to opt in for an interactive, non-daemon process. Named
daemons already provide an emacsclient server. Batch mode never opens a server
socket.

Optional desktop persistence is controlled by
`gsmlg-desktop-save-enabled` and is off by default.

## Agent Editor MCP

Agent Editor MCP autostart is off by default and always off in batch mode. Set
an explicit workspace with `gsmlg-agent-workspace` or
`EMACS_AGENT_WORKSPACE`, then run:

```text
M-x gsmlg-agent-start
```

The listener is loopback-only. Port 9876 is the compatibility default;
`EMACS_AGENT_PORT` overrides it. To opt into interactive autostart, set
`gsmlg-agent-autostart` or `EMACS_AGENT_AUTOSTART=1`.

The package supports one workspace per Emacs process, not multi-workspace
request routing. Prefer one named daemon per workspace:

```sh
EMACS_AGENT_WORKSPACE=/path/to/workspace \
  EMACS_AGENT_AUTOSTART=1 \
  emacs --daemon=workspace-name

emacsclient \
  --socket-name="${XDG_STATE_HOME:-$HOME/.local/state}/emacs/server/workspace-name" \
  -c
```

Connection metadata is written below
`${XDG_STATE_HOME:-~/.local/state}/emacs/agent-editor/`. `M-x
gsmlg-agent-stop` stops only MCP and does not terminate Emacs. Startup catches
MCP failures so they cannot prevent the editor from opening. See the bundled
[Agent Editor MCP README](site-lisp/agent-editor-mcp/README.md) for its
protocol and editing model.

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

To reuse a known-safe package data directory for the standalone startup test:

```sh
GSMLG_EMACS_STARTUP_MODE=reuse \
GSMLG_EMACS_TEST_DATA_HOME=/safe/path \
./test-emacs-startup.sh
```

`test-emacs-startup.sh` otherwise uses isolated temporary directories. The
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
