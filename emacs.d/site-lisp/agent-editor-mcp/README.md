# Emacs Agent Editor MCP

Emacs Agent Editor v0.3 exposes a running Emacs instance as a local HTTP MCP
server for software-development agents. The agent and the human use the same
Emacs buffers, so unsaved edits, undo history, major modes, save hooks, and
external file changes have one authoritative owner.

One server owns one long-running editor runtime. It starts with no project and
can then manage:

- direct local files addressed by canonical absolute path;
- zero or more projects registered explicitly through MCP;
- atomic edits spanning projects and direct files;
- runtime-wide changes, approvals, activity, and mutation serialization.

A project supplies an optional root for file enumeration, search, aggregate
diagnostics, symbols, and relative document paths. It is not required for
startup or ordinary file editing, and there is no current-project protocol
state.

See [design.md](design.md) for the architecture and safety invariants.

## Requirements

- GNU Emacs 30.2 or newer
- `ripgrep` for asynchronous project search, when available
- Eglot/Xref providers for language-server semantic tools
- Tree-sitter Python or YAML grammars for their parser diagnostics

Search falls back to an Emacs implementation when `ripgrep` is unavailable.
The server is pure Emacs Lisp and has no external MCP service.

Remote/TRAMP paths are not supported by this server version.

## Quick start

Add the package directory to `load-path`, load the package, and start the
project-optional runtime:

```elisp
(add-to-list
 'load-path
 (expand-file-name "site-lisp/agent-editor-mcp" user-emacs-directory))

(require 'emacs-agent-editor)

(setq emacs-agent-editor-access-mode 'autonomous
      emacs-agent-editor-save-policy 'immediate)

(emacs-agent-editor-start)
```

The package listens on port `9876` by default. Pass `0` explicitly when the
operating system should select an available port for one invocation:

```elisp
(emacs-agent-editor-start 0)
```

Stop only the MCP service with:

```elisp
(emacs-agent-editor-stop)
```

Stopping clears runtime, project, session, tool, cursor, and connection state.
It leaves Emacs and visiting buffers running.

## Addressing files and projects

### Direct absolute file

No project registration is needed:

```json
{
  "name": "emacs_agent_document_read",
  "arguments": {
    "path": "/home/user/.config/emacs/init.el"
  }
}
```

Use the returned opaque revision in a guarded mutation:

```json
{
  "name": "emacs_agent_document_apply_edits",
  "arguments": {
    "path": "/home/user/.config/emacs/init.el",
    "expected_revision": "rev:...",
    "edits": [
      {
        "start": {"line": 1, "column": 0},
        "end": {"line": 1, "column": 0},
        "new_text": ";; managed by Emacs\n"
      }
    ]
  }
}
```

### One project

Register an absolute local directory:

```json
{
  "name": "emacs_agent_project_open",
  "arguments": {
    "root": "/home/user/src/example"
  }
}
```

The result contains an opaque `project_id`. A relative path always carries
that ID explicitly:

```json
{
  "name": "emacs_agent_document_read",
  "arguments": {
    "project_id": "project_abc",
    "path": "lib/example.ex"
  }
}
```

Project operations also require the ID:

```json
{
  "name": "emacs_agent_project_search",
  "arguments": {
    "project_id": "project_abc",
    "query": "GenServer"
  }
}
```

Registering a directory does not switch the human's selected project, buffer,
window, or frame. Plain directories are supported when `project.el` does not
detect a native project.

### Multiple projects

Register each root independently:

```text
project_open(/home/user/src/service-a) -> project_a
project_open(/home/user/src/service-b) -> project_b
```

Both IDs remain valid concurrently. Every request supplies its own project
context; a previous request never selects context for a later one.

`emacs_agent_editor_apply_edits` can atomically combine project-relative
targets and direct absolute targets. All paths and revisions are validated
before any buffer changes.

Closing a project unregisters only its semantic context. It does not kill
buffers, discard edits, delete change sets, or prevent later direct access to
the same canonical file.

## Dotfiles integration

This repository loads the package from `emacs.d/lisp/gsmlg-agent.el`.
Loading the configuration never starts a listener in batch mode, and
autostart is disabled by default.

Start the service without a directory prompt:

```text
M-x gsmlg-agent-start
```

The integration uses port `9876` by default. `EMACS_AGENT_PORT` overrides the
port, and `gsmlg-agent-autostart` or `EMACS_AGENT_AUTOSTART=1` opts into
interactive startup. `M-x gsmlg-agent-stop` stops MCP without terminating
Emacs.

The recommended deployment is one dedicated named daemon, one endpoint, and
many optional projects:

```sh
EMACS_AGENT_AUTOSTART=1 emacs --daemon=agent-editor

emacsclient \
  --socket-name="${XDG_STATE_HOME:-$HOME/.local/state}/emacs/server/agent-editor" \
  -c
```

Multiple daemons remain possible when a user wants process isolation, but
project isolation does not require them.

## Connecting

At startup, the server atomically writes private connection metadata below:

```text
${XDG_STATE_HOME:-~/.local/state}/emacs-agent-editor/<daemon>/connection.json
```

This is also the authoritative discovery path for the dotfiles integration.
A normal interactive instance uses `interactive`; a named daemon uses its
daemon name. The containing directory has mode `0700` and the file has mode
`0600`.

Schema version 2 contains editor-runtime identity, not project state:

```json
{
  "schema_version": 2,
  "instance_id": "editor_abc",
  "daemon": "agent-editor",
  "pid": 12345,
  "endpoint": "http://127.0.0.1:9876/mcp",
  "token_authentication": false,
  "protocol_versions": ["2026-07-28", "2025-11-25", "2025-06-18"],
  "filesystem_scope": "unrestricted",
  "started_at": "2026-07-30T12:00:00Z"
}
```

The dynamic project list is obtained with `emacs_agent_project_list`; it is
not serialized in the connection file.

Token authentication is disabled by default, so `token` is omitted. When
enabled, `token_authentication` is true and the file also contains the token.
Clients should reread the connection file whenever they connect. Revoking the
writer rotates an enabled token, rewrites the file, and invalidates legacy
sessions.

Do not copy connection metadata into a repository or expose a token in logs.

## Protocol profiles

The loopback `/mcp` endpoint preserves both wire profiles:

- `2026-07-28`: stateless requests with MCP HTTP headers and per-request
  client metadata.
- `2025-11-25` and `2025-06-18`: compatibility flows using `initialize`,
  `notifications/initialized`, and `Mcp-Session-Id`.

Both profiles expose the same v0.3 tool registry. When authentication is
enabled, every request must include `Authorization: Bearer <token>`.

## Tools

Every name below uses the `emacs_agent_` prefix on the wire.

| Area | Tools |
| --- | --- |
| Editor runtime | `editor_info`, `editor_apply_edits`, `editor_checkpoint`, `editor_sync`, `editor_diff`, `editor_modified_documents`, `editor_context_get` |
| Projects | `project_open`, `project_list`, `project_info`, `project_close`, `project_files`, `project_search`, `project_diagnostics`, `project_symbols` |
| Documents | `document_read`, `document_status`, `document_apply_edits`, `document_replace`, `document_apply_patch`, `document_create`, `document_move`, `document_delete`, `document_diagnostics`, `document_symbols` |
| Change sets | `changeset_list`, `changeset_get`, `changeset_rollback` |
| Semantics | `symbol_definition`, `symbol_references`, `symbol_rename`, `code_actions` |
| Formatting | `format_document`, `format_range` |
| Collaboration | `approval_status`, `approval_cancel` |

`editor_info` reports runtime identity, policies, health, registered project
count, managed document count, protocols, supported tools, and runtime
capabilities. It does not select or report a current project.

Xref runtime metadata distinguishes `backend_present` from
`noninteractive_ready` and `available`. Emacs' fallback `etags` backend is
present even without a TAGS table, but it is advertised as unavailable until
an explicitly configured table can be verified without prompting.

Unsupported native language capabilities fail with
`CAPABILITY_UNAVAILABLE`; text search is never presented as semantic rename
or reference analysis. Xref calls run with interactive input disabled and a
server-side deadline controlled by `emacs-agent-semantic-xref-timeout`.
Yielding providers honor MCP cancellation; providers that never yield to the
Emacs event loop cannot be preempted safely.

## Document and edit contract

The canonical absolute path is document identity. Addressing the same file
directly and through `project_id` plus a relative path reuses one visiting
buffer, document object, revision, undo history, and change-set history.

Document-bearing results include:

```text
path           canonical absolute path
project_id     supplied context or false
relative_path project-relative path or false
```

Rules for inputs:

- `path` is always required.
- An absolute local `path` does not require a project.
- A relative `path` requires `project_id`.
- An absolute path supplied with `project_id` must be inside that project.
- A relative move destination requires `new_project_id`.
- Parent traversal, remote paths, and symlink escapes are rejected.

Revisions are opaque. A mutating client reads the document first, then sends
the returned revision as `expected_revision`. If a human, another request, or
the filesystem changes the document, the mutation fails and the client must
reread.

Positions use one-based logical lines and zero-based Emacs-character columns.
Ranges are half-open. Tabs count as one character; columns are not display,
UTF-8, or UTF-16 offsets. CRLF is represented as logical newlines while the
buffer preserves its coding system and EOL style.

All edits in a request refer to the same expected revision, are validated
together, and are applied in descending position order. Overlaps and
same-position inserts are rejected. Multi-document mutations are atomic and
form one meaningful undo unit per document.

Exact replacement, strict patching, semantic rename, code actions, and
formatting support guarded preview/dry-run behavior. Semantic previews are
frozen and bound to revisions. Language-server commands are never executed.

Write results include revision, change-set, checkpoint, modification, diff,
and truncation metadata. Live results are validated against the advertised
output schema before they are returned.

## Runtime and save policies

`emacs-agent-editor-access-mode` controls mutations:

- `read-only`: reject all mutations.
- `review`: require human approval for sensitive lifecycle and persistence
  operations.
- `autonomous`: permit guarded mutations without interactive approval.

`emacs-agent-editor-save-policy` controls persistence:

- `immediate`: save successful creates and edits immediately.
- `manual`: retain buffer changes until an explicit checkpoint.
- `explicit-per-call`: checkpoint only when the request asks for it.

Buffers remain the live source of truth. Saving is a filesystem checkpoint.
External changes reload a clean buffer; an external change concurrent with
unsaved buffer edits produces a reconciliation conflict.

Each successful mutation creates a runtime-scoped change set with revision
guards, in-memory before-images, and a frozen unified diff. Guarded rollback
is available only while every affected document still matches the recorded
final revision. Runtime restart intentionally invalidates revisions, cursors,
previews, approvals, and rollback history.

## Filesystem policy

`emacs-agent-policy-filesystem-scope` supports:

- `unrestricted`: allow local files accessible to the Emacs OS user, subject
  to all deny rules and access mode.
- `allowlist`: additionally require every canonical path to be inside
  `emacs-agent-policy-allowed-roots`.

Project registration never bypasses this policy. All existing and create/move
targets are canonicalized before authorization; missing targets are resolved
through their nearest existing ancestor to prevent symlink escapes.

The server continues to reject repository metadata, environment secret files,
credential basenames/extensions, binary files, special files, oversized
documents, and remote paths. `emacs-agent-policy-denied-paths` adds canonical
path globs or predicates.

Direct-file access broadens authority compared with a root-confined server.
Use `allowlist` for a narrower deployment, and run the daemon as an OS user
whose filesystem permissions match the intended agent authority.

## Human controls

| Command | Action |
| --- | --- |
| `M-x emacs-agent-editor-status` | Show runtime identity, project count, and connection file. |
| `M-x emacs-agent-editor-start` | Start with zero registered projects. |
| `M-x emacs-agent-editor-stop` | Stop MCP and remove connection metadata. |
| `M-x emacs-agent-editor-pause` | Pause all mutations while retaining read access. |
| `M-x emacs-agent-editor-resume` | Resume mutations. |
| `M-x emacs-agent-editor-revoke-writer` | Pause, rotate an enabled token, and clear sessions. |
| `M-x emacs-agent-show-activity` | Show runtime activity and pending approvals. |
| `M-x emacs-agent-show-changes` | Show runtime change sets. |
| `M-x emacs-agent-show-approvals` | Review approval details and TTL state. |

The activity, changes, and approval buffers provide keyboard controls for
approve/reject/cancel, partial document approval where safe, pause/resume,
credential revocation, checkpoint, rollback, review, diff display, file
visiting, and hunk highlighting. Human display abbreviates paths; runtime
state retains canonical absolute paths.

## Configuration

Core options are available under `M-x customize-group RET
emacs-agent-editor RET`.

| Variable | Default | Meaning |
| --- | --- | --- |
| `emacs-agent-editor-host` | `"127.0.0.1"` | IPv4 loopback listener address. |
| `emacs-agent-editor-port` | `9876` | Listener port; set to zero explicitly for an ephemeral port. |
| `emacs-agent-editor-endpoint` | `"/mcp"` | HTTP endpoint path. |
| `emacs-agent-editor-allowed-origins` | `nil` | Exact allowlist for a present `Origin` header. |
| `emacs-agent-editor-state-directory` | XDG state | Parent directory for private runtime state. |
| `emacs-agent-editor-access-mode` | `autonomous` | `read-only`, `review`, or `autonomous`. |
| `emacs-agent-editor-save-policy` | `immediate` | `immediate`, `manual`, or `explicit-per-call`. |
| `emacs-agent-editor-token-authentication-enabled` | `nil` | Require bearer authentication. |
| `emacs-agent-editor-bearer-token` | `nil` | Fixed token, or generate one when nil. |
| `emacs-agent-policy-filesystem-scope` | `unrestricted` | Direct-file authority mode. |
| `emacs-agent-policy-allowed-roots` | `nil` | Canonical roots permitted in `allowlist` mode. |
| `emacs-agent-policy-denied-paths` | `nil` | Additional denied path globs or predicates. |
| `emacs-agent-policy-maximum-document-bytes` | 4 MiB | Maximum managed document size. |
| `emacs-agent-journal-enabled` | `nil` | Enable the redacted JSONL activity journal. |
| `emacs-agent-semantic-format-function` | `nil` | Trusted string-in/string-out formatter. |

Example restricted configuration:

```elisp
(setq emacs-agent-editor-access-mode 'review
      emacs-agent-editor-save-policy 'manual
      emacs-agent-editor-token-authentication-enabled t
      emacs-agent-policy-filesystem-scope 'allowlist
      emacs-agent-policy-allowed-roots
      '("/home/user/src/" "/home/user/.config/emacs/")
      emacs-agent-editor-allowed-origins
      '("http://127.0.0.1:3000")
      emacs-agent-journal-enabled t)
```

The optional journal is stored in the private daemon state directory. Source
content, before-images, authorization data, credentials, and bearer tokens are
removed before serialization.

## Security model

Version 0.3 provides:

- IPv4 loopback-only HTTP;
- optional bearer authentication and exact origin allowlisting;
- bounded content-length framing and strict UTF-8/JSON-RPC validation;
- canonical, symlink-safe filesystem authorization;
- secret, metadata, binary, special-file, size, and remote-path denial;
- runtime-serialized mutations with optimistic revision guards;
- external-change reconciliation and request cancellation;
- private, atomically rewritten connection metadata;
- schema validation for tool inputs and outputs.

With token authentication disabled, any local process able to connect to the
port can call the endpoint. Enable authentication when the local process
boundary is not sufficiently trusted.

The server is for trusted local agents. It is not an internet-facing service.

## Development

Run the package suite:

```sh
emacs.d/site-lisp/agent-editor-mcp/run_tests.sh
```

After changing integration code, also run:

```sh
./test-emacs-startup.sh
./lint-emacs-config.sh
```

The suite covers transport/authentication, all three protocol versions,
runtime and project lifecycle, canonical target resolution,
revision/reconciliation, Unicode/tab/CRLF positions, guarded transformations,
cross-document atomicity, diagnostics, buffer-aware search, semantic previews,
change sets,
approval binding/expiry, redaction, rollback, journaling, and human controls.
