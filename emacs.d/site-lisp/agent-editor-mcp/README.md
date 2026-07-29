# Emacs Agent Editor MCP

Emacs Agent Editor exposes a running Emacs instance as a local HTTP MCP
server for software-development agents. The agent and the human use the same
Emacs buffers, so unsaved edits, undo history, major modes, save hooks, and
external file changes have one authoritative owner.

The current implementation covers guarded exact replacement and unified
patching, atomic multi-buffer edits, diagnostics, buffer-aware search,
change-set review, Eglot/Xref semantics, trusted formatting, and the
keyboard-driven approval UI described in [design.md](design.md).

## Requirements

- Emacs 29.1 or newer
- A local workspace directory
- `ripgrep` for asynchronous workspace search, when available
- Eglot/Xref providers for language-server semantic tools
- Tree-sitter Python or YAML grammars for their parser diagnostics

Search falls back to an Emacs implementation when `ripgrep` is unavailable.
The server is pure Emacs Lisp and has no external MCP service.

## Quick start

Add the package directory to `load-path`, load the package, and start one
server for one workspace:

```elisp
(add-to-list
 'load-path
 (expand-file-name "site-lisp/agent-editor-mcp" user-emacs-directory))

(require 'emacs-agent-editor)

(setq emacs-agent-editor-access-mode 'autonomous
      emacs-agent-editor-save-policy 'immediate)

(emacs-agent-editor-start "/path/to/workspace")
```

The port defaults to `0`, so the operating system selects an available port.
To use a fixed port:

```elisp
(setq emacs-agent-editor-port 9876)
```

or pass it for one invocation:

```elisp
(emacs-agent-editor-start "/path/to/workspace" 9876)
```

Stop the server with:

```elisp
(emacs-agent-editor-stop)
```

### Dotfiles integration

This repository already loads the package from
`emacs.d/lisp/init-agent-editor-mcp.el`. An Emacs daemon starts the server
after initialization and binds it to the directory from which the daemon was
launched. Interactive, non-daemon Emacs sessions load the package but do not
start a server automatically.

For predictable workspace isolation, start one named daemon from each project:

```sh
cd /path/to/workspace
emacs --daemon=workspace-name
emacsclient --socket-name=workspace-name -c
```

## Connecting

At startup, the server writes private connection metadata to:

```text
${XDG_STATE_HOME:-~/.local/state}/emacs-agent-editor/<daemon>/connection.json
```

For a normal interactive Emacs instance, `<daemon>` is `interactive`. A named
daemon uses its daemon name. The containing directory has mode `0700` and the
metadata file has mode `0600`.

The file contains:

```json
{
  "schema_version": 1,
  "daemon": "workspace-name",
  "pid": 12345,
  "workspace": "/path/to/workspace/",
  "endpoint": "http://127.0.0.1:54321/mcp",
  "token_authentication": false,
  "protocol_versions": ["2026-07-28", "2025-11-25"],
  "started_at": "2026-07-28T08:00:00Z"
}
```

Token authentication is disabled by default, so the `token` field is omitted.
When token authentication is enabled, `token_authentication` is `true` and
the file also contains a `token` field. Clients should read the connection
file each time they connect. Revoking the writer rotates an enabled token,
rewrites this file, and invalidates existing credentials and legacy sessions.

Do not copy the connection file into a repository or expose an enabled token
in logs.

### Discovery example

The modern protocol profile uses MCP method headers and per-request metadata:

```sh
connection="${XDG_STATE_HOME:-$HOME/.local/state}/emacs-agent-editor/workspace-name/connection.json"
endpoint="$(jq -r .endpoint "$connection")"

curl "$endpoint" \
  -H "Content-Type: application/json" \
  -H "MCP-Protocol-Version: 2026-07-28" \
  -H "Mcp-Method: server/discover" \
  --data-binary '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "server/discover",
    "params": {
      "_meta": {
        "io.modelcontextprotocol/protocolVersion": "2026-07-28",
        "io.modelcontextprotocol/clientInfo": {
          "name": "example-client",
          "version": "1.0"
        },
        "io.modelcontextprotocol/clientCapabilities": {}
      }
    }
  }'
```

When token authentication is enabled, also read the token and add the
authorization header:

```sh
token="$(jq -r .token "$connection")"
authorization="Authorization: Bearer $token"
```

Then add `-H "$authorization"` to the `curl` command above.

## Protocol profiles

The endpoint supports two wire profiles:

- `2026-07-28`: stateless requests with standard MCP HTTP headers and
  per-request client metadata.
- `2025-11-25`: compatibility profile using `initialize`,
  `notifications/initialized`, and `Mcp-Session-Id`.

Both profiles expose the same editor tool registry. When token authentication
is enabled, every request must include `Authorization: Bearer <token>`. The
v0.2 listener accepts only `127.0.0.1`.

## Tools

| Area | Tools |
| --- | --- |
| Workspace | `workspace_info`, `workspace_files`, `workspace_search`, `workspace_apply_edits`, `workspace_checkpoint`, `workspace_sync`, `workspace_diff`, `workspace_modified_documents`, `workspace_diagnostics`, `workspace_symbols` |
| Documents | `document_read`, `document_status`, `document_apply_edits`, `document_replace`, `document_apply_patch`, `document_create`, `document_move`, `document_delete`, `document_diagnostics`, `document_symbols` |
| Change sets | `changeset_list`, `changeset_get`, `changeset_rollback` |
| Semantics | `symbol_definition`, `symbol_references`, `symbol_rename`, `code_actions` |
| Formatting | `format_document`, `format_range` |
| Collaboration | `editor_context_get`, `approval_status`, `approval_cancel` |

Every name above has the `emacs_agent_` prefix on the wire. Unsupported native
language capabilities fail with `CAPABILITY_UNAVAILABLE`; text search is never
presented as semantic rename or reference analysis.

Document revisions are opaque. A mutating client must first read a document,
then send the returned revision as `expected_revision`. If the buffer or file
changes in the meantime, the mutation fails and the client must reread.

Positions use one-based logical lines and zero-based Emacs-character columns.
Ranges are half-open. Tabs count as one character; columns are not display
columns or UTF-8/UTF-16 offsets. CRLF is represented as logical newlines while
the document coding system preserves its EOL style. All edits refer to the
same `expected_revision`, are validated together, and are applied in
descending position order. Overlaps and multiple inserts at the same position
are rejected.

Exact replacement, patching, workspace edits, semantic rename, and formatting
support preview/dry-run flows. Rename and range formatting return a frozen
preview identifier that must be supplied to the apply call. Code actions are
classified; only pure workspace edits can be applied. Language-server commands
are never executed.

Core write results consistently include `old_revision`, `new_revision`,
`changeset_id`, `applied`, `checkpointed`, `modified`, `diff`, and
`truncated`. Public tool errors contain an uppercase stable `code`, `message`,
`retryable`, nested `details`, and a compatibility `legacy_code`.

## Workspace and save policies

`emacs-agent-editor-access-mode` controls mutations:

- `read-only`: reject all mutating tools.
- `review`: allow ordinary guarded editing, but require approve-then-retry for
  move, delete, checkpoint, and rollback operations.
- `autonomous`: allow guarded mutations without interactive approval.

`emacs-agent-editor-save-policy` controls persistence:

- `immediate`: save successful creates and edits immediately.
- `manual`: keep creates and edits in buffers until explicitly checkpointed.
- `explicit-per-call`: checkpoint only when the tool request asks for it.

Buffers are the live source of truth. Saving is a checkpoint to the
filesystem. External changes reload a clean buffer; a simultaneous external
change and unsaved buffer edit produces a reconciliation conflict.

Each successful mutation creates a change set containing revision guards,
before-images in memory, and a frozen unified diff. Rollback is allowed only
while every affected document still matches the recorded final revision.
Change-set contents persist for the lifetime of the Emacs daemon; restarting
the daemon intentionally invalidates revisions, cursors, previews, approvals,
and in-memory rollback history.

## Human controls

Useful interactive commands:

| Command | Action |
| --- | --- |
| `M-x emacs-agent-editor-status` | Show server status and connection-file path. |
| `M-x emacs-agent-editor-start` | Start a server for a selected workspace. |
| `M-x emacs-agent-editor-stop` | Stop the server and remove connection metadata. |
| `M-x emacs-agent-editor-pause` | Pause mutations while retaining read access. |
| `M-x emacs-agent-editor-resume` | Resume mutations. |
| `M-x emacs-agent-editor-revoke-writer` | Pause mutations, rotate an enabled token, and clear sessions. |
| `M-x emacs-agent-show-activity` | Show requests and pending approvals. |
| `M-x emacs-agent-show-changes` | Show recorded change sets. |
| `M-x emacs-agent-show-approvals` | Review approval details and TTL state. |

The activity buffer provides:

- `a`: approve the operation at point
- `x`: reject it
- `P` / `R`: pause or resume mutations
- `k`: revoke the writer credential
- `g`: refresh

The changes buffer provides:

- `RET`: visit the first affected file
- `d`: view the frozen diff
- `c`: checkpoint affected modified buffers
- `r`: roll back the change set
- `v`: mark it reviewed
- `P` / `R`: pause or resume mutations
- `g`: refresh

The approvals buffer shows redacted operation impact and supports approve,
reject, and cancel. Partial acceptance is explicitly unsupported. Change-set
diff buffers are read-only, can refresh, and can highlight current hunks in
their source buffers.

## Configuration

Core options can be set with `setq` or through
`M-x customize-group RET emacs-agent-editor RET`.

| Variable | Default | Meaning |
| --- | --- | --- |
| `emacs-agent-editor-host` | `"127.0.0.1"` | Listener address; v0.2 only accepts IPv4 loopback. |
| `emacs-agent-editor-port` | `0` | Listener port; zero selects an ephemeral port. |
| `emacs-agent-editor-endpoint` | `"/mcp"` | HTTP endpoint path. |
| `emacs-agent-editor-allowed-origins` | `nil` | Allowed values for a present `Origin` header. |
| `emacs-agent-editor-state-directory` | XDG state directory | Parent directory for private runtime state. |
| `emacs-agent-editor-access-mode` | `autonomous` | `read-only`, `review`, or `autonomous`. |
| `emacs-agent-editor-save-policy` | `immediate` | `immediate`, `manual`, or `explicit-per-call`. |
| `emacs-agent-editor-token-authentication-enabled` | `nil` | Require bearer-token authentication for MCP requests. |
| `emacs-agent-editor-bearer-token` | `nil` | When authentication is enabled, use this fixed token or generate one when nil. |
| `emacs-agent-policy-maximum-document-bytes` | 4 MiB | Maximum managed document size. |
| `emacs-agent-journal-enabled` | `nil` | Enable the redacted JSONL activity journal. |
| `emacs-agent-semantic-format-function` | `nil` | Trusted string-in/string-out document formatter configured by the Emacs user. |

Example:

```elisp
(setq emacs-agent-editor-access-mode 'review
      emacs-agent-editor-save-policy 'manual
      emacs-agent-editor-token-authentication-enabled t
      ;; Omit this setting to generate a fresh token at startup.
      emacs-agent-editor-bearer-token
      (getenv "AGENT_EDITOR_MCP_TOKEN")
      emacs-agent-editor-allowed-origins
      '("http://127.0.0.1:3000")
      emacs-agent-journal-enabled t)
```

The optional journal is stored in the private daemon state directory. Source
content, before-images, authorization data, credentials, and bearer tokens are
removed before journal entries are serialized.

## Security model

Version 0.2 applies the following boundaries:

- Loopback-only listener.
- Optional bearer authentication, disabled by default.
- Optional exact origin allowlist.
- Content-length framing with bounded headers and bodies.
- Strict UTF-8 and JSON-RPC validation.
- Canonical workspace path checks, including symlink containment.
- Denial of repository metadata, common secret files, binary files, special
  files, and oversized documents.
- Serialized mutations with revision checks and external-change
  reconciliation.
- Request cancellation when a client disconnects.
- Private state directories and credential files.

With token authentication disabled, any local process able to connect to the
loopback port can call the endpoint. Enable token authentication when the
local machine or process boundary is not sufficiently trusted.

The server is intended for trusted local agents. It is not an
internet-facing service.

## Development

Run the package test suite:

```sh
emacs.d/site-lisp/agent-editor-mcp/run_tests.sh
```

After changing Emacs Lisp in this repository, also run:

```sh
./test-emacs-startup.sh
./lint-emacs-config.sh
```

The test suite covers transport framing and authentication, both protocol
profiles, revision and reconciliation behavior, Unicode/tab/CRLF positions,
exact replacement and strict patching, multi-buffer atomicity, diagnostics,
buffer-aware search, native semantic previews, lifecycle operations, change
sets, approval replay/expiry, credential redaction, rollback, journaling, and
the review UI.

See [design.md](design.md) for the complete architecture, protocol contracts,
security rationale, and deferred roadmap.
