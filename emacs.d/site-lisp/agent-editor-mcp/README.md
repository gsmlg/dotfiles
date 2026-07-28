# Emacs Agent Editor MCP

Emacs Agent Editor exposes a running Emacs instance as a local HTTP MCP
server for software-development agents. The agent and the human use the same
Emacs buffers, so unsaved edits, undo history, major modes, save hooks, and
external file changes have one authoritative owner.

Version 0.1 implements the transport, workspace, document, editing, search,
change-set, rollback, and review features described in [design.md](design.md).
Semantic language-service tools are intentionally deferred.

## Requirements

- Emacs 29.1 or newer
- A local workspace directory
- `ripgrep` for asynchronous workspace search, when available

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
  "token": "generated-bearer-token",
  "protocol_versions": ["2026-07-28", "2025-11-25"],
  "started_at": "2026-07-28T08:00:00Z"
}
```

Clients should read `endpoint` and `token` each time they connect. Revoking
the writer rotates the token and rewrites this file, invalidating existing
credentials and legacy sessions.

Do not copy the connection file into a repository or expose its token in logs.

### Discovery example

The modern protocol profile uses MCP method headers and per-request metadata:

```sh
connection="${XDG_STATE_HOME:-$HOME/.local/state}/emacs-agent-editor/workspace-name/connection.json"
endpoint="$(jq -r .endpoint "$connection")"
token="$(jq -r .token "$connection")"

curl "$endpoint" \
  -H "Authorization: Bearer $token" \
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

## Protocol profiles

The endpoint supports two wire profiles:

- `2026-07-28`: stateless requests with standard MCP HTTP headers and
  per-request client metadata.
- `2025-11-25`: compatibility profile using `initialize`,
  `notifications/initialized`, and `Mcp-Session-Id`.

Both profiles expose the same editor tool registry. Requests are authenticated
with `Authorization: Bearer <token>`. The v0.1 listener accepts only
`127.0.0.1`.

## Tools

| Tool | Purpose |
| --- | --- |
| `emacs_agent_workspace_info` | Return workspace identity, policy, health, and capabilities. |
| `emacs_agent_document_read` | Read authoritative buffer content with an opaque revision. |
| `emacs_agent_document_apply_edits` | Apply guarded, non-overlapping range edits as one undo unit. |
| `emacs_agent_document_create` | Create a text document inside the workspace. |
| `emacs_agent_workspace_files` | List workspace files with glob filters and pagination. |
| `emacs_agent_workspace_search` | Search text with `ripgrep` or the Emacs fallback. |
| `emacs_agent_document_move` | Move a revision-guarded document while preserving its buffer. |
| `emacs_agent_document_delete` | Delete a guarded document with rollback metadata. |
| `emacs_agent_workspace_checkpoint` | Save guarded buffers through normal Emacs save hooks. |
| `emacs_agent_workspace_sync` | Reconcile managed buffers with filesystem changes. |
| `emacs_agent_workspace_diff` | Return paginated unified diffs for change sets. |
| `emacs_agent_changeset_rollback` | Roll back a compatible change set after revision checks. |

Document revisions are opaque. A mutating client must first read a document,
then send the returned revision as `expected_revision`. If the buffer or file
changes in the meantime, the mutation fails and the client must reread.

Positions are one-based lines and zero-based character columns. Columns count
Emacs characters, not UTF-8 bytes.

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

## Human controls

Useful interactive commands:

| Command | Action |
| --- | --- |
| `M-x emacs-agent-editor-status` | Show server status and connection-file path. |
| `M-x emacs-agent-editor-start` | Start a server for a selected workspace. |
| `M-x emacs-agent-editor-stop` | Stop the server and remove connection metadata. |
| `M-x emacs-agent-editor-pause` | Pause mutations while retaining read access. |
| `M-x emacs-agent-editor-resume` | Resume mutations. |
| `M-x emacs-agent-editor-revoke-writer` | Pause mutations, rotate the token, and clear sessions. |
| `M-x emacs-agent-show-activity` | Show requests and pending approvals. |
| `M-x emacs-agent-show-changes` | Show recorded change sets. |

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

## Configuration

Core options can be set with `setq` or through
`M-x customize-group RET emacs-agent-editor RET`.

| Variable | Default | Meaning |
| --- | --- | --- |
| `emacs-agent-editor-host` | `"127.0.0.1"` | Listener address; v0.1 only accepts IPv4 loopback. |
| `emacs-agent-editor-port` | `0` | Listener port; zero selects an ephemeral port. |
| `emacs-agent-editor-endpoint` | `"/mcp"` | HTTP endpoint path. |
| `emacs-agent-editor-allowed-origins` | `nil` | Allowed values for a present `Origin` header. |
| `emacs-agent-editor-state-directory` | XDG state directory | Parent directory for private runtime state. |
| `emacs-agent-editor-access-mode` | `autonomous` | `read-only`, `review`, or `autonomous`. |
| `emacs-agent-editor-save-policy` | `immediate` | `immediate`, `manual`, or `explicit-per-call`. |
| `emacs-agent-editor-bearer-token` | `nil` | Fixed token, or `nil` to generate one at startup. |
| `emacs-agent-policy-maximum-document-bytes` | 4 MiB | Maximum managed document size. |
| `emacs-agent-journal-enabled` | `nil` | Enable the redacted JSONL activity journal. |

Example:

```elisp
(setq emacs-agent-editor-access-mode 'review
      emacs-agent-editor-save-policy 'manual
      emacs-agent-editor-allowed-origins
      '("http://127.0.0.1:3000")
      emacs-agent-journal-enabled t)
```

The optional journal is stored in the private daemon state directory. Source
content, before-images, authorization data, credentials, and bearer tokens are
removed before journal entries are serialized.

## Security model

Version 0.1 applies the following boundaries:

- Loopback-only listener.
- Bearer authentication on every request.
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
profiles, revision and reconciliation behavior, atomic editing, policy
boundaries, search pagination, lifecycle operations, change sets, rollback,
journaling, and the review UI.

See [design.md](design.md) for the complete architecture, protocol contracts,
security rationale, and deferred roadmap.
