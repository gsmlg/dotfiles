# Emacs Agent Editor

## A Pure Emacs Lisp, Buffer-First HTTP MCP Runtime for AI Software Development Agents

**Status:** Implemented specification v0.3
**Minimum Emacs:** GNU Emacs 30.2
**Transport:** local HTTP/1.1 on IPv4 loopback
**Endpoint:** `/mcp`
**Primary profile:** `2026-07-28`
**Compatibility profiles:** `2025-11-25`, `2025-06-18`

## 1. Product definition

Emacs Agent Editor exposes one running Emacs process as a long-lived editing
runtime. AI agents use explicit MCP capabilities while a human continues to
use the same buffers, windows, modes, undo history, and save hooks.

The deployment model is:

```text
One Emacs process
└── One Agent Editor MCP server
    └── One editor runtime
        ├── Zero or more registered projects
        ├── Zero or more directly addressed documents
        ├── One canonical document registry
        ├── One mutation queue
        ├── One change-set registry
        ├── One approval registry
        └── One activity stream
```

Starting the server does not require a directory. A project is optional
semantic context for file enumeration, search, aggregate diagnostics, symbol
queries, and relative paths. Direct absolute local files remain first-class.

There is no selected project in the protocol. Every project operation carries
an explicit `project_id`, and prior requests never change later routing.

## 2. Goals and non-goals

### 2.1 Goals

- Keep Emacs buffers authoritative for content and editor state.
- Make one canonical local absolute path identify one document.
- Support direct-file, one-project, multi-project, and cross-project flows.
- Preserve guarded edits, previews, atomicity, undo, diagnostics, semantic
  services, reconciliation, review, approvals, and change sets.
- Serialize every mutation through one runtime queue.
- Publish one private, discoverable loopback endpoint.
- Keep project registration independent from filesystem authorization.
- Preserve both supported MCP wire profiles with one tool registry.

### 2.2 Non-goals

- Internet-facing or multi-user service operation.
- Remote/TRAMP document targets.
- Process or filesystem sandboxing beyond the Emacs OS user's authority.
- Automatic installation of language servers, formatters, grammars, or
  `ripgrep`.
- A second project framework alongside built-in `project.el`.
- A second LSP client alongside Eglot.
- Persistent revisions, approvals, previews, or rollback history across
  process restarts.
- Executing arbitrary shell commands or language-server commands.

## 3. Architectural invariants

### 3.1 Buffers are authoritative

All document reads and writes use the canonical visiting buffer. Disk contents
are a checkpoint, not a second mutable document model.

This preserves:

- unsaved human edits;
- buffer-local major modes and language providers;
- coding systems and EOL style;
- undo history;
- save and formatting hooks;
- reconciliation with external file changes.

Direct disk I/O must never bypass an already visiting buffer.

### 3.2 Canonical path is identity

The runtime document registry is keyed by canonical local absolute path. These
two targets identify the same document:

```json
{"path": "/home/user/src/app/lib/example.ex"}
```

```json
{
  "project_id": "project_abc",
  "path": "lib/example.ex"
}
```

They share one buffer, revision, change history, and undo history. A document
may be inside several nested projects without belonging to any one of them.

### 3.3 Context is explicit

Project context can affect relative-path rendering and project-level semantic
operations only when a request supplies `project_id`. The selected buffer,
`default-directory`, last project call, or human UI context cannot affect
request routing.

`editor_context_get` is observational. Its result never selects context for a
future call.

### 3.4 Mutations are runtime-serialized

One runtime-level queue and writer lease serialize edits across all direct
files and projects. Cross-project transactions validate every target and
revision before changing any buffer.

### 3.5 Projects are not authorization containers

A project provides semantic context and a root for project operations.
Opening one does not expand filesystem authority. Every target, project root,
and language-provider result still passes the independent runtime policy.

### 3.6 Registration preserves the human UI

Project registration does not switch projects, select a buffer, change a
window or frame, mutate global `default-directory`, start another Emacs
process, or start another MCP server.

### 3.7 Guarded editing remains exact

The runtime preserves:

- opaque revisions;
- stale-revision rejection;
- one-based logical lines and zero-based Emacs-character columns;
- half-open ranges;
- rejection of overlaps and same-position inserts;
- atomic edit groups;
- one meaningful undo unit per successful document mutation;
- dry-run and frozen semantic previews;
- save/checkpoint policy;
- guarded change-set rollback;
- human review and approval controls;
- output-schema validation.

## 4. System architecture

```text
HTTP client
  │
  ▼
emacs-agent-http.el
  │ authentication, framing, limits, origin policy
  ▼
emacs-agent-protocol.el
  ├── emacs-agent-protocol-2026.el
  └── emacs-agent-protocol-2025.el
  │ JSON-RPC, sessions, schema validation
  ▼
emacs-agent-editor.el
  │ lifecycle, tool registry, handlers, public conversion
  ├── emacs-agent-runtime.el
  ├── emacs-agent-project.el
  ├── emacs-agent-policy.el
  ├── emacs-agent-document.el
  ├── emacs-agent-edit.el
  ├── emacs-agent-transform.el
  ├── emacs-agent-transaction.el
  ├── emacs-agent-changeset.el
  ├── emacs-agent-search.el
  ├── emacs-agent-diagnostics.el
  ├── emacs-agent-semantic.el
  ├── emacs-agent-journal.el
  └── emacs-agent-ui.el
        │
        ▼
  Emacs buffers, project.el, Imenu, Xref, Eglot, Flymake
```

Transport and protocol state never own application buffers. Tool handlers
receive protocol-neutral request context and operate through the editor
runtime.

## 5. Editor Runtime Model

`emacs-agent-runtime.el` owns process-lifetime application state:

```text
EditorRuntime
├── instance_id
├── server_epoch
├── started_at
├── access_mode
├── save_policy
├── writer_lease
├── document_registry
├── project_registry
├── project_root_index
├── changeset_registry
├── mutation_queue
├── mutation_active
├── state_directory
├── health_state
├── paused
├── activity_ring
├── approval_registry
└── filesystem_policy
```

One active server binds one singleton runtime. Starting creates and binds it;
stopping clears its registries and unbinds it without killing visiting
buffers.

The runtime, rather than a project, owns:

- access and save policy;
- the writer credential;
- mutation pause/resume and queue state;
- documents and revisions;
- change sets and rollback state;
- approvals and revision bindings;
- activity, health, and journal location.

`editor_info` works with zero projects and reports runtime identity, policies,
health, project count, managed document count, protocols, authentication,
supported tools, capabilities, and position semantics. It returns no project
root or selected project.

## 6. Project Registry Model

`emacs-agent-project.el` stores optional project records:

```text
Project
├── project_id
├── root
├── canonical_root
├── project_object
├── name
├── type
└── opened_at
```

### 6.1 Opening

`project_open` accepts one absolute local directory. It rejects empty,
relative, remote, missing, non-directory, or policy-denied roots. It then:

1. expands and canonicalizes the root;
2. asks `project.el` for a native project without changing human selection;
3. creates a directory adapter when no native project exists;
4. registers one opaque ID in root and ID indexes;
5. records redacted activity;
6. returns public metadata.

Opening the same canonical root is idempotent for the runtime lifetime.
Nested roots may coexist and receive distinct IDs.

### 6.2 Lookup and listing

Project operations require an explicit ID. Listing returns deterministic
public metadata without changing request state. Unknown or closed IDs fail
with `PROJECT_NOT_FOUND`.

### 6.3 Closing

Closing removes registry entries only. It does not:

- kill a visiting buffer;
- discard an unsaved edit;
- delete a file or change set;
- stop Eglot automatically;
- stop the server;
- invalidate direct absolute access.

## 7. Document Target and Canonical Identity

### 7.1 Accepted target forms

A direct target is absolute:

```json
{"path": "/home/user/src/app/lib/example.ex"}
```

A project-relative target supplies context:

```json
{"project_id": "project_abc", "path": "lib/example.ex"}
```

An absolute path may also carry project context:

```json
{
  "project_id": "project_abc",
  "path": "/home/user/src/app/lib/example.ex"
}
```

That form is valid only when the canonical path is inside the supplied root.

### 7.2 Resolution

Resolution follows this order:

1. require a non-empty `path`;
2. reject remote names;
3. resolve and validate a supplied project ID;
4. require project context for relative paths;
5. reject parent traversal in project-relative paths;
6. expand against the supplied project root when relative;
7. canonicalize an existing path;
8. canonicalize a create/move target through its nearest existing ancestor;
9. verify project containment when project context is present;
10. authorize the canonical path through runtime policy;
11. use canonical path as the document registry key.

Missing targets are never authorized from an uncanonicalized string.

### 7.3 Public path fields

Document-bearing results contain:

```text
path           canonical absolute path
project_id     supplied context or false
relative_path project-relative path or false
```

Project service results include canonical `path` plus `relative_path`.

### 7.4 Move targets

Source and destination are resolved independently:

```text
path
project_id?
new_path
new_project_id?
```

An absolute destination needs no project. A relative destination requires
`new_project_id`. Cross-project moves are allowed when policy permits them,
and the canonical visiting buffer continues to represent the moved file.

### 7.5 Multi-document targets

Every entry carries its own path, optional project ID, revision, and edits.
Direct files and several projects may appear in one atomic request.

## 8. Filesystem Policy

`emacs-agent-policy.el` authorizes canonical paths independently from project
registration.

### 8.1 Scope

`emacs-agent-policy-filesystem-scope` has two values:

- `unrestricted`: permit local files accessible to the Emacs OS user, subject
  to every denial and document check.
- `allowlist`: additionally require containment in a canonical configured root.

An empty allowlist permits no files. Registering a project outside the
allowlist fails.

### 8.2 Canonical authorization

Policy is applied after symlink resolution. Create and move destinations use
the nearest existing ancestor, preventing a symlinked parent from escaping a
configured root.

Configured denied paths are matched against canonical absolute paths and, when
project context is present, may also match the relative form. Runtime policy
remains authoritative.

### 8.3 Denials

The server rejects:

- `.git` metadata;
- `.env` and `.env.*`;
- configured credential basenames and credential-bearing extensions;
- configured denied paths;
- binary files;
- special files;
- oversized documents;
- remote paths.

Direct-file authority is intentionally broad in `unrestricted` mode. A
deployment needing a smaller boundary should use `allowlist`, bearer
authentication, and an OS account with matching filesystem permissions.

## 9. Server lifecycle and discovery

### 9.1 Start

The public package API is:

```elisp
(emacs-agent-editor-start &optional port)
```

It never accepts or prompts for a directory. The default port is `9876`. A
supplied port must be an integer from 0 through 65535; passing zero explicitly
requests an ephemeral port. The host is restricted to `127.0.0.1`.

Startup:

1. validates listener configuration;
2. creates and binds the runtime;
3. registers tools;
4. opens the optional journal;
5. starts HTTP;
6. atomically publishes private connection metadata;
7. records runtime activity.

Any startup failure unwinds the listener, journal, runtime, callbacks, token,
and connection metadata.

### 9.2 Stop

Stop closes HTTP and journal state; clears semantic providers, sessions, tools,
cursors, registries, and callbacks; removes connection metadata; and leaves
Emacs and visiting buffers alive.

### 9.3 Connection schema

The authoritative discovery file is:

```text
${XDG_STATE_HOME:-~/.local/state}/emacs-agent-editor/<daemon>/connection.json
```

Schema version 2 contains:

```json
{
  "schema_version": 2,
  "instance_id": "editor_abc",
  "daemon": "agent-editor",
  "pid": 12345,
  "endpoint": "http://127.0.0.1:9876/mcp",
  "token_authentication": true,
  "token": "present-only-when-enabled",
  "protocol_versions": ["2026-07-28", "2025-11-25", "2025-06-18"],
  "filesystem_scope": "unrestricted",
  "started_at": "2026-07-30T12:00:00Z"
}
```

Projects are dynamic and excluded. Clients use `project_list`. The parent
directory is private, the file is mode `0600`, writes are atomic, and enabled
tokens rotate on writer revocation.

## 10. Request execution

`emacs-agent-request.el` tracks protocol-neutral request metadata,
cancellation, timeouts, and cleanup. Compatibility sessions carry only wire
metadata and never own editor state.

Requests fall into:

- read-only operations, which observe runtime/buffer state;
- synchronous mutations, serialized by the runtime queue;
- asynchronous providers such as `ripgrep` or Eglot, which register
  cancellation callbacks and return through the original request.

Disconnect, explicit cancellation, and absolute timeout cancel registered
effects. Tool observer data is bounded and redacted before reaching the
journal or activity UI.

## 11. Editing, revisions, and undo

### 11.1 Revisions

A revision is an opaque digest of authoritative buffer state and relevant
external-file metadata. Clients cannot synthesize one. Every guarded mutation
checks it immediately before applying.

### 11.2 Coordinates

- Lines are one-based.
- Columns are zero-based Emacs characters.
- Ranges are half-open.
- Tabs count as one character.
- Columns are not display, UTF-8, or UTF-16 offsets.
- Logical newlines preserve the buffer's coding and EOL style.

### 11.3 Edit pipeline

Each mutation:

1. resolves and authorizes every target;
2. obtains canonical buffers;
3. reconciles external changes;
4. validates all expected revisions;
5. validates ranges, overlaps, and transformation preconditions;
6. computes frozen preview/diff data when applicable;
7. applies changes in descending position order;
8. creates meaningful undo boundaries;
9. records a runtime change set;
10. checkpoints according to save policy;
11. returns schema-validated revision and change metadata.

Failure before completion restores all affected buffer state. A stale target
in a multi-document request aborts the complete transaction.

### 11.4 Human edits

Human buffer edits immediately affect authoritative state and therefore
invalidate stale agent revisions and approval revision bindings. Agent edits
do not suppress ordinary Emacs undo or save behavior.

## 12. Checkpoint and reconciliation

Save policies are:

- `immediate`;
- `manual`;
- `explicit-per-call`.

Saving runs ordinary Emacs hooks. The runtime does not bypass a visiting
buffer with direct writes.

When the file changes externally:

- an unmodified buffer may reload and receive a new revision;
- a modified buffer reports a reconciliation conflict;
- explicit `editor_sync` exposes controlled reconciliation results.

## 13. Change sets and approvals

Every successful mutation can produce a runtime-scoped change set containing
canonical paths, before/final revisions, in-memory before-images, and a frozen
diff.

Rollback:

1. resolves and authorizes every recorded canonical target;
2. verifies each current revision matches the recorded final revision;
3. restores documents through the serialized runtime mutation queue;
4. records completion, or reports partial completion and degrades runtime
   health when a restore fails after earlier documents were restored.

Review-mode approvals are bound to:

- operation name;
- a canonical digest of redacted arguments;
- current document revisions;
- writer credential;
- expiry time.

Approval is one-use. Replay, changed arguments, expired state, changed
revisions, or credential rotation rejects it. Safe multi-document checkpoint
requests can derive a separately bound approval for a proper subset; other
operations remain all-or-nothing.

## 14. Public MCP tool surface

All wire names use `emacs_agent_`.

### 14.1 Editor runtime

```text
editor_info
editor_apply_edits
editor_checkpoint
editor_sync
editor_diff
editor_modified_documents
editor_context_get
```

### 14.2 Projects

```text
project_open
project_list
project_info
project_close
project_files
project_search
project_diagnostics
project_symbols
```

### 14.3 Documents

```text
document_read
document_status
document_apply_edits
document_replace
document_apply_patch
document_create
document_move
document_delete
document_diagnostics
document_symbols
```

### 14.4 Changes, semantics, formatting, and collaboration

```text
changeset_list
changeset_get
changeset_rollback
symbol_definition
symbol_references
symbol_rename
code_actions
format_document
format_range
approval_status
approval_cancel
```

Both protocol profiles advertise this same registry. Every public tool has an
input schema, and mutation/core result types have explicit output schemas
validated before returning.

## 15. Semantic service layer

Document diagnostics combine available Emacs providers without requiring a
project. Project diagnostics aggregate only buffers and files under the
explicit project root.

Imenu supplies document symbols. Xref/Eglot supply definitions, references,
renames, code actions, and project symbols when available. Missing providers
return `CAPABILITY_UNAVAILABLE`; text matching is never presented as semantic
analysis.

Any path or URI returned by an LSP `WorkspaceEdit` is resolved through runtime
filesystem policy before preview or apply. Rename and range-format previews
are frozen and revision-bound. Only pure edit code actions can be applied;
language-server commands are never executed.

Trusted formatting is configured by the Emacs user and operates against
authoritative buffer text. External tools belong to the user's project
environment and are never installed automatically.

## 16. Project files and search

Project file enumeration uses the registered project root and native
`project.el` object when available. The directory adapter provides the same
root semantics without requiring a VCS.

Search prefers asynchronous `ripgrep`. Its process starts in the project
location, results remain inside the supplied canonical root, and cancellation
terminates it. An Emacs fallback is available when `ripgrep` is absent.

Dirty visiting buffers shadow disk search results for the same canonical file.
Another registered project is included only when its files are physically
inside the requested root.

## 17. HTTP and protocol boundaries

The HTTP server accepts one content-length-framed request per connection and
closes afterward. It enforces:

- exact `/mcp` target;
- `POST` for protocol requests;
- bounded headers and body;
- no transfer encoding or pipelining;
- strict RFC 3629 UTF-8;
- JSON content type;
- exact optional origin allowlist;
- constant-shape bearer comparison;
- idle timeout.

The modern profile uses stateless MCP method headers and per-request client
metadata. The `2025-11-25` and `2025-06-18` compatibility profiles use
initialize/session sequencing. Negotiation selects only these three supported
protocol versions.

JSON-RPC invalid parameters use `-32602`. Internal output-contract violations
use `-32603` with `OUTPUT_SCHEMA_VIOLATION`, tool name, and schema path.
Public tool failures keep stable uppercase codes, structured details,
retryability, and compatibility metadata.

## 18. Human control interface

Runtime-scoped tabulated views expose:

- recent activity and pending approvals;
- change sets and frozen diffs;
- approvals, expiry, revision state, and safe partial acceptance.

Controls support approve, reject, cancel, pause, resume, revoke writer,
checkpoint, rollback, review, open file, show diff, and highlight hunks.

Internal state retains canonical absolute paths. Human-facing buffers may use
`abbreviate-file-name` for readability. Project registration and agent
requests do not steal focus.

## 19. Observability and private state

The in-memory activity ring records bounded operation metadata, duration,
status, and safe impact summaries. It excludes source contents, before-images,
tokens, credentials, and authorization payloads.

The optional JSONL journal lives in the runtime's private state directory and
uses the same redaction rules. Connection metadata and journal data never live
inside the tracked package tree.

Health is runtime-wide. Provider or request failures can be reported without
changing document identity or project selection.

## 20. Security model

The security boundary combines:

- IPv4 loopback-only transport;
- optional bearer authentication;
- optional exact origin allowlisting;
- private, atomic discovery metadata;
- canonical filesystem policy;
- secret, metadata, type, size, and remote-path denials;
- optimistic revisions and runtime serialization;
- access mode, pause/resume, and writer revocation;
- approval binding in review mode;
- input/output schema validation;
- cancellation and bounded resource limits.

The service intentionally grants powerful editor capabilities to a trusted
local agent. It is not an isolation boundary against the Emacs OS user. The
`unrestricted` filesystem scope should be enabled only when that authority is
intended.

## 21. Testing strategy

The ERT suite covers:

- HTTP framing, limits, UTF-8, origin, and authentication;
- both wire profiles and schema validation;
- project-free start, status, stop, and connection schema 2;
- runtime pause/resume, writer revocation, activity, and approvals;
- native and directory project registration, nesting, idempotency, and close;
- direct, project-relative, create, move, allowlist, and symlink-safe targets;
- canonical document identity across addressing forms;
- revisions, reconciliation, Unicode/tab/CRLF coordinates, atomic edits, and
  undo;
- direct, cross-project, and mixed multi-document transactions;
- exact replacement, strict patching, previews, formatting, and semantic
  operations;
- project files/search/diagnostics/symbol boundaries;
- change-set query, diff, review, rollback, and partial failure;
- journal redaction and human control keymaps.

The package test runner loads every `test/*-test.el` file. Repository startup,
module-load, byte-compilation, checkdoc, dependency scans, and generated-file
checks run separately.

## 22. Final architecture statement

Agent Editor MCP v0.3 is one project-optional Emacs editor runtime, not a
directory-bound service. Canonical buffers are the document authority;
projects are explicit semantic contexts; filesystem policy is independent;
and one runtime queue preserves guarded, atomic collaboration across direct
files and any number of projects.
