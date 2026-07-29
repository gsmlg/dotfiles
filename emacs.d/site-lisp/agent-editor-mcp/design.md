# Emacs Agent Editor

## A Pure Emacs Lisp, Buffer-First HTTP MCP Runtime for AI Software Development Agents

**Status:** Implemented specification v0.2
**Date:** 2026-07-29
**Package name:** `emacs-agent-editor`
**Primary implementation language:** Emacs Lisp

---

## 0. Version 0.2 Implementation Profile

The implementation now covers the complete guarded editing, workspace
transaction, diagnostics, semantic navigation/refactoring, formatting,
change-set, and collaboration scope in `emacs-prd.md`. Native semantic
features fail closed when their Eglot/Xref provider is unavailable.

The supported runtime and operational defaults are:

```text
minimum Emacs version: 29
access mode:           autonomous
save policy:           immediate
listen address:        127.0.0.1
listen port:           ephemeral unless configured
workspace:             daemon launch directory
authentication:        token authentication disabled
```

The dotfiles integration loads the package for all Emacs sessions and
automatically starts it only for daemon sessions. Runtime connection metadata
is written to a mode-0600 file in a mode-0700, per-daemon state directory.
This repository configures port 9876, producing the stable local endpoint
`http://127.0.0.1:9876/mcp`; the package-level default remains an ephemeral
port so other installations can choose their own binding.

Review-mode approvals use an approve-then-retry flow. A protected tool returns
an opaque approval request identifier. A human approves the exact normalized
operation in Emacs, and the agent retries with the one-use identifier before it
expires.

---

## 1. Executive Summary

Emacs Agent Editor turns a running Emacs daemon into an AI-native software development editor.

The AI agent connects directly to Emacs through an HTTP Model Context Protocol endpoint implemented inside the same Emacs process. A human connects through `emacsclient`. Both operate on the same buffers, undo history, project state, diagnostics, and language services.

The system is not an AI assistant embedded in a conventional editor. Emacs is the agent's editor runtime and replaces the agent's default source-code workspace tools:

- Read
- Edit
- Write
- Glob
- Grep
- Language navigation
- Diagnostics

The agent keeps its native execution environment for shell commands, builds, tests, Git, Nix, containers, and deployment operations.

The central state model is:

```text
Human ── emacsclient ──┐
                       │
                       ▼
                  Emacs daemon
                       ▲
                       │
Agent ── HTTP MCP ─────┘
                       │
                       ▼
                Emacs buffers
                       │
                  checkpoint
                       ▼
                   Filesystem
```

The buffer is the authoritative live document state. The filesystem is a persisted checkpoint.

There is no external MCP service, no `emacsclient --eval` bridge, and no second workspace state owner. HTTP transport, MCP protocol handling, editor state, policy enforcement, change tracking, and the human review interface are all implemented in Emacs Lisp inside one Emacs daemon.

---

## 2. Motivation

Emacs is unusually well suited to AI-controlled software editing because its user interface is already built on programmable commands rather than hard-coded graphical interactions.

A human invokes an Emacs command through a key binding. An AI agent can invoke the corresponding capability through an MCP tool:

```text
Human: key binding → Emacs command
Agent: MCP tool    → Emacs service function
```

The agent must not simulate keyboard or mouse input. Keyboard automation would depend on focus, keymaps, minibuffer state, active windows, transient maps, major modes, and user configuration. Direct command-level access is deterministic and independent of presentation state.

Emacs already provides the infrastructure required by a serious AI editor:

- persistent buffers
- unsaved document state
- markers and ranges
- atomic change groups
- undo history
- project discovery
- syntax and major modes
- Tree-sitter integration
- Xref navigation
- Eglot language-server integration
- Flymake diagnostics
- formatting and save hooks
- diff and merge interfaces
- human keyboard control

The project therefore treats Emacs as a programmable editor operating system rather than as a terminal containing an AI chat client.

---

## 3. Product Definition

### 3.1 Definition

Emacs Agent Editor is a pure Emacs Lisp package that exposes a running Emacs daemon as a buffer-first, revision-aware, HTTP MCP editor for AI software development agents.

### 3.2 Primary use case

An AI agent performs a software development task using Emacs for all source-code discovery, reading, modification, diagnostics, and semantic navigation. A human can attach to the same daemon at any time, observe the agent's changes, edit with the keyboard, inspect diffs, pause the agent, undo changes, and approve checkpoints.

### 3.3 Default deployment unit

The recommended deployment unit is:

```text
one agent task
→ one Git worktree
→ one named Emacs daemon
→ one writable MCP workspace
→ one writer agent
```

This provides clean isolation while still allowing a human to attach through `emacsclient`.

---

## 4. Goals

The system MUST:

1. Run entirely inside one Emacs daemon using Emacs Lisp.
2. Expose a standards-compatible HTTP MCP endpoint.
3. Treat Emacs buffers as the authoritative live source-code state.
4. Allow an agent to read unsaved human edits.
5. Apply agent edits directly to buffers without stealing human focus.
6. detect stale reads and reject conflicting edits.
7. Make each agent mutation atomic and undoable.
8. Preserve project, major-mode, Eglot, Flymake, and save-hook behavior.
9. Support source discovery, text search, document editing, file lifecycle operations, checkpointing, and synchronization.
10. Provide a keyboard-driven human control and review interface.
11. Enforce workspace boundaries and capability-based permissions.
12. Avoid arbitrary Elisp evaluation and arbitrary Emacs command execution.
13. Allow the agent host to disable native Read, Edit, and Write tools.
14. Keep protocol concerns independent from the editor core.

---

## 5. Non-Goals

The first releases will not be:

- a general remote-control API for all Emacs functions
- a generic Elisp evaluation service
- a replacement for the agent's shell or build environment
- a complete Git or CI orchestration system
- a multi-user cloud editing service
- a browser-based Emacs frontend
- a full AI-agent orchestrator
- a keypress or mouse-event automation system
- a filesystem MCP server that merely happens to notify Emacs

The project exposes deliberate editor capabilities, not Emacs itself as an unrestricted execution environment.

---

## 6. Core Architectural Decisions

### 6.1 One process owns the complete editor state

The Emacs daemon contains:

- the HTTP listener
- the MCP protocol implementation
- the tool registry
- workspace state
- document state
- buffers
- revisions
- change sets
- language services
- policy enforcement
- operation history
- the human review UI

No external backend mirrors or proxies buffer state.

### 6.2 Buffer-first document semantics

Every source document accessed by an agent is represented by an Emacs buffer.

The lookup order is:

```text
existing visiting buffer
→ use the existing buffer, including unsaved changes

no visiting buffer
→ open with find-file-noselect
→ operate on the resulting buffer
```

Direct file reads and writes must not bypass an existing visiting buffer.

### 6.3 The filesystem is a checkpoint

The system distinguishes three states:

```text
Applied       The change exists in an Emacs buffer.
Checkpointed  The current buffer state has been saved to disk.
Committed     The change has been recorded by Git.
```

Emacs Agent Editor manages the first two. Git remains outside the editor protocol.

### 6.4 Agent operations are semantic capabilities

The agent calls tools such as:

```text
emacs_agent_document_read
emacs_agent_document_apply_edits
emacs_agent_workspace_search
emacs_agent_document_diagnostics
```

It does not send keys, move the user's point, manipulate windows, or invoke arbitrary interactive commands.

### 6.5 Optimistic concurrency is mandatory

Every document read returns an opaque revision. Every mutation must include the revision on which it was based.

A mismatched revision produces a conflict and no edit is applied.

### 6.6 Functional core, effectful editor shell

The implementation should isolate pure transformations from Emacs side effects.

Pure functions handle:

- JSON and schema validation
- path normalization
- policy evaluation
- edit validation
- range ordering
- overlap detection
- revision comparison
- result construction
- protocol translation

Effectful functions handle:

- opening buffers
- reading buffer contents
- applying edits
- saving files
- invoking Xref, Eglot, Flymake, and project APIs
- starting subprocesses
- updating UI buffers

This separation makes the editor core testable without requiring every test to run through live network and buffer state.

### 6.7 Protocol state does not own application state

Workspace, document, and change-set state belong to Emacs Agent Editor, not to an MCP transport session.

Protocol adapters may maintain compatibility metadata, but the editor core receives an explicit request context and never depends on a particular MCP lifecycle version.

---

## 7. Protocol Baseline and Compatibility

The protocol layer must be versioned independently from the editor core.

The architecture targets the stateless MCP protocol shape introduced by the `2026-07-28` specification while retaining a compatibility adapter for `2025-11-25` clients during migration.

The editor core must not depend on:

- an initialization handshake
- a transport-owned session ID
- client roots
- a persistent SSE connection

Workspace identity is supplied by server configuration or by explicit application-level workspace handles.

Recommended protocol modules:

```text
emacs-agent-protocol.el
emacs-agent-protocol-2025.el
emacs-agent-protocol-2026.el
```

### 7.1 `2026-07-28` profile

The primary profile should support:

- `server/discover`
- stateless requests
- protocol and client metadata on each request
- `Mcp-Method` and `Mcp-Name` request headers
- deterministic tool listing
- structured tool output
- explicit application-level workspace handles when multiple workspaces are enabled

Every request includes protocol version and client metadata in `_meta`.
Every successful result includes `resultType: complete` and server identity in
result `_meta`. List results include `ttlMs` and `cacheScope`. HTTP requests
must carry `MCP-Protocol-Version` and `Mcp-Method`; `tools/call` also carries
`Mcp-Name`. Header and body values must agree.

Long-lived subscriptions are not required for the MVP. They can later expose diagnostics, buffer changes, and activity updates.

### 7.2 `2025-11-25` compatibility profile

The compatibility adapter should support:

- `initialize`
- `notifications/initialized`
- `MCP-Session-Id`
- POST request/response operation
- GET returning `405 Method Not Allowed` until SSE is implemented

Version 0.2 mints a compatibility session ID after initialization, validates it
on subsequent legacy requests, and returns `405 Method Not Allowed` for
unsupported GET and DELETE session operations.

Compatibility sessions must map to ordinary request contexts. They must not become the owner of workspace or buffer state.

### 7.3 Tool output

Every tool should define:

- an input schema
- an output schema
- structured content
- a text serialization fallback for older clients

Successful tool calls return both `structuredContent` and a JSON text fallback.
Correctable tool failures set `isError` and include stable structured error
data.

Correctable editor failures should be returned as tool execution errors with structured error data, allowing the agent to recover by rereading or changing its request.

---

## 8. System Architecture

```text
┌───────────────────────────────────────────────────────────────┐
│                         AI Agent Host                         │
│                                                               │
│  Model · MCP client · tool policy · native shell              │
│  Native Read/Edit/Write disabled                              │
└─────────────────────────────┬─────────────────────────────────┘
                              │ Streamable HTTP MCP
                              ▼
┌───────────────────────────────────────────────────────────────┐
│                         Emacs Daemon                          │
│                                                               │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │ HTTP Transport                                          │  │
│  │ listener · framing · limits · auth · Origin policy      │  │
│  └──────────────────────────┬──────────────────────────────┘  │
│                             ▼                                 │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │ MCP Protocol Adapters                                   │  │
│  │ discovery/init · tools · errors · cancellation          │  │
│  └──────────────────────────┬──────────────────────────────┘  │
│                             ▼                                 │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │ Request Scheduler and Policy Engine                     │  │
│  │ validation · authorization · writer queue · timeouts    │  │
│  └──────────────────────────┬──────────────────────────────┘  │
│                             ▼                                 │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │ Agent Editor Core                                       │  │
│  │ workspace · documents · revisions · edits · changesets  │  │
│  └──────────────────────────┬──────────────────────────────┘  │
│                             ▼                                 │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │ Native Emacs Services                                   │  │
│  │ buffers · undo · project.el · Xref · Eglot · Flymake    │  │
│  │ imenu · treesit · formatters · save hooks · diff        │  │
│  └──────────────────────────┬──────────────────────────────┘  │
│                             │                                 │
│  Human ── emacsclient ──────┘                                 │
└─────────────────────────────┬─────────────────────────────────┘
                              │ checkpoint
                              ▼
                         Git Worktree
```

---

## 9. Package Structure

```text
emacs-agent-editor.el
emacs-agent-http.el
emacs-agent-jsonrpc.el
emacs-agent-protocol.el
emacs-agent-protocol-2025.el
emacs-agent-protocol-2026.el
emacs-agent-schema.el
emacs-agent-request.el
emacs-agent-session.el
emacs-agent-workspace.el
emacs-agent-document.el
emacs-agent-edit.el
emacs-agent-search.el
emacs-agent-semantic.el
emacs-agent-diagnostics.el
emacs-agent-changeset.el
emacs-agent-policy.el
emacs-agent-journal.el
emacs-agent-ui.el
```

### 9.1 Transport modules

`emacs-agent-http.el` owns bytes and HTTP behavior only.

`emacs-agent-jsonrpc.el` owns JSON-RPC parsing, identifiers, result envelopes, and protocol-level errors.

### 9.2 Protocol modules

Protocol adapters own version-specific lifecycle, headers, capabilities, and result shape.

They convert requests into a version-neutral internal request:

```text
EditorRequest
├── request_id
├── protocol_version
├── client_info
├── operation
├── arguments
├── authorization_context
├── progress_context
└── cancellation_token
```

### 9.3 Editor modules

Editor modules know nothing about HTTP headers, SSE, or MCP lifecycle methods. They return typed internal results that protocol adapters serialize.

---

## 10. HTTP Transport Design

### 10.1 Listener

The HTTP listener is implemented in Emacs Lisp using Emacs network processes.

Default binding:

```text
host: 127.0.0.1
port: configurable
endpoint: /mcp
```

IPv6 loopback may be enabled explicitly. Binding to non-loopback interfaces is disabled by default.

### 10.2 MVP HTTP subset

The first implementation should deliberately support a narrow HTTP/1.1 subset:

- one request at a time per connection
- `Content-Length` request bodies
- JSON request bodies
- `application/json` responses
- explicit response content length
- no request pipelining
- no multipart input
- no general file serving
- no WebSocket support
- no chunked request bodies in the MVP
- no persistent subscription stream in the MVP

Unsupported transfer encodings must be rejected explicitly rather than parsed partially.

The transport is an internal MCP transport, not a general web server.

### 10.3 Network process filter

The process filter must only:

1. collect raw bytes
2. detect complete headers and body frames
3. enforce size limits
4. create an immutable request object
5. enqueue the request for later dispatch

It must not perform buffer edits, project search, LSP requests, or heavy JSON-schema work directly inside the process filter.

Dispatch should run through the normal Emacs event loop after the filter returns.

### 10.4 Limits

Recommended defaults:

```text
maximum header size:       32 KiB
maximum request body:       1 MiB
maximum tool result text: 512 KiB
request idle timeout:       15 s
normal tool timeout:        30 s
absolute tool timeout:     120 s
```

Large documents and search results must be paginated rather than bypassing transport limits.

### 10.5 Security headers and authentication

The transport must:

- validate `Origin` when present
- reject unapproved origins
- bind to loopback by default
- disable bearer-token authentication by default
- allow users to enable bearer-token authentication
- use a configured token or generate a random token when authentication is enabled
- compare enabled tokens without leaking partial matches
- store enabled tokens in a user-only state file
- reject unsupported content types
- reject invalid protocol-version headers
- avoid including secrets in logs

With token authentication disabled, any local process that can reach the
loopback port can access the endpoint. Remote exposure must occur only through
an explicit trusted tunnel or authenticated reverse proxy. Direct public
binding is outside the supported security model.

---

## 11. Request Execution Model

Emacs has one primary event loop. Buffer mutations must therefore be short, deterministic, and serialized.

### 11.1 Request classes

Requests are classified as:

- read-only
- mutating
- destructive
- asynchronous

### 11.2 Mutation queue

Each workspace has one mutation queue and one writer lease.

The queue serializes:

- edits
- creates
- moves
- deletes
- checkpoints
- rollbacks
- semantic renames
- format operations that modify buffers

Human editing is never blocked by the lease. Human changes invalidate revisions and cause stale agent mutations to fail.

### 11.3 Asynchronous operations

Long operations should use asynchronous Emacs facilities:

- `make-process` for ripgrep and formatters
- Eglot's asynchronous JSON-RPC requests
- timers for chunked Elisp work
- pending request continuations

The request registry tracks:

```text
pending
completed
failed
cancelled
```

Cancellation should stop subprocesses and prevent pending continuations from producing a result after cancellation.

### 11.4 UI responsiveness

No synchronous tool handler should monopolize the event loop for a long period. Operations that cannot meet the synchronous time budget must become asynchronous or be split into bounded chunks.

---

## 12. Workspace Model

```text
Workspace
├── workspace_id
├── root
├── canonical_root
├── project
├── server_epoch
├── access_mode
├── save_policy
├── writer_lease
├── document_registry
├── changeset_registry
├── mutation_queue
├── denied_paths
├── allowed_paths
├── state_directory
└── health_state
```

### 12.1 Workspace binding

For the MVP, one Emacs daemon is bound to one configured workspace root.

Later releases may host multiple pre-approved workspaces. Multi-workspace operation must use explicit workspace identifiers or opaque workspace handles. The agent cannot submit an arbitrary filesystem root and gain access to it.

### 12.2 Recommended isolation

Concurrent agent tasks should normally use separate worktrees and separate named Emacs daemons.

Shared-daemon, multi-agent writing is not a first-release requirement.

---

## 13. Document Model

```text
Document
├── relative_path
├── canonical_path
├── buffer
├── major_mode
├── revision
├── content_hash
├── buffer_tick
├── disk_fingerprint
├── modified
├── externally_modified
├── coding_system
├── eol_style
├── last_changeset_id
└── diagnostics_revision
```

### 13.1 Canonical document ownership

A path maps to one canonical visiting buffer in a workspace.

All tools resolve the document through the document registry. A tool cannot independently read the same path from disk while a buffer exists.

### 13.2 Revision format

The revision is opaque to clients. Its logical inputs are:

```text
server epoch
buffer modification tick
content hash
```

A conceptual form is:

```text
rev:<epoch>:<tick>:<sha256>
```

The exact representation is private and may change.

Hashes should be cached by buffer modification tick.

### 13.3 Coordinate system

All public document ranges use:

```text
line:   1-based
column: 0-based Emacs character offset
end:    exclusive
```

Columns are not UTF-8 byte offsets, UTF-16 code units, or display-cell widths. A tab is one character. Ranges refer to the widened full buffer.

---

## 14. Editing Semantics

### 14.1 Core edit operation

The canonical mutation primitive is:

```text
apply_edits(path, expected_revision, edits)
```

Each edit contains:

```text
start
end
new_text
expected_text?   optional additional guard
```

### 14.2 Edit pipeline

A document edit must execute the following pipeline:

```text
resolve workspace-relative path
→ enforce path policy
→ obtain canonical buffer
→ reconcile external file state
→ compare expected revision
→ validate every position
→ reject overlapping ranges
→ validate expected text guards
→ convert ranges to stable positions
→ create undo boundary
→ apply edits from the end of the buffer toward the beginning
→ run normal modification hooks
→ optionally checkpoint
→ calculate final revision
→ generate final diff
→ record change set
→ return structured result
```

No edit may be partially applied.

### 14.3 Atomicity and undo

Single-buffer changes use Emacs atomic change grouping.

Each successful tool call must create one human-meaningful undo unit. Failure restores the exact pre-call buffer state.

### 14.4 Human UI preservation

Agent operations must preserve:

- selected window
- current buffer
- point
- mark
- window start
- narrowing
- active region

The agent has logical positions, not a human cursor.

### 14.5 Read-only and special buffers

The editor respects:

- `buffer-read-only`
- file permissions
- mode-specific restrictions
- remote-file policy
- binary-file policy

The agent may not silently override them.

---

## 15. Checkpoint and Reconciliation

### 15.1 Save policies

The workspace supports three policies.

#### Immediate

Every successful mutation is saved before returning.

Recommended for dedicated agent worktrees because shell tests immediately observe the latest code.

#### Manual

Mutations remain in buffers until a human or agent explicitly requests a checkpoint.

Recommended when the human and agent actively share the same workspace.

#### Explicit-per-call

Each mutation request chooses whether to checkpoint, subject to workspace policy.

### 15.2 Save hooks

Checkpointing uses normal Emacs save behavior and respects:

- `before-save-hook`
- format-on-save
- whitespace cleanup
- mode-specific hooks
- file coding system
- end-of-line style

The final returned revision and diff must describe the content after save hooks have completed.

A save hook that requires minibuffer interaction causes a structured `interactive_prompt_required` failure instead of leaving an invisible prompt active.

### 15.3 External changes

Shell commands, generators, Git operations, or formatters may modify files outside Emacs.

Reconciliation rules are:

```text
buffer unmodified + disk changed
→ reload safely
→ issue a new revision

buffer modified + disk changed
→ report external_change_conflict
→ never auto-overwrite either side
```

The `emacs_agent_workspace_sync` tool performs explicit reconciliation after external commands. File notifications may later mark documents stale automatically, but explicit synchronization remains part of the public API.

---

## 16. Change-Set Model

```text
ChangeSet
├── changeset_id
├── workspace_id
├── request_id
├── agent_identity
├── created_at
├── status
├── operations
├── touched_documents
├── base_revisions
├── final_revisions
├── before_snapshots
├── checkpoint_state
├── diagnostics_before
├── diagnostics_after
└── unified_diff
```

States:

```text
applied
checkpointed
reviewed
rolled_back
conflicted
```

### 16.1 Transaction boundary

One mutating tool call creates one change set.

Long-lived multi-call transactions are deferred because agents can abandon them or forget to commit them.

### 16.2 Rollback

Rollback is permitted only when every affected document still has the final revision recorded by the change set.

If a human or another operation has subsequently modified a document, rollback returns `rollback_conflict` and does not overwrite newer work.

### 16.3 Persistence

Full before-images remain in memory for active change sets. They are
intentionally daemon-scoped: restarting Emacs invalidates revisions, cursors,
semantic previews, approvals, and rollback history. The optional persistent
journal stores redacted metadata and diffs, never bearer credentials or full
before-images.

---

## 17. MCP Tool Surface

Tool names use a stable `emacs_agent_` prefix to avoid collisions with native agent tools.

### 17.1 Implemented tool groups

| Group | Tools |
|---|---|
| Workspace | `workspace_info`, `workspace_files`, `workspace_search`, `workspace_apply_edits`, `workspace_checkpoint`, `workspace_sync`, `workspace_diff`, `workspace_modified_documents`, `workspace_diagnostics`, `workspace_symbols` |
| Documents | `document_read`, `document_status`, `document_apply_edits`, `document_replace`, `document_apply_patch`, `document_create`, `document_move`, `document_delete`, `document_diagnostics`, `document_symbols` |
| Change sets | `changeset_list`, `changeset_get`, `changeset_rollback` |
| Semantics | `symbol_definition`, `symbol_references`, `symbol_rename`, `code_actions` |
| Formatting | `format_document`, `format_range` |
| Collaboration | `editor_context_get`, `approval_status`, `approval_cancel` |

All wire names use the `emacs_agent_` prefix.

### 17.2 Position and write-result contract

Public positions use one-based lines and zero-based Emacs-character columns.
Ranges are half-open, tabs count as one character, and CRLF files use logical
buffer lines while retaining their coding-system EOL style. Every edit in a
request is relative to one `expected_revision`. The server validates every
range before mutation, rejects overlaps and same-position inserts, and applies
valid edits in descending order.

Core write results include:

```text
old_revision
new_revision
changeset_id
applied
checkpointed
modified
diff
truncated
```

Workspace transaction edit items have an explicit exact-replacement schema:
`old_text`, `new_text`, `replace_all?`, and `expected_occurrences?`.
Transactions are always atomic. The optional `atomic` compatibility field may
only be true; false is rejected instead of being silently ignored.

`modified` describes whether authoritative content changed, or would change
for a dry-run; buffer dirty state belongs to `document_status`. Dry-run leaves
the revision unchanged. Multi-document writes provide exact per-document
revision pairs and use JSON false when a singular top-level revision is not
applicable. P0/P1 output schemas declare their required fields and collection
types, and the protocol validates every live structured result before
serialization. Input validation failures use JSON-RPC `-32602`. Output
validation failures are server contract errors using `-32603` with
`OUTPUT_SCHEMA_VIOLATION`, the tool name, and schema path.

Public errors expose an uppercase stable code, message, retryability, nested
details, and the legacy internal code for compatibility.

### 17.3 Semantic safety

Imenu supplies document symbols, Xref supplies navigation and workspace symbol
queries, and Eglot supplies rename, code actions, and range formatting.
Semantic rename and range formatting require a frozen preview identifier
before apply. Only pure workspace-edit code actions can be applied; advertised
language-server commands are classified but never executed. Missing native
providers return `CAPABILITY_UNAVAILABLE` rather than falling back to textual
search.

---

## 18. Key Tool Contracts

### 18.1 `emacs_agent_document_read`

Input:

```text
path
start_line?   optional
end_line?     optional
max_chars?    optional
```

Output:

```text
path
revision
modified
checkpointed
coding_system
eol_style
start_line
end_line
total_lines
truncated
content
```

Rules:

- content comes from the buffer, not directly from disk
- content does not contain prefixed line numbers
- large reads are paginated
- binary files are rejected by default
- returned revision must be used by the next mutation

Recommended defaults:

```text
maximum read: 256 KiB
maximum lines: 2,000
```

### 18.2 `emacs_agent_document_apply_edits`

Input:

```text
path
expected_revision
edits[]
checkpoint?   optional and policy-constrained
```

Output:

```text
path
changeset_id
old_revision
new_revision
applied
checkpointed
modified
diff
truncated
```

A stale revision returns a structured conflict rather than applying a best-effort merge.

### 18.3 `emacs_agent_workspace_search`

Input:

```text
query
regexp?          default false
include_globs?   optional
exclude_globs?   optional
max_results?     bounded
cursor?          optional
```

Output results contain:

```text
path
line
column
match
context
source
modified
revision
```

Dirty visiting buffers are searched first and shadow disk results for the same
path. The preferred disk provider is a controlled asynchronous ripgrep
subprocess using an argument vector rather than a shell command string. A pure
Emacs fallback may search `project-files`.

### 18.4 `emacs_agent_document_diagnostics`

Output:

```text
path
document_revision
diagnostics_revision
providers
pending
stale
diagnostics[]
```

Diagnostics must be associated with a document revision. Results from an older analysis pass are marked stale rather than presented as current.

---

## 19. Semantic Service Layer

The system reuses Emacs abstractions rather than implementing a second language-server client.

Provider order:

```text
Xref abstraction
→ Eglot-backed Xref when available
→ tags or project search fallback
```

Other services:

```text
outline      → imenu, then Tree-sitter fallback
errors       → Flymake
hover        → Eglot/ElDoc
format       → configured formatter or mode command
code actions → Eglot where supported
```

Tool responses identify the provider and document revision used to produce the result.

---

## 20. Security and Policy Model

### 20.1 Capability-oriented API

The server must never expose tools equivalent to:

```text
eval_elisp
execute_command
call_interactively
execute_keyboard_macro
shell_command
```

Only reviewed capabilities are registered.

### 20.2 Path boundary

Every input path is workspace-relative.

Resolution must:

1. reject absolute paths unless an internal trusted caller uses them
2. reject `..` escapes
3. resolve symlinks through the nearest existing parent
4. verify the final canonical path remains below the canonical workspace root
5. apply allow and deny patterns

Default denied targets include:

```text
.git/**
device files
FIFO and socket files
remote TRAMP paths
binary files
files above the configured size limit
root-escaping symlinks
```

Sensitive-file patterns are configurable and should include conservative defaults for common credential files.

### 20.3 Access modes

| Mode | Read | Buffer Edit | Move/Delete | Checkpoint |
|---|---:|---:|---:|---:|
| `read-only` | Yes | No | No | No |
| `review` | Yes | Yes | Approval required | Approval required |
| `autonomous` | Yes | Yes | Policy-controlled | Yes |

### 20.4 Writer lease

A workspace has one writer agent by default.

Read-only clients may coexist. Human editing is always permitted and is protected by revision conflict detection.

### 20.5 Agent-host enforcement

MCP instructions alone cannot guarantee exclusive use of Emacs tools.

To truly replace native Read/Edit/Write, the agent host must:

- omit or disable native source editing tools
- instruct the model to use Emacs for all source reads and writes
- retain shell access only for execution
- optionally sandbox the source tree so the agent process cannot write it directly

The strongest architecture makes Emacs the only process with source-write capability.

---

## 21. Human Control Interface

The human interface remains fully keyboard-driven.

### 21.1 Activity buffer

`*Emacs Agent Activity*` displays:

- active agent
- workspace
- current operation
- current document
- request duration
- writer lease
- save policy
- pending diagnostics
- recent conflicts
- server health

### 21.2 Changes buffer

`*Emacs Agent Changes*` lists change sets with:

- status
- affected files
- insertions and deletions
- checkpoint state
- diagnostics delta

Commands provide:

- open changed file
- view diff
- checkpoint
- rollback
- mark reviewed
- move to next or previous change set

### 21.3 Approval buffer

The approval buffer supports approve, reject, cancel, and document-granularity
partial acceptance for multi-document checkpoint requests. Partial acceptance
creates a new approved child request bound to the exact selected proper subset,
original revision guards, credential, and remaining TTL. The parent becomes
non-consumable. Delete, move, rollback, formatting, and other operations that
cannot be safely split remain all-or-nothing.

### 21.4 Emergency controls

The package provides commands equivalent to:

```text
pause mutations
resume mutations
revoke writer lease
stop HTTP server
stop editor service
```

Pausing prevents new mutations but may allow read-only tools to continue.

### 21.5 Focus preservation

Agent operations must not select windows, pop buffers, move point, or interrupt minibuffer use unless the human explicitly opens the activity or review UI.

---

## 22. Error Model

Protocol errors are reserved for invalid JSON-RPC, unknown methods, unsupported protocol versions, and malformed MCP envelopes.

An output schema violation is an internal server contract error, never an
invalid-arguments response.

Recoverable editor failures are tool execution errors with stable codes and structured fields.

Recommended codes:

```text
workspace_not_bound
workspace_paused
writer_lease_conflict

path_outside_root
path_denied
document_not_found
document_already_exists
unsupported_document_type
document_too_large

revision_conflict
external_change_conflict
invalid_position
overlapping_edits
expected_text_mismatch

save_failed
interactive_prompt_required
rollback_conflict

semantic_provider_unavailable
diagnostics_timeout
operation_cancelled
```

A revision conflict includes:

```text
path
expected_revision
current_revision
modified_by
requires_reread: true
```

It does not automatically include the complete new file contents. The agent must reread the required range.

---

## 23. Observability and Journal

The server maintains:

- an in-memory request ring
- an in-memory change-set registry
- an optional JSON Lines audit journal
- an Emacs activity buffer
- per-tool timing and error counters

Journal entries contain:

```text
timestamp
request_id
agent identity
workspace
tool
paths
result status
duration
changeset_id
revision transition
```

Secrets, bearer tokens, full source contents, and raw authorization headers are never logged.

---

## 24. Performance Boundaries

Recommended first-release limits:

```text
maximum document size:       4 MiB
maximum read result:       256 KiB
maximum search results:      100 default, 1,000 hard maximum
maximum edits per call:      200
maximum changed text:        1 MiB per call
maximum open idle buffers: configurable LRU
```

Optimization rules:

- cache hashes by buffer tick
- open buffers lazily
- retain buffers with unsaved changes or active change sets
- close clean agent-only buffers through LRU policy
- paginate file lists and search results
- debounce diagnostics
- use markers only for the duration of an edit operation
- avoid returning full diffs unless explicitly requested

---

## 25. Testing Strategy

### 25.1 Pure ERT tests

Test:

- path normalization
- symlink escape prevention
- schema validation
- range normalization
- overlap detection
- revision comparison
- policy evaluation
- error serialization
- protocol-version translation

### 25.2 Buffer integration tests

Test:

- unsaved buffer reads
- Unicode positions
- tabs and combining characters
- CRLF preservation
- encoding preservation
- atomic edit rollback
- single-step undo
- save-hook mutations
- point and window preservation
- read-only buffers
- external modification conflicts

### 25.3 HTTP and protocol tests

Each test starts a temporary listener and verifies:

- request framing
- size limits
- authentication
- Origin validation
- protocol-version dispatch
- tool listing
- structured results
- cancellation
- timeout behavior
- malformed requests
- connection cleanup

### 25.4 End-to-end daemon tests

Each scenario starts:

```text
temporary Git repository
named Emacs daemon
temporary workspace
MCP client fixture
optional emacsclient frame
```

It verifies that:

- the agent reads an unsaved human edit
- agent changes appear immediately in the human frame
- human edits invalidate stale agent revisions
- checkpointed changes are visible to shell tools
- rollback updates both buffer and filesystem safely
- create, move, and delete preserve buffer registry correctness

### 25.5 Fuzz and race tests

Fuzz:

- malformed HTTP headers
- invalid JSON
- path traversal
- symlink loops
- oversized bodies
- invalid Unicode positions
- huge lines
- overlapping edits
- cancellation races
- external file changes during edits

---

## 26. Implementation Plan

### Phase 0: Architectural Skeleton

Status: implemented.

Deliver:

- package layout
- functional core types
- workspace and document registries
- policy engine
- revision model
- protocol-neutral request/result types
- ERT foundation

### Phase 1: HTTP MCP Editing Loop

Status: implemented.

Deliver:

- local HTTP listener
- optional bearer authentication and Origin validation
- protocol adapter framework
- `server/discover` or compatibility initialization
- `tools/list`
- `emacs_agent_workspace_info`
- `emacs_agent_document_read`
- `emacs_agent_document_apply_edits`
- `emacs_agent_workspace_checkpoint`
- revision conflict handling
- one-step undo

Acceptance outcome:

```text
Agent reads buffer
→ agent submits guarded edit
→ human sees edit immediately
→ edit checkpoints
→ shell sees updated file
```

### Phase 2: Complete Workspace Tool Replacement

Status: implemented.

Deliver:

- project file listing
- asynchronous text search
- document creation
- move and delete
- workspace synchronization
- change-set diff
- rollback
- activity journal
- human activity and changes buffers

This phase replaces Read, Edit, Write, Glob, and Grep for normal source development.

### Phase 3: Semantic Editor Services

Status: implemented.

Deliver:

- document and workspace symbols
- definitions
- references
- revision-bound document and workspace diagnostics
- trusted document and Eglot range formatting
- preview-gated semantic rename
- safely classified code actions
- diagnostics revision tracking

### Phase 4: Advanced Protocol and Collaboration

Status: implemented for the bounded PRD collaboration scope.

Deliver:

- both MCP protocol profiles
- multi-document semantic transactions
- editor context with sensitive-buffer redaction
- approval status, cancellation, TTL, and revision invalidation
- read-only change-set diff buffers, source navigation, and overlays

Long-lived subscriptions, cross-daemon persistent rollback state, and
multi-workspace daemons remain outside the current package contract.

---

## 27. MVP Acceptance Criteria

The implemented release is complete only when all of the following are true:

1. The complete server runs inside Emacs with no external MCP backend.
2. An agent can connect through HTTP MCP.
3. Native source Read/Edit/Write tools can be removed from the agent host.
4. The agent can read a buffer containing unsaved human changes.
5. The agent can apply guarded range edits to that buffer.
6. A stale revision is rejected without partial modification.
7. Agent editing does not move the human's point or change the selected window.
8. One agent tool call creates one undo unit.
9. A failed mutation restores the exact original buffer state.
10. A checkpoint runs normal save hooks and returns the final revision.
11. Checkpointed content is immediately visible to shell tests.
12. Paths cannot escape the configured workspace through `..` or symlinks.
13. The server exposes no arbitrary Elisp or shell execution tool.
14. Every mutation is recorded as a change set.
15. The human can pause mutations, inspect a diff, and roll back an unchanged change set using only the keyboard.

---

## 28. Final Architecture Statement

Emacs Agent Editor is not an MCP wrapper around filesystem operations.

It is a stateful semantic editor exposed through a narrow agent protocol:

```text
AI Agent
    │
    │ HTTP MCP capabilities
    ▼
Emacs daemon
    │
    │ buffer state, revisions, undo, language intelligence
    ▼
Filesystem checkpoint
```

The human and the agent are peers operating on the same editor state through different interfaces:

```text
Human → keyboard and Emacs commands
Agent → MCP tools and Emacs service functions
```

The defining rule is:

> The AI does not operate Emacs like a human. It invokes the same underlying editor capabilities that human keyboard commands invoke.

This makes Emacs the agent's actual development editor rather than merely a place to display an agent terminal.
