# Org Note Emacs Client Design

Status: Approved
Date: 2026-08-13

## Purpose

`org-note` is an Emacs 30.2 client for the Org orchestration API served by
Agent Note at `https://agent-note.gsmlg.net/`. It combines two workflows:

1. Browse and edit server-owned Org documents in remote-backed Emacs buffers.
2. Inspect queues, agendas, item context, audit history, and perform work-item
   lifecycle operations.

Standard Org text remains the human-readable source of truth. The plugin does
not create a synchronized local mirror or derive operational queues by scanning
documents.

## Goals

- Open server documents as normal editable Org buffers.
- Make `C-x C-s` save directly to Agent Note with optimistic concurrency.
- Reject stale saves without discarding either the local or remote version.
- Browse workspaces, documents, indexed queues, agendas, item context, and
  workspace events.
- Support claims, heartbeats, releases, progress, results, transitions,
  retries, reviews, dependencies, and Markdown note links.
- Keep lease fencing tokens in memory only and redact them from diagnostics.
- Load without network access, timers, hooks, or global keybindings.
- Use only built-in Emacs libraries at runtime.

## Non-goals for v1

- Local document mirrors, offline editing, background synchronization, or
  automatic merge.
- MCP transport; the client uses the documented REST API directly.
- Authentication; the configured endpoint is accessed without credentials.
- Autonomous scheduling or capability-aware dispatch.
- Workspace creation, policy editing, archival, import, or export.
- Structured item creation, follow-up creation, assignment, or scheduling.
- Persistent recovery of leases after Emacs exits.
- Changes to the existing local Org capture, agenda, TODO, Babel, or keybinding
  workflows.

The omitted structured mutations remain possible through server-side tools or,
where appropriate, revision-safe document editing. They can be added as later
package features without changing the transport or buffer model.

## Architecture

The package has four Lisp modules:

| Module | Responsibility |
| --- | --- |
| `org-note-client.el` | Configuration, URL construction, HTTP, JSON, mutation envelopes, operation IDs, and normalized errors. |
| `org-note-document.el` | Workspace/document API calls, remote Org buffers, saves, revisions, lease proofs, and conflict resolution. |
| `org-note-operation.el` | Queue, agenda, context, events, work-item mutations, leases, and heartbeat timers. |
| `org-note.el` | Public entrypoint, interactive commands, tabulated browsers, context rendering, and contextual action dispatch. |

`org-note.el` explicitly requires the other three modules. Requiring it is
side-effect-free: the first request occurs only after an interactive command is
invoked.

The package uses built-in `url`, `json`, `org`, and `tabulated-list`. It does
not depend on Agent Editor MCP or Elpaca packages.

## Configuration

The `org-note` customization group exposes:

- `org-note-endpoint`, defaulting to `https://agent-note.gsmlg.net/`.
- `org-note-actor-id`, defaulting to
  `emacs:<user-login-name>@<system-name>`.
- `org-note-request-timeout`, controlling synchronous interactive requests.

The endpoint is normalized so callers may configure it with or without a
trailing slash. No authorization header or credential lookup is performed.

## REST Client Contract

User-initiated requests are synchronous so an interactive command, especially
a buffer save, has a definitive success or failure result. Automatic
heartbeats use asynchronous requests so their timers do not block editing.

JSON objects are parsed as symbol-keyed alists. Requests use UTF-8 JSON with
`Content-Type: application/json` and `Accept: application/json`.

Every mutation includes:

```json
{
  "schema_version": 1,
  "actor_id": "emacs:login@host",
  "operation_id": "unique-per-attempt"
}
```

A fresh operation ID is generated for each intentional mutation attempt. A
request is not automatically retried after an ambiguous network failure,
because doing so with a new ID could duplicate an operation and doing so with
the old ID requires an explicit retry policy.

Server errors are normalized into an `org-note-error` condition containing the
HTTP status, server `code`, safe `message`, `details`, and `retryable` flag.
Transport errors and malformed responses use specific child conditions.
Fencing tokens and complete request bodies are never included in messages,
debug output, or conditions.

## Workspace and Document Workflow

`M-x org-note-workspaces` opens a `tabulated-list-mode` buffer containing the
workspace name, slug, revision, and operational counts. `RET` opens that
workspace's document list. `g` refreshes the current page, and page navigation
keeps an in-memory cursor stack so forward and backward navigation remain
possible with an opaque server cursor.

`M-x org-note-documents` can open the same document list directly after
workspace selection. Each row contains the path and revision. `RET` retrieves
the document and creates a uniquely named, non-file-visiting Org buffer.

Each document buffer stores these values buffer-locally:

- workspace ID;
- document ID;
- server path;
- expected revision;
- content hash;
- last fetched remote revision and source when resolving a conflict.

The document mode derives from `org-mode` and remaps `save-buffer` locally to
`org-note-document-save`, so `C-x C-s` keeps its normal meaning without a
global binding. The buffer remains modified until the server confirms the
save.

Saving sends:

```text
PUT /api/org/documents/{document_id}
```

with the mutation envelope, workspace ID, path, full Org source,
`expected_revision`, and `lease_proofs`. Lease proofs include only live leases
held by this Emacs process for items in the same document. On success, the
buffer records the returned document revision and becomes unmodified.

### Conflict Handling

A `409 stale_revision` never changes the local buffer or marks it saved. The
client fetches the latest remote document and records it separately. The user
then chooses one of three explicit commands:

- `org-note-document-compare-latest` opens built-in Ediff between the edited
  buffer and the fetched remote version.
- `org-note-document-reload` discards local edits only after confirmation and
  replaces them with the latest remote source and revision.
- `org-note-document-rebase` retains local text but, after confirmation,
  advances its expected revision to the inspected remote revision. A later
  `C-x C-s` performs the intentional overwrite.

No automatic merge or last-writer-wins fallback is permitted.

## Operational Views

`M-x org-note-queue` prompts for one or more workspaces and one queue view:

```text
ready, assigned, running, blocked, review, failed, expired_lease, completed
```

`M-x org-note-agenda` prompts for workspaces and one agenda view:

```text
scheduled, upcoming_deadline
```

Both commands use the indexed server endpoints, render paginated
`tabulated-list-mode` buffers, preserve opaque cursors, and never scan Org
documents. Rows show the item type, title, state, priority, assignee, schedule
or deadline where relevant, attempt state, readiness, and lease state.

`RET` opens `org-note-item-context`, a read-only Org buffer with sections for:

- workspace and document identity;
- the item, parent, and children;
- dependencies and readiness blockers;
- Markdown note links;
- attempts, results, and recovery information;
- current lease and operational classifications;
- origin and append-only history segments.

`M-x org-note-events` displays a paginated workspace event table with optional
subject filters.

## Work-item Actions

Actions are available as named `M-x` commands and through a contextual
`org-note-item-dispatch` command in queue, agenda, and context buffers. No
global keys are installed.

The v1 action set is:

- claim execution or review work;
- send an explicit heartbeat;
- release a claim, optionally selecting a target state;
- record a progress summary and optional JSON metadata;
- submit a result with summary, note references, artifacts, and metadata;
- transition to a workspace-defined state;
- retry eligible failed or expired work;
- request review;
- approve or reject review work;
- add or remove a dependency;
- add or remove a Markdown note link.

State names, event types, blocker values, and workspace policy data are treated
as server-defined strings. The client does not hard-code a workflow beyond the
queue and agenda view names defined by the API.

Every revision-bound mutation uses the document revision from the latest item
context known to that command. A conflict is reported without silently
refreshing and replaying the mutation.

## Lease Lifecycle

A successful claim or retry returns a lease ID, fencing token, kind, expiry,
item ID, document ID, and context. The package stores this data in a private
in-memory table keyed by workspace, item, and lease kind.

The raw fencing token is never written to:

- Org text or buffer-local file variables;
- Customize or the external local override;
- kill rings, histories, logs, or error messages;
- files under the repository, XDG data, cache, or state directories.

For each active lease, an automatic timer schedules an asynchronous heartbeat
before the returned expiry. Only one heartbeat may be outstanding per lease.
Successful responses update local revision data when supplied. Release,
result completion, review handoff, stale-lease errors, or retry exhaustion
cancel the timer and remove the token. Other heartbeat failures warn the user
without exposing request data and reschedule only while the lease has not
expired.

Emacs shutdown does not make a network request. In-memory leases are forgotten
and recover through normal server-side expiry semantics.

## Error and Cancellation Behavior

- Network, timeout, malformed JSON, and non-2xx responses leave buffers and
  local lease data unchanged unless the server response definitively proves a
  lease is stale.
- `409` domain errors display their server code and safe message.
- `429` errors expose the retryable flag but are not retried automatically.
- User cancellation before confirmation sends no mutation.
- Destructive local operations, including reload and revision rebase, require
  confirmation when the document buffer is modified.
- Killing a modified remote document buffer uses Emacs's normal modified-buffer
  confirmation.

## Dotfiles Integration

The repository adds only the exact `emacs.d/site-lisp/org-note/` directory to
`load-path`; it does not scan `site-lisp` recursively. `gsmlg-apps.el` requires
`org-note` immediately after `gsmlg-org` so Org is available first. Package
loading remains inert and cannot make startup depend on the network.

The integration updates the architecture and migration documentation, test
load paths, module-load coverage, the complete test runner, and any existing
repository directive that currently says Agent Editor MCP is the sole
vendored package on `load-path`. It does not touch `gsmlg-org.el`, the existing
Org tests, the keybinding contract, Elpaca recipes, or `elpaca-lock.el`.

## Package Layout

```text
emacs.d/site-lisp/org-note/
├── README.md
├── design.md
├── org-note.el
├── org-note-client.el
├── org-note-document.el
├── org-note-operation.el
├── run_tests.sh
└── test/
    ├── org-note-test.el
    ├── org-note-client-test.el
    ├── org-note-document-test.el
    └── org-note-operation-test.el
```

All Lisp files use lexical binding and the `org-note-` public prefix. Internal
state and helpers use double-hyphen names.

## Testing Strategy

Package-local ERT tests never contact the live endpoint. They stub the single
transport boundary and cover:

- inert package loading and public command availability;
- endpoint normalization, UTF-8 JSON, headers, parsing, timeouts, malformed
  responses, server errors, and secret redaction;
- exact methods, paths, queries, mutation envelopes, and JSON bodies;
- workspace/document pagination and cursor history;
- remote buffer metadata, modified state, successful saves, and lease proofs;
- stale-save preservation, reload, compare, and explicit revision rebase;
- queue, agenda, context, and event rendering;
- every work-item mutation and cancellation path;
- execution and review lease storage, heartbeat scheduling, timer cancellation,
  expiry, and fencing-token non-disclosure.

Repository integration tests assert the exact load path and explicit feature
order. Validation runs the package-local suite, relevant module/startup checks,
and then the complete `./run-emacs-tests.sh` suite. Generated `.elc` and `.eln`
files must not remain in the tracked configuration tree.

## Acceptance Criteria

The v1 is complete when:

1. Requiring `org-note` performs no request and starts no timer.
2. A user can browse workspaces and documents, open Org source, edit it, and
   save it with `C-x C-s`.
3. A stale save preserves local edits and requires an explicit reload or
   inspected rebase decision.
4. Queue and agenda commands use indexed endpoints and support pagination.
5. Item context exposes hierarchy, dependencies, history, attempts, notes,
   readiness, recovery, and lease information.
6. Every listed lifecycle, review, dependency, and note-link action sends the
   exact schema-versioned request and handles domain errors without replay.
7. Claims heartbeat automatically, and fencing tokens remain memory-only and
   absent from all user-visible diagnostics.
8. Existing local Org behavior and Agent Note Markdown behavior are unchanged.
9. Package-local and repository validation pass without generated runtime
   files in the tracked tree.
