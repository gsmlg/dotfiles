# Org Note document lifecycle design

Status: Approved for implementation
Date: 2026-08-25

## Problem

Agent Note now supports Org document archive (soft delete), restore, and rename.
The vendored `org-note` Emacs client can list, open, create, and save remote Org
documents, but cannot perform these lifecycle operations from Emacs.

## Goals

- Archive a document from the document list or an open Org Note buffer.
- Rename a document from the document list or an open Org Note buffer.
- Restore an archived document from the document list.
- Toggle whether the document list includes archived rows.
- Show archived status in the document list.
- Refresh originating list buffers after successful lifecycle mutations.
- Keep loading inert: no network until the user invokes a command.

## Non-goals

- Permanent delete or trash APIs.
- Workspace archival or policy editing.
- Batch archive, restore, or rename.
- Global keybindings.
- Restore from Org Note buffers in v1 (list is the primary restore entry point).

## Chosen approach

Layered implementation aligned with document create:

- `org-note-operation.el` — HTTP primitives for archive, restore, and rename.
- `org-note.el` — interactive commands, document-list keys, include-archived toggle.
- `org-note-document.el` — buffer metadata updates and buffer cleanup after archive.

Rejected alternatives:

- Putting all HTTP and UI in `org-note.el` (weak operation-layer contract).
- A separate lifecycle module (too heavy for three endpoints).

## User flow

### Entry points

| Command | Document list | Org Note buffer |
| --- | --- | --- |
| `org-note-document-archive` | `d` | `C-c C-a` |
| `org-note-document-rename` | `r` | `C-c C-r` |
| `org-note-document-restore` | `u` | — |
| `org-note-document-toggle-archived` | `A` | — |

Register autoloads for the three lifecycle commands in `gsmlg-apps.el`. Do not
autoload the list-only toggle helper.

### Context resolution

1. From `org-note-document-list-mode`: use the selected row and
   `org-note--browser-workspace-id`.
2. From `org-note-document-mode`: use buffer metadata
   (`org-note-document-workspace-id`, `org-note-document-id`, path, revision).
3. Otherwise: prompt for workspace and document identifiers when invoked via
   `M-x`.

### Archive

1. Confirm with `y-or-n-p`.
2. If invoked from a buffer with unsaved edits, confirm before proceeding; do
   not auto-save.
3. `POST /api/org/documents/{id}/archive` with `expected_revision`.
4. On success:
   - Kill the document buffer when archive was invoked from that buffer.
   - Refresh the originating document list when applicable.

### Rename

1. Prompt for the new path (default: current path).
2. Validate with the same rules as create (`org-note--new-document-path-p`).
3. `PATCH /api/org/documents/{id}/path` with `new_path` and
   `expected_revision`.
4. On success:
   - Update buffer path, revision, and display name when invoked from a buffer.
   - Refresh the originating document list when applicable.

### Restore

1. Only offered for rows whose `archived_at` is non-nil.
2. `POST /api/org/documents/{id}/restore` with `expected_revision`.
3. On success, refresh the document list.

### Include archived toggle

- Buffer-local `org-note--browser-include-archived`, default `nil`.
- Pass `include_archived` to `GET /api/org/workspaces/{id}/documents`.
- Include the flag in the document browser `context-key` so pagination resets
  when toggling.
- Add a `Status` column: `Active` or `Archived`.

## API contract

### Archive

```text
POST /api/org/documents/{document_id}/archive
```

Body:

```json
{
  "schema_version": 1,
  "actor_id": "...",
  "operation_id": "...",
  "workspace_id": "...",
  "expected_revision": 1
}
```

Do not send `lease_proofs`.

### Restore

```text
POST /api/org/documents/{document_id}/restore
```

Same body shape as archive.

### Rename

```text
PATCH /api/org/documents/{document_id}/path
```

Body:

```json
{
  "schema_version": 1,
  "actor_id": "...",
  "operation_id": "...",
  "workspace_id": "...",
  "expected_revision": 1,
  "new_path": "notes/renamed.org"
}
```

### List

Existing endpoint accepts `include_archived=true|false`. Rows may include
`archived_at` (null or Unix timestamp).

Successful mutation responses include `document_revisions` and `data` with
`path`, `document_id`, and `archived_at`.

## Path rules

Reuse create path validation:

- Non-empty relative path.
- Reject absolute paths, empty segments, and `templates/` prefix.

## Failure behavior

- Abort before network when path validation fails.
- Propagate API errors (including stale revision) without mutating local buffer
  metadata.
- Do not leave a renamed path in buffer metadata when the request fails.
- Do not kill the buffer when archive fails.

## Documentation

- Update `design.md` goals and Emacs-client non-goals.
- Update `README.md` with lifecycle commands and list keys.

## Testing

- Operation tests for archive, restore, and rename request shapes.
- UI tests for key bindings, path validation, list refresh, and buffer cleanup.
- Document tests for rename metadata updates after success.
