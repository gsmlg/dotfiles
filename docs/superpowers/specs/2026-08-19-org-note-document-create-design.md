# Org Note document create design

Status: Approved for implementation planning
Date: 2026-08-19

## Problem

Creating an Org document in an Agent Note workspace is a basic Emacs client
capability. The vendored `org-note` package currently can list, open, and update
existing remote Org documents, but cannot create new ones. The package design
incorrectly listed document creation as an Emacs non-goal; that restriction
applied to the Web UI scope discussion, not the Emacs client.

## Goals

- Create a new Org document in a chosen workspace from Emacs.
- Support two creation sources:
  - Blank document (`source` is the empty string).
  - Template: copy `source` from an existing workspace document whose path
    starts with the literal prefix `templates/`.
- After a successful create, open the new remote Org buffer with the existing
  document editing and save model.
- Keep loading inert: no network until the user invokes create.

## Non-goals

- Dedicated server-side template catalog API.
- Structured work-item creation, follow-up creation, assignment, or scheduling.
- Workspace creation, policy editing, archival, import, or export.
- Local offline drafts that are not yet bound to a remote document id.
- Markdown note-link workflows (`org-note-item-link-note`); those are unrelated
  to Org document creation.

## Chosen approach

Interactive create command that reuses the existing document PUT endpoint.

Rejected alternatives:

- Local unbound draft buffers that create on first save (conflicts with the
  remote-backed revision model).
- A dedicated wizard major mode (too heavy for v1).

## User flow

### Entry points

- `M-x org-note-document-create`
- In `org-note-document-list-mode`, bind `c` to the same command.
- Register an autoload for `org-note-document-create` in `gsmlg-apps`, matching
  other Org Note entry commands.
- No new global keybinding.

### Steps

1. Resolve workspace id:
   - If invoked from a document list buffer that already has
     `org-note--browser-workspace-id`, use it.
   - Otherwise prompt for a workspace.
2. Prompt for source kind: `Blank` or `Template`.
3. If `Template`:
   - List documents in the workspace (follow `next_cursor` until exhausted).
   - Keep only paths with the literal prefix `templates/`.
   - Prompt the user to choose one template document.
   - `GET` that document and use its `source`.
4. Prompt for the new document path.
5. Generate a new document UUID on the client.
6. Create via PUT (see API contract below).
7. On success, open the new document through the existing open path (prefer a
   fresh `get-document` + `org-note-document-open` so buffer metadata is fully
   validated).
8. If create was started from a document list, refresh that list so the new row
   appears.

### Failure behavior

- If no template documents exist, tell the user and do not create.
- If path validation fails, abort before any network write.
- If the create request fails, leave no new document buffer behind and do not
  pretend the document list already contains the new row.

## Path rules

- New path must be a non-empty relative path string.
- Reject absolute paths and empty path segments.
- Reject new paths that start with `templates/`, so newly created notes are not
  placed in the template namespace by accident.
- Template identity is path-prefix only: `templates/` literal prefix,
  case-sensitive. No separate server template flag.

## API contract

Create uses the existing endpoint:

```text
PUT /api/org/documents/{document_id}
```

Request body includes the normal mutation envelope plus:

- `workspace_id`
- `path`
- `source` (`""` for blank, copied template source otherwise)
- `lease_proofs` as an empty JSON object when none apply

Create must **not** send a positive `expected_revision`. Omitting the field (or
sending JSON `null`) creates the document. Sending `0` is rejected by the
service.

Successful create returns `document_revisions` containing the new id at
revision `1`. Updates of existing documents continue to send a positive
`expected_revision` as today.

## Client changes

- `org-note-operation`: add a create helper, or extend put-document so a nil
  `expected-revision` omits that JSON field.
- `org-note-document` / `org-note.el`: implement `org-note-document-create`,
  template listing/filtering, path checks, and post-create open/refresh.
- Autoload the new command from `gsmlg-apps`.
- Update `emacs.d/site-lisp/org-note/design.md` and `README.md`:
  - Document creation is an Emacs client goal.
  - Describe blank vs `templates/` copy behavior.
  - Remove the incorrect “Emacs non-goal” wording for document creation.

## Testing

- Operation tests: create PUT body omits positive `expected_revision`; update
  path still includes it.
- UI/command tests:
  - Blank create opens a document buffer after success.
  - Template filtering keeps only `templates/` paths.
  - Reject creating under `templates/`.
  - API failure creates no leftover buffer.
- Run `./emacs.d/site-lisp/org-note/run_tests.sh`.

## Success criteria

- From a cold Emacs session, after Org Note autoloads are registered, the user
  can create a blank Org document and a template-based Org document in a
  workspace, then edit and save them with the existing remote document flow.
