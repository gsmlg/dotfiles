# Org Note for Emacs

Org Note is the Emacs 30.2 client for the Agent Note Org API. It browses
workspaces, documents, queues, agendas, item context, and events, and exposes
the supported work-item mutations. The default endpoint is
`https://agent-note.gsmlg.net/` and the service is accessed without
authentication.

Document buffers are remote-backed Org buffers. They do not visit a local
file and Org Note does not create a local mirror. Saving sends the complete
buffer to the service with its current document revision.

## Loading

The dotfiles configuration adds only the exact
`emacs.d/site-lisp/org-note/` directory to `load-path` and loads `org-note`
directly after the Org configuration. Requiring `org-note` defines commands,
modes, and local keymaps, but performs no request, starts no timer, and adds no
global binding.

For a standalone setup, add the package directory explicitly and require the
entry feature after Org:

```elisp
(add-to-list 'load-path "/path/to/dotfiles/emacs.d/site-lisp/org-note")
(require 'org)
(require 'org-note)
```

## Configuration

The package provides these user options:

- `org-note-endpoint`: service base URL. The default is
  `https://agent-note.gsmlg.net/`.
- `org-note-actor-id`: stable mutation actor identity. The default is
  `emacs:<user-login-name>@<system-name>`.
- `org-note-request-timeout`: synchronous request timeout in seconds. The
  default is 30.

For example:

```elisp
(setopt org-note-endpoint "https://agent-note.gsmlg.net/"
        org-note-actor-id "emacs:gao@workstation"
        org-note-request-timeout 30)
```

Org Note has no authentication option and sends no authentication credential.

## Workspace and document commands

- `org-note-workspaces`: browse workspaces.
- `org-note-documents`: browse a workspace's documents.
- `org-note-workspace-open`: open the selected workspace's documents.
- `org-note-document-list-open`: open the selected remote Org document.
- `org-note-document-open`: open a document by workspace and document ID.
- `org-note-document-save`: save the current remote document; normal
  `C-x C-s` uses the same path.
- `org-note-document-compare-latest`: compare a stale local buffer with the
  latest remote document using Ediff.
- `org-note-document-reload`: discard local content after confirmation and
  load the cached latest remote document.
- `org-note-document-rebase`: keep local content after confirmation while
  adopting the cached latest remote revision.
- `org-note-browser-refresh`: refresh the current list page.
- `org-note-browser-next-page`: fetch the next list page.
- `org-note-browser-previous-page`: return to the previous list page.

## Queue, agenda, context, and event commands

- `org-note-queue`: browse a queue view across selected workspaces.
- `org-note-agenda`: browse an agenda view across selected workspaces.
- `org-note-operational-open`: open the selected work item's context.
- `org-note-item-context`: display one work item's complete context as
  read-only Org.
- `org-note-item-context-refresh`: refresh the current item context.
- `org-note-events`: browse workspace events, optionally filtered by subject.

## Work-item actions

The following fourteen commands operate on the current queue, agenda, or item
context row:

- `org-note-item-claim`
- `org-note-item-heartbeat`
- `org-note-item-release`
- `org-note-item-report-progress`
- `org-note-item-submit-result`
- `org-note-item-transition`
- `org-note-item-retry`
- `org-note-item-request-review`
- `org-note-item-approve-review`
- `org-note-item-reject-review`
- `org-note-item-add-dependency`
- `org-note-item-remove-dependency`
- `org-note-item-link-note`
- `org-note-item-unlink-note`

`org-note-item-dispatch` prompts for one of these actions. Commands that need a
lease use the registered live lease rather than asking for a fencing token.

## Local keys

Org Note installs no global bindings. Its browser modes use buffer-local keys:

| Key | Action |
| --- | --- |
| `RET` | Open the selected workspace, document, or work item |
| `g` | Refresh the current view |
| `n` | Fetch the next page |
| `p` | Return to the previous page |
| `q` | Quit the view |
| `a` | Dispatch an action in operational item views |

## Safety model

- Fencing tokens remain internal to the in-memory lease registry. They are
  not entered through the minibuffer or exposed in user-facing errors.
- Mutations that discard data or change workflow state require confirmation
  where applicable.
- A stale document save caches the latest remote version for explicit
  compare, reload, or rebase commands. Org Note never resolves the conflict
  silently.
- User-initiated non-heartbeat mutations are not retried automatically. This
  avoids replaying an operation whose result may already have committed.
  Automatic lease heartbeats may retry retryable heartbeat failures.
- Org Note creates no persistent or on-disk service-data cache and no local
  mirror. UI rows, live leases, and stale-conflict snapshots are transient
  in-memory state.
- Requiring the package is inert. Automatic lease heartbeats begin only after
  an explicit successful claim or retry creates a live lease.

## Tests

Run the package suite:

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
```

Run the complete Emacs configuration suite:

```sh
./run-emacs-tests.sh
```
