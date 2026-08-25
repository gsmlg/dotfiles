# Org Note operational workspace preferences design

Status: Approved for implementation
Date: 2026-08-25

## Problem

`M-x org-note-agenda` and `M-x org-note-queue` prompt for comma-separated
workspace IDs. That is error-prone and not human readable. Document creation
already selects workspaces by slug; operational views should use the same
readable labels and persist user preferences.

## Goals

- Persist separate default workspace selections for agenda and queue.
- Configure on first use when no preference exists.
- Support reconfiguration through dedicated commands and `C-u` on the main
  commands.
- Allow multiple workspaces per preference.
- Store stable workspace IDs while displaying slug labels in the UI.
- Preserve Lisp-call semantics: explicit `workspace-ids` arguments bypass
  preferences.

## Non-goals

- Changing `org-note-documents` workspace selection in this iteration.
- Custom `:set` editors in Customize.
- Shared preference between agenda and queue.

## Data model

Add two `defcustom` variables in the `org-note` group:

- `org-note-agenda-workspace-ids`
- `org-note-queue-workspace-ids`

Both use type `(repeat string)`, default `nil`, and store workspace IDs only.
Persist with `customize-save-variable`.

## Configuration flow

Dedicated commands:

- `org-note-configure-agenda-workspaces`
- `org-note-configure-queue-workspaces`

Shared helper `org-note--configure-workspaces`:

1. Fetch all workspaces with paginated `org-note-operation-list-workspaces`.
2. Present toggle choices labeled by slug, falling back to ID.
3. Loop until the user selects `Done` with at least one workspace chosen.
4. Validate, assign the target variable, and save through Customize.

Reconfiguration preloads the current saved IDs as the initial selection.

## Main command behavior

| Invocation | Behavior |
| --- | --- |
| `M-x org-note-agenda` / `org-note-queue`, preference unset | Run configure, then open |
| Preference set | Use saved IDs, then prompt for view |
| `C-u M-x org-note-agenda` / `org-note-queue` | Reconfigure, then open |
| Lisp call with explicit `workspace-ids` | Ignore preferences |

## Stale workspace handling

Before opening with saved preferences:

- If some IDs are missing from the current workspace list and at least one
  remains valid, ask whether to reconfigure.
- If the user declines, continue with the valid subset and persist the
  trimmed list.
- If all saved IDs are stale, force reconfiguration.

## Error handling

| Scenario | Behavior |
| --- | --- |
| Network/API failure during configure | Propagate error; do not save |
| `C-g` during configure | Keep previous preference |
| First configure cancelled with empty preference | Do not open browser |
| `C-u` reconfigure cancelled | Keep previous preference; do not open |

## Testing

Add ERT coverage for:

- Configure saves multiple workspace IDs.
- Agenda and queue use separate preferences.
- Unconfigured interactive open runs configure.
- Configured interactive open uses saved IDs.
- `C-u` triggers reconfigure.
- Stale-ID yes/no branches.

Update `design.md`, `README.md`, and Org Note autoload registration.

## Documentation updates

- Describe the new custom variables and configure commands.
- Replace "prompts for workspaces" with "uses configured workspaces".
