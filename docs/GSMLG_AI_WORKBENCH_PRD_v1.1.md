# GSMLG AI Workbench and Inline Completion — Product Requirements and Design Specification

**Document status:** Approved for implementation  
**Target repository:** `gsmlg/dotfiles`  
**Target subsystem:** `emacs.d/`  
**Minimum runtime:** GNU Emacs 30.2  
**Primary implementation language:** Emacs Lisp with lexical binding  
**LLM transport/runtime dependencies:** `gptel`, `minuet`  
**Version:** 1.1  
**Date:** 2026-08-05

---

## 1. Executive Summary

Implement an Emacs-native AI editing workbench for the GSMLG Emacs configuration.

The workbench is intended for focused LLM-assisted work on explicitly selected text, buffers, and files. It must support:

1. ordinary LLM chat;
2. asking questions about selected context;
3. reviewing selected files;
4. preview-based rewriting of an active region;
5. a restricted multi-file editing agent that creates an in-memory proposal, presents the proposal for review, and applies accepted changes only to Emacs buffers; and
6. low-latency, Copilot-style inline code completion shown as ephemeral ghost text and inserted only after explicit acceptance.

The implementation must use two deliberately separate LLM integration planes:

1. **`gptel` for deliberate workbench requests** — chat, contextual questions, reviews, region rewrite, and the restricted multi-file proposal workflow.
2. **`minuet` for latency-sensitive inline code completion** — bounded prefix/suffix context, asynchronous suggestions, ghost-text presentation, and partial/full acceptance.

The implementation must not reimplement provider clients, HTTP streaming, authentication, Markdown rendering, generic chat session management, or a full inline-completion engine. First-party GSMLG modules own policy, safety, lifecycle, keybindings, and integration with the existing completion stack.

The multi-file editing workflow must be **buffer-first** and **review-first**:

```text
Explicit user context
        │
        ▼
Live Buffer / file snapshots
        │
        ▼
Restricted LLM tool loop
        │
        ▼
In-memory proposed content
        │
        ▼
Proposal review / diff / revise
        │
        ▼
Explicit user apply
        │
        ▼
Modified, unsaved Emacs buffers
```

The LLM must never directly save files, invoke a shell, evaluate arbitrary Emacs Lisp, scan an unapproved project, or bypass the proposal review step. Inline completion is the one deliberately lighter-weight editing path: it may insert only the currently displayed suggestion into the current buffer after an explicit accept command, and it must never save the buffer.

The two planes must remain independent:

```text
Deliberate editing plane                    Inline completion plane
------------------------                    -----------------------
gptel                                      minuet
explicit selected context                  current live buffer only
context-rich / multi-file                  bounded prefix + suffix
review-first proposal                      ephemeral ghost text
seconds-to-minutes interaction             latency-sensitive interaction
explicit proposal apply                    explicit suggestion accept
```

---

## 2. Product Name and Repository Placement

### 2.1 Product name

The product is named **GSMLG AI Workbench**.

The public Emacs Lisp namespace is:

```text
gsmlg-ai-*
```

The top-level feature is:

```text
gsmlg-ai
```

### 2.2 Repository placement

Implement the workbench as first-party GSMLG application modules under:

```text
emacs.d/lisp/
```

Do not add a new recursively scanned `site-lisp` tree. Do not alter the Agent Editor MCP vendoring or load-path behavior.

Recommended production modules:

```text
emacs.d/lisp/
├── gsmlg-ai.el
├── gsmlg-ai-completion.el
├── gsmlg-ai-context.el
├── gsmlg-ai-session.el
├── gsmlg-ai-tools.el
└── gsmlg-ai-review.el
```

Recommended responsibility boundaries:

| Module | Responsibility |
|---|---|
| `gsmlg-ai.el` | Public commands, shared user options, application facade and lifecycle |
| `gsmlg-ai-completion.el` | Minuet integration, inline-completion policy, Corfu/CAPF coexistence, sensitive-buffer blocking, completion keymap and diagnostics |
| `gsmlg-ai-context.el` | Context entries, context UI, snapshots, size checks, sensitive-file checks |
| `gsmlg-ai-session.el` | Request/session state machine, prompts, gptel request orchestration, cancellation |
| `gsmlg-ai-tools.el` | Request-scoped read/search/mutation tools operating only on session snapshots |
| `gsmlg-ai-review.el` | Proposal UI, diff/Ediff views, stale checks, transactional apply and discard |

`gsmlg-ai.el` is the workbench application facade. `gsmlg-ai-completion.el` is a separately deferred feature because a user may enable inline completion without opening the workbench UI, or use the workbench while keeping automatic inline completion disabled. Internal workbench modules may be loaded when a corresponding `gsmlg-ai` command is first invoked.

This layout should preserve enough internal separation to permit later extraction into a standalone package, but extraction and publication are not part of version 1.

---

## 3. Background and Problem Statement

The current Emacs configuration needs an AI feature for practical editor work rather than a terminal coding-agent frontend.

The desired workflow is primarily:

- receive low-latency inline code completions while typing;
- explain selected code or prose;
- summarize one or more files;
- review selected files;
- rewrite a selected region;
- request a coherent change across a small, explicitly chosen set of files;
- inspect all suggested changes before accepting them; and
- keep the resulting buffers unsaved so normal Emacs save, undo, formatting, and version-control workflows remain authoritative.

Existing full agent frontends are too broad for this purpose. They commonly add their own terminal runtime, session model, shell tools, file-writing tools, permission UI, transcript renderer, project/worktree abstraction, and provider adapters.

The implementation needs two narrow abstractions rather than a general coding-agent frontend:

> A provider-neutral, Emacs-native, restricted LLM editing transaction over explicitly authorized context.

> A latency-sensitive, current-buffer-only inline completion facility whose suggestions remain ephemeral until explicitly accepted.

---

## 4. Product Vision

Provide a small, dependable AI editing layer that behaves like a native Emacs editing facility.

The product should feel like this:

```text
LLM = an assisted source of proposed edits
Emacs Buffer = authoritative editable state
User review = mandatory commit boundary
Disk save = always controlled by the user
```

The product is not an autonomous software-development environment. It is a controlled editor feature for reading, reasoning about, and modifying selected text, plus a narrowly scoped inline completion facility.

Inline completion should feel like a native asynchronous completion layer:

```text
Typing pauses
    ↓
bounded current-buffer prefix/suffix is sent
    ↓
non-blocking ghost suggestion appears
    ↓
accept full / line / word, or dismiss
    ↓
ordinary modified buffer + ordinary undo
```

---

## 5. Goals

### G-1: Provider-neutral LLM access

Use `gptel` public APIs for backend/model selection, requests, streaming where appropriate, cancellation, tool calling, and standard chat/rewrite behavior.

### G-2: Buffer-first context

When a selected file has a live visiting buffer, the LLM must see the current widened buffer contents, including unsaved changes, rather than stale disk contents.

### G-3: Explicit context authorization

The LLM may read or search only entries explicitly selected by the user for the current workbench context or session.

### G-4: Safe multi-file edits

All agent mutations must update in-memory proposed content. No agent tool may directly change a source buffer or write to disk.

### G-5: Mandatory proposal review

Every multi-file edit must produce a reviewable proposal before application.

### G-6: Native Emacs apply semantics

Accepted changes must be applied to Emacs buffers, remain unsaved, preserve normal buffer modification state, and support ordinary Emacs undo.

### G-7: Small dependency and capability surface

The only new core third-party packages are `gptel` and `minuet`. Use built-in Emacs libraries for policy, state, UI integration, and safety whenever practical. Do not add another agent framework or completion client.

### G-8: Reproducible integration

Integrate with the repository's Elpaca lock, XDG rules, deferred application loading, keybinding contract, test suite, and documentation.

### G-9: Native, non-competing AI completion

Provide automatic and manual inline completion without replacing Eglot, CAPF, Cape, or Corfu. Deterministic language-server and local completion remains authoritative; AI ghost text is an independent asynchronous layer that yields whenever `completion-in-region-mode` is active.

---

## 6. Non-Goals

The following are explicitly out of scope for version 1:

1. Agent Editor MCP integration or modification.
2. `agent-shell`, ACP, Codex CLI, Claude Code CLI, Gemini CLI, or terminal-agent execution.
3. `macher`, `gptel-agent`, or another autonomous-agent dependency.
4. Shell execution, compilation, tests, Git commands, web browsing, arbitrary HTTP tools, arbitrary Elisp evaluation, or subprocess tools exposed to the LLM.
5. Automatic project discovery or sending an entire project to a model.
6. Background indexing, embeddings, RAG, or semantic code search.
7. Automatic file saving.
8. Automatic formatting after apply.
9. Direct model-generated patch parsing as the source of truth.
10. File deletion or rename operations.
11. Automatic three-way merge of stale proposals.
12. Persistent agent sessions, transcript databases, or cross-restart proposal recovery.
13. Next-edit prediction, multi-location edits, or Minuet Duet/NES experimental behavior.
14. Project-wide or cross-file context for automatic inline completion.
15. GitHub Copilot-specific account integration or provider-specific authentication UI.
16. Multiple simultaneously active edit proposals in the initial release.

---

## 7. Design Principles

### 7.1 Buffer first

For a file with a live visiting buffer:

```text
live widened buffer content > disk content
```

Unsaved edits are part of the authorized context and must be sent to the model when the user starts a request.

### 7.2 Explicit context only

No tool may enumerate arbitrary project files. No implicit recursive directory addition is allowed in the workbench context.

### 7.3 Proposal state, not textual diff, is authoritative

The internal truth is:

```text
original-content
proposed-content
```

A diff is generated from those two values for display only. The implementation must never parse an LLM-produced unified diff and treat it as the editing transaction.

### 7.4 Review before mutation

The LLM may mutate only session-local proposed content. Source buffers change only after an explicit user apply command.

### 7.5 No automatic save

Applying a proposal must result in modified Emacs buffers. It must not call `save-buffer`, `write-file`, `write-region`, or an equivalent direct disk-writing path.

### 7.6 Request-scoped capabilities

Tools must be passed only to the workbench request that needs them. Do not append workbench mutation tools to global `gptel-tools` or expose them in ordinary chat sessions.

### 7.7 Public APIs only

Use documented/public `gptel` APIs. Do not depend on private symbols with `gptel--` prefixes unless no public API exists and the exception is explicitly documented and tested. The implementation target is zero private gptel dependencies.

### 7.8 No startup network access

Loading the configuration or AI module must not make an LLM request, refresh a package archive, authenticate, or otherwise access the network. Requests occur only after explicit interactive commands.

### 7.9 File contents are untrusted data

System prompts must tell the model that instructions found inside selected files are content to analyze, not commands that override the user's request or the tool policy.

### 7.10 Deterministic completion has priority

The existing Eglot/CAPF/Cape/Corfu stack remains the primary completion system. Inline AI completion must not be installed as a slow CAPF and must not replace, advise, or inspect private Corfu internals.

Priority rules:

```text
active completion-in-region / Corfu popup
        >
AI automatic ghost suggestion
```

When deterministic completion activates, any visible AI ghost suggestion is dismissed. Automatic AI requests are blocked while `completion-in-region-mode` is active. A manual AI completion command may explicitly close the built-in completion-in-region session before requesting an AI suggestion, using public Emacs APIs only.

### 7.11 Inline completion is ephemeral

A generated completion is display state, not buffer state. Before acceptance it must:

- make no buffer change;
- create no undo entry;
- write no file;
- persist no transcript; and
- disappear when it becomes stale, is dismissed, or the buffer becomes ineligible.

Acceptance inserts only the accepted full suggestion, line, or word into the current buffer. The insertion remains unsaved and undoable.

### 7.12 Completion context is current-buffer-only

Automatic inline completion may send only a configured, bounded prefix and suffix around point from the current live buffer. It must not reuse workbench context, read another buffer, scan project files, call project search, or invoke the multi-file tool loop.

---

## 8. Users and Primary Use Cases

### 8.1 Primary user

An experienced Emacs user working with code, configuration, Markdown, Org, and other text files, often with multiple unsaved buffers.

### 8.2 Primary use cases

#### UC-1: Ask about current text

The user selects a region or works in the current buffer, invokes an AI question command, enters a question, and receives an answer in a dedicated response buffer.

#### UC-2: Ask about several files

The user explicitly adds multiple buffers/files to the workbench context and asks a question about their relationship or behavior.

#### UC-3: Review selected files

The user requests a focused code or document review. The model returns findings without modifying any file.

#### UC-4: Rewrite a region

The user selects a region and requests a rewrite/refactor. The result uses `gptel-rewrite` preview behavior and is not accepted automatically.

#### UC-5: Modify several files

The user selects a small set of files, starts an edit session, provides a task, lets the LLM read/search and modify in-memory copies, reviews a proposal, and applies selected or all files.

#### UC-6: Revise a proposal

The user reviews a staged proposal, gives the model a follow-up instruction, and receives an updated proposal without touching the original buffers.

#### UC-7: Reject a proposal

The user discards the session. No source buffer or disk file changes.

#### UC-8: Receive an inline code completion

The user types in an eligible programming buffer. After a configurable idle delay, the completion layer sends bounded current-buffer context and displays a ghost suggestion without blocking Emacs. The user accepts the whole suggestion, one line, or one word, or dismisses it.

#### UC-9: Request completion manually

The user invokes a manual completion command when automatic suggestions are disabled or when deterministic CAPF completion has no useful candidate. The command creates one bounded request and displays the result as ghost text.

---

## 9. Release Scope

### 9.1 Version 1 deliverables

Version 1 must include:

- deferred installation/configuration of `gptel` and `minuet`;
- standard gptel chat entry point;
- manual and optional automatic inline code completion;
- current-buffer-only prefix/suffix completion context;
- full, line, and word suggestion acceptance;
- explicit Corfu/CAPF coexistence and priority policy;
- one-shot ask and review commands;
- preview-based region rewrite wrapper;
- explicit in-memory context management;
- buffer-first snapshots;
- restricted multi-file edit sessions;
- request-scoped read/search/edit tools;
- staged proposal review;
- per-file and all-file application;
- stale-source detection;
- in-memory new-file proposals under a bounded creation root;
- cancellation and deterministic cleanup;
- keybindings and which-key labels;
- offline tests with all model calls stubbed;
- repository documentation and Elpaca lock update.

### 9.2 Version 1 constraints

- One active edit proposal per Emacs process.
- Any number of ordinary gptel chat buffers may exist independently.
- Workbench context is in memory only and is not persisted across Emacs restarts.
- Whole-file and whole-proposal acceptance are supported; per-hunk acceptance is deferred.
- Editing supports text files and file-visiting text buffers.
- Ask/review may include non-file text buffers and regions.
- New-file creation is local-only in version 1.
- Deletion, rename, and three-way merge are not supported.
- Inline completion uses one current buffer and one point location per request.
- One in-flight automatic completion request is permitted per buffer; a newer request supersedes or cancels an older request.
- Automatic inline completion is enabled only in explicitly eligible modes and only after the user has enabled it and configured a provider.
- Remote/TRAMP automatic completion, project-wide completion context, next-edit prediction, and multi-location suggestions are not supported in version 1.

---

## 10. User Experience and Commands

### 10.1 Public commands

The following interactive commands are required.

| Command | Behavior |
|---|---|
| `gsmlg-ai-chat` | Open or create a normal gptel chat buffer |
| `gsmlg-ai-menu` | Open the gptel transient/menu for model and request options |
| `gsmlg-ai-ask` | Ask a one-shot question using active workbench context, or current region/buffer when context is empty |
| `gsmlg-ai-review` | Run a read-only review prompt over active context |
| `gsmlg-ai-rewrite-region` | Invoke preview-based gptel region rewrite; require an active region |
| `gsmlg-ai-edit` | Start a restricted multi-file edit session and build a staged proposal |
| `gsmlg-ai-context-show` | Display the current context manager buffer |
| `gsmlg-ai-context-add-buffer` | Add the current buffer as a whole-buffer context entry |
| `gsmlg-ai-context-add-region` | Add the active region as read-only context |
| `gsmlg-ai-context-add-file` | Select and add one or more explicit files |
| `gsmlg-ai-context-add-project-files` | Select explicit files from `project-files`; never add the project recursively without selection |
| `gsmlg-ai-context-add-dired` | Add explicitly marked Dired files |
| `gsmlg-ai-context-clear` | Clear the current in-memory context after confirmation when non-empty |
| `gsmlg-ai-proposal-show` | Show the current staged proposal, if any |
| `gsmlg-ai-cancel` | Cancel the active workbench request and clean up incomplete staged state |
| `gsmlg-ai-completion-show` | Manually request one inline suggestion at point |
| `gsmlg-ai-completion-mode` | Toggle automatic AI suggestions in the current buffer |
| `gsmlg-ai-global-completion-mode` | Toggle automatic AI suggestions globally for eligible buffers |
| `gsmlg-ai-completion-diagnose` | Display provider readiness, eligibility blockers, request state, and effective completion limits |

### 10.2 Default context fallback

For `gsmlg-ai-ask` and `gsmlg-ai-review`:

1. Use the explicit workbench context when it is non-empty.
2. Otherwise use the active region when present.
3. Otherwise use the current widened buffer.

For `gsmlg-ai-edit`:

1. Use explicit editable file/buffer entries in the workbench context.
2. If the context is empty and the current buffer visits a file, use the current whole buffer.
3. If only a region is desired, direct the user to `gsmlg-ai-rewrite-region` rather than treating the region as a multi-file transaction.
4. Reject an edit request with no editable file-backed context.

### 10.3 Context manager buffer

Provide a dedicated buffer, for example:

```text
*GSMLG AI Context*
```

Use `tabulated-list-mode` or another built-in list-oriented mode.

Display at least:

- entry type: Buffer, File, or Region;
- display name/path;
- live/modified status;
- approximate character or byte size;
- editable/read-only capability;
- remote/local status.

Recommended context-buffer keys:

| Key | Action |
|---|---|
| `RET` | Visit the source entry |
| `b` | Add a buffer |
| `f` | Add arbitrary files |
| `p` | Add explicitly selected project files |
| `d` | Delete entry at point |
| `c` | Clear context |
| `g` | Refresh status |
| `q` | Quit window without clearing context |

### 10.4 Ask/review result buffer

Each one-shot request must show:

- request type;
- backend/model or selected preset;
- context entry count;
- request state: preparing, waiting, streaming, complete, cancelled, or failed;
- the user prompt;
- the model response.

Reuse gptel's response rendering and request behavior where public APIs permit. Do not implement a second Markdown renderer.

### 10.5 Proposal review buffer

Provide a dedicated proposal buffer, for example:

```text
*GSMLG AI Proposal*
```

Display:

- session ID and state;
- original user task;
- backend/model or preset;
- creation root for staged new files;
- tool-call count and configured limit;
- summary returned by the model;
- a table of files with status:
  - unchanged;
  - modified;
  - new;
  - stale/conflicted;
  - applied;
- change size summary per file;
- warnings or errors.

Recommended proposal-buffer keys:

| Key | Action |
|---|---|
| `RET` | Visit source/proposed file entry |
| `d` | Show diff for file at point |
| `D` | Show combined proposal diff |
| `e` | Ediff original/current proposal for file at point |
| `a` | Apply file at point |
| `A` | Preflight and transactionally apply all eligible files |
| `r` | Give a follow-up instruction and revise the staged proposal |
| `g` | Refresh stale state and display |
| `x` | Discard proposal after confirmation |
| `q` | Quit window without discarding proposal |

No proposal command may save a file automatically.

### 10.6 Inline completion interaction

Inline suggestions use an overlay/ghost-text presentation supplied by Minuet. They must not open the proposal review UI and must not be represented as staged multi-file changes.

The visible suggestion supports:

- accept full suggestion;
- accept one logical line;
- accept one word/token unit supported by the completion package;
- cycle next/previous alternatives when more than one exists;
- dismiss; and
- cancel the in-flight request.

The status should distinguish at least:

```text
disabled
ineligible
idle
waiting
showing
failed
```

Automatic requests must be debounced and throttled. A completion response is stale and must not be displayed when the originating buffer, point, narrowing, major mode, or relevant modification tick no longer matches the request snapshot.

The wrapper must provide an actionable diagnostic command rather than silently failing when the provider, executable dependency, endpoint, model, or credential source is missing.

---

## 11. Keybinding Requirements

Create a new global AI prefix map on:

```text
C-c A
```

Lowercase `C-c a` remains Org Agenda and must not change.

Recommended map:

| Key | Command | Label |
|---|---|---|
| `C-c A g` | `gsmlg-ai-chat` | chat |
| `C-c A m` | `gsmlg-ai-menu` | model/request menu |
| `C-c A a` | `gsmlg-ai-ask` | ask about context |
| `C-c A v` | `gsmlg-ai-review` | review context |
| `C-c A r` | `gsmlg-ai-rewrite-region` | rewrite region |
| `C-c A e` | `gsmlg-ai-edit` | staged multi-file edit |
| `C-c A c` | `gsmlg-ai-context-show` | context |
| `C-c A b` | `gsmlg-ai-context-add-buffer` | add buffer |
| `C-c A f` | `gsmlg-ai-context-add-file` | add file |
| `C-c A d` | `gsmlg-ai-context-add-dired` | add Dired marks |
| `C-c A p` | `gsmlg-ai-proposal-show` | proposal |
| `C-c A x` | `gsmlg-ai-cancel` | cancel request |
| `C-c A i` | `gsmlg-ai-completion-show` | inline suggestion now |
| `C-c A t` | `gsmlg-ai-completion-mode` | toggle inline completion in buffer |
| `C-c A T` | `gsmlg-ai-global-completion-mode` | toggle inline completion globally |
| `C-c A ?` | `gsmlg-ai-completion-diagnose` | completion diagnostics |

Implementation requirements:

- define the prefix with `defvar-keymap`;
- install it with the Emacs 30 keymap APIs;
- add which-key labels;
- update `gsmlg-keybinding-contract`;
- update `emacs.d/tests/keybindings-test.el`;
- update `emacs.d/docs/keybindings.md`;
- do not repurpose existing `M-/` or `C-M-/` completion-at-point bindings;
- do not repurpose existing `M-i`; and
- do not bind acceptance keys globally.

While an AI suggestion is visibly active, a transient Minuet/GSMLG completion map must provide:

| Key | Action |
|---|---|
| `TAB` / `<tab>` | Accept the full visible suggestion |
| `M-RET` | Accept one line |
| `M-f` | Accept one word |
| `M-n` | Show next suggestion |
| `M-p` | Show previous suggestion |
| `C-g` | Dismiss the suggestion and cancel any applicable request |

These bindings exist only while ghost text is active. When no AI suggestion is active, `TAB`, `M-f`, `M-n`, `M-p`, and `C-g` retain their normal current-mode behavior. Corfu's active map remains authoritative while completion-in-region is active.

---

## 12. Functional Requirements

### FR-1: LLM package integration

1. Declare `gptel` and `minuet` in `gsmlg-app-packages.el` through Elpaca/use-package.
2. Keep both packages deferred; do not load either during normal startup solely because its declaration exists.
3. Register `gsmlg-ai` as a deferred application feature in `gsmlg-apps.el`.
4. Autoload public `gsmlg-ai-*` commands from `gsmlg-apps.el`.
5. Do not add an `after-init-hook` that loads `gsmlg-ai`.
6. Do not change `init.el` unless repository architecture or tests demonstrate that it is strictly necessary.
7. Update `elpaca-lock.el` using the repository's normal locked-package process.
8. Use only documented/public Minuet and gptel interfaces. Do not depend on private package variables or functions.
9. Keep Minuet provider/runtime concerns inside `gsmlg-ai-completion.el`; workbench modules must not call Minuet.

### FR-2: Backend and model ownership

1. The workbench is provider-neutral.
2. It uses the active gptel backend/model by default.
3. It may optionally use a named gptel preset through a user option.
4. Provider credentials and private endpoint values belong in the external local override, `auth-source`, or environment configuration.
5. No API key or token may be committed to the repository or included in `local.el.example`.
6. Do not hardcode provider model names in workbench logic.
7. `gsmlg-ai-edit` must require tool-capable model/backend behavior. Detect capability through a public gptel facility when available; otherwise fail cleanly when tool use is unavailable. Do not silently fall back to parsing free-form patches.

### FR-3: Context management

1. Maintain one current in-memory context for version 1.
2. Give the context an internal opaque ID so the data model can later support multiple named contexts.
3. Deduplicate file-backed entries by normalized file identity.
4. Do not recursively scan directories.
5. `project-files` may be used only to offer completion candidates for explicit user selection.
6. Dired integration may add only marked regular files.
7. Only text files/buffers are accepted by the workbench context.
8. Context entries are live references until a request starts.
9. A request creates immutable source snapshots.
10. Region entries are read-only context and are not multi-file mutation targets.
11. Non-file buffers are allowed for ask/review but are read-only in edit sessions.
12. Context is not persisted in version 1.

### FR-4: Buffer-first snapshot behavior

When a request starts:

1. If an entry refers to a live buffer, widen it and snapshot `buffer-substring-no-properties`.
2. Preserve unsaved buffer content.
3. Record `buffer-chars-modified-tick`.
4. For a file-backed buffer, also record normalized file identity, relevant file attributes, and a SHA-256 content hash.
5. If a selected file is not visited, read it through file-handler-aware Emacs APIs into a temporary buffer, then snapshot decoded text.
6. Record the file attributes and content hash used for stale detection.
7. Do not save buffers before snapshotting.
8. Do not replace live buffer contents with disk contents.
9. Do not include text properties.
10. Preserve the file's normal Emacs coding-system behavior by applying text to the visiting buffer rather than writing encoded bytes directly.

### FR-5: Size and type limits

Provide user options with conservative defaults:

| Option | Suggested default | Purpose |
|---|---:|---|
| `gsmlg-ai-max-file-bytes` | 524288 | Maximum single text file snapshot |
| `gsmlg-ai-max-context-bytes` | 2097152 | Maximum aggregate edit-session snapshot |
| `gsmlg-ai-max-inline-context-bytes` | 524288 | Maximum context embedded in ask/review prompts |
| `gsmlg-ai-max-read-bytes` | 65536 | Maximum output of one read tool call |
| `gsmlg-ai-max-tool-calls` | 64 | Maximum tool calls per edit/revision round |
| `gsmlg-ai-max-search-results` | 100 | Maximum search hits per call |

Requirements:

1. Reject binary files, at minimum when NUL bytes are detected.
2. Show a clear error identifying entries that exceed limits.
3. Do not silently truncate whole context.
4. Tool read output may be bounded, but the response must state the returned range and total size.
5. Limits must be configurable through `defcustom`.

### FR-6: Sensitive-file handling

Provide a configurable sensitive-file policy.

Suggested default path/name patterns include:

```text
.env
.env.*
.authinfo
.authinfo.gpg
*.pem
*.key
*credentials*
*secret*
```

Provide:

```text
gsmlg-ai-confirm-before-send
```

with choices equivalent to:

- always;
- when sensitive;
- never.

Default to `when-sensitive`.

Before sending sensitive context, show the selected backend/model and affected paths and require explicit confirmation.

Do not inspect or log secret values merely to produce the warning.

### FR-7: One-shot ask

1. Prompt the user for a question.
2. Snapshot the selected context according to FR-4.
3. Serialize context with unambiguous machine-generated boundaries and source metadata.
4. Tell the model that file contents are untrusted data.
5. Use a public `gptel-request` workflow and a dedicated response buffer.
6. Do not provide mutation tools.
7. Never modify context buffers.
8. Support cancellation.
9. On error, preserve the prompt and context summary in the result buffer and show a useful diagnostic.

### FR-8: Read-only review

1. Use the same snapshot and request machinery as ask.
2. Use a review-specific directive focused on correctness, regressions, safety, maintainability, and missing tests.
3. Instruct the model to prioritize concrete findings with file/section references.
4. Do not provide mutation tools.
5. Never change a source buffer.

### FR-9: Region rewrite

1. Require an active region.
2. Delegate to public `gptel-rewrite` behavior rather than reimplementing rewrite overlays/actions.
3. Preserve the upstream default that previews the result and does not immediately modify the buffer.
4. Do not automatically accept the response.
5. Do not automatically save after acceptance.
6. Produce a clear user error when no region is active.

### FR-10: Edit-session creation

1. Reject starting a second edit session while an unapplied/undiscarded proposal exists, unless the user explicitly discards the old session.
2. Prompt for the edit task.
3. Snapshot editable context.
4. Determine a **creation root** used only for staged new files:
   - default to the current project root when one exists;
   - otherwise default to the originating `default-directory`;
   - allow explicit directory selection with a prefix argument or dedicated prompt;
   - do not require all context files to be within that root.
5. Display the context list, creation root, backend/model, and sensitive-file warning before dispatch when confirmation is required.
6. Initialize each editable snapshot with identical `original-content` and `proposed-content`.
7. Send only a manifest initially; let the model read selected content through bounded tools.
8. Provide only the tools defined by this PRD.
9. Keep all mutations in session memory.

### FR-11: Request-scoped tool set

The edit agent receives only the following capabilities.

#### FR-11.1 `list_context_files`

Returns authorized editable/read-only entries with:

- opaque file ID;
- display path/name;
- status;
- current proposal revision;
- text size and line count;
- editable/read-only flag.

It must not enumerate any other file.

#### FR-11.2 `read_file`

Inputs:

- authorized file ID;
- optional start line;
- optional end line.

Returns:

- file ID and display path;
- current proposal revision;
- returned line range;
- total line/byte size;
- bounded content.

It must read current `proposed-content`, not disk and not only `original-content`, so the model can observe its previous edits.

#### FR-11.3 `search_files`

Inputs:

- literal query by default;
- optional regular-expression mode;
- optional subset of authorized file IDs;
- optional result limit bounded by configuration.

Returns matches from current proposed content with:

- file ID/path;
- current revision;
- line number;
- bounded excerpt.

Requirements:

- catch invalid regular expressions;
- never call project-wide grep;
- never search files outside the session;
- never search disk behind a live snapshot.

#### FR-11.4 `replace_text`

Inputs:

- authorized editable file ID;
- expected proposal revision;
- exact old text;
- replacement text;
- expected occurrence count, defaulting to exactly one.

Behavior:

1. Verify the revision.
2. Count exact matches in current proposed content.
3. Reject zero, ambiguous, or unexpected match counts.
4. Apply all explicitly expected matches only when the count matches.
5. Increment the proposal revision.
6. Return the new revision and concise change metadata.
7. Never change a source buffer.

#### FR-11.5 `set_file_content`

This guarded whole-file replacement is allowed for cases where exact replacement is impractical.

Inputs:

- authorized editable file ID;
- expected proposal revision;
- complete new content.

Requirements:

- enforce per-file size limits;
- verify revision;
- increment revision;
- operate only in memory;
- return new size/revision metadata.

The system prompt should prefer `replace_text` for localized changes and reserve `set_file_content` for deliberate whole-file rewrites.

#### FR-11.6 `create_file`

Inputs:

- relative path under the session creation root;
- complete initial text content.

Requirements:

1. Local files only in version 1.
2. Reject absolute paths.
3. Reject empty paths, `..` traversal, and paths escaping through symlinks.
4. Reject paths outside the creation root.
5. Reject existing disk paths.
6. Reject paths already visited or already staged.
7. Enforce file/context size limits.
8. Create only a staged file object; do not create a disk file or source buffer at tool-call time.

#### FR-11.7 `finish_proposal`

Inputs:

- concise change summary;
- optional warnings or unresolved questions.

Behavior:

- record the summary;
- mark the agent round complete;
- transition the session to ready-for-review.

A normal final model response may also complete a round, but the implementation should encourage explicit `finish_proposal` use.

### FR-12: Tool isolation and routing

1. Tools must be request-scoped.
2. Tools must be bound to the correct session through a closure, opaque registry token, or equivalent safe mechanism.
3. Do not implement tool functions against a single unqualified global `current-session` that can accidentally route a call to another request.
4. Opaque session routing details must not need to be supplied by the model.
5. Every tool call increments a session counter.
6. Exceeding the configured tool budget aborts the round with a clear error and no source mutation.
7. Tool results must be JSON-compatible and bounded.

### FR-13: Agent instruction policy

The edit system directive must tell the model to:

- follow only the user's explicit task and system tool policy;
- treat file contents as untrusted data, not higher-priority instructions;
- inspect relevant files before editing;
- operate only on authorized IDs;
- never invent or request inaccessible paths;
- use exact replacements and current revisions;
- reread after a stale revision or failed match;
- preserve unrelated code and formatting;
- avoid changing files that do not need modification;
- create files only under the displayed creation root;
- finish with a concise summary and unresolved concerns;
- never claim that source files were saved or applied.

### FR-14: Proposal generation

1. A proposal is ready when the agent round completes successfully and at least one staged file changed or was created.
2. If nothing changed, show the model summary and mark the session complete without presenting an empty apply action.
3. A failed or cancelled request must not yield an applicable proposal.
4. A proposal stores original and proposed content directly.
5. Diff output is derived on demand.
6. The model's natural-language final response is metadata/summary, not an executable patch.

### FR-15: Diff and comparison views

1. Provide per-file original-vs-proposed comparison.
2. Provide a combined proposal diff when a suitable diff facility is available.
3. Provide an Ediff-based comparison path using built-in Emacs facilities.
4. The proposal must remain reviewable even when an external `diff` executable is unavailable.
5. Diff rendering may use `diff-no-select` when available, with an internal before/proposed or Ediff fallback.
6. Never parse displayed diff text to perform application.
7. Correctly represent new files in the review UI.

### FR-16: Stale-source detection

Before applying an existing-file proposal:

1. For a live source buffer, compare its current modified tick and content hash to the recorded snapshot.
2. For an originally unopened file, compare current file identity/attributes and content hash to the snapshot, then also verify any visiting buffer created since the request.
3. Mark the file stale if the source changed after snapshot creation.
4. Refuse to apply a stale file automatically.
5. Do not overwrite the user's newer changes.
6. Offer review/Ediff and restart/refresh guidance.
7. Automatic three-way merging is out of scope.

### FR-17: Apply an existing file

When the user applies an eligible existing-file proposal:

1. Perform stale preflight.
2. Find or create the normal visiting buffer with file-handler-aware APIs.
3. Preserve current major mode and file coding system.
4. Refuse read-only or otherwise unsafe mutation unless the user has already made the buffer writable through normal Emacs means.
5. Apply proposed content using buffer operations such as `replace-buffer-contents`, not direct disk writes.
6. Wrap the mutation in a change group/atomic change boundary.
7. Add a clear undo boundary so the AI apply is reversible as one logical buffer edit.
8. Leave the buffer modified.
9. Do not call save, format, revert, or Git commands.
10. Mark the proposal entry applied.

### FR-18: Apply a new file

When applying a staged new file:

1. Revalidate that the target path still does not exist.
2. Revalidate the creation-root boundary.
3. Open a normal file-visiting buffer for the target path.
4. Insert the proposed content.
5. Leave the buffer modified and unsaved.
6. Verify that the path does not exist on disk after application.
7. Do not create parent directories on disk automatically. If a parent directory does not exist, fail clearly and require the user to create/approve it through normal means.

### FR-19: Apply all transaction

`Apply All` must provide transaction-like behavior across buffers:

1. Preflight every eligible file before changing any buffer.
2. If any file is stale, read-only, invalid, or otherwise ineligible, do not apply any file.
3. Prepare per-buffer change groups before mutation.
4. Apply all proposed contents.
5. If any unexpected error occurs, cancel/roll back all prepared change groups and clean up session-created buffers where safe.
6. Accept all change groups only after every application succeeds.
7. Preserve one logical undo unit per affected buffer.
8. Never leave a silent half-applied proposal.

Per-file `Apply` remains independent and may be used when the user intentionally wants partial file-level acceptance.

### FR-20: Proposal revision

1. From a ready proposal, prompt for a follow-up instruction.
2. Keep the original source snapshots unchanged.
3. Use current proposed content as the tool-visible state.
4. Start a new bounded agent round with the same authorized file set.
5. Permit further read/search/mutation/create operations.
6. Increment or otherwise distinguish the revision round.
7. Return to ready-for-review after successful completion.
8. Source stale checks remain relative to the original snapshot until apply.

### FR-21: Cancellation and failure

1. `gsmlg-ai-cancel` must abort the active gptel request through a public cancellation mechanism when available.
2. Cancellation must not modify any source buffer or file.
3. Incomplete staged edits from a cancelled round must not become applicable.
4. Clean up temporary buffers, generated tool bindings, overlays, timers, and process/request handles owned by the workbench.
5. Preserve a concise error/cancellation report for the user.
6. A provider or network error must transition the session to failed, not leave it indefinitely running.
7. Retrying must require an explicit user action.

### FR-22: No save and no side effects

Tests and implementation review must verify that the workbench's agent path does not invoke:

- `save-buffer`;
- `basic-save-buffer`;
- `write-region` for source files;
- an external patch program;
- shell commands;
- arbitrary Elisp evaluation;
- project-wide scans;
- Git operations;
- Agent Editor MCP functions.

Temporary files used only for display-time diff generation are permitted when they are placed under an XDG cache/temp location and are reliably deleted, but proposal state must not depend on them.

### FR-23: Minuet integration boundary

1. Use Minuet as the inline completion engine.
2. `gsmlg-ai-completion.el` owns all GSMLG policy and exposes stable `gsmlg-ai-completion-*` commands.
3. Other GSMLG modules must not depend directly on Minuet internals.
4. The integration must support Minuet's provider-neutral chat-completion path and its FIM path where the configured provider/model supports FIM.
5. Prefer FIM-compatible completion for low-latency code completion, but do not hardcode a provider or model.
6. Do not implement a second HTTP client, streaming parser, overlay engine, or provider adapter.

### FR-24: Completion eligibility

Before an automatic request, the current buffer must pass all configured predicates. By default, block completion when any of the following is true:

- the current buffer is a minibuffer;
- the buffer is read-only;
- the buffer is not derived from an explicitly allowed major mode, with `prog-mode` as the initial default family;
- the buffer or path matches a sensitive-file rule;
- the buffer exceeds the configured size limit;
- the buffer is remote/TRAMP;
- `completion-in-region-mode` is active;
- an active region is being manipulated;
- multiple-cursor editing is active when that mode is present; or
- the user or a major mode has disabled the local completion minor mode.

Manual requests must still enforce read-only, sensitive-data, size, and remote policy. The user may configure additional documented blocker predicates.

### FR-25: Bounded current-buffer completion context

1. Capture context from the current live buffer, including unsaved text.
2. Send only a bounded prefix before point and bounded suffix after point.
3. Respect narrowing or deliberately widen according to one documented option; record the chosen behavior in diagnostics.
4. Strip text properties.
5. Do not include the workbench context, another buffer, file contents from disk, project metadata, repository search results, or session proposal state.
6. Enforce separate maximum prefix, suffix, and aggregate completion-context limits.
7. A completion request must carry enough language/mode metadata for the configured Minuet prompt/FIM template without sending unrelated user data.

### FR-26: Automatic request scheduling

1. Automatic suggestions are buffer-local and opt-in through the local or global GSMLG completion mode.
2. Use configurable idle debounce and request throttle values.
3. Permit at most one in-flight automatic completion request per buffer.
4. A newer request supersedes or cancels the previous request through public Minuet behavior.
5. No request occurs on Emacs startup, package load, mode definition, or merely visiting an ineligible buffer.
6. Deterministic CAPF/Corfu completion gets the first opportunity; AI automatic completion starts only when completion-in-region is inactive at dispatch time.
7. Default to one generated candidate per request unless the user explicitly configures more.

### FR-27: Stale completion suppression

Record sufficient request-origin state to reject or dismiss a response when any relevant state has changed, including:

- current buffer identity;
- point position;
- buffer modification tick;
- narrowing boundaries;
- major mode;
- completion-mode eligibility; or
- a newer completion request generation.

A stale response must not flash briefly, insert text, or replace a newer suggestion. Use Minuet's public cancellation/staleness facilities where available and add only a minimal public wrapper guard when necessary.

### FR-28: Ghost-text lifecycle

1. A suggestion is rendered without mutating buffer text.
2. Typing, moving point outside the valid origin, activating completion-in-region, changing major mode, disabling completion, killing the buffer, or invoking dismiss removes the suggestion.
3. Opening Corfu/CAPF completion dismisses the AI overlay through documented Emacs completion state/hooks, not private Corfu internals.
4. Suggestion overlays and timers must be buffer-local and cleaned on kill/change-major-mode.
5. Provider errors must remove stale visual state and expose a concise diagnostic without inserting error text into the source buffer.

### FR-29: Suggestion acceptance

1. Support acceptance of the full suggestion, one line, and one word through public Minuet commands or GSMLG wrappers.
2. Accept only text currently displayed for the active request generation.
3. Accepted text is inserted at point into the current buffer and is never saved automatically.
4. Full acceptance should form one logical undo unit. Partial acceptance must preserve normal, predictable Emacs undo behavior.
5. Acceptance must respect read-only state and ordinary buffer modification hooks.
6. Dismiss/cancel must leave buffer content and undo history unchanged.

### FR-30: CAPF, Corfu and Eglot coexistence

1. Do not add AI completion to `completion-at-point-functions`.
2. Do not replace or remove Eglot, Cape, Corfu, or existing CAPFs.
3. Do not call or inspect private `corfu--*` symbols.
4. Use public `completion-in-region-mode` state and documented hooks/APIs for arbitration.
5. Existing `M-/`, `C-M-/`, Corfu navigation, and normal `TAB` behavior must remain unchanged when no AI suggestion is active.
6. If deterministic completion activates while AI ghost text is visible, deterministic completion wins and the AI suggestion is dismissed.

### FR-31: Completion provider and credential configuration

1. Minuet provider, model, endpoint, prompt mode/FIM template, and credential source are machine-local configuration.
2. No secret may be stored in committed Emacs Lisp, Customize files tracked by the repository, logs, messages, or diagnostics.
3. Credential references should use environment variables, `auth-source`, or an instant function supported by the provider integration.
4. The workbench's gptel backend and the completion provider may be different; do not assume one model is optimal for both.
5. `local.el.example` documents non-secret configuration shapes and recommends a low-latency FIM-capable endpoint when available.
6. Missing configuration produces a clear diagnostic and no repeated automatic retry loop.

### FR-32: Completion telemetry and persistence boundary

1. Collect no telemetry.
2. Persist no completion prompts, responses, or suggestion history by default.
3. Do not place completion state in the project tree or alter Git ignore files.
4. Debug logging is opt-in, redacted, and must never include credentials; full source context is excluded by default.

---

## 13. Domain Model

Use `cl-defstruct` or equally explicit records. Exact field names may vary, but the following information is required.

### 13.1 Context

```text
Context
├── id
├── name
├── entries
├── created-at
└── updated-at
```

### 13.2 Context entry

```text
ContextEntry
├── id
├── kind                 buffer | file | region
├── display-name
├── canonical-file       optional
├── source-buffer        optional
├── start-marker         region only
├── end-marker           region only
├── editable-p
├── remote-p
└── added-at
```

### 13.3 Snapshot/proposal file

```text
SessionFile
├── id
├── display-path
├── canonical-file
├── source-kind          live-buffer | disk | staged-new
├── source-buffer
├── source-buffer-tick
├── source-content-hash
├── source-file-attributes
├── original-content
├── proposed-content
├── proposal-revision
├── operation            unchanged | modify | create
├── editable-p
├── remote-p
├── apply-status
└── conflict-reason
```

### 13.4 Session

```text
Session
├── id
├── kind                  ask | review | edit
├── state
├── user-prompt
├── system-directive
├── context-id
├── files
├── creation-root
├── backend/model/preset metadata
├── request-handle
├── tool-call-count
├── revision-round
├── model-summary
├── warnings
├── errors
├── created-at
└── updated-at
```

Do not place API keys, bearer tokens, or complete provider request payloads in session records.

---

## 14. State Machines

### 14.1 Ask/review state machine

```text
collecting-context
        ↓
preparing
        ↓
running
   ↙         ↘
complete    failed
   
Any active state → cancelled
```

### 14.2 Edit state machine

```text
collecting-context
        ↓
snapshotting
        ↓
running
   ↙         ↘
ready       failed
  ↓  ↘
revising  applying
  ↓          ↓
running   applied

ready → discarded
Any pre-ready active state → cancelled
ready/applying → stale when source preflight fails
```

State transitions must be validated. Commands invoked in an invalid state must produce clear user errors rather than corrupting the session.

---

## 15. Architecture

### 15.1 Layered architecture

```text
┌─────────────────────────────────────────────────────────────┐
│ User commands / keymaps / context and proposal UI          │
│                    gsmlg-ai.el                              │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│ Context and snapshot model                                  │
│                    gsmlg-ai-context.el                       │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│ Session state machine and prompt/request orchestration       │
│                    gsmlg-ai-session.el                       │
└───────────────┬──────────────────────────────┬──────────────┘
                │                              │
┌───────────────▼────────────────┐  ┌─────────▼────────────────┐
│ Request-scoped tools           │  │ Proposal review/apply     │
│ gsmlg-ai-tools.el              │  │ gsmlg-ai-review.el        │
└───────────────┬────────────────┘  └─────────┬────────────────┘
                │                              │
                └──────────────┬───────────────┘
                               │
┌──────────────────────────────▼──────────────────────────────┐
│ gptel public API: backend/model, request, tools, rewrite     │
└─────────────────────────────────────────────────────────────┘

Independent latency-sensitive path:

┌─────────────────────────────────────────────────────────────┐
│ gsmlg-ai-completion.el                                      │
│ eligibility, scheduling, safety, keymap, CAPF arbitration   │
└───────────────────────────┬─────────────────────────────────┘
                            │ public APIs only
┌───────────────────────────▼─────────────────────────────────┐
│ minuet                                                     │
│ provider request, streaming, ghost text, acceptance         │
└───────────────────────────┬─────────────────────────────────┘
                            │ current buffer only
┌───────────────────────────▼─────────────────────────────────┐
│ Emacs buffer + completion-in-region state                   │
└─────────────────────────────────────────────────────────────┘
```

### 15.2 Dependency direction

Allowed dependency direction:

```text
gsmlg-ai
  → context
  → session
  → tools
  → review
  → gptel

gsmlg-ai-completion
  → minuet
  → public Emacs completion state
```

Internal modules must not require `gsmlg-keybindings.el`. Keybindings depend on public command symbols, not the reverse.

`gsmlg-ai` must not require or call `gsmlg-agent` or any Agent Editor MCP feature.

Workbench modules must not require `gsmlg-ai-completion`, and completion code must not require workbench context/session/tool/review modules. Shared options or helper code, if any, must remain small and provider-neutral.

### 15.3 gptel usage

Use gptel for:

- backend/model selection;
- ordinary chat;
- one-shot programmable requests;
- async transport and streaming where supported;
- tool schema and tool-call exchange;
- request cancellation where supported;
- region rewrite preview.

Do not use gptel for:

- defining the workbench context model;
- directly mutating source buffers through generic file tools;
- storing authoritative proposal state;
- automatic file application;
- broad agent tool collections.

### 15.4 Minuet usage

Use Minuet for:

- provider-neutral chat-style code completion;
- FIM completion where supported;
- asynchronous/streaming suggestion generation;
- overlay/ghost-text display;
- full, line, and word acceptance;
- candidate cycling; and
- request cancellation through public interfaces.

Do not use Minuet for:

- workbench chat or multi-file editing;
- project scanning;
- CAPF registration;
- persistence;
- shell or tool use; or
- experimental next-edit prediction in version 1.

### 15.5 Completion arbitration with the existing stack

The existing configuration uses Corfu as the UI over standard CAPF completion, including Eglot/Cape sources. The AI completion path is intentionally parallel rather than another CAPF:

```text
CAPF / Eglot / Cape ──► completion-in-region ──► Corfu popup

current buffer ───────► Minuet async request ──► ghost text
```

Arbitration belongs in `gsmlg-ai-completion.el`. It must observe only documented Emacs completion state and must never inspect a Corfu child frame or `corfu--*` internal variable.

### 15.6 Concurrency

Version 1 permits one active edit session/proposal per Emacs process.

Requirements:

- read-only ordinary gptel chat remains independent;
- workbench tool routing must still be session-safe;
- starting another edit while one exists must offer to show or discard the existing proposal, not silently replace it;
- future multi-session support must not require redesigning file/session records.

Inline completion concurrency is separate:

- completion requests are scoped per buffer;
- only one current request generation is authoritative per buffer;
- a response from an older generation is discarded; and
- completion does not acquire or block the workbench edit-session slot.

---

## 16. Configuration

Define a `defgroup`:

```text
gsmlg-ai
```

Required or strongly recommended `defcustom` options:

```text
gsmlg-ai-default-preset
gsmlg-ai-confirm-before-send
gsmlg-ai-sensitive-file-patterns
gsmlg-ai-max-file-bytes
gsmlg-ai-max-context-bytes
gsmlg-ai-max-inline-context-bytes
gsmlg-ai-max-read-bytes
gsmlg-ai-max-tool-calls
gsmlg-ai-max-search-results
gsmlg-ai-ask-system-directive
gsmlg-ai-review-system-directive
gsmlg-ai-edit-system-directive
gsmlg-ai-completion-provider
gsmlg-ai-completion-auto-enable
gsmlg-ai-completion-allowed-mode-predicates
gsmlg-ai-completion-block-predicates
gsmlg-ai-completion-sensitive-file-patterns
gsmlg-ai-completion-max-buffer-bytes
gsmlg-ai-completion-prefix-chars
gsmlg-ai-completion-suffix-chars
gsmlg-ai-completion-context-window
gsmlg-ai-completion-debounce
gsmlg-ai-completion-throttle
gsmlg-ai-completion-timeout
gsmlg-ai-completion-candidate-count
gsmlg-ai-completion-allow-remote
```

Rules:

1. Prefer a gptel preset when the option is non-nil and valid.
2. Otherwise use current gptel backend/model selection.
3. User options must use `setopt` cleanly.
4. No option may contain a committed secret.
5. Add non-sensitive examples/comments to `emacs.d/local.el.example`.
6. Do not force one provider or model.
7. Local OpenAI-compatible servers and hosted providers should work through normal gptel backend configuration without workbench-specific provider code.
8. Minuet provider configuration is separate from gptel backend configuration because the latency/model requirements may differ.
9. The repository must not hardcode a completion model. `local.el.example` should demonstrate configuration using placeholders and non-secret credential references.
10. Recommended initial completion defaults are one candidate, a bounded context window, an idle debounce near 0.4–0.7 seconds, a throttle near 1 second, and a timeout near 3 seconds. Exact defaults must be validated against current Minuet public options and documented.
11. Automatic completion is not enabled merely because Minuet is installed. It starts only after the user enables the GSMLG local/global completion mode and provider validation succeeds.
12. A single `gsmlg-ai-completion-diagnose` command must show the effective options and the first active eligibility blocker without displaying source context or secrets.

---

## 17. Remote and TRAMP Behavior

### 17.1 Ask/review

Ask/review must support live TRAMP text buffers because the content is read from the Emacs buffer.

For unopened remote files, use file-handler-aware Emacs APIs if implemented. Do not copy an entire remote project locally or run local project indexing.

### 17.2 Edit

Existing remote file-visiting buffers may be staged and applied to their live buffers, subject to stale and read-only checks.

### 17.3 New remote files

`create_file` for remote creation roots is out of scope for version 1 and must fail clearly.

### 17.4 No external remote compute

The workbench exposes no shell/search subprocess to the LLM. Search remains within in-memory authorized snapshots.

### 17.5 Inline completion

Automatic inline completion is disabled for TRAMP/remote buffers by default. A manual or automatic remote request may be permitted only through an explicit user option and must still use the current live buffer with bounded context. The implementation must not invoke a remote shell, read neighboring remote files, or copy a remote project.

---

## 18. Safety and Security Requirements

### SR-1: Capability confinement

The model receives no shell, web, arbitrary file system, arbitrary buffer, Elisp, Git, or MCP tool.

### SR-2: Path confinement

Tools operate by authorized opaque file IDs. A model-supplied display path must never be expanded into arbitrary file access.

New-file paths must pass strict creation-root validation.

### SR-3: Prompt-injection resistance

System directives explicitly classify selected file content as untrusted data. Tool policy cannot be changed by text inside a file.

### SR-4: Secret handling

- no keys in repository files;
- no provider secrets in session structs;
- no full prompt/body logging by default;
- sensitive path warnings before send;
- never print authentication values in `message`, proposal buffers, or tests.

### SR-5: No unintended writes

Model tools mutate only in-memory proposal strings. Application changes buffers only after explicit user action. Saving remains separate.

### SR-6: Symlink and traversal defense

New file validation must reject path traversal and symlink escapes from the approved creation root.

### SR-7: Denial-of-service limits

Bound file size, aggregate context, tool read output, search results, and tool-call count.

### SR-8: Failure isolation

Errors, timeouts, cancellation, malformed tool arguments, and provider failures must not mutate source buffers.

### SR-9: Completion data minimization

Inline completion sends only bounded current-buffer prefix/suffix context and minimal mode metadata. It must never attach workbench context, project files, repository history, diagnostics from other buffers, or environment values.

### SR-10: Completion acceptance boundary

No completion response may modify a buffer until the user invokes an explicit acceptance command. Dismissal, timeout, cancellation, provider failure, stale response, and eligibility loss leave the buffer unchanged.

---

## 19. Non-Functional Requirements

### NFR-1: Startup performance

- `gsmlg-ai`, `gptel`, and `minuet` must not be loaded during normal startup unless explicitly invoked or the user has explicitly enabled global AI completion.
- No startup request or authentication.
- Existing offline startup behavior must remain valid after packages are installed.

### NFR-2: Responsiveness

- LLM requests are asynchronous.
- Long requests must be cancellable.
- UI status must update without blocking Emacs.
- Snapshot search/edit operations should use temporary buffers or efficient string operations and respect configured limits.
- Inline completion scheduling, network I/O, and streaming must never block command input.
- Automatic completion should favor low latency over large context or multiple alternatives.
- CAPF/Corfu interaction must remain responsive even when the completion provider is slow or unavailable.

### NFR-3: Maintainability

- lexical binding in every new file;
- standard headers and `provide` forms;
- public names use `gsmlg-` prefix;
- named hook/callback functions where practical;
- no unnecessary advice;
- no private gptel, Minuet, or Corfu API dependency;
- responsibilities follow the module split.

### NFR-4: Testability

- every gptel and Minuet/provider interaction can be stubbed;
- tests require no network or credentials;
- tools can be tested against in-memory session fixtures;
- apply tests use temporary files/directories and buffers;
- tests clean all temporary state.

### NFR-5: Observability

The user must be able to see:

- selected backend/model or preset;
- active request state;
- context size and entry count;
- tool budget use in edit sessions;
- proposal state and conflicts;
- actionable errors;
- current inline-completion eligibility and blocker;
- completion request state; and
- whether a visible suggestion is current or stale.

No telemetry is collected.

### NFR-6: Compatibility

- GNU Emacs 30.2 minimum;
- preserve existing keybindings;
- preserve XDG storage rules;
- preserve package locking and deferred application architecture;
- preserve TRAMP file-handler behavior;
- preserve existing Corfu/CAPF/Eglot behavior and keys.

---

## 20. Repository Integration Changes

The implementation is expected to modify or add the following files.

### 20.1 Production

```text
emacs.d/lisp/gsmlg-app-packages.el
emacs.d/lisp/gsmlg-apps.el
emacs.d/lisp/gsmlg-keybindings.el
emacs.d/lisp/gsmlg-ai.el                 new
emacs.d/lisp/gsmlg-ai-completion.el          new
emacs.d/lisp/gsmlg-ai-context.el         new
emacs.d/lisp/gsmlg-ai-session.el         new
emacs.d/lisp/gsmlg-ai-tools.el           new
emacs.d/lisp/gsmlg-ai-review.el          new
emacs.d/elpaca-lock.el
```

### 20.2 Tests

Recommended:

```text
emacs.d/tests/ai-completion-test.el       new
emacs.d/tests/ai-context-test.el          new
emacs.d/tests/ai-tools-test.el            new
emacs.d/tests/ai-review-test.el           new
emacs.d/tests/ai-integration-test.el      new
emacs.d/tests/keybindings-test.el
emacs.d/tests/modules-test.el             as required
emacs.d/tests/smoke-test.el               as required
```

### 20.3 Documentation

```text
emacs.d/README.md
emacs.d/docs/architecture.md
emacs.d/docs/keybindings.md
emacs.d/local.el.example
AGENTS.md                                 only if needed to document the new boundary
```

Do not reformat or refactor unrelated dotfiles.

---

## 21. Testing Requirements

All tests must run without network access and without a real LLM.

### 21.1 Context and snapshot tests

1. Current unsaved buffer content is captured instead of disk content.
2. Snapshot widens a narrowed buffer.
3. Text properties are excluded.
4. Duplicate file entries are deduplicated.
5. Region markers capture the expected text at request start.
6. Binary files are rejected.
7. Oversized files and aggregate contexts are rejected with named diagnostics.
8. Sensitive file detection triggers the configured confirmation path.
9. Non-file buffers are read-only in edit sessions.
10. File-handler-aware paths are not normalized through unsafe local-only logic.

### 21.2 Tool tests

1. `list_context_files` returns only authorized entries.
2. `read_file` returns proposed state and bounded ranges.
3. `search_files` searches proposed state after earlier mutations.
4. Invalid regex input returns a tool error, not an uncaught exception.
5. `replace_text` succeeds on the expected exact match count.
6. `replace_text` rejects zero matches.
7. `replace_text` rejects unexpected duplicate matches.
8. Mutation rejects a stale proposal revision.
9. `set_file_content` respects size limits and revisions.
10. `create_file` rejects absolute paths.
11. `create_file` rejects `..` traversal.
12. `create_file` rejects symlink escapes.
13. `create_file` rejects existing paths.
14. Tools cannot access paths outside the session.
15. Tool budget exhaustion terminates the round.
16. Tool operations do not change live source buffers.

### 21.3 Session/request tests

1. Loading AI modules does not dispatch a request.
2. Public commands are autoloaded without loading gptel on normal startup.
3. Ask uses explicit context before fallback region/buffer.
4. Review provides no mutation tools.
5. Edit passes only request-scoped workbench tools.
6. Mutation tools are not added to global gptel chat tools.
7. A second edit session cannot silently replace an active proposal.
8. Provider failure transitions to failed.
9. Cancellation removes incomplete staged state and owned temporary resources.
10. No test emits or requires an API key.

### 21.4 Proposal and apply tests

1. Successful model tool calls change only proposed content.
2. Source buffers remain unchanged before apply.
3. Source disk files remain unchanged before and after buffer apply.
4. Diff display is generated from original/proposed state.
5. Applying an existing file changes its buffer and leaves it modified.
6. One ordinary undo restores the pre-apply buffer state.
7. Applying a new file creates an unsaved visiting buffer but no disk file.
8. A source buffer changed after snapshot is detected as stale.
9. A disk file changed after snapshot is detected as stale.
10. Per-file apply works independently.
11. Apply All preflights every file before mutation.
12. Apply All rolls back all buffers after an injected mid-apply failure.
13. No apply path calls save or direct source-file write functions.
14. Read-only buffers are refused.
15. Discard leaves all source buffers unchanged.

### 21.5 Keybinding and integration tests

1. `C-c A` resolves to the AI prefix map.
2. Every documented AI key resolves to the expected command.
3. Existing lowercase `C-c a` remains `org-agenda`.
4. The keybinding contract and docs match implementation.
5. Module load tests include new first-party features.
6. Full repository suite passes through:

```bash
./run-emacs-tests.sh
```

### 21.6 Inline completion tests

1. Normal startup does not load Minuet or dispatch a completion request.
2. Enabling the local mode with valid stub configuration does not request until eligible typing idle or manual invocation.
3. Manual completion sends bounded prefix/suffix from the current live unsaved buffer.
4. Request payload contains no workbench context, project file, other buffer, or secret.
5. Minibuffer, read-only, sensitive, oversized, ineligible-mode, and default-remote cases are blocked.
6. Active `completion-in-region-mode` blocks automatic AI dispatch.
7. Activating completion-in-region while ghost text is visible dismisses the AI suggestion.
8. No implementation/test refers to a private `corfu--*` symbol.
9. Point movement or a modified tick change makes an older response stale.
10. A newer request generation prevents an older response from displaying.
11. At most one authoritative in-flight automatic request exists per buffer.
12. Full acceptance inserts the visible suggestion, leaves the buffer modified, performs no save, and can be undone predictably.
13. Line and word acceptance insert only the intended prefix of the suggestion.
14. Dismiss and cancellation leave buffer text and undo history unchanged.
15. `TAB` and other transient keys fall through to ordinary behavior when no suggestion is active.
16. Buffer kill, major-mode change, and disabling the local mode clean timers, requests, and overlays.
17. Provider errors are visible through diagnostics and do not create automatic retry loops.
18. All Minuet/provider calls are stubbed; no test needs network or credentials.

### 21.7 Locked package workflow

After adding gptel and Minuet:

1. test the unlocked updated package graph using the repository-prescribed environment;
2. write the Elpaca lock through `M-x gsmlg-elpaca-write-lock-file`;
3. review the lock diff;
4. run the complete suite normally;
5. commit package declaration and lock update together.

---

## 22. Acceptance Criteria

Version 1 is accepted only when all of the following scenarios work.

### AC-1: Basic chat

`C-c A g` opens a normal provider-neutral gptel chat without affecting Agent Editor MCP or starting a terminal agent.

### AC-2: Ask sees unsaved content

1. Open a file.
2. make an unsaved edit;
3. invoke ask with no explicit context;
4. inspect the stubbed/request payload in test or a real model response;
5. verify the request contains the unsaved buffer content.

### AC-3: Safe region rewrite

A region rewrite is previewed and is not inserted until the user accepts it through gptel's rewrite UI.

### AC-4: Multi-file staged edit

1. Add two live file buffers to context.
2. Start an edit task.
3. Simulate/model tool calls that change both proposed files.
4. Verify neither live buffer changes while the agent runs.
5. Open the proposal and inspect both changes.

### AC-5: Explicit buffer apply

Applying one proposed file updates only that buffer, marks it modified, does not save it, and can be undone in one logical undo action.

### AC-6: Transactional Apply All

Applying all eligible files updates every target buffer. An injected failure during application rolls every target back and leaves no half-applied state.

### AC-7: Stale protection

Changing a source buffer after the proposal was created causes apply to be refused without overwriting the newer user edit.

### AC-8: Staged new file

A model may stage a new file under the approved local creation root. Applying it opens a modified visiting buffer while the target path remains absent from disk until the user saves normally.

### AC-9: Capability boundary

The edit model can only list, read, search, replace, rewrite, create, and finish within authorized session state. It cannot run shell, Git, web, Elisp, MCP, or arbitrary file operations.

### AC-10: Startup and reproducibility

Normal startup remains deferred and network-free, offline startup works after package installation, the package lock is committed, and the complete test suite passes.

### AC-11: Manual inline completion

In an eligible programming buffer containing unsaved text, `C-c A i` sends only bounded current-buffer prefix/suffix context and displays a ghost suggestion without modifying the buffer.

### AC-12: Automatic completion and CAPF priority

With local or global AI completion enabled, eligible idle typing may display a suggestion. Opening ordinary CAPF/Corfu completion suppresses or dismisses the AI suggestion, and existing completion keys continue to work unchanged.

### AC-13: Explicit, undoable acceptance

Accepting the full visible suggestion changes only the current buffer, leaves it modified and unsaved, and can be undone. Dismissing the suggestion leaves buffer text unchanged.

### AC-14: Stale and sensitive completion safety

Responses produced for an older point/tick generation are ignored. Sensitive, oversized, read-only, minibuffer, and default-remote buffers do not dispatch automatic completion requests.

---

## 23. Implementation Milestones

Cursor should implement in the following order. Each milestone must leave tests passing before continuing.

### Milestone 1: Package and application integration

- Add locked, deferred `gptel` and `minuet` declarations.
- Add deferred `gsmlg-ai` and `gsmlg-ai-completion` entry points and autoloads.
- Implement `gsmlg-ai-chat`, `gsmlg-ai-menu`, and `gsmlg-ai-rewrite-region`.
- Add the `C-c A` prefix and contract tests.
- Confirm normal startup loads neither provider package and makes no network request.

### Milestone 2: Inline completion

- Implement `gsmlg-ai-completion.el` as a policy wrapper over Minuet public APIs.
- Implement manual, buffer-local automatic, and global eligible-buffer modes.
- Implement bounded current-buffer context, eligibility blockers, sensitive/remote limits, debounce/throttle, diagnostics, and cleanup.
- Implement active-suggestion acceptance/dismissal keys.
- Integrate with public `completion-in-region-mode` state so Corfu/CAPF always has priority.
- Complete offline Minuet stubs and inline-completion acceptance tests.

### Milestone 3: Context and one-shot read workflows

- Implement context records and manager UI.
- Implement buffer/file/region/Dired/project-file selection.
- Implement snapshot, limits, sensitive warnings, and buffer-first semantics.
- Implement ask and review with stubbed gptel requests.

### Milestone 4: Edit session and restricted tools

- Implement session records/state machine.
- Implement request-scoped tools and tool budget.
- Implement prompts and gptel tool request wiring.
- Verify all edits remain in proposed content.

### Milestone 5: Proposal review and application

- Implement proposal UI and comparisons.
- Implement stale detection.
- Implement per-file apply, new-file apply, undo boundaries, and no-save behavior.
- Implement transactional Apply All and rollback tests.
- Implement proposal revise and discard.

### Milestone 6: Hardening and documentation

- Complete cancellation/error cleanup for both LLM planes.
- Complete remote/live-buffer edge cases within version 1 scope.
- Update README, architecture, keybindings, and local example.
- Update Elpaca lock.
- Run lint/startup/full tests.
- Remove dead code and any accidental private gptel, Minuet, or Corfu dependencies.

---

## 24. Definition of Done

The work is complete when:

1. all version 1 functional requirements are implemented;
2. no non-goal has been added accidentally;
3. no Agent Editor MCP code was modified for this feature;
4. no `agent-shell`, ACP, `macher`, `gptel-agent`, or provider-locked Copilot dependency was added;
5. gptel and Minuet are deferred, pinned, and used through public APIs;
6. multi-file edits remain staged until explicit apply;
7. apply changes only buffers and never saves;
8. stale proposals cannot overwrite newer user changes;
9. Apply All is transaction-like and rollback-tested;
10. new files remain unsaved buffers until the user saves;
11. tool capabilities are request-scoped and context-confined;
12. all new code uses lexical binding, standard feature declarations, and `gsmlg-` public names;
13. keybinding contract and documentation are synchronized;
14. full offline-capable tests pass;
15. `elpaca-lock.el` is intentionally updated and reviewed;
16. repository documentation describes configuration and use;
17. inline completion uses bounded current-buffer-only context;
18. automatic completion coexists with Corfu/CAPF without private API usage;
19. ghost suggestions make no change before acceptance;
20. accepted suggestions are unsaved and undoable; and
21. stale, sensitive, remote-by-default, read-only, and oversized completion cases are covered by offline tests.

---

## 25. Deferred Roadmap

These capabilities may be considered only after version 1 is stable:

- multiple named/persistent contexts;
- multiple concurrent edit proposals;
- per-hunk accept/reject;
- three-way merge/rebase of stale proposals;
- file rename/delete operations;
- persistent proposal history under XDG state;
- context token estimation;
- Eglot/Xref-assisted read-only context suggestions;
- optional format command after apply, still user-triggered;
- remote new-file support;
- extraction into a standalone distributable package;
- Minuet Duet or another next-edit-prediction/NES workflow;
- project-aware or cross-file inline completion context;
- multi-location completion acceptance;
- GitHub Copilot-specific adapter as an optional alternative backend;
- persisted completion history or analytics;
- a custom GSMLG completion transport/overlay engine replacing Minuet.

None of these should be partially introduced into version 1 without explicit approval.

---

## 26. Version 1.1 Change Summary

Version 1.1 adds first-class inline AI code completion to the previously approved Workbench PRD. It introduces Minuet as the completion engine and `gsmlg-ai-completion.el` as the GSMLG policy/integration layer. The completion plane is intentionally independent from gptel workbench context and multi-file transactions.

The main added constraints are:

- current-buffer-only bounded prefix/suffix context;
- no project or workbench context reuse;
- ghost text with explicit full/line/word acceptance;
- no buffer mutation before acceptance and no automatic save;
- deterministic CAPF/Corfu completion priority;
- no private Corfu, Minuet, or gptel APIs;
- sensitive, remote-by-default, oversized, read-only, and stale-response blocking;
- deferred package loading and no startup request; and
- complete offline tests for scheduling, arbitration, acceptance, cancellation, and cleanup.

---

## Appendix A: Cursor Implementation Directive

Use this PRD as the implementation contract.

Before editing:

1. Read repository `AGENTS.md`.
2. Read `emacs.d/docs/architecture.md`.
3. Inspect `emacs.d/init.el`, `gsmlg-app-packages.el`, `gsmlg-apps.el`, `gsmlg-keybindings.el`, `gsmlg-paths.el`, and the existing test runner.
4. Inspect the pinned/current gptel public APIs for `gptel-request`, request-scoped tools, cancellation, presets, and `gptel-rewrite` before writing integration code.
5. Inspect the pinned/current Minuet public APIs for provider setup, manual/automatic suggestion mode, block predicates, request cancellation, active suggestion map, full/line/word acceptance, and FIM configuration.
6. Inspect the repository's current `gsmlg-completion.el` and keybinding contract. Preserve Corfu auto-completion, CAPF sources, `M-/`, `C-M-/`, and existing `M-i` behavior.

Implementation rules:

- Work only in the Emacs subsystem and related documentation/tests.
- Preserve the existing orchestrator and responsibility boundaries.
- Implement milestones in order and keep tests green.
- Do not create a second provider client or generic agent framework.
- Do not modify Agent Editor MCP.
- Do not expose shell, Git, web, arbitrary file, or Elisp tools.
- Never allow the model to write source buffers or disk directly.
- Treat original/proposed content records as the transaction truth; diff is display only.
- Use public gptel APIs and request-scoped tools.
- Use Minuet only through public APIs and keep it behind `gsmlg-ai-completion.el`.
- Keep AI inline completion parallel to CAPF; do not register it as a CAPF and do not inspect `corfu--*` internals.
- Send only bounded current-buffer prefix/suffix for inline completion. Never attach workbench or project context.
- Do not enable experimental next-edit prediction or project-wide completion.
- Add comprehensive offline ERT tests using stubs instead of real model calls.
- Update the Elpaca lock only after exercising the updated package graph.
- At completion, report changed files, architecture decisions, test commands/results, and any requirement that could not be met exactly.

Do not stop after adding simple gptel chat configuration. The deliverable includes both the complete version 1 inline AI code-completion workflow and the staged multi-file editing workflow, including their integration and safety tests.
