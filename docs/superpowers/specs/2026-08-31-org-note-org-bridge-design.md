# Org Note as Org data source (bridge) design

Status: Draft for review (revised after blocking review)
Date: 2026-08-31
Revised: 2026-09-01

## Problem

Standard Org entry points (`org-agenda`, `org-capture`, `org-refile`,
`org-todo`, `org-clock-*`, archive) still read and write local files under
`gsmlg-org-directory` / `gsmlg-org-agenda-files`. Org Note already owns the
canonical workspaces, documents, agenda index, transitions, leases, and
document archive APIs. Users expect `C-c a`, `C-c c`, and related muscle
memory to operate on Org Note data only.

A partial feed append for agenda still leaves local files in
`org-agenda-files`, so agenda looks unchanged. An earlier draft of this
bridge was not implementation-ready: refile was non-atomic, feed entries
could not be located in source documents, capture could still write local
files, TODO keywords were incompatible with server states, and cold start
could skip bridge activation.

## Goals

- Make Org Note the data source for these Org workflows in one delivery,
  with blockers resolved explicitly (including narrowed refile scope):
  1. Agenda (`C-c a`)
  2. Capture (`C-c c` / Alfred capture frame)
  3. Refile (same-document only; see Refile)
  4. TODO state changes
  5. Clock in/out/cancel/goto (current Emacs session only)
  6. Archive
- Keep existing keybindings and command entrypoints where possible; bridge
  behavior behind them.
- Prompt to configure workspaces (and capture targets) when unset; never
  fall back to local GTD files for bridged flows.
- Avoid unconditional network access at Emacs startup.
- Preserve native `M-x org-note-*` commands and the vendored package API
  contracts.

## Non-goals

- Cross-document atomic refile until the server exposes an atomic move (or
  an equally safe two-phase protocol). This delivery must not ship
  dual-PUT move.
- Refile / structural edit starting from agenda feed rows (item id only,
  no source offset or stable marker).
- Using Org Note for Babel or treating a full local mirror of every
  document as the source of truth.
- Fine-grained GTD keyword mapping tables beyond making `org-todo-keywords`
  recognize configured Org Note states.
- Cross-restart clock recovery. The vendored client intentionally keeps
  fencing tokens in memory and relies on server-side lease expiry after Emacs
  exits; this delivery does not persist or reconstruct them.
- Automatic clock switching between two items. Users explicitly end the
  current claim before claiming another item.
- Multi-row/bulk TODO or archive from agenda. This delivery permits one
  identified row per mutation so partial batch success cannot occur.
- Concurrent bridge captures. One active capture attempt at a time protects the
  single durable recovery journal; ordinary non-bridge capture is out of scope
  because all configured templates are bridged in this delivery.
- Capture into a document for which this Emacs process holds any live lease
  proof. Durable capture recovery must never serialize fencing credentials; the
  user releases that claim or chooses another inbox first.
- Inferring an item id from an id-less heading in an Org Note document.
- Writing local `CLOCK:` drawers or `*_archive.org` / `~/Documents/org/`
  capture targets for bridged flows.
- Startup-time org-note network calls.
- Power-loss or kernel-crash durability of recovery directory entries. Emacs
  30.2 can fsync file contents through `write-region` but exposes no Lisp API to
  fsync a parent directory; this delivery guarantees atomic recovery across
  Emacs/process crashes and does not add a runtime native/external helper.

## Blocking issues and resolutions

| # | Blocker | Resolution in this design |
| --- | --- | --- |
| 1 | Cross-doc refile needs two PUTs (loss/dup on partial failure) | **Out of scope.** Same-document refile only; cross-doc refused with `user-error`. |
| 2 | Agenda has item/document ids but no source heading location | **Refuse refile from feed.** Require an already-open `org-note-document-mode` buffer containing the full body; never locate by title. |
| 3 | Stock capture templates write `~/Documents/org/` before remote success | **Non-file capture targets only.** Finalize runs remote put first; failure keeps capture buffer and asserts no local GTD write. |
| 4 | Server states like `RUNNING` unknown to `org-todo-keywords` | **Must configure compatible keywords this delivery** (defcustom list). No “map later.” |
| 5 | Cold or indirect agenda/capture entry may skip the bridge | **Named around advice on the original commands.** It is installed without network access before the commands load and covers keys, `M-x`, speed commands, and Alfred. |
| 6 | Retrying capture finalize can append twice | **One frozen mutation per attempt.** Reuse its operation id and payload; a confirmed commit is never sent again. |
| 7 | Native refile assumes a file-backed target | **Custom same-buffer engine.** Select markers only in the current Org Note buffer, build the result in memory, and perform one document put. |
| 8 | Clock restart recovery needs unavailable credentials/API | **Session-only.** Do not persist fencing tokens or claim cross-restart recovery. |
| 9 | Release-then-claim can lose the previous clock | **No automatic switch.** Refuse a second clock-in until the current claim is explicitly ended. |
| 10 | Last-good agenda cache may be stale or from other workspaces | **Atomic, metadata-bound snapshot.** Reuse only when schema and workspace ids match; refresh on redo. |
| 11 | Capture abort enters prepare-finalize before Org checks `org-note-abort` | **Abort guard first.** Prepare-finalize returns before attempt creation or network dispatch. |
| 12 | Bridge clock state can outlive the registered lease | **Registry is authoritative.** Reconcile presentation state before every display/action and after lease changes. |
| 13 | Revision-bound item mutations can use stale or foreign context | **Shared preflight.** Fetch context immediately before mutation; validate identities, revision, and lease proof. |
| 14 | Atomic feed rename leaves an already visiting buffer stale | **Synchronize the generated buffer.** Safely revert it before any agenda rebuild. |
| 15 | Agenda clock/archive commands mutate or reject before lower-level advice | **Intercept agenda entrypoints directly.** Do not enter native marker/file mutation paths. |
| 16 | Existing Org buffers retain old TODO regexps | **Apply before mode setup and recompute live bridge buffers** when state configuration changes. |
| 17 | Full-document transform commands can save unrelated drafts or overwrite in-flight edits | **Require a clean transform start and preserve post-snapshot edits.** Normal save is an explicit separate mode. |
| 18 | Refile / id-less TODO PUT can commit before an ambiguous failure | **Frozen mutation attempt.** Reuse its operation id or reconcile before another mutation. |
| 19 | Claim/release can commit before an ambiguous failure | **Frozen clock attempt.** Block further clock actions until same-id retry resolves it. |
| 20 | Release helper forgets a lease before validating a successful response | **Strengthen the vendored helper without changing its signature.** Validate identity/result first, then forget. |
| 21 | Native capture `:clock-resume` requires native clock markers | **Do not use native clock template properties.** A bridge post-commit hook only revalidates existing session state. |
| 22 | Agenda TODO/bulk dispatch can continue native mutation after transition | **Intercept `org-agenda-todo` and bulk dispatch directly.** Single-row only. |
| 23 | Nested redo-all can refresh twice | **One dynamically scoped refresh owner** covers agenda, redo-all, nested redo, and mutation-triggered rebuilds. |
| 24 | Capture path alone cannot call document APIs | **Persist stable workspace/document ids.** Path is validated metadata, not lookup identity. |
| 25 | Identified transition can commit before an ambiguous result or failed refresh | **Frozen transition attempt.** Validate every response, mark committed before UI refresh, and retry only with the same operation id. |
| 26 | Diverged post-commit text can overwrite the confirmed remote mutation on save | **Post-commit conflict gate.** Block normal save until explicit compare/reload/rebase resolution. |
| 27 | Capture dispatch precedes native before-finalize semantics | **Two-phase semantic finalize.** Complete shaping/hooks before freezing and dispatching. |
| 28 | TODO target selection is undefined without native mutation | **Pure target resolver.** Define every supported argument/fast-key case before transition. |
| 29 | Pending capture recovery can be lost before Customize persistence | **Durable XDG state journal.** Persist the frozen attempt before dispatch and clear it only after remote and local persistence succeed. |
| 30 | Feed publication and redo both rebuild agenda buffers | **Single rebuild owner.** Publication replaces/reverts only; the outer caller rebuilds exactly once. |
| 31 | Concurrent captures can overwrite the single recovery journal | **Single-active-attempt guard.** Refuse a second bridge capture before staging until the first finalizes, aborts safely, or resolves recovery. |
| 32 | Org shift commands pass TODO arguments `right` / `left` | **Pure bidirectional cycling.** Resolve them before mutation using the configured ordered state sequence. |
| 33 | Exact Capture journal would serialize lease fencing tokens | **Refuse claimed inbox targets and whitelist structure.** Capture schema permits only an exact empty proof object and no credential field; opaque user source is not scanned as structure. |
| 34 | Non-Capture ambiguous attempts disappear after crash | **Per-operation nonsecret crash marker.** Restart blocks related mutations and requires explicit remote reconciliation; it never blind-replays without in-memory credentials/payload. |
| 35 | Journal can recover as pre-dispatch `staged` after an uncertain crash boundary | **Durable state transitions.** Distinguish `prepared` from conservatively `dispatched`; recovered dispatched/legacy staged is ambiguous. |
| 36 | Capture/global before-finalize hooks can mutate after remote commit | **Enforce hook isolation.** Dynamically suppress the native global hook after running all supported shaping before freeze. |
| 37 | Capture create/update lacks a committed-response validator | **Use the existing document mutation wire contract.** Validate the exact document revision entry; same-id idempotency remains an integration gate. |
| 38 | Transition/release revision checks can accept stale context | **Operation-specific revision invariant.** State-changing transitions advance; lease-only or already-current-target release may retain revision only after lease/state reconciliation. |
| 39 | Interactive nil TODO cannot choose fast-key mode | **Honor `org-use-fast-todo-selection`.** Interactive nil prompts when enabled; other nil calls cycle. |
| 40 | Post-commit compare/reload/rebase state transitions are undefined | **Specify each recovery command.** Only explicit validated resolution clears the save gate. |
| 41 | Journal durability/corruption handling is underspecified | **Private versioned fail-closed storage within the Emacs 30.2 capability boundary.** Define permissions, file fsync, atomic rename, checksum, ownership checks, and corrupt-record quarantine; do not claim parent-directory fsync. |
| 42 | Normal document save bypasses mutation attempts | **Route `write-contents-functions` through save-mode full-document attempts.** Preserve normal save semantics with stable operation recovery. |
| 43 | Whole-document archive lacks ambiguity/commit recovery | **Frozen archive attempt plus an archived-inclusive list reconciliation.** Verify archived state before buffer kill or list refresh. |
| 44 | Agenda navigation/refile can still visit or mutate the generated feed | **Intercept every agenda navigation/refile entrypoint.** Open context/canonical document or refuse before native file logic. |
| 45 | Prepare hooks can reinstall a before-finalize hook | **Clear it again at the bridge commit boundary.** No native before-finalize hook runs after remote commit. |
| 46 | Single Capture guard is process-local | **Atomic cross-process reservation directory.** Acquire before configuration/staging and recover stale owners safely. |
| 47 | Remote commit can precede a failed committed-journal write | **`committed-pending-journal`.** Retry local durability only; never resend in the live process. |
| 48 | Verified release may change the helper return value | **Preserve the original POST result.** Context GET is reconciliation-only. |
| 49 | Fast selection `auto` with no valid keys should cycle | **Prompt only with a nonempty validated key map.** Otherwise use ordinary cycling. |
| 50 | “Affected agenda buffers” is ambiguous | **Buffer-local feed provenance.** Rebuild every matching live agenda exactly once; after rename commit, invalidate mismatched/presentation-failed snapshots before feed revert or marker use. |
| 51 | Native Agenda commands can write through markers into the generated cache feed | **Fail-closed feed write firewall.** Make the feed immutable outside controlled publication and reject every unsupported Agenda mutator before native dispatch. |
| 52 | A stale Agenda can retain markers into a newly reverted feed buffer | **Detach immediately after rename commit and before feed-buffer revert.** Remove every row, overlay, restriction, and marker before new feed contents become visible. |
| 53 | Archived documents are not guaranteed to remain readable through direct document GET | **Use the documented list contract.** Exhaustively paginate `list-documents` with `include-archived` and reconcile the exact id. |
| 54 | A transition to the authoritative current state cannot satisfy the revision-advance invariant | **Treat it as a pre-dispatch no-op.** Report “Already STATE” and create no attempt, marker, or mutation request. |
| 55 | Invalid state/customization combinations can construct ambiguous Org keyword tables | **Validate the entire configuration atomically.** Reject invalid states, keys, or archive target while retaining the previous working configuration. |
| 56 | `inhibit-modification-hooks` can bypass a feed buffer's change hook | **Digest-gate every consumption boundary.** Detect silent tampering, invalidate dependent markers, restore only validated cache bytes, and abort before the feed is used. |
| 57 | Emacs 30.2 cannot provide the specified parent-directory fsync | **Narrow the guarantee explicitly.** Use file fsync plus same-directory atomic rename for process-crash recovery; power-loss durability is a non-goal and no external helper is introduced. |
| 58 | Feed rename can commit before buffer revert or Agenda rebuild fails | **Rename is the publication commit point.** Persist generation in the snapshot; post-commit presentation failure invalidates affected buffers and retries presentation only. |
| 59 | Capture/archive buffers can change while synchronous network I/O yields | **Freeze source/tick and guard the dispatch window.** A detected post-commit divergence preserves the buffer and enters explicit local-recovery state. |
| 60 | Exhaustive pagination can receive infinitely many unique cursors | **One bounded pager.** All exhaustive reads enforce cursor/id uniqueness plus page, row, request-limit, and elapsed-time budgets. |
| 61 | Re-encoding a frozen Lisp request does not prove byte-identical replay | **Freeze the actual wire envelope.** Initial dispatch and retry send the same method/route/query/headers and pre-encoded UTF-8 JSON bytes. |
| 62 | Capture create revision `0` is valid under the current client contract | **Accept nonnegative create revisions.** Updates still require strict advancement over the frozen expected revision. |
| 63 | Some Emacs characters are not valid Org fast-selection keys | **Round-trip through Org's parser.** Require printable keys and exact agreement with the resulting `org-todo-key-alist`/trigger map. |
| 64 | Nested Agenda marker actions and global restrictions bypass row guards | **Guard the real entrypoints and detach global state.** Route/refuse context, indirect, link, and restriction commands before feed marker access. |
| 65 | Release with an already-current target can remove a lease without advancing revision | **Freeze whether the target already matches.** Require lease absence/state equality, but allow revision equality only for that case. |
| 66 | Raw API fields can inject Org syntax or recovery-record structure | **Typed serializers with parse-back validation.** Encode feed identities/fields unambiguously and whitelist each recovery schema by operation kind. |
| 67 | The operation layer cannot depend on the full context validator in `org-note.el` | **Extract dependency-neutral DTO validation.** Both operation and UI layers depend downward on a new validation module. |
| 68 | Claim validation accepts a context revision older than its frozen preflight | **Bind the response to the request.** Require response document revision greater than or equal to frozen expected revision before registering the lease. |
| 69 | Two Emacs processes can publish different bytes under the same feed generation | **Endpoint/workspace-keyed files plus a publication reservation.** Allocate generation under a cross-process lock and compare every buffer with the full on-disk descriptor. |
| 70 | A frozen replay can follow a changed client endpoint to another service | **Freeze the absolute destination.** The wire envelope includes the normalized endpoint/URL; replay never resolves it from later configuration. |
| 71 | Org auto-assigns keys to states omitted from an empty/partial fast-key map | **Rebuild the parsed key tables.** Validate explicit mappings, then remove Org's automatic assignments before installing buffer-local triggers. |
| 72 | Release revision validation needs preflight state absent from the public signature | **Use the explicit internal release builder.** Bridge passes validated context; the compatible public wrapper performs its own fresh context preflight. |
| 73 | Identical workspace ids at different endpoints can share a feed or recovery identity | **Endpoint is part of provenance.** Key files/locks and validate snapshots, Agenda buffers, attempts, and recovery against one canonical endpoint identity. |
| 74 | A live process can leak the Agenda publication reservation after presentation failure | **Release in an outer `unwind-protect`.** Every path attempts nonce-checked release; the owner can explicitly recover its own failed release. |
| 75 | Archive classifies a definitive non-committing `409` as ambiguous | **Separate non-commit from uncertainty.** Durably mark definitive noncommit, restore the buffer, and perform local-only guard/marker cleanup. |
| 76 | Feed parse-back verifies only part of the typed DTO | **Compare the entire canonical DTO.** Reject unknown/duplicate properties or any identity, state, priority, title, tag, or timestamp mismatch. |
| 77 | Raw mutation or endpoint-bound reads may follow a redirect to another destination | **Never follow workflow redirects.** Any 3xx ends after the original request; after mutation dispatch it is ambiguity. |
| 78 | Endpoint can change between pager pages or mutation reconciliation GETs | **Freeze one endpoint-bound read context per workflow.** Every request URL derives from it, never from mutable globals. |
| 79 | A candidate fetched before acquiring the publication lock can overwrite newer data | **Acquire before the first fetch.** Hold the reservation across paging, validation, rename, and local presentation finalization. |
| 80 | Unsafe endpoint syntax could persist query credentials in feeds/markers | **Strict endpoint validator.** Permit only canonical HTTP(S) base URLs with no userinfo, query, fragment, or controls. |
| 81 | Public release retry can fail fresh preflight before reaching its pending attempt | **Pending recovery wins.** Check the per-lease pending record first; only a brand-new release performs fresh preflight. |

## Architecture

Bridge layer in first-party config: expand `gsmlg-org-note-agenda.el` or
rename/split into `gsmlg-org-note-org.el` plus focused helpers.

| User entry | Surface | Backend |
| --- | --- | --- |
| Agenda | Around-advised `org-agenda` / redo | Feed file **only**; refresh from agenda API |
| Capture | Around-advised `org-capture` | Non-file staging target → one idempotent create/put |
| Save | `org-note-document-mode` write handler | Save-mode frozen document put with ambiguity/crash recovery |
| Refile | Around-advised `org-refile` | Custom same-document move → one document put |
| TODO | Around-advised `org-todo` and `org-agenda-todo` | Single-item `transition`; frozen document put for an explicitly id-less remote heading |
| Clock | Around-advised `org-clock-*` and `org-agenda-clock-*` | Context-validated `claim` / `release` + heartbeat; session state only |
| Archive | Guarded `org-archive-subtree` and `org-agenda-archive-with` | Explicit document archive; context-validated item transition |

### Local plain `.org` policy

In buffers that are **not** `org-note-document-mode` and **not** the
generated feed / item-context bridge buffers:

- Body text editing remains allowed (native Org editing).
- Bridged commands **refuse**: TODO, refile, archive, clock — with a clear
  prompt to use Org Note.
- Agenda never scans these files.

### Shared validation, pagination, and frozen wire envelope

The bridge and vendored package use three dependency-safe primitives rather
than reimplementing these rules per workflow:

1. A new lower-level `org-note-validation.el` owns symbol-keyed DTO shape,
   identity, document/item/context, lease, revision, and page validation.
   `org-note-operation.el` and `org-note.el` both require it; the operation
   layer never requires the UI layer. Existing private UI validator names may
   remain as compatibility wrappers, and public operation signatures do not
   change. Every server-derived list/context/document buffer and persisted
   capture target also records canonical endpoint identity; a command refuses
   when current configuration differs and requires reopen/reconfigure rather
   than applying an old identity to another service. Endpoint validation permits
   only `http` or `https`, a nonempty canonical host, explicit effective port and
   normalized base path, and rejects userinfo, query, fragment, control
   characters, malformed authority, and every other scheme before any cache,
   marker, or journal write.
2. One pager is used by Agenda views, Capture workspace/document selection,
   archive reconciliation, and every other exhaustive workspace/document read.
   It passes an explicit bounded `limit`, accepts only nil or a nonempty opaque
   string cursor, compares cursors with `equal`, and tracks every seen cursor and
   row identity across pages. Configurable finite ceilings bound page count,
   accumulated rows, requests, and elapsed wall time. A repeated cursor/identity,
   malformed page, or exceeded budget fails closed. Each multi-request workflow
   freezes one endpoint-bound read context containing canonical endpoint and an
   absolute-URL builder at entry; every page, preflight GET, and post-mutation
   reconciliation GET uses it even if timer/process code changes global
   configuration. Endpoint drift never redirects/reclassifies the frozen
   workflow's result; after completion it makes old-endpoint presentations stale
   and blocks further requests until configuration matches again. Before a
   mutation this is a normal abort; after mutation dispatch it is ambiguity.
   Endpoint-bound GET transport also disables redirects; any 3xx fails the page
   without sending a second request and preserves any already-dispatched
   attempt.
3. Before mutation dispatch, an operation-specific typed request is validated
   once, including the exact operation id and the secret policy, then encoded
   once through the client's UTF-8 JSON encoder. Vendored operation code exposes
   internal operation-specific freeze builders plus one explicit
   `org-note-operation--dispatch-frozen` path that accepts the resulting envelope
   and invokes the same operation-specific response validator; bridge attempts
   call this path directly and never inject bytes through dynamic variables.
   Existing public operation functions keep their signatures and delegate to
   the same builder/dispatcher for one-shot callers. The frozen wire envelope
   holds method, canonical endpoint identity and absolute URL,
   complete route/query, relevant headers, and the resulting body bytes plus
   their SHA-256. It also holds a redaction-secret set derived only from typed
   structural fencing-token fields. The raw response/error parser receives that
   set so an HTTP body cannot echo credentials. Redaction secrets remain
   memory-only and are excluded from every non-Capture marker; Capture's set must
   be empty before its replayable envelope may be journaled. A new internal
   raw-body transport sends that exact URL/bytes without invoking
   `json-serialize` again. Initial dispatch and every in-process retry
   use the same envelope. Capture persists the allowed envelope fields and body
   bytes (base64 or another lossless representation); non-Capture crash markers
   continue to omit source and credentials and therefore cannot replay after
   restart. Configuration or actor changes after freezing never rewrite a wire
   envelope or redirect it to a newly configured endpoint. Mutation raw
   transport disables automatic redirects: any 301/302/303/307/308, including a
   same-host redirect, returns after the single original request and enters the
   operation's post-dispatch ambiguity handling; it never sends body, headers,
   or redaction secrets to a second URL. The canonical
   endpoint identity normalizes scheme/host/explicit effective port/base path
   and trailing slashes; it is also stored in every non-Capture mutation marker.
   Recovery displays it and refuses to inspect/acknowledge/send against a
   different current endpoint unless the user explicitly switches back.

### Identified item mutation preflight

TODO transitions, clock claim/release, and item archive share one named
preflight instead of assembling revision-bound calls independently:

1. Resolve workspace/item ids only from generated feed properties, validated
   item-context buffer metadata, or explicit properties in an Org Note
   document. Feed properties must pass the versioned decoder and re-encode to
   the identical canonical value before use. Require the origin buffer's
   endpoint provenance to equal the current canonical endpoint before any GET;
   never infer identity from title or outline position.
2. Immediately before the mutation, call
   `org-note-operation-get-item-context` and validate the response against the
   requested workspace/item ids.
3. When the origin is `org-note-document-mode`, also require the context
   document id to equal `org-note-document-id`; a mismatched origin aborts.
4. Take document id and `expected_document_revision` from that validated
   context, not from agenda text or cached presentation state.
5. If `org-note-operation-find-lease` returns a matching registered lease,
   include its lease proof in transition mutations. Release uses the exact
   registered lease credentials; claim asserts that no current bridge clock
   lease conflicts and uses the validated context identity/revision.
6. After a validated response, reconcile the bridge clock presentation cache
   against the vendored lease registry before refreshing the originating view.
   No error path silently refreshes and replays the mutation.
7. When TODO transition or item archive originates in
   `org-note-document-mode`, require an unmodified buffer before dispatch so a
   server-side source/revision change cannot strand unrelated local drafts.
   Clock claim/release may run with local edits because they do not rewrite the
   document source, but still use the authoritative server revision.

### Identified transition attempts

Identified TODO and item archive transitions use a shared attempt protocol:

1. After preflight, freeze workspace/item/document ids, expected revision,
   target state, optional lease proof, origin buffer/tick, and one explicit
   operation id, then validate and freeze the resulting wire envelope. Dispatch
   through the internal frozen-transition path and its normal response validator;
   its initial dispatch and in-process retry share the frozen bytes. The public
   `org-note-operation-transition` wrapper remains compatible for native callers.
2. Strengthen the vendored transition helper so response validation runs for
   every transition, not only when a registered lease was supplied. A valid
   response must contain schema version 1, matching workspace and operation ids,
   nonempty event ids, `data.context` with matching workspace/item/document
   identity, a document revision strictly greater than the frozen expected
   revision, the requested item state, and a structurally valid or nil lease
   field.
3. Any post-dispatch non-validated outcome is ambiguous. Block another
   transition for that item and resolve only through same-frozen-wire,
   same-operation-id replay while the original process still owns the frozen
   request. Observing
   the target state or event history alone is not proof that this attempt
   committed because the current event DTO has no operation id. Crash recovery
   follows the nonsecret marker protocol below and never fabricates a replay.
4. Set `committed-p` immediately after response validation and reconcile the
   registered lease before any feed/document refresh. A refresh failure reports
   “transition succeeded; view stale” and exposes refresh-only retry; it never
   reissues the transition.
5. For a document-buffer origin, snapshot its source/tick before dispatch. If it
   remains unchanged, GET and validate the committed remote document before
   replacing it. If it diverged while I/O yielded, preserve the live text, GET
   the new remote source/revision into conflict state, and enter the post-commit
   save gate described below. If that GET fails, retain a committed-pending-
   refresh record and block save until refresh-only recovery succeeds. Never
   replace drafts or permit an ordinary save against the new revision blindly.

Shipping this path requires integration coverage that same-id transition replay
returns the original result without applying the transition twice.

### Full-document mutation attempts

Normal document save, same-document refile, and id-less document TODO use one
shared mutation helper with two modes:

1. Both modes require `org-note-document-mode`, complete metadata including a
   canonical endpoint identity matching current configuration, and no
   ambiguity/post-commit conflict gate. Transform mode (refile/document TODO)
   additionally requires `(buffer-modified-p)` nil, preventing an Org command
   from silently saving unrelated drafts. Save mode is invoked by the existing
   write handler and intentionally accepts a modified buffer.
2. Snapshot the original source, modification tick, document id/path,
   expected revision, and lease proofs. In save mode the frozen proposed source
   is that snapshot. In transform mode, build the proposed source in a temporary
   buffer without changing the live buffer.
3. Generate one operation id, validate the typed PUT, and freeze its exact wire
   envelope before dispatch. After dispatch, every non-validated outcome is
   ambiguous unless the server explicitly proves the mutation did not commit.
4. While ambiguous, block further refile, document TODO, and normal save in that
   buffer. Resolution resends the same frozen wire envelope with the same
   operation id while the current process retains the exact payload; never generate a
   fresh blind retry. A GET/source comparison is useful diagnosis but cannot by
   itself prove which actor/operation produced identical content because the
   current document/event DTOs carry no operation id. Crash recovery follows the
   nonsecret marker protocol below.
   Shipping retry requires an integration assertion that repeating the same PUT
   operation id returns the original result without applying twice.
5. The existing document PUT wire contract guarantees only a response object
   whose `document_revisions[document-id]` value is an integer greater than the
   frozen expected revision. Set `committed-p` immediately after validating that
   exact entry. PUT does not echo workspace/operation identity or document
   contents: request association plus the server same-id integration gate owns
   idempotency, and the frozen proposed source is the confirmed `base-source`.
6. If the live source/tick still equals the original snapshot, replace it with
   the proposed source when transform mode changed it, then mark unmodified. If
   timer/process-hook edits made it diverge while I/O yielded, preserve live
   text, update revision/base-source to the confirmed proposal, and mark the
   buffer modified. For save mode those are ordinary later edits and may be
   saved by a new attempt. For transform mode, warn that the remote mutation
   committed while local in-flight edits remain and enter the gate below.
7. An error before `committed-p` preserves the old buffer metadata. An error or
   `quit` after `committed-p` must retain the new revision/base-source and a
   recoverable post-commit record; it must never restore stale metadata.
8. A divergent transform-mode buffer or any post-commit metadata/recovery error
   enters an explicit post-commit conflict state. Save-mode divergence alone
   does not: it represents edits made after the committed snapshot. While the
   gate is set, `write-contents-functions`, refile, and document TODO refuse.
   The user must compare against the frozen confirmed source,
   or against the freshly fetched confirmed source for an item transition;
   reload it (discarding local edits after confirmation), or explicitly rebase
   after manual merge:
   - Compare opens Ediff and changes no source, revision, modified flag, conflict
     state, or recovery record.
   - Reload GETs and validates workspace/document identity and a revision at
     least as new as the recorded committed revision, asks for confirmation,
     replaces live source, sets revision/base-source to that response, clears
     modified/conflict state, and deletes the recovery record.
   - Rebase first performs the same fresh GET/validation and comparison, then
     requires explicit confirmation that the user manually reconciled live text.
     It sets authoritative revision/base-source to the fetched remote version,
     preserves live text as modified, clears the gate, and deletes the recovery
     record so the next save is an explicit reconciled overwrite.
   Only reload/rebase clear the gate; ordinary `C-x C-s` can never overwrite and
   undo the confirmed mutation.
9. `org-note-document--write-contents` and `org-note-document-save` invoke this
   helper in save mode with an explicit operation id. They no longer call
   `org-note-operation-put-document` outside the attempt/marker protocol.

### Mutation attempt persistence and crash boundary

Identified transitions and full-document attempts persist one nonsecret marker
per operation id before dispatch:

1. Atomically store `prepared` with operation kind, canonical endpoint identity,
   workspace/item/document ids, target state or source hashes, expected revision,
   and timestamp. Do not store source text or any lease/fencing credential.
2. Immediately before network dispatch, durably replace it with `dispatched`.
   A crash after this write is conservatively ambiguous even if the request may
   not have reached the server. Recovered legacy `staged` is treated the same.
3. In the live process, the exact payload remains only in memory and same-id
   replay is allowed. On restart, the marker blocks related mutations; an
   explicit recovery command fetches current remote state/source, shows the
   recorded target/hashes/revision, and requires the user to acknowledge the
   authoritative remote result. It never claims operation identity and never
   automatically replays without the original payload/credentials.
4. Normal buffer kill or Emacs exit is refused while a dispatched, ambiguous,
   committed-pending-refresh, or post-commit conflict record remains. Abnormal
   termination is handled by the restart marker above.
5. Delete the marker only after pre-dispatch cancellation, definitive
   non-commit, or explicit completion/reconciliation. After a dispatched request
   returns a validated definitive non-commit, first durably write
   `definitive-noncommit-pending-cleanup`; the live process then retries only
   local guard/marker cleanup. An abnormal crash before that state reaches disk
   leaves the older record conservatively ambiguous. Session-only clock attempts
   are excluded: their secret credentials remain memory-only and server expiry
   is the documented crash recovery.

### Recovery record storage

All Capture journals and nonsecret mutation markers use one fail-closed storage
implementation:

1. Create/verify the parent directory as an owned, non-symlink mode-0700
   directory. Refuse operation if ownership, type, or permissions are unsafe.
2. Create a same-directory temporary regular file exclusively at mode 0600;
   never write through a symlink. Each schema version and operation kind has an
   exact key/type/cardinality/depth/size whitelist; reject unknown or duplicate
   keys, mixed symbol/string-key aliases, noncanonical objects, and any field not
   permitted for that operation. Capture source is one opaque string and may
   contain ordinary text such as `fencing_token`; the structural schema—not a
   substring scan of user text—rejects credential fields. Capture
   `lease_proofs` must be the exact empty JSON object. Non-Capture schemas have
   no source, lease, proof, or token field at all. The record also contains a
   checksum over its canonical serialized body.
3. Bind `write-region-inhibit-fsync` to nil while writing and closing the
   temporary file, then atomically rename within the same directory before
   considering a lifecycle-state update durable for Emacs/process crashes.
   Emacs 30.2 cannot fsync the parent directory from Lisp, so this protocol does
   not claim power-loss/kernel-crash durability and calls no external helper.
   Before rename, the old record remains authoritative; after successful rename,
   the new record is authoritative. Cleanup uses the same directory and never
   broad recursive deletion. Orphan temporary files are validated and
   quarantined/removed by precise recovery rules without replacing the named
   authoritative record.
4. On read, validate file ownership/type/mode, framing, schema version, known
   lifecycle state, operation id, exact whitelist, sizes, and checksum before
   decoding any typed payload or opaque source. A truncated, corrupt,
   unsupported, or structurally secret-bearing record is retained/quarantined
   and blocks affected commands; it is never ignored, replayed, or automatically
   deleted.
5. Tests cover first creation, replacement, `write-region-inhibit-fsync` binding,
   crash between every write/rename boundary, permission/symlink attacks,
   checksum/schema/state/size failures, file write/fsync and rename errors,
   recovery, and precise cleanup. Tests assert no parent-fsync primitive or
   external program is invoked.
6. Capture additionally acquires an atomic `capture.lock` directory (exclusive
   directory creation) before configuration or attempt creation. Its mode-0600
   owner record contains hostname, PID, Emacs start timestamp, and random nonce.
   A matching live owner always causes refusal. An ownerless, incomplete, or
   unreadable lock is conservatively busy—never immediately stale—covering the
   window between atomic directory creation and owner-record publication. Stale
   recovery requires a configured grace interval, repeated unchanged mtime/
   contents observation, no unresolved journal, proof that a recorded local PID
   and start time are no longer live, and explicit confirmation for a missing
   owner. Recovery atomically renames the stale lock aside before acquiring a
   new one. Release occurs only after safe pre-dispatch abort or completed
   journal recovery/persistence. Tests launch two Emacs processes against one
   state directory and cover the mkdir/owner-publication window, owner death,
   PID reuse, ownerless grace, stale lock plus unresolved journal, and cleanup
   races.

### Cold-start activation

- `gsmlg-apps` eagerly loads the inert first-party bridge module and installs
  all named command guards/advice. In particular, around advice on
  `org-agenda` and `org-capture` owns cold-start activation. Installing any
  advice must not load Org Note or perform network I/O.
- On first invocation, each advice function:
  1. `(require 'org-note)`.
  2. Activates the bridge once under a reentrancy guard.
  3. Applies feed-only or non-file capture settings.
  4. Calls the original command.
- Advice is required instead of rebinding only `C-c a` / `C-c c`, because it
  must also cover `M-x`, Org speed command `q`, programmatic calls, and Alfred
  `gsmlg-org-capture-frame`.
- The global keys remain bound to `org-agenda` and `org-capture`, so the exact
  keybinding contract does not change.
- Tests must prove the first `C-c a`, `M-x org-agenda`, speed-command agenda,
  `C-c c`, and Alfred capture in a clean session never read local agenda or
  capture paths.

## Configuration

- `org-note-agenda-workspace-ids` — agenda feed workspaces (existing).
- Capture: canonical endpoint identity + workspace id + stable inbox document id
  (new defcustoms), plus the
  last validated document path for display/create. Interactive configuration
  persists them via Customize. Document id is authoritative; path alone is
  never passed to `get-document` as identity.
- `gsmlg-org-note-todo-states` and `gsmlg-org-note-done-states`: configurable
  server state strings. Bridge activation constructs one Org TODO sequence
  with a `|` boundary; defaults include `TODO`, `RUNNING`, and `DONE` and must
  make `org-todo` operable without startup network access. Install the sequence
  through `with-eval-after-load` before the first bridge Org buffer initializes.
  The defcustom setters safely recompute Org keyword tables/regexps in every
  live generated-feed, item-context, and `org-note-document-mode` buffer.
- `gsmlg-org-note-state-fast-keys`: optional alist from unique character keys
  to configured state strings. Bridge-generated TODO keywords use these keys;
  unknown, duplicate, unmapped, non-printable, whitespace/control, or
  Org-reserved logging keys (`!`, `@`, `/`) are rejected before mutation.
- A single shared validator checks the complete candidate state configuration
  before any defcustom value or buffer-local keyword table changes. The active
  and done lists must each be nonempty; every entry must be a nonempty string
  containing no whitespace/control character or Org sequence syntax (`|`, `(`,
  or `)`); entries must be unique within each list and the lists must be
  disjoint. Fast-key keys must be unique Emacs characters and every mapped
  target must occur in exactly one of those lists; an empty fast-key alist is
  valid. Validation constructs the candidate sequence in a temporary Org
  buffer and runs normal Org keyword setup to prove the state union and every
  explicit mapping parse correctly. Because Org auto-assigns keys to unmapped
  states, the bridge then rebuilds buffer-local `org-todo-key-alist` and
  `org-todo-key-trigger` from only the candidate mappings and verifies that
  normalized result exactly; missing/remapped explicit keys reject the
  transaction, automatic extras are removed, and an empty map installs no
  trigger. The item archive target must occur in the configured done-state
  list.
  Each setter validates its candidate value together with the other current
  values and directs an interdependent change to
  `gsmlg-org-note-apply-state-configuration`. That transaction accepts and
  stages all four candidate values (active states, done states, fast keys, and
  archive target), validates the tuple once, and is the only supported path for
  multi-field migrations; its interactive configuration UI collects every
  value before calling the transaction, rather than invoking setters in
  sequence. The validator precomputes every derived table/regexp before
  committing the values and all live-buffer state as one logical change; an
  unexpected commit failure rolls back from the captured prior configuration.
  Validation or recomputation failure leaves every prior value, TODO sequence,
  regexp, and live buffer unchanged. Activation and every bridged command also
  validate the complete current tuple so invalid direct `setq` cannot bypass the
  fail-closed policy.
- Item archive target state: validated defcustom string naming one configured
  done state (server-defined).
- Clock presentation state: an in-memory record containing workspace, item and
  document ids, title, kind, start time, and the registered lease key. It is
  never serialized and is never authoritative over the vendored lease registry.
- Mutation recovery directory: a private mode-0700 directory under
  `gsmlg-state-directory`, never the repository or `~/Documents/org/`.
  Per-operation records are keyed by operation id. Non-Capture records contain
  only nonsecret identity, state/target, revision, hashes, and timestamps—never
  source text, lease proof, lease id, or fencing token. The single active Capture
  record may contain its opaque exact source and frozen wire bytes only after
  typed request validation proves lease proofs are the exact empty object. The
  operation-kind whitelist contains no credential key; ordinary user source is
  never recursively interpreted or rejected by credential-looking substrings.

Missing required prefs trigger configure-then-continue (same spirit as
`org-note-configure-agenda-workspaces`). Cancel does not fall back to
local GTD files.

## Agenda

1. Force bridge activation before building the agenda.
2. Validate/freeze canonical endpoint and normalized workspace ids, then acquire
   the atomic endpoint/workspace-keyed publication reservation before the first
   agenda request. The outer refresh owner holds it across all paging,
   validation, generation allocation, rename, local revert/rebuild or staling,
   and final release. Only then refresh `scheduled` and `upcoming_deadline`.
3. Fetch both views through the shared bounded pager and convert every row to a
   typed feed DTO before emitting Org text. Workspace/item/document ids use the
   exact property encoding `v1_` plus uppercase hexadecimal UTF-8 bytes; values
   are decoded/validated and required to re-encode identically at every
   navigation or mutation boundary. State must
   be in the configured state union; priority must be nil or a configured Org
   priority character; every API tag is emitted as the collision-free Org tag
   `ONX_` plus uppercase hexadecimal UTF-8 bytes, while the encoded original
   list remains metadata. Title's original UTF-8 bytes remain encoded metadata;
   its display form is control-free/single-line and receives U+2060 WORD JOINER
   when it would match Org's trailing-tag grammar, so it cannot create structural
   tags and is never used as identity. Scheduled/deadline values are parsed as
   one active Org timestamp and rendered
   canonically rather than inserting arbitrary `raw` text. Duplicate
   workspace/item identities within/across the two views must carry identical
   typed data or the refresh fails. After serialization, parse the candidate in
   a temporary Org buffer back into canonical DTOs and compare every field:
   property cardinality and decoded workspace/item/document identities, state,
   priority, ordered original tags, original title bytes, and scheduled versus
   deadline values. Reject unknown/duplicate generated properties or any
   mismatch. Only then write a same-directory temporary file and prepare to
   replace the last-good snapshot. The selected feed/lock names are
   keyed by SHA-256 of the canonical endpoint identity plus normalized
   workspace-id list, so processes using different services or workspace
   selections never overwrite one another.
4. Store snapshot schema version, canonical endpoint identity, normalized
   workspace ids, and a canonical body checksum in its header. Under the
   endpoint/workspace-keyed publication reservation,
   read the current descriptor and allocate generation exactly one greater than
   its valid predecessor (or 1 when absent); never derive generation before
   acquiring the reservation. The checksum covers all bytes after the metadata
   header under fixed UTF-8-unix encoding with one final newline; bridge
   provenance additionally records the SHA-256 of those complete encoded
   published bytes. Buffer digest checks encode with that same coding system and
   never depend on `buffer-file-coding-system`. Activation never truncates or
   replaces a valid snapshot.
5. Before publication or any feed consumption, validate the authoritative
   on-disk header/body and compare its complete descriptor—canonical path,
   endpoint, schema, workspaces, generation, body checksum, and full-byte digest—with every
   visiting feed and dependent Agenda provenance. Also compare each feed
   buffer's complete-byte digest with that descriptor; do not
   rely on `buffer-modified-p` or modification hooks. On a mismatch, first
   invalidate every dependent Agenda as described below, then, under private
   publication authorization, replace the feed buffer from an independently
   schema/workspace/checksum-validated on-disk snapshot and update its digest.
   If no such snapshot exists, kill the feed buffer. In either case abort the
   current command and require a fresh Agenda invocation; never continue using
   or save the suspect bytes. Before rename, inventory every live dependent
   feed/Agenda buffer and its generation, but do not change the old snapshot or
   presentation. Any validation/write/rename failure leaves all old bytes,
   digests, rows, markers, restrictions, and generations intact.
6. The reservation acquired in step 2 uses the same owner PID/start/nonce,
   ownerless window, stale recovery, and ownership checks as the Capture lock
   (but no mutation journal). Revalidate the current descriptor, assign the
   generation, finalize/checksum/fsync the candidate, and keep the reservation
   through local revert/rebuild or fail-closed staling. An outer
   `unwind-protect` attempts nonce-checked reservation release after every
   success, pre-rename error, post-rename presentation error, `C-g`, or nested
   error-handler failure. If release itself fails, the live owner retains its
   nonce and an explicit cleanup command may retry/rename aside only that exact
   owned lock; other processes remain conservative until release or normal stale
   recovery. Atomic rename is the sole
   publication commit point. The new snapshot header
   immediately becomes the authoritative descriptor and last-good generation;
   post-rename errors never roll it back or select the old snapshot. Enter
   `published-pending-presentation`, then before reverting a feed buffer detach
   every marker owned by every dependent Agenda (including
   `org-agenda-markers`, bulk and undo marker objects, row properties, and
   overlays). If `org-agenda-restrict`, `org-agenda-restrict-begin/end`, or a
   restriction overlay points to the feed, remove the restriction without redo,
   detach the global markers, clear `org-agenda-overriding-restriction` and the
   `org-agenda-files` `org-restrict` property, and assert that no restriction
   still references the feed. Safely revert visiting feed buffers under a
   private publication-transition authorization, then validate the new complete
   digest. A failed/quit revert kills or invalidates that feed buffer and makes
   every dependent Agenda a marker-free stale notice. It is a presentation
   failure, not a refresh failure.
7. Every generated-feed buffer is an immutable cache view: set
   `buffer-read-only`, clear `buffer-offer-save`, install a buffer-local
   `before-change-functions` guard that rejects changes unless the private
   publication authorization is bound, and install a `write-contents-functions`
   guard that rejects ordinary saves. The change guard remains effective when
   native code binds `inhibit-read-only`; only bridge publication/revert may
   authorize a change. A buffer-local `pre-command-hook` permits only an audited
   read-only command allowlist and bridge-owned commands, rejecting every other
   interactive command before execution; a `post-command-hook` digest check
   fails closed if an allowed command unexpectedly changed the bytes.
8. Set `org-agenda-files` to **only** the workspace-keyed selected feed file.
9. If workspace ids are unset or configuration is cancelled, use a separate
   endpoint-keyed empty feed file for that invocation; do not overwrite the
   last-good file.
10. On a pre-rename refresh failure, offer the unchanged last-good snapshot only
   when its schema and workspace ids exactly match the current request. If no
   matching snapshot exists, abort. Never silently use a snapshot from another
   workspace set, and never run this fallback after rename commit.
11. `org-agenda`, `org-agenda-redo-all`, nested `org-agenda-redo`, and
   mutation-triggered refresh/rebuild share one dynamically scoped refresh
   owner. Only the outermost entry refreshes the feed; nested calls reuse it.
   After committed publication/revert, that outer owner invokes the native
   build/rebuild exactly once per affected agenda buffer. Before rebuilding an
   eligible buffer, clear it to a marker-free `presentation-pending` state. Only
   a complete successful rebuild installs rows/markers and the new generation.
   Error or `C-g` resets every marker/overlay and leaves a read-only stale notice,
   never partial rows. Successfully rebuilt sibling Agendas remain valid. A
   presentation-only retry rebuilds only failed buffers from the committed
   snapshot and performs zero feed fetches/publications. This applies to normal
   and exhaustive redo-all across agenda buffers.
12. Remove GTD skip-on-`ORGNOTE` so custom commands can see feed headings;
   fine state filtering remains a follow-up.
13. Every bridge agenda buffer records buffer-local provenance: canonical feed
    truename, canonical endpoint identity, feed schema version, normalized
    workspace ids, and published
    snapshot generation. Eligibility for a new publication matches canonical
    feed path, schema, and workspace ids—not the new generation, since existing
    agendas necessarily carry the immediately prior generation. The refresh
    owner rebuilds each eligible live `org-agenda-mode` buffer exactly once,
    preserves selected windows/point, and updates its generation only after
    successful rebuild. A same-path buffer with mismatched schema/workspaces is
    invalidated after publication commit and is never silently rebuilt. A guard
    seeing authoritative disk generation newer than a feed/Agenda buffer accepts
    only the private presentation transition; ordinary consumption fails closed.
14. Every bridge Agenda navigation or mutation guard first rejects a stale or
    `presentation-pending` buffer and digest-validates its provenance/feed
    before inspecting point, regions, rows, or markers. Agenda build/rebuild
    performs the same validation
    before parsing the feed. Around advice on `org-agenda-goto`,
    `org-agenda-switch-to`,
    `org-agenda-show`, `org-agenda-show-and-scroll-up`, mouse navigation, and
    equivalent marker-following entrypoints resolves and strictly decodes feed
    identity properties and opens
    `org-note-item-context`; none may visit/display the generated feed buffer.
15. A separate explicit bridge command fetches item context and opens the
    canonical `org-note-document-mode` document at document scope. It does not
    guess a heading position while the API lacks a source marker.
16. Around advice on `org-agenda-refile` detects bridge provenance and raises
    `user-error` before `org-agenda-maybe-loop`, row removal, marker movement, or
    native `org-refile`. Feed-origin refile remains forbidden.
17. Named around advice also covers `org-agenda-do-context-action`,
    `org-agenda-tree-to-indirect-buffer`, `org-agenda-open-link`, and
    `org-agenda-set-restriction-lock-from-agenda`, including direct,
    programmatic, and movement-command nested calls. In a bridge Agenda,
    context/follow previews route to item context (or safely do nothing when
    follow is off), indirect subtree and restriction creation are refused, and
    open-link never scans the generated source entry. Digest/generation guards
    run before any of these functions read a marker.
18. Agenda mutation dispatch is fail-closed. Only the explicitly bridged
    single-row TODO, item archive, and clock commands may mutate server state.
    Each bridge Agenda buffer uses the same pre-command allowlist gate: audited
    read-only Agenda commands plus explicit bridge commands may run, while every
    unclassified interactive command is rejected before it can inspect a source
    marker. This covers newly bound keys and direct `M-x` execution in the
    Agenda command loop without needing to infer dynamically whether an unknown
    function mutates.
    Named advice rejects unsupported source-writing entrypoints before native
    code touches a row or marker, including schedule/deadline changes, date
    shifts, priority up/down/set, tag/property/effort setters, kill, archive-tag
    toggle, note insertion, and equivalent commands reachable from the active
    Agenda keymap or dispatcher. The initial audited set includes
    `org-agenda-schedule`, `org-agenda-deadline`,
    `org-agenda-do-date-later`, `org-agenda-do-date-earlier`,
    `org-agenda-priority`, `org-agenda-priority-up`,
    `org-agenda-priority-down`, `org-agenda-set-tags`,
    `org-agenda-set-property`, `org-agenda-set-effort`, `org-agenda-kill`,
    `org-agenda-toggle-archive-tag`, and `org-agenda-add-note`. Activation audits
    the installed Agenda keymaps/dispatch tables against the command allowlist;
    a newly discovered binding remains blocked until classified as read-only or
    given an explicit remote protocol. Named advice still protects programmatic
    `call-interactively` paths that bypass command-loop hooks. The digest gate
    detects an edit that suppressed modification hooks and prevents any later
    bridge command from consuming it; arbitrary Lisp that both bypasses command
    advice/hooks and writes the cache path directly is outside the command
    bridge's security boundary.

## Capture

1. Keep template keys `t` / `n` / `b` and Alfred `gsmlg-org-capture-frame`.
   Before configuration or staging, around advice atomically acquires the
   cross-process Capture reservation described below, then checks journals and
   live bridge buffers. If any capture is staged, ambiguous,
   committed/persistence-pending, or awaiting recovery, a second bridge capture
   raises `user-error` without changing the first attempt.
2. Templates use a named function target, a dedicated non-file staging buffer,
   `:no-save t`, a named `:prepare-finalize` validator, and one bridge-owned
   `:before-finalize` commit function. No `(file …)` target under
   `gsmlg-org-directory` is permitted.
3. Each capture buffer owns one current attempt record containing an operation
   id, the frozen source/digest/modification tick and wire envelope once
   prepared, and a state: `staged`, `ambiguous`,
   `definitive-noncommit-pending-cleanup`, `committed-pending-journal`,
   `committed-local-divergence`, or `committed`.
4. Semantic-finalize order:
   - First inspect `org-note-abort` and the attempt state. Before dispatch,
     abort returns without resolving preferences, creating an attempt, or
     sending a request. In `ambiguous` state, abort raises `user-error` and
     preserves the buffer/attempt until same-operation-id retry resolves it. In
     `definitive-noncommit-pending-cleanup`, abort may retry local cleanup but
     never dispatch. In
     `committed-pending-journal`, abort is refused until local durability
     succeeds. In `committed-local-divergence`, abort is refused until the user
     copies/exports the divergent text or explicitly confirms discard. In
     `committed`, abort performs local teardown without another request.
   - `:prepare-finalize` performs only abort/state/config validation and never
     dispatches. Org then completes its native staging-buffer postprocessing and
     `org-capture-prepare-finalize-hook` before any remote source is frozen.
   - Bridge capture buffers prohibit additional template `:before-finalize`
     functions, encrypted-target rewriting, and buffer-local/global
     `org-capture-before-finalize-hook` mutation. Around-finalize explicitly
     binds `org-capture-before-finalize-hook` to nil for the subsequent native
     phase, so the stock hook cannot run after commit. It also intercepts
     `org-capture--run-template-functions` when keyword is `:before-finalize`
     and bridge provenance is present: ignore the mutable template plist,
     directly invoke the fixed named bridge commit function exactly once, and
     clear the plist property before native continuation. The commit function
     first clears any buffer-local hook reintroduced by prepare hooks and
     re-establishes the dynamic nil binding. A prepare hook therefore cannot
     remove, replace, duplicate, or follow the commit. This dispatch point is the
     final semantic step and freezes only after supported shaping is complete.
   - Resolve capture workspace + stable document id (configure if needed).
     For an existing target, interactive configuration uses the shared bounded
     pager (including archived rows for validation) and persists the selected
     endpoint/workspace/id/path tuple. A configured tuple whose endpoint differs
     from current configuration requires explicit reconfiguration. For a new
     target, generate one document id and mark it pending-create.
   - For a pending-create target, create that id/path with the captured heading
     as its initial source in one mutation and persist the target only after
     validated success. A concurrent path conflict triggers explicit re-list /
     reconfiguration; it never creates another id or appends automatically.
   - For an existing target, get by document id, reject archived or mismatched
     workspace/path metadata, and collect lease proofs for that document. If the
     proof object is nonempty, refuse Capture before journaling or PUT because
     exact durable replay would expose fencing credentials. Otherwise append the
     heading in memory and freeze the complete put request with an empty proof
     object, the capture's operation id, and expected revision.
   - Before either create or put is dispatched, freeze source bytes, SHA-256,
     `buffer-chars-modified-tick`, and the exact encoded wire envelope. Construct
     the `prepared` journal from the Capture operation's exact schema; persist
     the lossless body bytes and require the typed `lease_proofs` field to be the
     empty object. Immediately before network I/O, durably replace state with
     `dispatched`, install a buffer-local change guard that only a private native
     cleanup authorization can bypass, and send the frozen raw bytes. Recovered
     `dispatched` or legacy `staged` is always ambiguous, while recovered
     `prepared` is known not to have been sent.
   - Validate success using the existing document mutation response contract:
     `document_revisions[document-id]` must be a nonnegative integer for create
     and strictly greater than expected revision for update. The response is not
     required to echo workspace/operation/event fields. Immediately after this
     check set in-memory state to `committed-pending-journal`, durably update the
     journal to committed, then set `committed`. Native finalize may tear down
     the buffer only after that mark is durable and the divergence check below
     succeeds.
5. A later finalize invocation in `committed` skips the remote mutation. In
   `committed-pending-journal` it first retries local durability and still never
   calls the server. Thus a journal/hook/teardown failure cannot duplicate the
   capture in the live process.
6. Once a mutation is dispatched, every outcome except a validated success or
   a response explicitly known to be non-committing is ambiguous. This includes
   transport failure, `quit`/`C-g` after dispatch, malformed or unvalidated 2xx
   responses, and unexpected post-dispatch exceptions.
7. An ambiguous outcome marks the buffer `ambiguous`, makes the staged entry
   read-only, and blocks editing, normal finalize, and payload rebuild. An
   explicit retry command sends the same frozen method/route/query/headers/body
   bytes with the same operation id; it never refetches and appends again. This
   is the explicit old-operation-id retry policy required by the Org Note
   mutation contract.
   Shipping this path requires an integration test proving that the service
   deduplicates a byte-identical repeated operation id and returns the original
   mutation result without applying it twice.
8. A definitive conflict received before commit keeps the capture editable and
   requires an explicit rebuild against a newly fetched revision; no silent
   replay. First durably mark the journal
   `definitive-noncommit-pending-cleanup`, remove the dispatch guard, and delete
   the old record. Because the request is known not to have committed, rebuilding
   then discards that frozen attempt and creates a new operation id and payload.
9. Immediately after validated commit and before native teardown, compare the
   current source, digest, and modification tick with the frozen values. If they
   differ—even when modification hooks were inhibited—enter
   `committed-local-divergence`, retain the committed journal/reservation, make
   the capture buffer read-only, and signal before native teardown. This state
   never resends or rebuilds the mutation; explicit commands may copy/export the
   divergent text, compare it with the committed source, or confirm discard and
   run cleanup. Only an unchanged buffer may enter a narrowly scoped private
   authorization that lets Org perform its first native teardown bookkeeping.
   An around-finalize error after `committed` keeps the surviving buffer
   read-only. A repeated finalize similarly authorizes internal cleanup only,
   skips the mutation, and requires divergence to be resolved first.
10. The recovery journal remains through ambiguous outcomes and through any
    post-commit Customize/teardown failure. Startup performs only a local journal
    check and no network I/O; it marks capture recovery required. The next
    explicit capture/recovery command blocks conflicting work and asks to
    resolve it. An ambiguous journal is resolved only by same-frozen-wire,
    same-operation-id replay. A journal already marked committed may GET its
    stable document id to complete preference persistence and teardown without
    repeating the mutation.
    For pending-create, persist the target preference after remote commit, mark
    persistence complete, then delete the journal. An Emacs exit or buffer kill
    cannot silently lose the id/operation record.
11. If committed-journal file write/fsync or atomic rename fails, remain
    `committed-pending-journal`, make the buffer read-only, retain the reservation,
    and retry only local journal durability—never remote mutation—in that live
    process. Normal teardown/exit is blocked. After abnormal termination the
    disk record may still be dispatched; same-id replay is then an idempotent
    recovery query and must never re-apply the mutation.
12. On every failure: show a safe error and perform **no** write under
   `~/Documents/org/`.
13. Templates must not set native `:clock-in`, `:clock-keep`, or
   `:clock-resume`; those paths require native markers and drawers before
   prepare-finalize. Capture does not pause an existing bridge claim, so there
   is normally nothing to resume. A custom post-commit hook only reconciles and
   refreshes any pre-existing bridge clock presentation; its failure must not
   resend or roll back the committed capture.

## Refile

1. Around advice intercepts `org-refile` in an already-open
   `org-note-document-mode` buffer. It must not call native file-target
   insertion, because the remote document has no `buffer-file-name`.
2. A custom target picker scans heading markers in the current buffer only.
   It never includes agenda, local file, or another Org Note document targets.
3. Require the shared full-document mutation preconditions, then validate that
   the destination is not within the source subtree and build the moved source
   in the frozen attempt's temporary buffer.
4. Dispatch the attempt once with expected revision and lease proofs. Commit,
   ambiguity, in-flight edits, and post-commit errors follow the shared helper;
   no refile-specific blind retry or whole-buffer replacement is allowed.
5. **Forbidden this delivery:**
   - Cross-document move (non-atomic dual PUT).
   - Refile from agenda feed / item-id-only markers (cannot locate unique
     heading among duplicates or nests).
6. Cross-document refile has no manual bridge fallback; wait for a future
   atomic move API. Unconfigured workspace never falls back to local targets.

## TODO

1. Compatible `org-todo-keywords` installed as part of bridge activation.
2. When item ids are present, run the shared identified-item preflight, pass
   any matching registered lease proof to `transition`, reconcile the lease
   result, then refresh the originating feed/buffer.
3. Both identified and id-less paths call one pure target-state resolver before
   changing text or dispatching:
   - The ordered sequence is active states followed by done states; current
     state comes from validated item context or the remote document heading.
   - Interactive nil first validates the configured fast-key map. With
     `org-use-fast-todo-selection` equal to `auto`, t, or `expert`, show the
     bridge fast-key prompt only when that map is nonempty; `expert` affects only
     prompt confirmation, not target resolution. An empty map falls back to
     ordinary cycling for every option. Noninteractive nil, disabled fast
     selection, or that fallback cycles to the next configured state and wraps
     after the last; bridge items never cycle to an empty server state.
   - `right` uses the same forward cycle as nil. `left` cycles backward through
     the same ordered states and wraps from the first to the last.
   - An explicit string must name a configured state; `done` chooses the first
     configured done state; a positive numeric prefix chooses the Nth state;
     numeric zero uses normal cycling while suppressing local note behavior.
   - `none`, the empty string, negative repeater cancellation, forced logging
     `(4)`, blocker bypass `(64)`, `nextset`, `previousset`, and `(16)` are
     unsupported in this single-sequence/server-policy bridge and raise
     `user-error` before mutation.
   - Fast-key selection resolves through `gsmlg-org-note-state-fast-keys` and
     must produce the same configured target as explicit selection.
   - After resolution and before creating an attempt, crash marker, local text
     edit, or mutation request, compare the target with the authoritative current
     state. Equality raises `user-error` with “Already STATE” and performs no
     mutation; it is not a transition success and does not require a revision
     advance. The authoritative preflight read may already have occurred. This
     rule applies uniformly to explicit strings, `done`, numeric prefixes, fast
     keys, `right` / `left`, and nil cycling.
4. Around advice on `org-agenda-todo` intercepts a bridge agenda before
   `org-agenda-maybe-loop`, source-buffer mutation, or agenda-line
   postprocessing. It permits only the current row, performs one transition,
   and refreshes through the shared agenda refresh owner without calling the
   native implementation.
5. Active-region, marked, and `org-agenda-bulk-action` TODO are refused before
   the first transition; this delivery has no sequential partial-success mode.
6. Plain local `.org`: refuse with prompt (see policy).
7. The bridge never guesses whether an id-less heading is indexed. For an
   id-less heading in `org-note-document-mode`, use the shared full-document
   mutation helper to freeze and put the keyword edit. After confirmed commit,
   report “document text updated; no item transition”; do not claim a transition
   event occurred. Ambiguity, in-flight edits, and post-commit recovery use the
   shared rules instead of assuming every error left the remote unchanged.
8. Clock and item archive require explicit item ids and refuse id-less document
   headings. This limitation is part of the delivery scope, not an inferred
   identity lookup.
9. Unknown state strings: do not pretend success; message clearly.
10. A pre-dispatch or definitively non-committing error leaves visible state and
   metadata unchanged. Ambiguous and post-commit outcomes follow the appropriate
   item-transition or full-document attempt rules instead of claiming rollback.

## Clock

1. Clock-in resolves identity from the current bridge origin, runs preflight,
   and passes its document id/revision to `claim` (default kind `execution`).
   Clock-out/cancel are global: regardless of current buffer, they start from
   the exact active presentation/registered lease key, then fetch and validate
   fresh item context before `release`. Point/current-buffer properties are
   never used for out/cancel. Heartbeats use existing Org Note lease timers.
2. Claim and release each create a frozen attempt containing one operation id
   and the exact context/lease arguments and wire envelope. Claim response
   validation receives the frozen expected revision and requires its context
   document revision to be an integer greater than or equal to that value before
   registering a lease; claim need not advance the document revision. Release
   additionally freezes the validated preflight item state and whether an
   optional target already equals it. The bridge supplies this context to the
   explicit internal frozen-release builder; it is never passed through a
   dynamic variable. A post-dispatch non-validated outcome is ambiguous and
   blocks further clock mutations. Ambiguous claim requires
   same-id replay to recover the fencing token; if replay cannot recover it,
   context may prove a server lease exists but the bridge remains blocked until
   that lease expires. Ambiguous release may resolve by same-id replay or by
   authoritative context proving the lease released/stale, at which point it
   forgets the local registry entry and clears presentation. Until resolved it
   retains presentation as “release pending” without asserting the lease is
   active. Shipping these retries requires integration assertions for same-id
   claim and release deduplication.
3. On validated claim success, record workspace, item and document ids, title,
   kind, start time, and registered lease key in one in-memory presentation
   value. A failed or ambiguous claim does not create local active state.
4. `org-clock-goto` uses the stored workspace/item ids to open
   `org-note-item-context`; duration display derives from the stored start time.
   Header-line hooks render this bridge state without native clock markers.
5. Before every display and clock action, reconcile the presentation value with
   `org-note-operation-find-lease`. If expiry, stale-heartbeat handling, a
   transition, or a native `org-note-item-release` removed/replaced the lease,
   clear the presentation value and header immediately. Native Org Note claims
   that were not created by the clock bridge remain valid leases but do not
   silently become clocks.
6. The vendored `org-note-operation-release` keeps its public signature but
   generates/stores `request-operation-id` before dispatch (even when the caller
   omitted one; an internal per-lease pending-release record retains it), sends
   that exact id, and retains the raw successful POST result as its public return
   value. Bridge clock attempts call the internal frozen-release
   builder/dispatcher directly with their validated preflight context. On every
   native call, the compatible public wrapper first looks up an exact
   lease-key/id pending-release record. If one exists, it uses that record's
   frozen endpoint/read context and wire envelope for authoritative context
   reconciliation or same-id replay; it does not re-preflight against stale
   caller revision, generate a new id, or rebuild the request. Only a brand-new
   release performs its own fresh context GET through the neutral validator,
   requires the requested workspace/item/document and document revision to match
   its arguments, freezes the current state, then builds/dispatches. No hidden
   dynamic state is used.
   The existing release POST response has no
   authoritative documented body contract, so success is verified through the
   existing item-context GET instead of inventing new response fields:
   - after a non-error POST return, GET context for the requested workspace/item
     and run the dependency-neutral full context/lease validator;
   - require context document id to match and the `lease` key to be present. Nil
     proves no current lease. A structurally valid active lease with an id
     different from the frozen released lease is an allowed replacement claim;
     it is neither rejected nor adopted as the bridge clock. The frozen lease id
     itself must be absent (or explicitly terminal), never active;
   - without `target_state`, require document revision not older than frozen
     expected revision; with `target_state`, require item state to equal that
     target. When the target differed at preflight, revision must be strictly
     greater than expected. When it was already current, release is not a no-op:
     the lease still must disappear, but revision equality is allowed.
   Only after POST plus authoritative GET checks pass may the helper forget the
   frozen registered lease, then it returns the original POST result unchanged.
   A replacement lease remains solely server context. POST
   transport/malformed errors, GET failure, mismatched identity/state/revision,
   or active frozen/missing lease signal reconciliation-pending/ambiguity,
   return no alternate context value, and retain the registry entry for
   same-operation-id recovery.
7. A clock-in for a different item while a current bridge claim is active raises
   `user-error`; it never releases automatically. The user explicitly runs
   clock-out/cancel first. A failed release retains the active claim and timer.
8. Direct around advice covers `org-agenda-clock-in`,
   `org-agenda-clock-out`, `org-agenda-clock-cancel`, and
   `org-agenda-clock-goto`. These entrypoints route to bridge operations before
   checking native markers or overlays; they never call the native file-backed
   path for a bridge agenda.
9. Session end forgets local claim state and performs no shutdown network
   mutation. The server lease expires normally. Cross-restart recovery is not
   claimed in this delivery.
10. Bridge clocking never writes `CLOCK:` drawers, native clock history, or
   `org-clock-persist-file`. Bridge activation disables future native clock
   persistence; it does not delete the existing state file. If a legacy native
   clock is already active, bridge clock commands refuse until the user resolves
   it explicitly.
11. Refuse clock-in without item ids and refuse clock on plain local `.org`.
12. Pomodoro is explicitly unsupported in this delivery. Keep `P` bound to
   `org-pomodoro`; around advice detects a bridge agenda and raises a clear
   unsupported `user-error` rather than starting a native local clock. This
   preserves the exact keybinding contract.
13. Existing `org-clock-menu` bindings remain unchanged. In bridge session
   state, around advice presents only supported actions (goto, out, cancel)
   and never invokes native file-backed clock behavior.

## Archive

1. Whole-document archive remains the explicit
   `org-note-document-archive` command (including its existing `C-c C-a`
   binding and confirmations). A subtree command never implies whole-document
   archive. Accepting the existing unsaved-changes confirmation means those
   local drafts are not uploaded and will be discarded only after verified
   archive commit.
2. Whole-document archive uses a frozen attempt: workspace/document/path,
   expected revision, origin buffers, and one explicit operation id are stored
   in a nonsecret recovery marker before POST. After the unsaved-draft
   confirmation and immediately before freezing, snapshot every origin buffer's
   source, SHA-256, and modification tick. Freeze the exact archive wire
   envelope, install the dispatch-window change guard, and retain it through
   reconciliation. A validated HTTP conflict such as stale-revision `409` is
   definitive non-commit: perform no list reconciliation, first durably replace
   the marker state with `definitive-noncommit-pending-cleanup`, then remove the
   change guard, restore the original buffer/list state as editable, and delete
   the marker. A live-process marker-update/delete failure blocks teardown/exit
   and retries local persistence/cleanup only. After abnormal termination, an
   older on-disk `dispatched` state remains conservatively ambiguous and may
   resolve only by same-wire same-id replay; the repeated `409` then records the
   definitive state without rebuilding the request. Every other non-validated
   post-dispatch outcome is ambiguous, keeps the buffer read-only, and permits
   only same-wire same-id replay
   in-process or crash-marker reconciliation after restart.
3. `org-note-operation-archive-document` preserves its signature and successful
   POST return value, but does not authorize UI cleanup from the raw POST body.
   After a non-error POST, use the shared bounded pager over
   `org-note-operation-list-documents` for the frozen workspace with
   `:include-archived t`, following each opaque `next_cursor`, and locate exactly
   one row with the frozen document id. Validate the row's id and path, a nonnil
   positive `archived_at`, and revision strictly greater than the frozen expected
   revision. Do not use direct `get-document` as archive verification because
   its contract does not promise archived visibility. POST/list/page failure,
   a malformed or cyclic cursor chain, duplicate id, exceeded page/row/request/
   elapsed-time budget, missing id after the final page, or metadata mismatch
   remains ambiguous.
4. Mark archive committed only after that exhaustive list reconciliation. Then
   compare each origin's source/digest/tick with the post-confirmation frozen
   snapshot. If unchanged, privately authorize buffer kill and refresh every
   live list/context that displays it. If changed—even through inhibited
   modification hooks—enter `archive-committed-local-divergence`: retain and
   lock the buffer, prohibit saving/refile/TODO against the archived document,
   and allow only compare/copy/export or a new explicit discard confirmation
   before cleanup. Cleanup-only retry never kills divergent text without that
   confirmation. Cleanup/refresh failure is
   reported as “archive succeeded; view stale” with cleanup-only retry; it never
   reissues archive. The operation helper still returns the original POST result
   on verified success.
5. Around advice on `org-archive-subtree` intercepts before native document
   mutation. Around advice on the shared `org-agenda-archive-with` entrypoint
   intercepts before Org removes any agenda row, covering default,
   confirmation, subtree, and archive-sibling agenda commands.
   Marked/bulk agenda archive is refused before any per-row dispatch. Around
   advice on `org-agenda-bulk-action` rejects archive actions while marks are
   active; `org-agenda-archive-with` also checks the marked-dispatch predicate
   before the first transition. Only the current identified row may transition.
6. For an identified item, run the shared preflight and pass any matching lease
   proof to the configured archive/done transition. Before creating an attempt,
   compare the configured archive target with the authoritative current item
   state. Equality raises “Already STATE” and creates no transition request,
   marker, or row change; the preflight context read is permitted. Only after
   validated transition success may the bridge refresh the feed and agenda. A
   pre-commit failure leaves both buffers/rows unchanged; a post-commit refresh
   failure reports success with a stale view and offers refresh-only recovery.
7. An id-less heading in an Org Note document refuses item archive instead of
   guessing identity or archiving the whole document.
8. No local `*_archive.org` targets.
9. Multi-item subtrees: current headline’s identified item only, with clear
   messaging. Plain local `.org` refuses with a prompt.

## Error handling

| Scenario | Behavior |
| --- | --- |
| Prefs unset | Interactive configure; no local file fallback |
| Agenda failure before snapshot rename | Use only the unchanged matching, validated last-good snapshot after confirmation; otherwise abort |
| Agenda revert/rebuild failure after snapshot rename | Keep the committed generation; detach markers and stale/kill failed presentations; presentation-only retry with zero fetch/publication |
| Agenda publication reservation busy/release failure | Busy owner refuses publication; exact live owner retries nonce-checked release, otherwise normal stale recovery applies |
| Pagination cursor/id/budget failure | Abort before mutation; after archive POST retain ambiguity and every buffer/attempt |
| Endpoint validation/provenance mismatch | Refuse before request/file write; require matching endpoint or reopen/reconfigure |
| Endpoint-bound GET or mutation redirect | Never follow; before mutation abort, after mutation dispatch retain ambiguity |
| Network / malformed API | Propagate; before dispatch preserve state, after dispatch apply the capture/full-document/transition/clock ambiguity rule for that attempt |
| Validated `409` revision conflict | Definitive non-commit: abort with no refresh/replay; archive durably marks noncommit, restores the buffer, then performs local-only guard/marker cleanup |
| `C-g` during configure | Keep previous prefs; select the separate empty feed where needed |
| Capture definitive failure | Keep staged buffer; no local GTD write or automatic replay |
| Capture abort before dispatch | Finalize locally with no preference lookup or remote request |
| Capture abort while ambiguous | Refuse and retain the frozen attempt until same-operation-id retry resolves it |
| Capture post-dispatch ambiguity | Freeze read-only and block; explicit same-wire, same-operation-id retry only |
| Capture committed with post-freeze local divergence | `committed-local-divergence`; preserve/lock buffer, never teardown or resend until explicit copy/export/discard resolution |
| Capture target has live local lease proofs | `user-error` before journal or PUT; never serialize fencing credentials |
| Capture reservation held by another live process | `user-error` before configuration/staging; do not touch its lock or journal |
| Capture committed but journal update failed | `committed-pending-journal`; retry local durability only and block teardown/exit |
| Recovered Capture `prepared` | Known unsent; allow explicit cancellation or resume after validation |
| Recovered Capture `dispatched` / legacy `staged` | Treat as ambiguous; resend only the checksum-validated frozen wire envelope with the same operation id |
| Transform-mode mutation starts with local edits/conflict | `user-error`; save, resolve, or discard before refile/document TODO |
| Refile / id-less TODO ambiguity | Freeze the attempt; same-frozen-wire, same-id replay only |
| Normal document save ambiguity | Freeze save-mode attempt; block another save until same-id/restart reconciliation |
| Full-document post-commit local error | Keep confirmed revision/base-source and recovery record; preserve divergent live edits |
| Diverged post-commit document buffer | Enter conflict gate; block save/refile/TODO until compare/reload/rebase resolution |
| Cross-doc or feed refile | `user-error`; no PUT |
| Agenda navigation from feed | Open item context/canonical document; never visit the generated feed |
| Agenda refile from feed | `user-error` before marker/row/file mutation |
| Unsupported Agenda source mutation | `user-error` before native dispatch; feed bytes and visible rows remain unchanged |
| Stale bridge Agenda | After rename commit and before feed-buffer revert/marker use, rows/overlays/markers/restrictions are detached; every entrypoint asks the user to rerun Agenda |
| Feed-backed global restriction or nested marker action | Detach/refuse before feed marker access; never create an indirect/restricted view of the generated feed |
| TODO or item archive already at target | `user-error` with “Already STATE”; after authoritative preflight, no attempt, crash marker, mutation request, text, or row change |
| Invalid state configuration | Reject the complete candidate atomically and preserve previous values, regexps, and live buffers |
| Silently tampered generated feed | Before build/navigation/mutation, invalidate dependent markers, restore only checksum-valid cache bytes or kill the feed buffer, then abort and require rerun |
| Clock-in while another claim is active | `user-error`; do not release current claim |
| Clock release failure | Keep active claim and heartbeat state |
| Clock claim/release ambiguity | Block clock mutations; same-wire, same-id replay/context reconciliation only |
| Claim response revision older than preflight | Ambiguous/invalid; do not register a lease or start heartbeat |
| Registered clock lease expires/is forgotten | Clear bridge presentation state; do not call release with stale credentials |
| Identified transition ambiguity | Freeze operation id/wire envelope; same-wire, same-id replay only |
| Non-Capture crash marker on restart | Block related mutations; explicit remote inspection/acknowledgement, never automatic replay |
| Transition committed, view refresh failed | Report success with stale view; expose refresh-only retry and never reissue transition |
| Whole-document archive ambiguity | Retain buffer/list state; same-wire, same-id in-process replay or crash-marker reconciliation only |
| Archive committed, cleanup/refresh failed | Report success; cleanup-only retry and never reissue archive |
| Archive committed with post-confirmation local divergence | Preserve/lock document buffer; prohibit save and require copy/export or explicit discard before cleanup |
| Agenda item transition/archive pre-commit failure | Preserve feed and visible agenda rows |
| Agenda TODO/archive bulk or active region | Refuse before the first transition |
| Unknown/oversize/corrupt/structurally secret-bearing recovery record | Fail closed and quarantine/retain; never decode payload, replay, or delete automatically |
| Bridged cmd on plain local `.org` | Refuse TODO/refile/archive/clock; editing ok |

## Testing

Must include, with mocks and filesystem assertions where relevant:

- Agenda feed-only; configure-on-empty; cold-start entry through `C-c a`,
  `M-x`, speed command, and Alfred/programmatic paths activates the bridge and
  never reads local agenda/capture files.
- Snapshot publication treats rename as its commit point. Activation and every
  pre-rename validation/write/rename failure preserve the old disk generation,
  visiting feed buffers, and Agenda presentations exactly. Cache reuse succeeds
  only for matching schema/workspace ids; no-cache and mismatched-workspace
  failures abort. After rename, injected revert error/`C-g` leaves the new disk
  generation authoritative, kills/invalidates old feed buffers, and makes every
  dependent Agenda marker-free stale; recovery performs zero network fetches.
  A second failure inside the post-commit error handler still leaves ordinary
  navigation/mutation blocked by authoritative disk generation.
- Two Emacs processes with different canonical endpoints or workspace selections
  use different endpoint/workspace-hashed feed/lock paths. Same-endpoint/
  same-workspace publishers serialize through
  the reservation acquired before their first fetch, allocate strictly
  increasing generations from the descriptor
  read under lock, and recover dead/ownerless locks with the documented owner
  rules. After a later process publishes, the earlier process compares the full
  on-disk descriptor (not only generation or its own buffer digest), invalidates
  its presentation, and never consumes same-generation/different-byte content.
  Reservation release is asserted after success and every injected pre/post-
  rename error/`C-g`/nested-handler failure. A failed release can be retried only
  by the live owner with the matching nonce; it never permanently wedges another
  process after owner cleanup/death.
  A barrier test attempts A-lock/fetch, B-refresh, then A-publish and proves B
  cannot fetch/publish between A's fetch and rename, so a stale candidate cannot
  receive a later generation.
- `org-agenda-redo` and `org-agenda-redo-all` refresh once before rebuilding.
  Normal and exhaustive redo-all share the outer refresh with nested redo calls;
  mutation-triggered rebuilds do not issue a second fetch. Publication performs
  zero rebuilds; fetch and native rebuild counts are asserted separately.
- A second successful snapshot refresh reverts the already visiting generated
  feed buffer before redo; changed rows and markers appear immediately. A
  digest-mismatched generated-feed buffer is never treated as user-owned data:
  dependent Agenda markers are invalidated before it is restored from a
  checksum-valid snapshot (or killed), and the current publication/build aborts.
- Generated feed buffers reject direct edits even under `inhibit-read-only` and
  reject ordinary save/write paths; controlled publication/revert succeeds only
  while its private authorization is bound. Every unsupported Agenda source
  mutator in the audited keymap/dispatcher set fails before native execution,
  with feed bytes, visible rows, markers, and remote request count unchanged.
  Separate tamper tests bind `inhibit-modification-hooks` (including
  `with-silent-modifications`), change bytes, and clear the modified flag; every
  build/navigation/mutation boundary detects the digest mismatch before marker
  dereference, invalidates dependent Agenda state, restores only validated disk
  bytes, and aborts. A corrupt disk checksum kills the feed buffer and also
  aborts. Pre-command allowlist tests reject unknown/newly bound commands before
  execution; post-command digest checks catch a misclassified read-only command.
  Named mutator advice remains effective under programmatic `call-interactively`.
  No suspect bytes are saved or parsed.
- Typed feed serializer tests inject newline/NUL/control text, `:END:`, fake
  properties, trailing fake tags, invalid priority, colon-containing tags,
  malformed/multiline timestamps, unsafe ids, and state outside the configured
  union. Every case fails before rename and preserves last-good. Unicode and
  encoded ids/tags round-trip without collisions. Parse-back canonical DTOs must
  reproduce exact property cardinality, all decoded identities, state, priority,
  ordered original tags, original title bytes, and scheduled/deadline values;
  unknown/duplicate generated properties fail before checksum/publication.
- Two simultaneous live agendas with matching feed/workspace provenance are
  both rebuilt exactly once after mutation; a mismatched snapshot is marked
  stale, has every row/overlay/marker removed before feed revert, and rejects
  navigation/mutation without dereferencing point; an unrelated agenda is
  untouched. Matching prior-generation buffers are eligible and receive the new
  generation only after successful rebuild. If the second rebuild or its error
  handler fails after clearing/partially inserting rows, the first remains on
  the new generation and the second becomes marker-free stale; a
  presentation-only retry rebuilds only the second with zero fetch/publication.
  Feed restrictions detach `org-agenda-restrict-begin/end`, overlays, and
  globals before revert without touching restrictions for unrelated buffers.
  Navigation/switch/show/mouse
  entrypoints never display the feed, and `org-agenda-refile` fails before native
  mutation. Movement with follow-indirect, direct/nested
  `org-agenda-do-context-action`, indirect-tree, open-link, and restriction-lock
  calls never dereference or display the feed and are covered under digest
  mismatch too.
- Capture success uses one mutation and no local write. Confirmed commit plus
  later hook/finalize failure does not resend. Ambiguous retry reuses the exact
  payload/operation id, with an integration assertion that the service applies
  it once; malformed 2xx and post-dispatch `C-g` are ambiguous. Abort before
  dispatch performs no get/create/put; abort while ambiguous is refused and
  retains the attempt; committed abort only cleans up locally. Ambiguous state
  and a committed buffer surviving teardown failure are read-only. Native
  teardown receives write authorization only after the frozen
  source/digest/tick still matches. Timer/process-hook edits during request are
  rejected; `inhibit-modification-hooks` divergence is detected after response.
  Confirmed divergence enters `committed-local-divergence`, leaves the buffer
  and journal intact/read-only, performs one remote call, and requires explicit
  copy/export/discard recovery. Conflict retains the editable buffer without
  replay.
- Starting a second bridge capture is refused while any live attempt or journal
  recovery exists; the first attempt/journal remains byte-for-byte unchanged.
- Two Emacs processes racing to start Capture produce one reservation owner and
  one pre-staging refusal. Tests cover PID reuse, dead/stale owner recovery,
  unresolved journal ownership, the mkdir-before-owner-publication window,
  ownerless grace/confirmation, and reservation cleanup.
- Capture target configuration stores workspace/document ids and validates
  canonical endpoint and path metadata. Endpoint changes force reconfiguration;
  existing-target pagination, new-target create, archived target,
  concurrent path conflict, and reconfiguration are covered. Templates contain
  no native clock properties; post-commit reconciliation neither releases nor
  reclaims the existing bridge clock. Bridge semantic finalize includes all
  supported shaping before freeze, dynamically suppresses native/global
  before-finalize hook rerun, and persists/replays the mode-0600 recovery
  journal across Customize, teardown, buffer-kill, and restart failures.
- Capture response tests accept integer create revision `0` or greater and
  require an update revision strictly greater than expected in the exact
  document-revisions map; negative/float/string/missing/wrong-document entries
  remain ambiguous.
- Capture with nonempty lease proofs is refused before journaling. Initial,
  in-process retry, and restart replay compare method/route/query/headers/body
  byte-for-byte and use the raw transport without a second `json-serialize`,
  even after actor/config/object-order changes. Wire checksum tampering fails
  closed with zero requests. Exact-schema serialization tests prove no lease
  id/fencing token is written while ordinary source text containing those words
  remains allowed. Unknown/duplicate/mixed-key/wrong-type/oversize/deep fields
  and nonempty proof objects fail closed. Recovery tests
  distinguish prepared, dispatched, legacy staged, ambiguous,
  definitive-noncommit-pending-cleanup,
  committed-pending-journal, committed-local-divergence, committed, and
  persistence-complete states. A
  prepare hook that installs a mutating global or buffer-local before-finalize
  hook or replaces/removes the template commit function is bypassed/cleared at
  the fixed bridge dispatch boundary.
  Every journal durability failure after remote success enters
  committed-pending-journal, performs zero additional remote calls in-process,
  and blocks teardown/exit until local persistence succeeds.
- Normal `org-note-document-save` and the write handler use save-mode frozen
  attempts with explicit operation ids, ambiguity/crash markers, response
  validation, and in-flight edit preservation. They never call the raw PUT path
  outside the shared helper.
- Frozen-wire tests cover identified transition, full-document PUT, Capture,
  claim/release, and archive. Initial dispatch and every in-process retry use
  identical endpoint/absolute URL/method/route/query/headers/body bytes and
  operation id; later endpoint changes cannot redirect replay. Public wrappers
  and bridge attempts both use explicit internal freeze/dispatch functions, with
  no dynamic byte injection. Raw HTTP errors that echo a fencing token are
  sanitized from the envelope's memory-only redaction set; Capture requires an
  empty set, and no marker/journal contains it. Capture
  restart recovery alone persists replayable bytes; non-Capture restart markers
  cannot dispatch. The raw transport never serializes a body twice.
- Same-document refile uses current-buffer targets and one put with lease
  proofs from a clean buffer. Existing drafts are refused. Ambiguous retry keeps
  one payload/operation id, with an integration assertion that the service
  applies it once. Confirmed commit uses frozen proposed source as base-source,
  preserves in-flight edits, and retains confirmed metadata after post-commit
  local failure. Diverged post-commit state blocks ordinary save until explicit
  conflict resolution. Response validation checks the existing
  `document_revisions[document-id]` contract; same-id idempotency is asserted at
  integration level because PUT does not echo operation identity.
  Cross-document and feed refile are refused. Crash markers block restart
  mutations without storing source/proofs; recovery never auto-replays absent
  in-memory payload. Compare changes no state; reload and rebase exercise every
  specified metadata/gate transition.
- Duplicate / nested headings: feed refile never picks the wrong node
  (refusal is the safety property).
- TODO keywords include configured active/done server states; identified
  transition success/failure/ambiguity with frozen operation id, unconditional
  response validation, committed-before-refresh behavior, context
  identity/revision, and lease proof;
  id-less remote document edit uses the same clean-buffer, frozen-attempt,
  in-flight preservation, and commit-boundary rules as refile; it validates the
  returned revision but commits the frozen proposed source rather than expecting
  response content. Plain local TODO is refused.
  Opening an Org Note document before first bridge activation and changing the
  state defcustoms both produce updated buffer-local keyword regexps. Target
  resolver tests cover interactive nil with fast selection enabled/disabled,
  `auto`/t/`expert` with populated and empty key maps, noninteractive nil
  cycling, explicit/numeric/done, fast keys, empty,
  `right`/`left`, next/previous set, forced logging, blocker bypass, and
  unsupported arguments without changing live text first. Every supported
  selector resolving to the authoritative current state reports “Already
  STATE” after preflight with zero attempt/marker/mutation-dispatch/text changes
  and no mutation-response revision validation. Configuration tests cover empty
  lists, malformed state strings, duplicates, active/done overlap,
  invalid/duplicate/unmapped fast keys, an archive target outside done states,
  multi-field migration, and atomic rollback of values/regexps/live buffers
  after validation, recomputation, or commit failure. Fast-key tests reject
  `!`, `@`, `/`, whitespace/control/non-printable keys and require the temporary
  Org setup to parse every explicit mapping and the exact state union. They then
  assert bridge rebuilding removes Org's automatic assignments for empty and
  partial maps, preserves only explicit mappings, and gives an empty map no
  trigger. Transition commit
  plus feed/document refresh failure performs refresh-only recovery; a diverged
  document origin enters the save-blocking post-commit conflict gate.
- Clock claim/release, session goto/duration, second clock-in refusal, release
  failure/ambiguity recovery with the same operation id, release-response
  validation before lease removal, authoritative context revision,
  same-id claim/release integration deduplication, lease expiry/native release
  reconciliation, legacy native-clock refusal, and no persistence file
  or drawer writes. Agenda clock in/out/cancel/goto succeed without a native
  marker. Restart recovery is asserted unsupported. `org-clock-menu` exposes
  only bridge actions, and `P` remains bound to `org-pomodoro` while bridge
  agendas refuse it through advice. Global out/cancel are tested from a
  non-Org buffer and use active lease identity rather than point. Release wire
  tests cover POST success followed by authoritative context GET, plus POST/GET
  failure, mismatched identity/state/revision, active frozen lease, nil lease,
  and a different replacement active lease. With
  a changed target state revision must advance; an already-current target still
  releases the lease and permits equality; without a target equality is allowed.
  Bridge and public-wrapper release paths both freeze preflight state through the
  internal builder; the public path's fresh GET rejects identity/revision drift,
  and no dynamic variable supplies the flag. When POST commits but verification
  GET fails, a second native call with stale context finds the pending record
  first and resolves/replays its original endpoint/wire/id with zero new mutation.
  Claim response revision below expected never registers a lease, while equal or
  greater succeeds and same-id replay creates one lease/timer. Validation is
  available when loading `org-note-operation` without `org-note.el`, and module
  load tests prove the neutral validator introduces no dependency cycle. Verified
  success returns the exact original POST object; GET failure returns no context
  substitute and leaves reconciliation pending.
- Whole-document archive tests cover explicit operation id, POST followed by
  exhaustive multi-page `list-documents :include-archived t` reconciliation,
  opaque cursors, exact-id/path match, positive `archived_at`, revision advance,
  duplicate/missing/malformed rows and page failures,
  page/row/request/deadline budgets and maximum request counts, ambiguous
  same-id retry/crash marker, unsaved-change confirmation, no premature buffer kill,
  original POST return compatibility, and cleanup-only retry after committed UI
  failure. Timer/process-hook changes during POST or list are guarded; an
  inhibited-hook divergence after commit preserves a save-blocked document and
  requires copy/export or explicit discard without reissuing archive. The
  verifier never calls direct document GET.
  A validated archive `409` performs no list request, durably records
  `definitive-noncommit-pending-cleanup`, restores the original editable
  buffer/list, and removes its guard/marker without live-process network retry.
  Marker-update/delete failures exercise local-only retry and exit blocking;
  abnormal crash with an older dispatched record resolves only through the same
  frozen wire/id and repeated definitive conflict.
- Explicit document archive, identified item archive-transition, id-less
  remote item refusal, and plain local archive refusal. Every agenda archive
  variant is intercepted before row removal; pre-commit transition failure
  preserves the visible agenda, while committed refresh failure is reported as
  stale-view success. Marked/bulk agenda archive is refused before any
  transition. After its authoritative preflight read, an item already in the
  archive target state performs zero transition requests and creates no marker.
- Direct `org-agenda-todo` advice handles one row without calling native
  postprocessing; active-region and bulk TODO are refused before any transition.
- Keybinding tests continue to assert `C-c a` → `org-agenda` and `C-c c` →
  `org-capture`; behavior is installed behind those commands by advice.
- Shared pager tests cover cursor cycles, infinitely many unique cursors stopped
  exactly at the page budget, cross-page duplicate ids, row/request/deadline
  budgets, and bounded request counts for Agenda, Capture selection, and archive.
  Changing global endpoint between pages never changes the frozen absolute URL.
  Changing it between archive POST and reconciliation GET keeps every remaining
  request and commit verification on the frozen original endpoint; afterward,
  provenance guards refuse presentation/network work under the new endpoint.
  Endpoint-bound GET 3xx responses issue no redirected request; pre-mutation
  workflows abort and post-mutation reconciliation remains ambiguous.
- Recovery-storage tests cover 0700 parent and 0600 exclusive temp creation,
  symlink/ownership rejection, exact schema/checksum validation,
  `write-region-inhibit-fsync` bound nil, file write/fsync and rename failures,
  atomic same-directory rename, process-crash boundaries,
  corrupt/truncated/unknown-state fail-closed behavior, and precise cleanup.
  They assert no parent-directory fsync primitive or external helper is invoked;
  power-loss durability is explicitly not claimed. Golden records cover every
  schema version/operation kind; unknown/duplicate/mixed keys, wrong cardinality,
  excessive depth/size, structural credentials, and orphan temporary files all
  follow the specified fail-closed/quarantine behavior.
- Endpoint-provenance tests open feed/context/document buffers, Capture targets,
  and non-Capture markers under endpoint A, then switch configuration to endpoint
  B with identical workspace/document/item ids. Every consume/mutation/recovery
  path refuses before network dispatch; Capture frozen replay displays and uses
  only its original absolute endpoint after current configuration is explicitly
  switched back and recovery is confirmed.
- Endpoint validation rejects query/fragment credentials, userinfo, controls,
  missing host, malformed port/authority, and non-HTTP(S) schemes before cache,
  marker, journal, or network work. Capture journals contain only the fixed
  non-sensitive header allowlist.
- Raw mutation redirect tests cover 301/302/303/307/308 to same and different
  endpoints, HTTPS downgrade, userinfo, and base-path escape. Exactly one request
  reaches the frozen URL, no body/token is sent to a redirect target, and the
  original operation remains ambiguous under its same-wire recovery policy.
- Update Org tests that assumed local agenda/capture paths.
- Run org-note `run_tests.sh` if the vendored package changes.
- Scoped then full `./run-emacs-tests.sh` before done.

## Migration notes

- Users must configure agenda and capture workspaces/docs before bridged
  commands are useful.
- Capture configuration migrates from path-only settings to canonical endpoint
  plus stable workspace/document ids; endpoint-changed, stale, missing,
  archived, or path-mismatched targets require reconfiguration.
- An unresolved capture recovery journal is processed before new capture work.
  It may temporarily contain the exact frozen source under mode 0600 in XDG
  state, is never treated as an Org data source, and is deleted after recovery.
- Local `gsmlg-org-directory` files remain on disk for manual editing but
  are unused by agenda and refused for TODO/refile/archive/clock.
- Existing `org-clock-persist-file` data remains on disk but is ignored by the
  bridge; migration never deletes or silently resumes it.
- No automatic import of local GTD into Org Note in this design.

## Open follow-ups (explicitly later)

- Server atomic move (or safe two-phase) → cross-document refile.
- Agenda/API markers for stable heading location → feed-origin refile.
- Stable item metadata/source lookup for transition, clock, and item archive
  directly from arbitrary headings in an Org Note document.
- Server-supported fencing-token rotation/reacquisition → optional
  cross-restart clock recovery and atomic clock switching.
- Richer GTD keyword ↔ state mapping and custom-command filtering.
- Bulk archive / multi-item subtree moves.
- Optional local export mirror for offline reading.
