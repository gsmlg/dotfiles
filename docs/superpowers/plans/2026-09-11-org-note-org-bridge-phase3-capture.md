# Org Note Org Bridge Phase 3: Capture

> Execute this plan after Phase 2. Preserve the bridge default-disabled until
> Phases 3-7 and the required live idempotency gates are complete.

## Goal

Route the existing `t`/`n`/`b` Org Capture templates and
`gsmlg-org-capture-frame` through Org Note document mutations, with no write
under `gsmlg-org-directory`, frozen-wire replay, and fail-closed recovery.

## Scope

- `emacs.d/lisp/gsmlg-org-note-org.el`
- `emacs.d/lisp/gsmlg-org.el`
- `emacs.d/site-lisp/org-note/org-note-operation.el` only where a missing
  capture-specific primitive is required
- new focused tests under `emacs.d/tests/`
- XDG state helpers only if the existing `gsmlg-paths` API cannot represent the
  private recovery directory

Do not implement refile, clock, archive, or cross-document movement here.
Keep `gsmlg-org-note-org-enable` default `nil`.

## Invariants

1. Capture uses a named function target and a dedicated non-file staging buffer.
2. `:prepare-finalize` validates only; remote I/O occurs exactly once from the
   bridge-owned `:before-finalize` commit function.
3. The frozen request stores the exact method, route, query, headers, body
   bytes, checksum, endpoint identity, and operation id.
4. Capture PUTs always carry an empty `lease_proofs` object. Claimed targets
   are refused before journaling or dispatch.
5. A validated response must contain a nonnegative revision for create and a
   strictly advanced revision for update.
6. Every post-dispatch non-success is ambiguous and replayable only with the
   same frozen bytes and operation id.
7. No failure path writes under `~/Documents/org/`.
8. Startup recovery is local-only and performs no network request.

## Tasks

### 1. Capture target configuration and staging tracer bullet

- Add endpoint/workspace/document/path capture configuration with stable ids.
- Replace bridge-active file targets with named function targets and
  `:no-save t` staging buffers.
- Preserve the existing template keys and text shaping.
- Add tests proving bridge-active templates have no `(file ...)` target and
  bridge-disabled templates remain unchanged.

### 2. Semantic finalize and single-attempt state

- Add buffer-local attempt records and the states from the design.
- Implement abort/prepare-finalize guards before any preference lookup or
  network work.
- Isolate native/global before-finalize hooks and invoke one named bridge
  commit function exactly once.
- Add tests for abort-before-dispatch, duplicate finalize, concurrent staged
  capture refusal, and post-freeze source divergence.

### 3. Frozen create/update mutation

- Resolve and validate the configured target; list/configure interactively when
  needed, including archived rows for validation.
- GET existing documents by stable id, reject archived or mismatched metadata,
  and refuse nonempty lease proofs.
- Build one frozen CREATE or PUT envelope with the capture operation id and
  expected revision, then validate the committed response.
- Add unit tests for request shape, revision `0` create responses, strict update
  advancement, conflict refusal, and no local file write.

### 4. Private recovery journal

- Add a mode-0600, versioned, checksum-protected XDG journal with atomic
  same-directory rename and fail-closed parsing/quarantine.
- Persist `prepared` before dispatch, `dispatched` immediately before I/O,
  and committed/persistence-pending states after validated success.
- Never persist fencing credentials or opaque structural fields beyond the
  capture schema.
- Add corruption, permission, checksum, restart, and local-write-failure tests.

### 5. Recovery and cleanup commands

- Implement explicit same-operation-id retry for ambiguous attempts.
- Implement local-only retry for committed-but-journal-pending attempts.
- Keep ambiguous or divergent staging buffers read-only until explicit
  resolution; never rebuild or resend with a new operation id implicitly.
- Add tests for recovery state transitions and bounded network call counts.

### 6. Cross-process reservation and integration gates

- Add the atomic capture reservation directory, owner metadata, stale-owner
  recovery, and nonce-checked release.
- Add process-race tests for one owner and one refusal.
- Run the real service gates against a designated disposable workspace/document:
  same-operation-id CREATE/PUT replay must return the original result without a
  duplicate mutation.

## Validation

Run after each task:

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
./run-emacs-tests.sh
```

Before Phase 3 can be marked complete, also verify the service idempotency
gates against `https://agent-note.gsmlg.net` using only a disposable target.
Do not use workspace `114757c0-5a49-4d54-ba36-752288b7eba3` for mutation tests.

