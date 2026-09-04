# Org Note Org Bridge Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make Org Agenda / Capture / Refile / TODO / Clock / Archive use Org Note as the sole data source behind existing Org entrypoints, per the approved bridge design.

**Architecture:** First-party bridge modules advise Org commands without changing the keybinding contract. Vendored `org-note` gains a lower-level `org-note-validation.el`, frozen-wire dispatch helpers, and release/transition response strengthening. Agenda uses an endpoint/workspace-keyed immutable feed snapshot; Capture uses non-file staging plus a durable XDG journal; mutations share preflight + frozen attempts. Clock is session-only (no cross-restart fencing recovery).

**Tech Stack:** GNU Emacs 30.2, lexical-binding Emacs Lisp, Org, ERT, existing `org-note-*` client/operation/document modules, XDG state via `gsmlg-paths`.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-08-31-org-note-org-bridge-design.md` (Approved 2026-09-04). Every task implicitly includes that document’s invariants.
- Minimum Emacs: 30.2.
- No startup network I/O; first command may `(require 'org-note)` and then network.
- Do not fall back to `gsmlg-org-agenda-files` / `~/Documents/org/` for bridged flows.
- Do not ship cross-document dual-PUT refile or feed-origin refile.
- Do not persist fencing tokens or claim cross-restart clock recovery.
- Keybinding contract letters stay on `org-agenda` / `org-capture` (advice wraps them).
- Phases are development checkpoints, not independently releasable features.
  Keep `gsmlg-org-note-org-enable` nil by default until Phases 1-7, the complete
  suite, and every operation-specific service idempotency gate pass.
- **Do not create commits unless the user explicitly authorizes them.** Skip commit steps or stop and ask.
- After touching vendored org-note: run `emacs.d/site-lisp/org-note/run_tests.sh`.
- After first-party Lisp changes: run scoped ERT, then `./run-emacs-tests.sh` before declaring a phase done.

## Phasing (separate detailed plans per phase)

This umbrella plan sequences work. **Only Phase 1 tasks below are fully expanded.** After Phase 1 is green, write/execute the next phase plan before continuing.

| Phase | Deliverable | Plan file |
| --- | --- | --- |
| 1 | Validation module + frozen wire primitives + cold-start advice + feed-only Agenda | this file (Tasks 1–6) |
| 2 | State configuration + local `.org` refuse policy + TODO transition | `docs/superpowers/plans/2026-09-04-org-note-org-bridge-phase2-todo.md` (create when starting) |
| 3 | Capture non-file + journal + idempotent put | `...-phase3-capture.md` |
| 4 | Same-document refile engine | `...-phase4-refile.md` |
| 5 | Session clock claim/release | `...-phase5-clock.md` |
| 6 | Document/item archive | `...-phase6-archive.md` |
| 7 | Hardening pass: remaining spec tests (multi-process locks, digest firewall, etc.) | `...-phase7-hardening.md` |

Do not start Phase N+1 until Phase N’s scoped tests and `./run-emacs-tests.sh` pass (or failures are recorded as out-of-scope and stopped per AGENTS.md).

## File Map (Phase 1)

Create:

- `emacs.d/site-lisp/org-note/org-note-validation.el` — dependency-neutral DTO/endpoint/page validators.
- `emacs.d/site-lisp/org-note/test/org-note-validation-test.el`
- `emacs.d/lisp/gsmlg-org-note-org.el` — bridge activation, cold-start advice, feed-only agenda ownership (may absorb/replace `gsmlg-org-note-agenda.el`).
- `emacs.d/tests/org-note-org-bridge-agenda-test.el` — cold start, feed-only, refresh failure UX, snapshot basics.

Modify:

- `emacs.d/site-lisp/org-note/org-note-operation.el` — require validation; expose `--dispatch-frozen` and operation-specific freeze builders without changing public signatures.
- `emacs.d/site-lisp/org-note/org-note.el` — thin wrappers over validation where UI validators duplicate logic (optional in Phase 1 if tests still pass).
- `emacs.d/lisp/gsmlg-apps.el` — eagerly load inert bridge; install agenda/capture around-advice without requiring org-note at startup.
- `emacs.d/lisp/gsmlg-org.el` — `org-agenda-files` feed-only when bridge active; remove `ORGNOTE` GTD skip.
- `emacs.d/lisp/gsmlg-org-note-agenda.el` — either delete after move into `gsmlg-org-note-org.el`, or reduce to a compatibility require.
- `emacs.d/tests/org-note-agenda-test.el` — retarget to new module names / feed-only semantics.
- `emacs.d/docs/architecture.md` — one paragraph on the bridge (Phase 1 closeout).

---

### Task 1: `org-note-validation.el` endpoint + page primitives

**Files:**
- Create: `emacs.d/site-lisp/org-note/org-note-validation.el`
- Test: `emacs.d/site-lisp/org-note/test/org-note-validation-test.el`
- Modify: `emacs.d/site-lisp/org-note/run_tests.sh` (ensure new test file is loaded)

**Interfaces:**
- Produces:
  - `(org-note-validation-canonical-endpoint url-or-string) -> string` or signals `org-note-error`
  - `(org-note-validation-endpoint-bound-read-context endpoint) -> alist` with keys `endpoint`, `url-builder`
  - `(org-note-validation-page-cursor cursor) -> cursor` accepting only `nil` or nonempty string
  - `(org-note-validation-bounded-pager-state &key limit max-pages max-rows max-requests max-seconds) -> mutable state object`
  - `(org-note-validation-bounded-pager-step state page-fetcher) -> (values rows done-p)` failing closed on repeated cursor/id or budget

- [ ] **Step 1: Write failing tests for endpoint validation**

In `org-note-validation-test.el`:

```elisp
(ert-deftest org-note-validation-rejects-endpoint-userinfo-and-query ()
  (require 'org-note-validation)
  (should-error (org-note-validation-canonical-endpoint
                 "https://user:pass@example.com/api")
                :type 'org-note-error)
  (should-error (org-note-validation-canonical-endpoint
                 "https://example.com/api?token=1")
                :type 'org-note-error)
  (should (string-match-p "\\`https://example.com"
                          (org-note-validation-canonical-endpoint
                           "https://example.com/api/"))))
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
cd emacs.d/site-lisp/org-note && ./run_tests.sh 2>&1 | rg "org-note-validation-rejects|No test|Cannot open|void"
```

Expected: failure because feature/function missing.

- [ ] **Step 3: Implement minimal `org-note-validation.el`**

Implement lexical-binding module with `gsmlg`-independent `org-note-validation-` prefix, `require` only `cl-lib` / `url-parse` / existing `org-note` error symbol (or define soft dependency on `org-note-client` error type already used by the package). Reject schemes other than http/https, userinfo, query, fragment, empty host, and control characters. Normalize trailing slashes on base path.

- [ ] **Step 4: Add bounded-pager tests (repeat cursor / budget)**

```elisp
(ert-deftest org-note-validation-pager-rejects-repeated-cursor ()
  (let ((state (org-note-validation-bounded-pager-state :limit 10 :max-pages 5))
        (calls 0))
    (should-error
     (org-note-validation-bounded-pager-fold
      state
      (lambda (cursor)
        (cl-incf calls)
        (list :rows '(("id" . "a")) :next-cursor (or cursor "c1"))))
     :type 'org-note-error)))
```

Adjust API names to match Step 3 exports; keep the fail-closed properties from the spec § Shared validation.

- [ ] **Step 5: Implement pager; run package tests**

Run:

```bash
cd emacs.d/site-lisp/org-note && ./run_tests.sh
```

Expected: all package tests pass including new validation tests.

- [ ] **Step 6: Commit only if user authorized**

If authorized:

```bash
git add emacs.d/site-lisp/org-note/org-note-validation.el \
        emacs.d/site-lisp/org-note/test/org-note-validation-test.el \
        emacs.d/site-lisp/org-note/run_tests.sh
git commit -m "$(cat <<'EOF'
Add org-note validation primitives for endpoints and bounded paging.

EOF
)"
```

---

### Task 2: Wire validation into operations; frozen dispatch stub

**Files:**
- Modify: `emacs.d/site-lisp/org-note/org-note-operation.el`
- Test: `emacs.d/site-lisp/org-note/test/org-note-operation-test.el`

**Interfaces:**
- Consumes: `org-note-validation-*` from Task 1
- Produces:
  - `(org-note-operation--freeze-request typed-request) -> frozen-envelope`
  - `(org-note-operation--dispatch-frozen frozen-envelope) -> response`
  - Public `org-note-operation-transition` / `claim` / `release` / document put keep signatures; delegate to freeze+dispatch internally where touched in Phase 1 (minimum: one mutation path exercised by tests)

- [ ] **Step 1: Write failing test that dispatch uses identical bytes on retry**

```elisp
(ert-deftest org-note-operation-frozen-dispatch-reuses-bytes ()
  (let (bodies)
    (cl-letf (((symbol-function 'org-note-client-request-raw)
               (lambda (&rest args)
                 (push (plist-get args :body) bodies)
                 ;; return minimal valid stub for the exercised op
                 ...)))
      (let* ((env (org-note-operation--freeze-...))
             (_ (org-note-operation--dispatch-frozen env))
             (_ (org-note-operation--dispatch-frozen env)))
        (should (equal (car bodies) (cadr bodies)))))))
```

Fill the freeze builder for whichever mutation is easiest to stub (document put or transition) using existing test helpers in `org-note-operation-test.el`.

- [ ] **Step 2: Run test — expect fail (missing freeze/dispatch)**

- [ ] **Step 3: Implement freeze + raw dispatch without changing public signatures**

Require `org-note-validation`. Add internal builders. Disable redirect following for mutation transport per spec. Keep fencing tokens out of any persisted structure.

- [ ] **Step 4: Run `./run_tests.sh` — expect PASS**

- [ ] **Step 5: Commit if authorized**

---

### Task 3: Bridge module scaffold + cold-start advice (no network at install)

**Files:**
- Create: `emacs.d/lisp/gsmlg-org-note-org.el`
- Modify: `emacs.d/lisp/gsmlg-apps.el`
- Test: `emacs.d/tests/org-note-org-bridge-agenda-test.el`

**Interfaces:**
- Produces:
  - `(gsmlg-org-note-org-install-guards)` — installs around advice; must not `require` org-note
  - `(gsmlg-org-note-org-activate)` — idempotent; may require org-note
  - `(gsmlg-org-note-org--around-agenda orig &rest args)`
  - `(gsmlg-org-note-org--around-capture orig &rest args)` (capture body can be stub that only activates + calls orig until Phase 3)

- [ ] **Step 1: Failing test — installing guards does not load org-note**

```elisp
(ert-deftest gsmlg-org-note-org-install-guards-is-inert ()
  (when (featurep 'org-note) (unload-feature 'org-note t))
  (require 'gsmlg-org-note-org)
  (gsmlg-org-note-org-install-guards)
  (should-not (featurep 'org-note))
  (should (advice-member-p #'gsmlg-org-note-org--around-agenda #'org-agenda)))
```

- [ ] **Step 2: Run focused ERT — expect fail**

```bash
emacs -Q --batch -L emacs.d/lisp -L emacs.d/tests \
  --eval "(load \"emacs.d/tests/org-note-org-bridge-agenda-test.el\")" \
  --eval "(ert-run-tests-batch-and-exit \"gsmlg-org-note-org-install\")"
```

(Adjust load path / stubs for `gsmlg-paths` like existing org-note-agenda tests.)

- [ ] **Step 3: Implement install + around wrappers**

`gsmlg-apps.el`: replace `gsmlg-apps--activate-org-note-agenda` pattern with eager `(require 'gsmlg-org-note-org)` + `gsmlg-org-note-org-install-guards` from apps load (still no org-note). Around-agenda: require org-note, activate, then `apply orig args`.

- [ ] **Step 4: Test first agenda invocation requires org-note exactly once**

Mock `org-agenda` to record calls; ensure wrapper loads feature then calls original.

- [ ] **Step 5: Run tests PASS; commit if authorized**

---

### Task 4: Feed-only `org-agenda-files` + remove ORGNOTE skip

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org.el`
- Modify: `emacs.d/lisp/gsmlg-org-note-org.el` (or migrate from `gsmlg-org-note-agenda.el`)
- Test: `emacs.d/tests/org-note-org-bridge-agenda-test.el`, update `emacs.d/tests/org-note-agenda-test.el`

**Interfaces:**
- Produces: `(gsmlg-org-note-org-agenda-files) -> list` of exactly the selected feed path (or empty-feed path)
- Consumes: existing feed refresh helpers (move into `gsmlg-org-note-org.el`)

- [ ] **Step 1: Failing test — expanded files no longer append local sources**

```elisp
(ert-deftest gsmlg-org-note-org-agenda-files-are-feed-only ()
  (let ((local (make-temp-file "local" nil ".org")))
    (setq gsmlg-org-agenda-files local)
    (gsmlg-org-note-org-activate)
    (gsmlg-org-apply-path-settings)
    (should (equal org-agenda-files
                   (list (gsmlg-org-note-org-feed-file))))
    (should-not (member local org-agenda-files))))
```

- [ ] **Step 2: Run — expect fail (current code still appends)**

- [ ] **Step 3: Change `gsmlg-org-apply-path-settings` to set feed-only when bridge active; delete skip-org-note from GTD skip functions**

- [ ] **Step 4: Update old agenda tests; run PASS**

- [ ] **Step 5: Commit if authorized**

---

### Task 5: Snapshot refresh + configure-on-empty + failure prompt

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org-note-org.el`
- Test: `emacs.d/tests/org-note-org-bridge-agenda-test.el`

**Interfaces:**
- Produces:
  - `(gsmlg-org-note-org-refresh-feed &optional force) -> feed-path`
  - On unset workspaces: call configure helper; on cancel write endpoint-keyed empty feed without clobbering last-good
  - On pre-rename failure: `(yes-or-no-p ...)` choose last-good matching schema/workspaces or abort

- [ ] **Step 1: Tests for configure-on-empty and cache-or-abort**

```elisp
(ert-deftest gsmlg-org-note-org-refresh-asks-on-failure ()
  (let (asked)
    (cl-letf (((symbol-function 'gsmlg-org-note-org--fetch-views)
               (lambda (&rest _) (error "network")))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt) (setq asked prompt) nil)))
      (should-error (gsmlg-org-note-org-refresh-feed t))
      (should asked))))
```

Add a second test where last-good matches and user says yes → returns existing path without rewriting from network.

- [ ] **Step 2: Run — fail until implemented**

- [ ] **Step 3: Implement refresh ownership + empty-feed path + prompt (Phase 1 may use single-process file write; multi-process reservation locks are Phase 7 unless already cheap)**

Document any deferred multi-process lock items from spec §§ publication reservation as Phase 7 explicitly in a code comment referencing the spec blocker numbers (69, 74, 79).

- [ ] **Step 4: Run PASS**

- [ ] **Step 5: Commit if authorized**

---

### Task 6: Phase 1 closeout — architecture note + full suite

**Files:**
- Modify: `emacs.d/docs/architecture.md`
- Possibly remove or thin `emacs.d/lisp/gsmlg-org-note-agenda.el` with `(require 'gsmlg-org-note-org)` compatibility.

- [x] **Step 1: Document bridge in architecture.md (short)**

- [x] **Step 2: Run org-note package tests + focused bridge ERT + `./run-emacs-tests.sh`**

```bash
emacs.d/site-lisp/org-note/run_tests.sh
# focused:
emacs -Q --batch ... ert-run-tests-batch-and-exit \"gsmlg-org-note-org\"
./run-emacs-tests.sh
```

Expected: pass, or stop and record out-of-scope failures per AGENTS.md.

- [x] **Step 3: Mark Phase 1 complete; create Phase 2 plan file before continuing**

---

### Task 7: Review closeout — source guards and release gate

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org-note-org.el`
- Modify: `emacs.d/tests/org-note-org-bridge-agenda-test.el`
- Modify: `docs/superpowers/specs/2026-08-31-org-note-org-bridge-design.md`

- [x] **Step 1: Keep the phased bridge default-disabled until final release**
- [x] **Step 2: Guard every public Agenda producer and final `org-agenda-files` resolution**
- [x] **Step 3: Stabilize feed ordering and make identical writes a no-op**
- [x] **Step 4: Add focused regression tests for direct entrypoints, overrides, ordering, and no-op writes**
- [ ] **Step 5: Run focused ERT, vendored Org Note tests, and the complete suite**

---

## Spec coverage (self-review)

| Spec area | Phase / Task |
| --- | --- |
| Validation module, pager, endpoint | Phase 1 Task 1 |
| Frozen wire / dispatch | Phase 1 Task 2 |
| Cold-start advice | Phase 1 Task 3 |
| Feed-only agenda files, drop ORGNOTE skip | Phase 1 Task 4 |
| Refresh failure ask cache/abort; empty configure | Phase 1 Task 5 |
| Capture journal / non-file | Phase 3 |
| Same-doc refile | Phase 4 |
| TODO keywords + transition attempts | Phase 2 |
| Clock session-only | Phase 5 |
| Archive | Phase 6 |
| Multi-process locks, digest firewall, full mutator audit | Phase 7 |
| Default-disabled release gate; direct producer/source guards | Phase 1 Task 7 |
| Stable ordering and unchanged-generation no-op | Phase 1 Task 7 + Phase 7 publication |
| Archive same-id service integration gate | Phase 6 release gate |
| Cross-doc refile / feed refile / cross-restart clock | Non-goals — no task |

## Placeholder scan

No TBD/TODO implementation holes in Phase 1 steps; Phase 2–7 are intentionally separate plan files to be written with the same TDD granularity before execution.
