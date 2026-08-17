# Org Note Emacs Client Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox syntax for tracking.

**Goal:** Build a combined Emacs 30.2 client that edits Agent Note Org documents with revision-safe saves and exposes its indexed work-item operations.

**Architecture:** Four focused Lisp modules share one REST transport. org-note-client.el owns HTTP and JSON, org-note-operation.el owns the API and in-memory leases, org-note-document.el owns remote-backed Org buffers, and org-note.el owns interactive views and commands. Loading is inert; network access follows an interactive command or an active-lease heartbeat.

**Tech Stack:** GNU Emacs 30.2, lexical-binding Emacs Lisp, built-in url, json, org, tabulated-list, ediff, cl-lib, and ERT.

---

**Approved design:** emacs.d/site-lisp/org-note/design.md

**Execution constraint:** Do not create commits. This repository requires explicit commit authorization, and none has been given.

## File Map

Create:

- emacs.d/site-lisp/org-note/org-note-client.el: configuration, transport, JSON, operation IDs, and safe errors.
- emacs.d/site-lisp/org-note/org-note-operation.el: REST mappings, queries, mutations, leases, and heartbeats.
- emacs.d/site-lisp/org-note/org-note-document.el: remote Org buffers, saves, revisions, lease proofs, and conflicts.
- emacs.d/site-lisp/org-note/org-note.el: public entrypoint, browsers, context, pagination, and actions.
- emacs.d/site-lisp/org-note/README.md: standalone usage and safety contract.
- emacs.d/site-lisp/org-note/run_tests.sh: package-local ERT runner.
- emacs.d/site-lisp/org-note/test/org-note-client-test.el
- emacs.d/site-lisp/org-note/test/org-note-operation-test.el
- emacs.d/site-lisp/org-note/test/org-note-document-test.el
- emacs.d/site-lisp/org-note/test/org-note-test.el

Modify:

- emacs.d/init.el
- emacs.d/lisp/gsmlg-apps.el
- emacs.d/tests/test-helper.el
- emacs.d/tests/module-load-test.sh
- run-emacs-tests.sh
- emacs.d/docs/architecture.md
- emacs.d/docs/migration.md
- AGENTS.md

Do not modify gsmlg-org.el, Org workflow/keybinding tests, Elpaca recipes, or elpaca-lock.el.

### Task 1: Package Runner and REST Transport

**Files:**

- Create: emacs.d/site-lisp/org-note/test/org-note-client-test.el
- Create: emacs.d/site-lisp/org-note/org-note-client.el
- Create: emacs.d/site-lisp/org-note/run_tests.sh

- [ ] **Step 1: Write failing transport tests**

Create these ERT cases:

~~~elisp
(ert-deftest org-note-client-defaults-are-correct ())
(ert-deftest org-note-client-url-normalizes-slashes-and-query ())
(ert-deftest org-note-client-request-encodes-utf8-json ())
(ert-deftest org-note-client-request-parses-json-success ())
(ert-deftest org-note-client-request-supports-empty-success ())
(ert-deftest org-note-client-request-signals-safe-http-error ())
(ert-deftest org-note-client-request-signals-malformed-response ())
(ert-deftest org-note-client-async-calls-back-once ())
(ert-deftest org-note-client-errors-never-contain-fencing-token ())
(ert-deftest org-note-client-new-operation-id-is-unique ())
~~~

Stub url-retrieve-synchronously and url-retrieve. No test may resolve or contact the endpoint. Assert:

~~~elisp
(should (equal org-note-endpoint "https://agent-note.gsmlg.net/"))
(should (equal org-note-actor-id
               (format "emacs:%s@%s" (user-login-name) (system-name))))
(should (= org-note-request-timeout 30))
~~~

Capture the dynamically bound URL request variables and verify the HTTP method, UTF-8 body bytes, Accept header, and Content-Type header. Use a 409 stale_revision fixture and assert that the condition exposes status, code, safe message, details, and retryable while never exposing a synthetic fencing token through error-message-string.

- [ ] **Step 2: Run the client tests and confirm red**

~~~sh
emacs -Q --batch \
  -L emacs.d/site-lisp/org-note \
  -L emacs.d/site-lisp/org-note/test \
  -l emacs.d/site-lisp/org-note/test/org-note-client-test.el \
  -f ert-run-tests-batch-and-exit
~~~

Expected: failure because org-note-client does not exist.

- [ ] **Step 3: Implement configuration, JSON, IDs, and errors**

Define:

~~~elisp
(defgroup org-note nil
  "Edit and operate Agent Note Org workspaces."
  :group 'org)

(defcustom org-note-endpoint "https://agent-note.gsmlg.net/"
  "Base URL of the Agent Note server."
  :type 'string
  :group 'org-note)

(defcustom org-note-actor-id
  (format "emacs:%s@%s" (user-login-name) (system-name))
  "Actor identifier sent with Agent Note mutations."
  :type 'string
  :group 'org-note)

(defcustom org-note-request-timeout 30
  "Seconds to wait for an interactive Agent Note request."
  :type 'integer
  :group 'org-note)

(define-error 'org-note-error "Agent Note error")
(define-error 'org-note-transport-error
  "Agent Note transport error" 'org-note-error)
(define-error 'org-note-response-error
  "Invalid Agent Note response" 'org-note-error)
(define-error 'org-note-http-error
  "Agent Note HTTP error" 'org-note-error)
~~~

Add org-note-client-empty-object and org-note-client-new-operation-id. The empty object returns an equal-tested hash table. The operation ID is an opaque SHA-256 over current time, process ID, randomness, and host-local time text.

Implement org-note-client--url, org-note-client--encode, org-note-client--decode, org-note-client-error-status, org-note-client-error-code, org-note-client-error-details, and org-note-client-error-retryable-p. Parse objects as symbol-keyed alists, arrays as vectors, null as nil, and false as :json-false. Omit nil query values, encode booleans as true/false, and URL-escape names and values.

- [ ] **Step 4: Implement request entrypoints**

Provide:

~~~elisp
(defun org-note-client-request (method route &optional query body))
(defun org-note-client-request-async
    (method route query body callback))
~~~

The synchronous function uses url-retrieve-synchronously with silent output, inhibited cookies, and org-note-request-timeout. The asynchronous function calls CALLBACK exactly once as either result/nil or nil/safe-error. Both kill response buffers in unwind-protect.

Treat status 200 through 299 as success and an empty success body as nil. Signal non-success responses with only status, server code, safe message, details, and retryable. Never log headers, bodies, or tokens.

- [ ] **Step 5: Add and run the package runner**

Create an executable Bash runner using env bash, set -e, script-relative paths, the EMACS environment override, sorted test/*-test.el loading, and ert-run-tests-batch-and-exit.

~~~sh
chmod +x emacs.d/site-lisp/org-note/run_tests.sh
./emacs.d/site-lisp/org-note/run_tests.sh
~~~

Expected: all client tests pass with no network request.

### Task 2: Read, Query, and Document REST Operations

**Files:**

- Create: emacs.d/site-lisp/org-note/test/org-note-operation-test.el
- Create: emacs.d/site-lisp/org-note/org-note-operation.el

- [ ] **Step 1: Write failing read/query tests**

Create:

~~~elisp
(ert-deftest org-note-operation-envelope-has-required-fields ())
(ert-deftest org-note-operation-list-workspaces-encodes-pagination ())
(ert-deftest org-note-operation-get-workspace-uses-id-route ())
(ert-deftest org-note-operation-list-documents-scopes-workspace ())
(ert-deftest org-note-operation-get-document-uses-workspace-query ())
(ert-deftest org-note-operation-put-document-sends-revision-and-proofs ())
(ert-deftest org-note-operation-query-queue-encodes-filters ())
(ert-deftest org-note-operation-query-agenda-uses-agenda-route ())
(ert-deftest org-note-operation-get-item-context-scopes-workspace ())
(ert-deftest org-note-operation-list-events-encodes-subject-filter ())
~~~

Stub only org-note-client-request and assert exact method, route, query, and body. Verify empty lease_proofs is a hash table so it serializes as an object.

- [ ] **Step 2: Run the operation tests and confirm red**

~~~sh
emacs -Q --batch \
  -L emacs.d/site-lisp/org-note \
  -L emacs.d/site-lisp/org-note/test \
  -l emacs.d/site-lisp/org-note/test/org-note-operation-test.el \
  -f ert-run-tests-batch-and-exit
~~~

Expected: failure because org-note-operation does not exist.

- [ ] **Step 3: Implement the common mutation envelope**

Implement org-note-operation--mutation-body so every mutation contains schema_version 1, org-note-actor-id, one operation ID, workspace_id, and endpoint-specific fields. Each intentional attempt gets one ID. Do not replay ambiguous failures automatically.

- [ ] **Step 4: Implement browsing and document calls**

Implement:

~~~elisp
(cl-defun org-note-operation-list-workspaces
    (&key cursor limit include-archived))
(defun org-note-operation-get-workspace (workspace-id))
(cl-defun org-note-operation-list-documents
    (workspace-id &key cursor limit include-archived))
(defun org-note-operation-get-document (workspace-id document-id))
(cl-defun org-note-operation-put-document
    (workspace-id document-id path source expected-revision lease-proofs
                  &key operation-id))
~~~

Map them exactly:

~~~text
GET /api/org/workspaces
GET /api/org/workspaces/{workspace_id}
GET /api/org/workspaces/{workspace_id}/documents
GET /api/org/documents/{document_id}?workspace_id={workspace_id}
PUT /api/org/documents/{document_id}
~~~

The PUT body contains path, source, expected_revision, and lease_proofs in addition to the envelope.

- [ ] **Step 5: Implement indexed queries, context, and events**

Define exact client view constants:

~~~elisp
(defconst org-note-operation-queue-views
  '("ready" "assigned" "running" "blocked" "review" "failed"
    "expired_lease" "completed"))

(defconst org-note-operation-agenda-views
  '("scheduled" "upcoming_deadline"))
~~~

Implement:

~~~elisp
(cl-defun org-note-operation-query-queue
    (workspace-ids view
     &key item-type state priority tags assignee
     scheduled-from scheduled-to deadline-from deadline-to
     completed-from completed-to from to include-archived cursor limit))
(cl-defun org-note-operation-query-agenda
    (workspace-ids view
     &key item-type state priority tags assignee
     scheduled-from scheduled-to deadline-from deadline-to
     completed-from completed-to from to include-archived cursor limit))
(defun org-note-operation-get-item-context (workspace-id item-id))
(cl-defun org-note-operation-list-events
    (workspace-id &key subject-kind subject-id cursor limit))
~~~

Routes are GET /api/org/queue, GET /api/org/agenda, GET /api/org/items/{item_id}/context, and GET /api/org/workspaces/{workspace_id}/events. Join workspace IDs and tags with commas. Preserve cursors unchanged. Do not validate server-defined states.

- [ ] **Step 6: Run operation tests**

Run the Task 2 command again.

Expected: all read/query operation tests pass.

### Task 3: Work-item Mutation Contract

**Files:**

- Modify: emacs.d/site-lisp/org-note/test/org-note-operation-test.el
- Modify: emacs.d/site-lisp/org-note/org-note-operation.el

- [ ] **Step 1: Write failing lifecycle tests**

Add:

~~~elisp
(ert-deftest org-note-operation-claim-sends-revision-and-kind ())
(ert-deftest org-note-operation-heartbeat-sends-lease-proof ())
(ert-deftest org-note-operation-release-supports-target-state ())
(ert-deftest org-note-operation-progress-defaults-metadata-object ())
(ert-deftest org-note-operation-result-defaults-collections ())
(ert-deftest org-note-operation-transition-supports-optional-lease ())
(ert-deftest org-note-operation-retry-sends-document-revision ())
~~~

Assert the seven POST routes under /api/org/items/{item_id}: claim, claim/heartbeat, claim/release, progress, result, transition, and retry. Empty metadata is an object; empty note_refs and artifacts are vectors.

- [ ] **Step 2: Run lifecycle tests and confirm red**

~~~sh
emacs -Q --batch \
  -L emacs.d/site-lisp/org-note \
  -L emacs.d/site-lisp/org-note/test \
  -l emacs.d/site-lisp/org-note/test/org-note-operation-test.el \
  --eval '(ert-run-tests-batch-and-exit "^org-note-operation-\(claim\|heartbeat\|release\|progress\|result\|transition\|retry\)-")'
~~~

Expected: new cases fail because lifecycle functions are undefined.

- [ ] **Step 3: Implement lifecycle functions**

Implement:

~~~elisp
(cl-defun org-note-operation-claim
    (workspace-id item-id document-id expected-revision kind
                  &key operation-id))
(cl-defun org-note-operation-heartbeat
    (workspace-id item-id lease-id kind fencing-token
                  &key operation-id))
(cl-defun org-note-operation-release
    (workspace-id item-id document-id expected-revision
                  lease-id kind fencing-token
                  &key target-state operation-id))
(cl-defun org-note-operation-report-progress
    (workspace-id item-id lease-id kind fencing-token summary
                  &key metadata operation-id))
(cl-defun org-note-operation-submit-result
    (workspace-id item-id document-id expected-revision
                  lease-id fencing-token result-summary
                  &key note-refs artifacts metadata operation-id))
(cl-defun org-note-operation-transition
    (workspace-id item-id document-id expected-revision target-state
                  &key lease error metadata operation-id))
(cl-defun org-note-operation-retry
    (workspace-id item-id document-id expected-revision
                  &key operation-id))
~~~

Use expected_document_revision for item mutations. Omit absent optional target_state, lease, and error rather than encoding null.

- [ ] **Step 4: Write failing review/relationship tests**

Add:

~~~elisp
(ert-deftest org-note-operation-request-review-sends-execution-proof ())
(ert-deftest org-note-operation-approve-review-sends-review-proof ())
(ert-deftest org-note-operation-reject-review-sends-reason ())
(ert-deftest org-note-operation-add-dependency-sends-revision-map ())
(ert-deftest org-note-operation-remove-dependency-uses-delete-body ())
(ert-deftest org-note-operation-link-note-sends-description ())
(ert-deftest org-note-operation-unlink-note-omits-description ())
~~~

Run the package runner. Expected: only the new cases fail.

- [ ] **Step 5: Implement review/relationship functions**

Implement:

~~~elisp
(cl-defun org-note-operation-request-review
    (workspace-id item-id document-id expected-revision lease-id fencing-token
                  &key result-summary note-refs artifacts metadata operation-id))
(cl-defun org-note-operation-approve-review
    (workspace-id item-id document-id expected-revision lease-id fencing-token
                  &key metadata operation-id))
(cl-defun org-note-operation-reject-review
    (workspace-id item-id document-id expected-revision lease-id fencing-token
                  reason &key metadata operation-id))
(cl-defun org-note-operation-add-dependency
    (workspace-id item-id dependency-id document-id expected-revisions
                  &key lease operation-id))
(cl-defun org-note-operation-remove-dependency
    (workspace-id item-id dependency-id document-id expected-revisions
                  &key lease operation-id))
(cl-defun org-note-operation-link-note
    (workspace-id item-id document-id purpose note-id description
                  expected-revisions &key lease operation-id))
(cl-defun org-note-operation-unlink-note
    (workspace-id item-id document-id purpose note-id expected-revisions
                  &key lease operation-id))
~~~

Map exactly to review/request, review/approve, review/reject, POST/DELETE dependencies, and POST/DELETE note-links. Always send expected_revisions for dependency/link changes; include lease only when supplied.

- [ ] **Step 6: Run package tests**

~~~sh
./emacs.d/site-lisp/org-note/run_tests.sh
~~~

Expected: all wire-contract tests pass.

### Task 4: In-memory Leases and Heartbeats

**Files:**

- Modify: emacs.d/site-lisp/org-note/test/org-note-operation-test.el
- Modify: emacs.d/site-lisp/org-note/org-note-operation.el

- [ ] **Step 1: Write failing registry tests**

Add:

~~~elisp
(ert-deftest org-note-operation-registers-claim-in-memory ())
(ert-deftest org-note-operation-retry-registers-execution-lease ())
(ert-deftest org-note-operation-lease-proofs-select-live-document-leases ())
(ert-deftest org-note-operation-forget-lease-cancels-timer ())
(ert-deftest org-note-operation-lease-data-never-enters-message-text ())
~~~

Reset private state with unwind-protect. Proof maps are keyed by item UUID and contain only lease_id, kind, and fencing_token. Exclude expired and other-document leases.

- [ ] **Step 2: Run registry tests and confirm red**

Run the operation test with selector matching lease and registers-claim.

Expected: failure because the registry is absent.

- [ ] **Step 3: Implement the registry**

~~~elisp
(cl-defstruct (org-note-operation-lease
               (:constructor org-note-operation--make-lease))
  workspace-id item-id document-id kind lease-id fencing-token expires-at
  timer heartbeat-p)

(defvar org-note-operation--leases (make-hash-table :test #'equal))
~~~

Implement:

~~~elisp
(defun org-note-operation-find-lease (workspace-id item-id kind))
(defun org-note-operation-register-claim
    (workspace-id item-id document-id kind response))
(defun org-note-operation-forget-lease (workspace-id item-id kind))
(defun org-note-operation-lease-proofs (document-id))
~~~

Claim and retry register successful responses. expires_at is Unix epoch seconds. Never format or print a lease struct.

- [ ] **Step 4: Write failing heartbeat tests**

Add:

~~~elisp
(ert-deftest org-note-operation-heartbeat-schedules-before-expiry ())
(ert-deftest org-note-operation-heartbeat-allows-one-outstanding-request ())
(ert-deftest org-note-operation-heartbeat-refreshes-context-for-expiry ())
(ert-deftest org-note-operation-heartbeat-stale-lease-forgets-token ())
(ert-deftest org-note-operation-heartbeat-transient-failure-retries ())
(ert-deftest org-note-operation-terminal-actions-cancel-heartbeat ())
~~~

Stub run-at-time, cancel-timer, float-time, and org-note-client-request-async; never sleep.

- [ ] **Step 5: Implement heartbeat scheduling**

Implement:

~~~elisp
(defun org-note-operation--schedule-heartbeat (lease))
(defun org-note-operation--heartbeat-timer (lease-key))
(defun org-note-operation--heartbeat-finished (lease-key result error))
(defun org-note-operation--refresh-lease-context (lease-key))
~~~

Schedule at 60 percent of remaining lifetime, minimum one second. Mark heartbeat-p before dispatch to prevent overlap. Heartbeat data is opaque: if it lacks authoritative expires_at, asynchronously fetch item context and use its lease expiry. Never guess a renewed duration.

On transient failure, warn safely and retry after the smaller of five seconds or half the remaining old lifetime, minimum one second. Never retry at/after expiry. stale_lease removes the lease.

Successful release, result, review request, approval, and rejection remove the relevant lease/timer. A transition retains it until server context shows it inactive because states are workspace-defined.

- [ ] **Step 6: Run package tests**

Expected: all operation and heartbeat cases pass with no timers surviving cleanup.

### Task 5: Remote-backed Org Buffers and Saving

**Files:**

- Create: emacs.d/site-lisp/org-note/test/org-note-document-test.el
- Create: emacs.d/site-lisp/org-note/org-note-document.el

- [ ] **Step 1: Write failing document-open tests**

Add:

~~~elisp
(ert-deftest org-note-document-open-populates-org-buffer ())
(ert-deftest org-note-document-open-records-remote-identity ())
(ert-deftest org-note-document-open-reuses-live-buffer ())
(ert-deftest org-note-document-mode-remaps-save-buffer ())
~~~

Assert a clean Org buffer and buffer-local workspace ID, document ID, path, revision, content hash, base source, and conflict.

- [ ] **Step 2: Run document tests and confirm red**

~~~sh
emacs -Q --batch \
  -L emacs.d/site-lisp/org-note \
  -L emacs.d/site-lisp/org-note/test \
  -l emacs.d/site-lisp/org-note/test/org-note-document-test.el \
  -f ert-run-tests-batch-and-exit
~~~

Expected: failure because org-note-document is absent.

- [ ] **Step 3: Implement document mode and reuse**

Define org-note-document-mode from org-mode and remap save-buffer locally to org-note-document-save using keymap-set.

Implement:

~~~elisp
(defun org-note-document-open (workspace-id document-id))
(defun org-note-document--find-buffer (workspace-id document-id))
(defun org-note-document--populate-buffer (buffer response))
~~~

Name buffers *Org Note: PATH*, let Emacs disambiguate duplicate paths, reuse only when both IDs match, and do not assign buffer-file-name.

- [ ] **Step 4: Write failing save tests**

Add:

~~~elisp
(ert-deftest org-note-document-save-sends-full-source-and-revision ())
(ert-deftest org-note-document-save-includes-live-lease-proofs ())
(ert-deftest org-note-document-save-updates-revision-and-clean-state ())
(ert-deftest org-note-document-save-preserves-point-and-narrowing ())
(ert-deftest org-note-document-save-failure-leaves-buffer-modified ())
~~~

Use a successful response whose document_revisions map is keyed by document UUID.

- [ ] **Step 5: Implement revision-safe save**

Implement org-note-document-save, org-note-document--source, and org-note-document--returned-revision. Save widened source with current revision and org-note-operation-lease-proofs. On success update revision/base source, clear conflict, and mark clean. Preserve point/narrowing. On failure leave text, metadata, and modified state unchanged.

- [ ] **Step 6: Run document tests**

Expected: all ordinary open/save tests pass.

### Task 6: Explicit Conflict Resolution

**Files:**

- Modify: emacs.d/site-lisp/org-note/test/org-note-document-test.el
- Modify: emacs.d/site-lisp/org-note/org-note-document.el

- [ ] **Step 1: Write failing conflict tests**

Add:

~~~elisp
(ert-deftest org-note-document-stale-save-preserves-local-edit ())
(ert-deftest org-note-document-stale-save-fetches-latest-remote ())
(ert-deftest org-note-document-compare-latest-invokes-ediff ())
(ert-deftest org-note-document-reload-requires-confirmation ())
(ert-deftest org-note-document-reload-uses-remote-source ())
(ert-deftest org-note-document-rebase-requires-confirmation ())
(ert-deftest org-note-document-rebase-keeps-local-source ())
~~~

A stale fixture signals org-note-http-error with stale_revision. Assert latest content is fetched/cached and local text remains modified.

- [ ] **Step 2: Run conflict tests and confirm red**

Run the document test with selector matching stale, compare, reload, and rebase.

Expected: failure because conflict commands are absent.

- [ ] **Step 3: Implement conflict commands**

Implement:

~~~elisp
(defun org-note-document--record-conflict ())
(defun org-note-document-compare-latest ())
(defun org-note-document-reload ())
(defun org-note-document-rebase ())
~~~

Compare invokes ediff against a read-only temporary Org buffer. Reload confirms before replacing edits and becomes clean. Rebase confirms, keeps local text, advances expected revision/base to inspected remote, and remains modified. Catch only stale_revision in save; all other errors propagate without an extra fetch.

- [ ] **Step 4: Run package tests**

Expected: client, operation, lease, document, and conflict tests pass.

### Task 7: Workspace and Document Browsers

**Files:**

- Create: emacs.d/site-lisp/org-note/test/org-note-test.el
- Create: emacs.d/site-lisp/org-note/org-note.el

- [ ] **Step 1: Write failing load/browser tests**

Add:

~~~elisp
(ert-deftest org-note-require-is-inert ())
(ert-deftest org-note-workspaces-renders-counts ())
(ert-deftest org-note-workspace-open-fetches-documents ())
(ert-deftest org-note-documents-renders-path-and-revision ())
(ert-deftest org-note-document-list-open-passes-both-ids ())
(ert-deftest org-note-browser-refresh-preserves-row ())
(ert-deftest org-note-browser-pages-preserve-opaque-cursors ())
~~~

Stub requests and timer creation before require; assert zero calls. UI tests stub public operation/document functions and inspect tabulated-list-entries.

- [ ] **Step 2: Run UI tests and confirm red**

Run org-note-test.el under emacs -Q with package/test load paths.

Expected: failure because the entrypoint is absent.

- [ ] **Step 3: Implement entrypoint and pagination**

Give org-note.el Version 0.1.0 and Package-Requires Emacs 30.2. Explicitly require all three internal modules.

Define buffer-local fetcher, current cursor, next cursor, cursor history, and row data. Implement org-note-refresh, org-note-next-page, org-note-previous-page, and org-note--replace-table. Refresh reuses current cursor; preserve selection if its row survives.

- [ ] **Step 4: Implement workspace/document list modes**

Provide:

~~~elisp
(define-derived-mode org-note-workspace-list-mode
  tabulated-list-mode "Org-Note-Workspaces")
(define-derived-mode org-note-document-list-mode
  tabulated-list-mode "Org-Note-Documents")
(defun org-note-workspaces ())
(defun org-note-documents (workspace-id))
(defun org-note-workspace-open ())
(defun org-note-document-list-open ())
~~~

Workspace columns: Workspace, Slug, Revision, Ready, Running, Blocked, Review. Document columns: Path, Revision. Store complete row objects by ID.

Local keys: RET open, g refresh, n next, p previous, q quit. No global bindings.

- [ ] **Step 5: Run UI tests**

Expected: inert loading, browsers, open behavior, refresh, and pagination pass.

### Task 8: Operational Views and Actions

**Files:**

- Modify: emacs.d/site-lisp/org-note/test/org-note-test.el
- Modify: emacs.d/site-lisp/org-note/org-note.el

- [ ] **Step 1: Write failing operational-view tests**

Add:

~~~elisp
(ert-deftest org-note-queue-prompts-and-renders-rows ())
(ert-deftest org-note-agenda-uses-agenda-views ())
(ert-deftest org-note-operational-row-opens-context ())
(ert-deftest org-note-item-context-renders-required-sections ())
(ert-deftest org-note-events-renders-audit-rows ())
(ert-deftest org-note-operational-pages-preserve-cursors ())
~~~

The context fixture contains workspace, document, item, parent, children, dependencies, note links, attempts, origin, history segments, lease, and operational data. Assert a heading for every section, including empty collections.

- [ ] **Step 2: Run view tests and confirm red**

Run org-note-test.el with a selector matching queue, agenda, operational, item-context, and events.

Expected: new view tests fail.

- [ ] **Step 3: Implement queue, agenda, context, and events**

Provide:

~~~elisp
(defun org-note--read-workspace-ids ())
(defun org-note-queue (workspace-ids view))
(defun org-note-agenda (workspace-ids view))
(defun org-note-operational-open ())
(define-derived-mode org-note-queue-mode
  tabulated-list-mode "Org-Note-Queue")
(define-derived-mode org-note-agenda-mode
  tabulated-list-mode "Org-Note-Agenda")
(defun org-note-item-context (workspace-id item-id))
(define-derived-mode org-note-item-context-mode
  org-mode "Org-Note-Context")
(defun org-note-events (workspace-id &optional subject-kind subject-id))
(define-derived-mode org-note-event-list-mode
  tabulated-list-mode "Org-Note-Events")
~~~

Interactive queue/agenda calls select workspaces and exact view constants. Operational columns: Type, Title, State, Priority, Assignee, When, Attempt, Ready, Lease.

Context is deterministic read-only Org with identity, hierarchy, dependencies, blockers, note links, attempts/results/recovery, lease, origin, and history. Event columns: Sequence, Time, Type, Subject, Actor, Previous, Result, Summary.

- [ ] **Step 4: Write failing action tests**

Add one org-note-action-NAME-dispatches-exact-context test for:

~~~text
claim, heartbeat, release, progress, submit-result, transition, retry,
request-review, approve-review, reject-review, add-dependency,
remove-dependency, link-note, unlink-note, item-dispatch
~~~

Stub prompts and operation functions. Assert IDs, revision, lease kind, and user fields. Cancellation must send no mutation.

- [ ] **Step 5: Run action tests and confirm red**

Run org-note-test.el with selector ^org-note-action-.

Expected: action tests fail because commands are absent.

- [ ] **Step 6: Implement context helpers and actions**

Define buffer-local current context plus helpers for workspace ID, item ID, document ID, document revision, JSON object input, and JSON array input. Parse empty objects as hash tables and arrays as vectors. Never put fencing tokens in minibuffer history.

Lease rules:

| Command | Lease |
| --- | --- |
| Claim | None; execution or review. |
| Heartbeat/release | Existing selected-kind lease. |
| Progress/result/review request | Existing execution lease. |
| Approve/reject | Existing review lease. |
| Transition | Optional live lease. |
| Retry | None; response creates execution lease. |
| Dependency/note-link changes | Optional live lease. |

Define org-note-item-dispatch over all 14 named actions. Each command sends one mutation and refreshes context only after confirmed success. Bind a to dispatch only in queue, agenda, and context maps.

- [ ] **Step 7: Run package tests**

~~~sh
./emacs.d/site-lisp/org-note/run_tests.sh
~~~

Expected: all package-local ERT passes without real HTTP, live timers, or exposed tokens.

### Task 9: README and Dotfiles Integration

**Files:**

- Create: emacs.d/site-lisp/org-note/README.md
- Modify: emacs.d/tests/test-helper.el
- Modify: emacs.d/tests/module-load-test.sh
- Modify: emacs.d/init.el
- Modify: emacs.d/lisp/gsmlg-apps.el
- Modify: run-emacs-tests.sh
- Modify: emacs.d/docs/architecture.md
- Modify: emacs.d/docs/migration.md
- Modify: AGENTS.md

- [ ] **Step 1: Write standalone README**

Document Emacs 30.2, exact load path, inert loading, unauthenticated endpoint, actor customization, all commands/local keys, save/conflict behavior, heartbeat/token safety, and tests. State that no local mirror exists.

- [ ] **Step 2: Add a failing module-load assertion**

Add the exact package path to test-helper.el. Add org-note to the module load test's existing expected features, or:

~~~elisp
(unless (featurep 'org-note)
  (error "org-note was not loaded"))
~~~

Run:

~~~sh
./emacs.d/tests/module-load-test.sh
~~~

Expected: failure because normal init does not yet load the package.

- [ ] **Step 3: Integrate exact load path and feature order**

In init.el, add only site-lisp/org-note beside exact Agent Editor MCP. Never scan site-lisp. In gsmlg-apps.el, declare org-note and require it immediately after gsmlg-org:

~~~elisp
(gsmlg-apps-require 'org-note)
~~~

Do not create a wrapper module.

- [ ] **Step 4: Wire the package runner**

Add the org-note run_tests.sh call beside Agent Editor MCP in run-emacs-tests.sh, using its existing repository-root variable.

- [ ] **Step 5: Update docs and repository policy**

Document both exact vendored paths, inert loading, and gsmlg-org then org-note order in architecture/migration docs. Update AGENTS.md to permit exactly agent-editor-mcp and org-note while retaining the recursive-scan prohibition.

- [ ] **Step 6: Run scoped integration checks**

~~~sh
./emacs.d/site-lisp/org-note/run_tests.sh
./emacs.d/tests/module-load-test.sh
./lint-emacs-config.sh
~~~

Expected: package tests and module load pass; strict lint/checkdoc/byte compilation reports no first-party warning.

### Task 10: Complete Validation

**Files:**

- No planned source changes.

- [ ] **Step 1: Run the complete suite serially**

~~~sh
./run-emacs-tests.sh
~~~

Expected: the complete configuration suite, Org Note suite, and Agent Editor MCP suite pass. If an out-of-scope test fails, record it and stop without changing unrelated code.

- [ ] **Step 2: Check generated artifacts**

~~~sh
find emacs.d -type f \( -name '*.elc' -o -name '*.eln' \) -print
~~~

Expected: no output.

- [ ] **Step 3: Inspect final scoped state**

~~~sh
git status --short
git diff --check
~~~

Expected: only the approved design, this plan, the Org Note package, and listed integration/documentation files are changed; diff check exits zero.

Do not stage, commit, or push.
