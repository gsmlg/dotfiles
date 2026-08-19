# Org Note Document Create Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let Emacs create a new Org document in an Agent Note workspace as blank or by copying `source` from a `templates/` document, then open it with the existing remote document flow.

**Architecture:** Reuse `PUT /api/org/documents/{id}` for create by omitting positive `expected_revision`. Add path/template helpers and an interactive `org-note-document-create` command that generates a UUID, writes the document, then opens it via `org-note-document-open`. Wire `c` in the document list and an autoload in `gsmlg-apps`.

**Tech Stack:** Emacs 30.2 Lisp, built-in `url`/`json`/`org`/`org-id`, existing `org-note-*` modules and ERT suite.

**Spec:** `docs/superpowers/specs/2026-08-19-org-note-document-create-design.md`

## Global Constraints

- GNU Emacs 30.2 minimum.
- Only built-in libraries at runtime (plus the vendored `org-note` package itself).
- No authentication; default endpoint remains `https://agent-note.gsmlg.net/`.
- Requiring `org-note` stays inert (no request/timer/global binding until a command runs).
- New notes must not be created under the `templates/` path prefix.
- Template identity is the literal case-sensitive path prefix `templates/`.
- Do not invent a server template API; copy from existing Org documents.
- Commits only when the user explicitly asks (skip commit steps otherwise; keep working tree ready to commit).

## File map

| File | Responsibility |
| --- | --- |
| `emacs.d/site-lisp/org-note/org-note-operation.el` | Omit nil `expected_revision` on PUT; add `org-note-operation-create-document` |
| `emacs.d/site-lisp/org-note/org-note.el` | Path/template helpers, interactive create, document-list `c` binding |
| `emacs.d/lisp/gsmlg-apps.el` | Autoload `org-note-document-create` |
| `emacs.d/site-lisp/org-note/README.md` | Document create commands and template rules |
| `emacs.d/site-lisp/org-note/design.md` | Fix non-goals; document create as Emacs goal |
| `emacs.d/site-lisp/org-note/test/org-note-operation-test.el` | Create/update PUT body tests |
| `emacs.d/site-lisp/org-note/test/org-note-test.el` | Create UX, filtering, failure atomicity tests |
| `emacs.d/tests/modules-test.el` | Include create command in Org Note autoload assertions |

---

### Task 1: Create PUT omits nil expected_revision

**Files:**
- Modify: `emacs.d/site-lisp/org-note/org-note-operation.el` (`org-note-operation-put-document`, add create helper)
- Test: `emacs.d/site-lisp/org-note/test/org-note-operation-test.el`

**Interfaces:**
- Consumes: `org-note-operation--mutation-body`, `org-note-client-request`, `org-note-client-empty-object`
- Produces:
  - `org-note-operation-put-document` — when `expected-revision` is nil, body has no `expected_revision` key
  - `(org-note-operation-create-document workspace-id document-id path source &key operation-id)` → same response shape as put; always omits `expected_revision`; `lease_proofs` empty object

- [ ] **Step 1: Write the failing tests**

Add to `org-note-operation-test.el` (keep the existing update test that sends revision `3`). Replace the body expectation in `org-note-operation-puts-document-sends-empty-proof-object` so nil revision means the key is absent, and add a create helper test:

```elisp
(ert-deftest org-note-operation-puts-document-omits-nil-expected-revision ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-put-document
            "workspace-1" "document-1" "notes/today.org" "* Today" nil nil)))
         (body (nth 3 request))
         (operation-id (alist-get 'operation_id body)))
    (should (stringp operation-id))
    (should-not (assq 'expected_revision body))
    (org-note-operation-test--should-equal-json-object
     body
     `((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . ,operation-id)
       (workspace_id . "workspace-1")
       (path . "notes/today.org")
       (source . "* Today")
       (lease_proofs . ,(org-note-client-empty-object))))))

(ert-deftest org-note-operation-create-document-omits-expected-revision ()
  (let* ((org-note-actor-id "emacs:test@example")
         (request
          (org-note-operation-test--capture-request
           (org-note-operation-create-document
            "workspace-1" "document-new" "notes/new.org" ""
            :operation-id "operation-create")))
         (body (nth 3 request)))
    (should (equal (cl-subseq request 0 3)
                   '("PUT" "/api/org/documents/document-new" nil)))
    (should-not (assq 'expected_revision body))
    (org-note-operation-test--should-equal-json-object
     body
     '((schema_version . 1)
       (actor_id . "emacs:test@example")
       (operation_id . "operation-create")
       (workspace_id . "workspace-1")
       (path . "notes/new.org")
       (source . "")
       (lease_proofs . ,(org-note-client-empty-object))))))
```

Delete or rename the old `org-note-operation-puts-document-sends-empty-proof-object` test so it no longer expects `(expected_revision . nil)`.

- [ ] **Step 2: Run tests to verify they fail**

Run:

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
```

Expected: FAIL on the new/updated tests (`org-note-operation-create-document` void and/or `expected_revision` still present).

- [ ] **Step 3: Implement minimal PUT/create changes**

In `org-note-operation-put-document`, build fields so nil revision is omitted:

```elisp
(cl-defun org-note-operation-put-document
    (workspace-id document-id path source expected-revision lease-proofs
                  &key operation-id)
  "Write DOCUMENT-ID in WORKSPACE-ID with PATH and SOURCE.

EXPECTED-REVISION controls optimistic concurrency for updates.  When it is
nil, the field is omitted so the service can create the document.
LEASE-PROOFS is required; a nil value is encoded as an empty JSON object.
OPERATION-ID optionally supplies the mutation ID."
  (org-note-client-request
   "PUT"
   (format "/api/org/documents/%s"
           (org-note-operation--path-segment document-id))
   nil
   (org-note-operation--mutation-body
    workspace-id
    (append
     `((path . ,path)
       (source . ,source))
     (and expected-revision
          `((expected_revision . ,expected-revision)))
     `((lease_proofs . ,(or lease-proofs (org-note-client-empty-object)))))
    operation-id)))

(cl-defun org-note-operation-create-document
    (workspace-id document-id path source &key operation-id)
  "Create DOCUMENT-ID in WORKSPACE-ID at PATH with SOURCE.

SOURCE may be the empty string.  The request omits expected_revision."
  (org-note-operation-put-document
   workspace-id document-id path source nil nil
   :operation-id operation-id))
```

- [ ] **Step 4: Run tests to verify they pass**

Run:

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
```

Expected: the Task 1 tests PASS (full suite may still lack later create UX tests).

- [ ] **Step 5: Commit (only if the user asked to commit)**

```bash
git add emacs.d/site-lisp/org-note/org-note-operation.el \
  emacs.d/site-lisp/org-note/test/org-note-operation-test.el
git commit -m "$(cat <<'EOF'
fix(org-note): omit nil expected_revision on document create PUT

EOF
)"
```

---

### Task 2: Path validation and template filtering helpers

**Files:**
- Modify: `emacs.d/site-lisp/org-note/org-note.el`
- Test: `emacs.d/site-lisp/org-note/test/org-note-test.el`

**Interfaces:**
- Consumes: document list rows with `id` / `path` keys (same as `org-note--document-row`)
- Produces:
  - `(defconst org-note-document-template-path-prefix "templates/")`
  - `(org-note--new-document-path-p path)` → non-nil when PATH is a non-empty relative path, has no empty segments, is not absolute, and does **not** start with `templates/`
  - `(org-note--template-document-path-p path)` → non-nil when PATH is a string starting with `templates/`
  - `(org-note--filter-template-documents rows)` → list of rows whose path matches the template prefix

- [ ] **Step 1: Write the failing tests**

```elisp
(ert-deftest org-note-new-document-path-validation ()
  (should (org-note--new-document-path-p "notes/today.org"))
  (should (org-note--new-document-path-p "inbox.org"))
  (should-not (org-note--new-document-path-p ""))
  (should-not (org-note--new-document-path-p nil))
  (should-not (org-note--new-document-path-p "/abs/notes.org"))
  (should-not (org-note--new-document-path-p "notes//today.org"))
  (should-not (org-note--new-document-path-p "templates/base.org"))
  (should-not (org-note--new-document-path-p "templates/")))

(ert-deftest org-note-filter-template-documents-uses-path-prefix ()
  (let* ((template (org-note-test--document-row
                    "template-a" "templates/base.org" 1))
         (nested (org-note-test--document-row
                  "template-b" "templates/nested/x.org" 2))
         (normal (org-note-test--document-row
                  "document-a" "notes/a.org" 3))
         (almost (org-note-test--document-row
                  "document-b" "template/base.org" 4)))
    (should (equal
             (mapcar (lambda (row) (alist-get 'id row))
                     (org-note--filter-template-documents
                      (list normal template almost nested)))
             '("template-a" "template-b")))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run:

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
```

Expected: FAIL with void-function for the new helpers.

- [ ] **Step 3: Implement helpers**

Near other org-note validation helpers in `org-note.el`:

```elisp
(defconst org-note-document-template-path-prefix "templates/"
  "Path prefix that marks Org documents usable as create templates.")

(defun org-note--template-document-path-p (path)
  "Return non-nil when PATH is under the template prefix."
  (and (stringp path)
       (string-prefix-p org-note-document-template-path-prefix path)))

(defun org-note--new-document-path-p (path)
  "Return non-nil when PATH is valid for a newly created document."
  (and (stringp path)
       (> (length path) 0)
       (not (file-name-absolute-p path))
       (not (org-note--template-document-path-p path))
       (let ((segments (split-string path "/" t)))
         (and segments
              (= (length segments)
                 (length (split-string path "/" nil)))
              (cl-every (lambda (segment)
                          (> (length segment) 0))
                       segments)))))

(defun org-note--filter-template-documents (rows)
  "Return ROWS whose paths are template documents."
  (cl-remove-if-not
   (lambda (row)
     (org-note--template-document-path-p (alist-get 'path row)))
   rows))
```

Note: reject `notes//today.org` by comparing segment counts with/without omission of empties, or by checking for `"//"` / leading/trailing `/` explicitly. Prefer an explicit check:

```elisp
(and (not (string-match-p "\\`/" path))
     (not (string-match-p "/\\'" path))
     (not (string-match-p "//" path))
     (not (org-note--template-document-path-p path))
     ...)
```

Use whichever implementation makes the tests pass without accepting absolute or empty-segment paths.

- [ ] **Step 4: Run tests to verify they pass**

Run:

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
```

Expected: Task 2 tests PASS.

- [ ] **Step 5: Commit (only if the user asked to commit)**

```bash
git add emacs.d/site-lisp/org-note/org-note.el \
  emacs.d/site-lisp/org-note/test/org-note-test.el
git commit -m "$(cat <<'EOF'
feat(org-note): add create path and template filter helpers

EOF
)"
```

---

### Task 3: Interactive `org-note-document-create`

**Files:**
- Modify: `emacs.d/site-lisp/org-note/org-note.el`
- Possibly modify: `emacs.d/site-lisp/org-note/org-note-document.el` only if open helpers are needed (prefer keeping create orchestration in `org-note.el`)
- Test: `emacs.d/site-lisp/org-note/test/org-note-test.el`

**Interfaces:**
- Consumes:
  - `org-note-operation-list-documents`
  - `org-note-operation-get-document`
  - `org-note-operation-create-document`
  - `org-note-document-open`
  - Task 2 helpers
  - `org-id-uuid` from `org-id`
- Produces:
  - `(org-note--list-all-documents workspace-id)` → complete list of document rows
  - `(org-note-document-create &optional workspace-id)` interactive command

- [ ] **Step 1: Write the failing tests**

```elisp
(ert-deftest org-note-document-create-blank-opens-new-document ()
  (let (created opened buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-id-uuid)
                   (lambda () "11111111-1111-4111-8111-111111111111"))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Blank"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "notes/created.org"))
                  ((symbol-function 'org-note-operation-create-document)
                   (lambda (workspace-id document-id path source &rest _)
                     (setq created
                           (list workspace-id document-id path source))
                     '((document_revisions
                        . (("11111111-1111-4111-8111-111111111111" . 1))))))
                  ((symbol-function 'org-note-document-open)
                   (lambda (workspace-id document-id)
                     (setq opened (list workspace-id document-id))
                     (setq buffer (get-buffer-create " *org-note-create*"))
                     buffer)))
          (should (eq buffer
                      (org-note-document-create "workspace-a")))
          (should (equal created
                         '("workspace-a"
                           "11111111-1111-4111-8111-111111111111"
                           "notes/created.org"
                           "")))
          (should (equal opened
                         '("workspace-a"
                           "11111111-1111-4111-8111-111111111111"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest org-note-document-create-template-copies-source ()
  (let (created)
    (cl-letf (((symbol-function 'org-id-uuid)
               (lambda () "22222222-2222-4222-8222-222222222222"))
              ((symbol-function 'completing-read)
               (let ((n 0))
                 (lambda (&rest _)
                   (setq n (1+ n))
                   (pcase n
                     (1 "Template")
                     (_ "templates/base.org")))))
              ((symbol-function 'read-string)
               (lambda (&rest _) "notes/from-template.org"))
              ((symbol-function 'org-note--list-all-documents)
               (lambda (_workspace-id)
                 (list (org-note-test--document-row
                        "template-a" "templates/base.org" 1)
                       (org-note-test--document-row
                        "document-a" "notes/a.org" 2))))
              ((symbol-function 'org-note-operation-get-document)
               (lambda (_workspace-id document-id)
                 (should (equal document-id "template-a"))
                 '((id . "template-a")
                   (workspace_id . "workspace-a")
                   (path . "templates/base.org")
                   (source . "* Template body\n")
                   (content_hash . "hash")
                   (revision . 1))))
              ((symbol-function 'org-note-operation-create-document)
               (lambda (workspace-id document-id path source &rest _)
                 (setq created
                       (list workspace-id document-id path source))
                 '((document_revisions
                    . (("22222222-2222-4222-8222-222222222222" . 1))))))
              ((symbol-function 'org-note-document-open)
               (lambda (&rest _) (get-buffer-create " *org-note-create*"))))
      (org-note-document-create "workspace-a")
      (should (equal created
                     '("workspace-a"
                       "22222222-2222-4222-8222-222222222222"
                       "notes/from-template.org"
                       "* Template body\n")))
      (kill-buffer " *org-note-create*"))))

(ert-deftest org-note-document-create-rejects-templates-destination ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) "Blank"))
            ((symbol-function 'read-string)
             (lambda (&rest _) "templates/nope.org"))
            ((symbol-function 'org-note-operation-create-document)
             (lambda (&rest _)
               (ert-fail "create must not run for templates/ paths"))))
    (should-error (org-note-document-create "workspace-a")
                  :type 'user-error)))

(ert-deftest org-note-document-create-failure-leaves-no-buffer ()
  (let ((before (buffer-list)))
    (cl-letf (((symbol-function 'org-id-uuid)
               (lambda () "33333333-3333-4333-8333-333333333333"))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "Blank"))
              ((symbol-function 'read-string)
               (lambda (&rest _) "notes/fail.org"))
              ((symbol-function 'org-note-operation-create-document)
               (lambda (&rest _)
                 (signal 'org-note-error '("create failed"))))
              ((symbol-function 'org-note-document-open)
               (lambda (&rest _)
                 (ert-fail "open must not run after create failure"))))
      (should-error (org-note-document-create "workspace-a")
                    :type 'org-note-error)
      (should (equal (buffer-list) before)))))
```

Adjust completing-read stubbing if the implementation uses `yes-or-no` style or a fixed prompt table; the important assertions are create args, open args, and failure atomicity.

- [ ] **Step 2: Run tests to verify they fail**

Run:

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
```

Expected: FAIL because `org-note-document-create` is undefined.

- [ ] **Step 3: Implement create command**

Require `org-id` from `org-note.el` (or call `require` inside the command). Implement:

```elisp
(defun org-note--list-all-documents (workspace-id)
  "Return every document row in WORKSPACE-ID."
  (let ((cursor nil)
        (rows nil)
        done)
    (while (not done)
      (let* ((response
              (org-note-operation-list-documents
               workspace-id :cursor cursor))
             (page (org-note--prepare-page response #'org-note--document-row))
             (row-data (nth 1 page))
             (next (nth 2 page)))
        (maphash (lambda (_id row) (push row rows)) row-data)
        (if next
            (setq cursor next)
          (setq done t))))
    (nreverse rows)))

(defun org-note--read-create-source (workspace-id)
  "Return SOURCE string for a new document in WORKSPACE-ID."
  (pcase (completing-read "Create from: " '("Blank" "Template") nil t nil nil "Blank")
    ("Blank" "")
    ("Template"
     (let* ((templates
             (org-note--filter-template-documents
              (org-note--list-all-documents workspace-id)))
            (choices
             (mapcar (lambda (row)
                       (cons (alist-get 'path row) row))
                     templates)))
       (unless choices
         (user-error "No Org Note templates under templates/"))
       (let* ((path (completing-read "Template: " choices nil t))
              (row (cdr (assoc path choices)))
              (document-id (alist-get 'id row))
              (response
               (org-note-operation-get-document workspace-id document-id))
              (source (alist-get 'source response)))
         (unless (stringp source)
           (signal 'org-note-error
                   '("Org Note template source is malformed")))
         source)))
    (_ (user-error "Unknown Org Note create source"))))

(defun org-note-document-create (&optional workspace-id)
  "Create a new Org Note document in WORKSPACE-ID and open it.

When WORKSPACE-ID is nil, use the current document list workspace or prompt."
  (interactive
   (list (or org-note--browser-workspace-id
             (org-note--read-workspace-id-for-create))))
  (unless (org-note-document--non-empty-string-p workspace-id)
    ;; If document helpers are not visible, duplicate the non-empty string
    ;; check locally or expose a tiny shared predicate.
    (user-error "Org Note workspace id must be a non-empty string"))
  (let* ((source (org-note--read-create-source workspace-id))
         (path (read-string "New document path: "))
         (document-id (org-id-uuid))
         list-buffer)
    (unless (org-note--new-document-path-p path)
      (user-error "Invalid Org Note document path: %s" path))
    (when (derived-mode-p 'org-note-document-list-mode)
      (setq list-buffer (current-buffer)))
    (org-note-operation-create-document workspace-id document-id path source)
    (prog1 (org-note-document-open workspace-id document-id)
      (when (buffer-live-p list-buffer)
        (with-current-buffer list-buffer
          (org-note-browser-refresh))))))
```

Implement `org-note--read-workspace-id-for-create` by listing workspaces (reuse `org-note-operation-list-workspaces` + completing-read on display/slug/id) following patterns already used by queue/agenda workspace prompts in `org-note.el`. Prefer the smallest existing helper if one already reads a single workspace id.

Also bind create in the document list map:

```elisp
(defvar-keymap org-note-document-list-mode-map
  ...
  "c" #'org-note-document-create
  ...)
```

- [ ] **Step 4: Run tests to verify they pass**

Run:

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
```

Expected: create-related tests PASS. Fix prompt/stub mismatches if any test assumes exact completing-read order.

- [ ] **Step 5: Commit (only if the user asked to commit)**

```bash
git add emacs.d/site-lisp/org-note/org-note.el \
  emacs.d/site-lisp/org-note/test/org-note-test.el
git commit -m "$(cat <<'EOF'
feat(org-note): add interactive Org document create

EOF
)"
```

---

### Task 4: Autoload, docs, and architecture wording

**Files:**
- Modify: `emacs.d/lisp/gsmlg-apps.el`
- Modify: `emacs.d/tests/modules-test.el`
- Modify: `emacs.d/site-lisp/org-note/README.md`
- Modify: `emacs.d/site-lisp/org-note/design.md`

**Interfaces:**
- Consumes: `org-note-document-create`
- Produces: startup autoload for that command; docs matching the shipped behavior

- [ ] **Step 1: Write/update the failing autoload assertion**

In `gsmlg-modules-org-note-entry-commands-are-autoloaded`, add `org-note-document-create` to the command list.

- [ ] **Step 2: Run the focused modules assertion path or full org-note suite after code changes**

After editing `gsmlg-apps.el`, a minimal check:

```sh
emacs -Q --batch \
  -L emacs.d/lisp -L emacs.d/lisp/lang \
  -L emacs.d/site-lisp/org-note \
  -L emacs.d/site-lisp/agent-editor-mcp \
  --eval "(progn (require 'gsmlg-apps) \
    (unless (autoloadp (symbol-function 'org-note-document-create)) \
      (error \"missing autoload\")) \
    (message \"PASS\"))"
```

Expected before code change: FAIL / missing autoload.

- [ ] **Step 3: Add autoload and update docs**

In `gsmlg-apps.el`, add `org-note-document-create` to the Org Note autoload list.

In `README.md`, document:

- `org-note-document-create`
- document-list `c`
- Blank vs Template (`templates/` prefix)
- new paths must not start with `templates/`

In `design.md`:

- Remove document creation from Emacs non-goals (keep true non-goals: local mirrors, auth, workspace creation, structured item creation, etc.).
- Add a short “Document creation” subsection matching the approved spec.
- State that Web UI scope is separate from the Emacs client goals.

- [ ] **Step 4: Re-run verification**

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
emacs -Q --batch \
  -L emacs.d/lisp -L emacs.d/lisp/lang \
  -L emacs.d/site-lisp/org-note \
  -L emacs.d/site-lisp/agent-editor-mcp \
  --eval "(progn (require 'gsmlg-apps) \
    (unless (autoloadp (symbol-function 'org-note-document-create)) \
      (error \"missing autoload\")) \
    (message \"PASS\"))"
```

Expected: PASS.

- [ ] **Step 5: Commit (only if the user asked to commit)**

```bash
git add emacs.d/lisp/gsmlg-apps.el \
  emacs.d/tests/modules-test.el \
  emacs.d/site-lisp/org-note/README.md \
  emacs.d/site-lisp/org-note/design.md
git commit -m "$(cat <<'EOF'
docs(org-note): document create flow and wire autoload

EOF
)"
```

---

### Task 5: Final verification

**Files:**
- None beyond fixes discovered while verifying

- [ ] **Step 1: Run the Org Note package suite**

```sh
./emacs.d/site-lisp/org-note/run_tests.sh
```

Expected: all tests pass.

- [ ] **Step 2: Smoke against the live API (optional but recommended)**

```sh
emacs -Q --batch \
  -L emacs.d/site-lisp/org-note \
  --eval "(progn
    (require 'org-note)
    (require 'org-id)
    (let* ((ws \"114757c0-5a49-4d54-ba36-752288b7eba3\")
           (id (org-id-uuid))
           (path (format \"notes/emacs-create-%s.org\" id)))
      (org-note-operation-create-document ws id path \"* Created from plan\\n\")
      (org-note-document-open ws id)
      (message \"PASS %s\" path)))"
```

Expected: `PASS notes/emacs-create-....org` and an openable buffer.

- [ ] **Step 3: Manual checklist in interactive Emacs**

1. `M-x org-note-workspaces` → open documents → press `c`
2. Create Blank at `notes/manual-blank.org`
3. Ensure a `templates/...` document exists, then create from Template
4. Confirm `templates/new.org` is rejected
5. Edit and `C-x C-s` the new document

- [ ] **Step 4: Commit remaining changes only if the user asks**

---

## Spec coverage self-review

| Spec requirement | Task |
| --- | --- |
| Blank create with empty source | Task 3 |
| Template copy from `templates/` docs | Task 2 + Task 3 |
| PUT create without positive expected_revision | Task 1 |
| Open via existing document flow after create | Task 3 |
| Document list `c` + M-x command | Task 3 |
| Autoload | Task 4 |
| Reject create under `templates/` | Task 2 + Task 3 |
| Failure leaves no buffer | Task 3 |
| Docs/design non-goal correction | Task 4 |
| Package tests | Task 1–5 |

## Placeholder / consistency self-review

- No TBD/TODO left in steps.
- Create helper name is consistently `org-note-operation-create-document`.
- Interactive command name is consistently `org-note-document-create`.
- Template prefix constant is `org-note-document-template-path-prefix` = `"templates/"`.
- Commit steps are gated on explicit user request to respect repo commit policy.
