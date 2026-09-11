# Org Note Org Bridge — Phase 2 (TODO / State / Local Refuse) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Status:** Phase 2 implementation and scoped tests complete on 2026-09-10.
The full suite is blocked before bridge checks by the baseline Elpaca failure
recorded under Task 8; the bridge remains default-disabled.

**Goal:** Make Org TODO keyword configuration and single-row TODO transitions use Org Note as the sole mutation path while refusing plain local `.org` TODO/refile/archive/clock, without enabling the bridge for normal users yet.

**Architecture:** Extend Phase 1’s `gsmlg-org-note-org.el` with atomic state configuration, a pure target-state resolver, plain-local refuse advice, identified-item preflight, and frozen transition / id-less document-put attempts. Strengthen vendored `org-note-operation-transition` to freeze+dispatch and validate every response. Prefer keeping Phase 2 helpers in `gsmlg-org-note-org.el` while the file stays maintainable (~900 lines); split only if that limit is clearly exceeded.

**Tech Stack:** GNU Emacs 30.2, lexical-binding Emacs Lisp, Org, ERT, Phase 1 `gsmlg-org-note-org` + `org-note-operation` freeze/dispatch primitives, XDG paths via `gsmlg-paths`.

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
- **Do not create commits unless the user explicitly authorizes them.** Skip commit steps or stop and ask. Every Commit step below is **SKIP** unless the user authorizes.
- After touching vendored org-note: run `emacs.d/site-lisp/org-note/run_tests.sh`.
- After first-party Lisp changes: run scoped ERT, then `./run-emacs-tests.sh` before declaring a phase done.
- Follow `AGENTS.md`: `gsmlg-` prefix, lexical-binding, `setopt`/`defcustom`, named hooks, Emacs 30 keymap APIs where relevant, Elpaca ownership, no package.el.
- **Phase 2 out of scope (defer):** Capture journal (Phase 3), same-doc refile engine (Phase 4), clock claim/release engines (Phase 5), archive document flow beyond refuse stubs / archive-target validation (Phase 6), multi-process locks (Phase 7). Crash markers / full ambiguity recovery may be minimal fail-closed stubs sufficient for Phase 2 paths only.

## File Map (Phase 2)

Create:

- `emacs.d/tests/org-note-org-bridge-todo-test.el` — state configuration, resolver, plain-local refuse, identified transition, agenda-todo intercept, id-less document TODO.

Modify:

- `emacs.d/lisp/gsmlg-org-note-org.el` — state defcustoms + transaction, keyword install/recompute, refuse advice, preflight, transition/document-TODO attempts, `org-todo` / `org-agenda-todo` advice.
- `emacs.d/site-lisp/org-note/org-note-operation.el` — freeze+dispatch for transition; unconditional strengthened response validation.
- `emacs.d/site-lisp/org-note/test/org-note-operation-test.el` — frozen transition reuse + unconditional validation tests.
- Optionally (only if `gsmlg-org-note-org.el` clearly exceeds ~900 lines after Task 7): extract pure helpers into `emacs.d/lisp/gsmlg-org-note-todo.el` and `(require ...)` from the bridge. Prefer same-file until then.

Do not invent parallel bridge stacks. Do not enable `gsmlg-org-note-org-enable` by default.

### Focused test commands (reuse across tasks)

Bridge TODO ERT (from repository root):

```bash
emacs -Q --batch \
  -L emacs.d/lisp \
  -L emacs.d/tests \
  -L emacs.d/site-lisp/org-note \
  -l emacs.d/tests/org-note-org-bridge-todo-test.el \
  --eval "(ert-run-tests-batch-and-exit \"^gsmlg-org-note-\")"
```

Vendored package:

```bash
cd emacs.d/site-lisp/org-note && ./run_tests.sh
```

---

### Task 1: Atomic TODO state configuration + live keyword recompute

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org-note-org.el`
- Test: `emacs.d/tests/org-note-org-bridge-todo-test.el` (create)

**Interfaces:**
- Consumes: Phase 1 `gsmlg-org-note-org-activate`, `gsmlg-org-note-org-enable`, Org keyword APIs (`org-set-regexps-and-options`, `org-todo-keywords`, `org-todo-key-alist`, `org-todo-key-trigger`)
- Produces:
  - `gsmlg-org-note-todo-states` — list of nonempty active state strings; default `("TODO" "RUNNING")`
  - `gsmlg-org-note-done-states` — list of nonempty done state strings; default `("DONE")`
  - `gsmlg-org-note-state-fast-keys` — alist `(CHAR . STATE-STRING)`; default `nil`
  - `gsmlg-org-note-archive-target` — one done-state string; default `"DONE"`
  - `(gsmlg-org-note-apply-state-configuration todo-states done-states fast-keys archive-target) -> t` or signals `user-error`; sole multi-field commit path
  - `(gsmlg-org-note-org--validate-state-configuration todo done keys archive) -> plist` of precomputed `:sequence` `:key-alist` `:key-trigger` or signals
  - `(gsmlg-org-note-org--install-todo-keywords)` — installs validated current tuple into Org + live bridge buffers
  - Individual defcustom `:set` functions validate candidate + other current values and either commit one field or `user-error` directing multi-field changes to `gsmlg-org-note-apply-state-configuration`

- [x] **Step 1: Create failing tests for validation and atomic apply**

Create `emacs.d/tests/org-note-org-bridge-todo-test.el`:

```elisp
;;; org-note-org-bridge-todo-test.el --- Phase 2 TODO bridge tests -*- lexical-binding: t; -*-

;;; Commentary:
;; State configuration, refuse policy, TODO transitions, agenda todo intercept.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)

(unless (require 'gsmlg-paths nil t)
  (defvar gsmlg-cache-directory
    (file-name-as-directory
     (make-temp-file "gsmlg-org-note-org-cache-" t)))
  (defvar gsmlg-state-directory
    (file-name-as-directory
     (make-temp-file "gsmlg-org-note-org-state-" t)))
  (defun gsmlg-cache-file (name)
    (expand-file-name name gsmlg-cache-directory))
  (defun gsmlg-state-file (name)
    (expand-file-name name gsmlg-state-directory))
  (defun gsmlg-ensure-parent-directory (file)
    (make-directory (file-name-directory file) t)
    file)
  (provide 'gsmlg-paths))

(ert-deftest gsmlg-org-note-apply-state-configuration-rejects-overlap ()
  (require 'gsmlg-org-note-org)
  (should-error
   (gsmlg-org-note-apply-state-configuration
    '("TODO" "DONE") '("DONE") nil "DONE")
   :type 'user-error))

(ert-deftest gsmlg-org-note-apply-state-configuration-rejects-bad-fast-key ()
  (require 'gsmlg-org-note-org)
  (should-error
   (gsmlg-org-note-apply-state-configuration
    '("TODO") '("DONE") '((?! . "TODO")) "DONE")
   :type 'user-error)
  (should-error
   (gsmlg-org-note-apply-state-configuration
    '("TODO") '("DONE") '((?t . "MISSING")) "DONE")
   :type 'user-error))

(ert-deftest gsmlg-org-note-apply-state-configuration-rejects-archive-outside-done ()
  (require 'gsmlg-org-note-org)
  (should-error
   (gsmlg-org-note-apply-state-configuration
    '("TODO") '("DONE") nil "ARCHIVED")
   :type 'user-error))

(ert-deftest gsmlg-org-note-apply-state-configuration-commits-atomically ()
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-todo-states '("TODO" "RUNNING"))
        (gsmlg-org-note-done-states '("DONE"))
        (gsmlg-org-note-state-fast-keys nil)
        (gsmlg-org-note-archive-target "DONE"))
    (gsmlg-org-note-apply-state-configuration
     '("NEXT" "WAITING") '("FINISHED") '((?n . "NEXT") (?f . "FINISHED"))
     "FINISHED")
    (should (equal gsmlg-org-note-todo-states '("NEXT" "WAITING")))
    (should (equal gsmlg-org-note-done-states '("FINISHED")))
    (should (equal gsmlg-org-note-state-fast-keys
                   '((?n . "NEXT") (?f . "FINISHED"))))
    (should (equal gsmlg-org-note-archive-target "FINISHED"))
    (should (member '("sequence" "NEXT" "WAITING" "|" "FINISHED")
                    (mapcar #'identity org-todo-keywords)))))

(ert-deftest gsmlg-org-note-apply-state-configuration-rolls-back-on-recompute-failure ()
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-todo-states '("TODO"))
        (gsmlg-org-note-done-states '("DONE"))
        (gsmlg-org-note-state-fast-keys nil)
        (gsmlg-org-note-archive-target "DONE")
        (calls 0))
    (cl-letf (((symbol-function 'gsmlg-org-note-org--recompute-live-buffers)
               (lambda (&rest _)
                 (cl-incf calls)
                 (error "simulated recompute failure"))))
      (should-error
       (gsmlg-org-note-apply-state-configuration
        '("A") '("B") nil "B"))
      (should (equal gsmlg-org-note-todo-states '("TODO")))
      (should (equal gsmlg-org-note-done-states '("DONE")))
      (should (= calls 1)))))

(ert-deftest gsmlg-org-note-state-defaults-make-org-todo-operable ()
  (require 'gsmlg-org-note-org)
  (gsmlg-org-note-org--install-todo-keywords)
  (should (member "TODO" org-todo-keywords-1))
  (should (member "RUNNING" org-todo-keywords-1))
  (should (member "DONE" org-done-keywords)))

(provide 'org-note-org-bridge-todo-test)
;;; org-note-org-bridge-todo-test.el ends here
```

- [ ] **Step 2: Run tests — expect fail (missing symbols)**

```bash
emacs -Q --batch -L emacs.d/lisp -L emacs.d/tests -L emacs.d/site-lisp/org-note \
  -l emacs.d/tests/org-note-org-bridge-todo-test.el \
  --eval "(ert-run-tests-batch-and-exit \"^gsmlg-org-note-apply-state\|^gsmlg-org-note-state-defaults\")"
```

Expected: FAIL with void-function / void-variable for `gsmlg-org-note-apply-state-configuration` or related symbols.

- [x] **Step 3: Implement configuration APIs in `gsmlg-org-note-org.el`**

Add after the existing `defcustom gsmlg-org-note-org-enable` block (keep enable default `nil`):

```elisp
(defun gsmlg-org-note-org--state-string-valid-p (value)
  "Return non-nil when VALUE is a valid Org Note TODO state string."
  (and (stringp value)
       (not (string-empty-p value))
       (not (string-match-p "[[:space:]\n\r\t\f\v|()]", value))))

(defun gsmlg-org-note-org--fast-key-char-valid-p (key)
  "Return non-nil when KEY is a printable non-reserved Org fast-selection char."
  (and (characterp key)
       (not (memq key '(?! ?@ ?/ ?\s ?\t ?\n ?\r ?\f ?\v)))
       (>= key 33)
       (<= key 126)))

(defun gsmlg-org-note-org--validate-state-configuration
    (todo-states done-states fast-keys archive-target)
  "Validate the four-field state tuple and return precomputed keyword tables.

Signals `user-error' without mutating defcustoms or live buffers."
  (unless (and (listp todo-states) todo-states
               (cl-every #'gsmlg-org-note-org--state-string-valid-p todo-states))
    (user-error "Org Note active TODO states are invalid"))
  (unless (and (listp done-states) done-states
               (cl-every #'gsmlg-org-note-org--state-string-valid-p done-states))
    (user-error "Org Note done TODO states are invalid"))
  (let ((all (append todo-states done-states)))
    (unless (= (length all) (length (cl-delete-duplicates (copy-sequence all)
                                                          :test #'equal)))
      (user-error "Org Note TODO states must be unique and disjoint"))
    (unless (and (stringp archive-target)
                 (member archive-target done-states))
      (user-error "Org Note archive target must be a configured done state"))
    (unless (listp fast-keys)
      (user-error "Org Note state fast keys must be an alist"))
    (let ((seen-keys nil)
          (key-alist nil))
      (dolist (entry fast-keys)
        (unless (and (consp entry)
                     (gsmlg-org-note-org--fast-key-char-valid-p (car entry))
                     (member (cdr entry) all))
          (user-error "Org Note state fast keys are invalid"))
        (when (memq (car entry) seen-keys)
          (user-error "Org Note state fast keys must be unique"))
        (push (car entry) seen-keys)
        (push (cons (car entry) (cdr entry)) key-alist))
      (setq key-alist (nreverse key-alist))
      (let* ((sequence (append '("sequence")
                               todo-states
                               '("|")
                               done-states))
             (keywords (list sequence))
             (parsed-alist nil)
             (parsed-trigger nil))
        (with-temp-buffer
          (org-mode)
          (setq-local org-todo-keywords keywords)
          (org-set-regexps-and-options)
          (setq parsed-alist org-todo-key-alist
                parsed-trigger org-todo-key-trigger)
          ;; Prove Org accepts the union; then rebuild maps from explicit keys only.
          (setq-local org-todo-key-alist nil
                      org-todo-key-trigger nil)
          (dolist (pair key-alist)
            (let* ((ch (car pair))
                   (state (cdr pair))
                   (found (rassoc state parsed-alist)))
              (unless found
                (user-error "Org Note fast key target %s is not a TODO keyword"
                            state))
              (push (cons ch state) org-todo-key-alist)))
          (setq-local org-todo-key-alist (nreverse org-todo-key-alist)
                      org-todo-key-trigger (and org-todo-key-alist t))
          (dolist (pair key-alist)
            (unless (equal (cdr (assq (car pair) org-todo-key-alist))
                           (cdr pair))
              (user-error "Org Note fast-key map failed Org round-trip")))
          (list :sequence sequence
                :keywords keywords
                :key-alist (copy-sequence org-todo-key-alist)
                :key-trigger org-todo-key-trigger
                :todo-states (copy-sequence todo-states)
                :done-states (copy-sequence done-states)
                :fast-keys (copy-tree fast-keys)
                :archive-target archive-target))))))

(defun gsmlg-org-note-org--bridge-buffer-p ()
  "Return non-nil when the current buffer is a bridge-owned Org presentation."
  (or (and (boundp 'org-note-document-mode)
           (derived-mode-p 'org-note-document-mode))
      (and (stringp buffer-file-name)
           (or (equal buffer-file-name (gsmlg-org-note-org-feed-file))
               (string-prefix-p
                (file-name-as-directory (expand-file-name gsmlg-cache-directory))
                (file-name-as-directory
                 (file-name-directory (expand-file-name buffer-file-name))))))
      (and (stringp (buffer-name))
           (or (string-prefix-p "*Org Note" (buffer-name))
               (string-match-p "Org Note" (buffer-name))))))

(defun gsmlg-org-note-org--apply-keywords-in-buffer (precomputed)
  "Install PRECOMPUTED keyword tables in the current Org buffer."
  (when (derived-mode-p 'org-mode)
    (setq-local org-todo-keywords (plist-get precomputed :keywords))
    (org-set-regexps-and-options)
    (setq-local org-todo-key-alist (copy-sequence (plist-get precomputed :key-alist))
                org-todo-key-trigger (plist-get precomputed :key-trigger))))

(defun gsmlg-org-note-org--recompute-live-buffers (precomputed)
  "Recompute keyword tables in every live bridge Org buffer from PRECOMPUTED."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (gsmlg-org-note-org--bridge-buffer-p)
        (gsmlg-org-note-org--apply-keywords-in-buffer precomputed)))))

(defun gsmlg-org-note-org--install-todo-keywords ()
  "Install the validated current state configuration into Org."
  (require 'org)
  (let ((precomputed
         (gsmlg-org-note-org--validate-state-configuration
          gsmlg-org-note-todo-states
          gsmlg-org-note-done-states
          gsmlg-org-note-state-fast-keys
          gsmlg-org-note-archive-target)))
    (setq org-todo-keywords (plist-get precomputed :keywords))
    (org-set-regexps-and-options)
    (setq org-todo-key-alist (copy-sequence (plist-get precomputed :key-alist))
          org-todo-key-trigger (plist-get precomputed :key-trigger))
    (gsmlg-org-note-org--recompute-live-buffers precomputed)
    precomputed))

(defun gsmlg-org-note-apply-state-configuration
    (todo-states done-states fast-keys archive-target)
  "Atomically commit the four Org Note TODO configuration fields.

Validates once, precomputes keyword tables, commits all values, then
recomputes live bridge buffers.  Any failure restores the prior values."
  (let* ((prior-todo gsmlg-org-note-todo-states)
         (prior-done gsmlg-org-note-done-states)
         (prior-keys gsmlg-org-note-state-fast-keys)
         (prior-archive gsmlg-org-note-archive-target)
         (precomputed
          (gsmlg-org-note-org--validate-state-configuration
           todo-states done-states fast-keys archive-target)))
    (condition-case err
        (progn
          (setq gsmlg-org-note-todo-states
                (plist-get precomputed :todo-states)
                gsmlg-org-note-done-states
                (plist-get precomputed :done-states)
                gsmlg-org-note-state-fast-keys
                (plist-get precomputed :fast-keys)
                gsmlg-org-note-archive-target
                (plist-get precomputed :archive-target))
          (gsmlg-org-note-org--install-todo-keywords)
          t)
      (error
       (setq gsmlg-org-note-todo-states prior-todo
             gsmlg-org-note-done-states prior-done
             gsmlg-org-note-state-fast-keys prior-keys
             gsmlg-org-note-archive-target prior-archive)
       (ignore-errors (gsmlg-org-note-org--install-todo-keywords))
       (signal (car err) (cdr err))))))

(defun gsmlg-org-note-org--set-todo-states (symbol value)
  "Defcustom setter for SYMBOL with candidate VALUE."
  (gsmlg-org-note-apply-state-configuration
   value
   gsmlg-org-note-done-states
   gsmlg-org-note-state-fast-keys
   gsmlg-org-note-archive-target)
  (set-default symbol gsmlg-org-note-todo-states))

(defun gsmlg-org-note-org--set-done-states (symbol value)
  "Defcustom setter for SYMBOL with candidate VALUE."
  (gsmlg-org-note-apply-state-configuration
   gsmlg-org-note-todo-states
   value
   gsmlg-org-note-state-fast-keys
   gsmlg-org-note-archive-target)
  (set-default symbol gsmlg-org-note-done-states))

(defun gsmlg-org-note-org--set-fast-keys (symbol value)
  "Defcustom setter for SYMBOL with candidate VALUE."
  (gsmlg-org-note-apply-state-configuration
   gsmlg-org-note-todo-states
   gsmlg-org-note-done-states
   value
   gsmlg-org-note-archive-target)
  (set-default symbol gsmlg-org-note-state-fast-keys))

(defun gsmlg-org-note-org--set-archive-target (symbol value)
  "Defcustom setter for SYMBOL with candidate VALUE."
  (gsmlg-org-note-apply-state-configuration
   gsmlg-org-note-todo-states
   gsmlg-org-note-done-states
   gsmlg-org-note-state-fast-keys
   value)
  (set-default symbol gsmlg-org-note-archive-target))

(defcustom gsmlg-org-note-todo-states '("TODO" "RUNNING")
  "Active Org Note TODO state strings used by the bridge."
  :type '(repeat string)
  :initialize #'custom-initialize-default
  :set #'gsmlg-org-note-org--set-todo-states
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-done-states '("DONE")
  "Done Org Note TODO state strings used by the bridge."
  :type '(repeat string)
  :initialize #'custom-initialize-default
  :set #'gsmlg-org-note-org--set-done-states
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-state-fast-keys nil
  "Optional alist mapping unique characters to configured TODO states."
  :type '(alist :key-type character :value-type string)
  :initialize #'custom-initialize-default
  :set #'gsmlg-org-note-org--set-fast-keys
  :group 'gsmlg-org-note-org)

(defcustom gsmlg-org-note-archive-target "DONE"
  "Configured done state used as the item archive target."
  :type 'string
  :initialize #'custom-initialize-default
  :set #'gsmlg-org-note-org--set-archive-target
  :group 'gsmlg-org-note-org)
```

Use `:initialize #'custom-initialize-default` so file load does not run Org keyword setup through `:set` before Org is available. Customize UI and `gsmlg-org-note-apply-state-configuration` remain the commit paths.

Also call `(gsmlg-org-note-org--install-todo-keywords)` from `gsmlg-org-note-org-activate` after `(require 'org-note)`, wrapped so activation still validates the complete current tuple (fail closed on invalid direct `setq`). Use `with-eval-after-load 'org` around the first install so keywords exist before the first bridge Org buffer initializes, without network I/O:

```elisp
(with-eval-after-load 'org
  (when gsmlg-org-note-org-enable
    (gsmlg-org-note-org--install-todo-keywords)))
```

And inside `gsmlg-org-note-org-activate`, after requiring org-note:

```elisp
(require 'org)
(gsmlg-org-note-org--install-todo-keywords)
```

- [x] **Step 4: Run Task 1 tests — expect PASS**

```bash
emacs -Q --batch -L emacs.d/lisp -L emacs.d/tests -L emacs.d/site-lisp/org-note \
  -l emacs.d/tests/org-note-org-bridge-todo-test.el \
  --eval "(ert-run-tests-batch-and-exit \"^gsmlg-org-note-apply-state\|^gsmlg-org-note-state-defaults\")"
```

Expected: PASS.

- [ ] **Step 5: Commit — SKIP unless user authorizes**

If authorized:

```bash
git add emacs.d/lisp/gsmlg-org-note-org.el \
        emacs.d/tests/org-note-org-bridge-todo-test.el
git commit -m "$(cat <<'EOF'
Add atomic Org Note TODO state configuration for the Org bridge.

EOF
)"
```

---

### Task 2: Pure TODO target-state resolver

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org-note-org.el`
- Test: `emacs.d/tests/org-note-org-bridge-todo-test.el`

**Interfaces:**
- Consumes: `gsmlg-org-note-todo-states`, `gsmlg-org-note-done-states`, `gsmlg-org-note-state-fast-keys`, `org-use-fast-todo-selection`
- Produces:
  - `(gsmlg-org-note-org--ordered-states) -> list` active then done
  - `(gsmlg-org-note-org--resolve-todo-target arg current-state &optional interactive-p) -> string`
    - Supports: `nil` (cycle or fast prompt), `right`, `left`, explicit state string, `done`, positive numeric prefix meaning, numeric `0` as cycle-without-note
    - Raises `user-error` for unsupported: `none`, `""`, negative cancel, `(4)`, `(64)`, `nextset`, `previousset`, `(16)`, unknown strings
  - Does not mutate buffers or network state

- [x] **Step 1: Write failing resolver tests**

Append to `org-note-org-bridge-todo-test.el`:

```elisp
(ert-deftest gsmlg-org-note-resolve-todo-target-cycles-and-wraps ()
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-todo-states '("TODO" "RUNNING"))
        (gsmlg-org-note-done-states '("DONE"))
        (gsmlg-org-note-state-fast-keys nil)
        (org-use-fast-todo-selection nil))
    (should (equal (gsmlg-org-note-org--resolve-todo-target nil "TODO")
                   "RUNNING"))
    (should (equal (gsmlg-org-note-org--resolve-todo-target nil "DONE")
                   "TODO"))
    (should (equal (gsmlg-org-note-org--resolve-todo-target 'left "TODO")
                   "DONE"))
    (should (equal (gsmlg-org-note-org--resolve-todo-target 'right "RUNNING")
                   "DONE"))))

(ert-deftest gsmlg-org-note-resolve-todo-target-explicit-done-numeric ()
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-todo-states '("TODO" "RUNNING"))
        (gsmlg-org-note-done-states '("DONE")))
    (should (equal (gsmlg-org-note-org--resolve-todo-target "RUNNING" "TODO")
                   "RUNNING"))
    (should (equal (gsmlg-org-note-org--resolve-todo-target 'done "TODO")
                   "DONE"))
    (should (equal (gsmlg-org-note-org--resolve-todo-target 2 "TODO")
                   "RUNNING"))
    (should-error (gsmlg-org-note-org--resolve-todo-target "MISSING" "TODO")
                  :type 'user-error)
    (should-error (gsmlg-org-note-org--resolve-todo-target 'none "TODO")
                  :type 'user-error)
    (should-error (gsmlg-org-note-org--resolve-todo-target 'nextset "TODO")
                  :type 'user-error)
    (should-error (gsmlg-org-note-org--resolve-todo-target '(4) "TODO")
                  :type 'user-error)))

(ert-deftest gsmlg-org-note-resolve-todo-target-fast-keys-when-enabled ()
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-todo-states '("TODO" "RUNNING"))
        (gsmlg-org-note-done-states '("DONE"))
        (gsmlg-org-note-state-fast-keys '((?r . "RUNNING") (?d . "DONE")))
        (org-use-fast-todo-selection t)
        (chosen ?d))
    (cl-letf (((symbol-function 'gsmlg-org-note-org--read-fast-todo-key)
               (lambda () chosen)))
      (should (equal (gsmlg-org-note-org--resolve-todo-target nil "TODO" t)
                     "DONE")))
    (let ((gsmlg-org-note-state-fast-keys nil))
      (should (equal (gsmlg-org-note-org--resolve-todo-target nil "TODO" t)
                     "RUNNING")))))
```

- [ ] **Step 2: Run — expect FAIL**

```bash
emacs -Q --batch -L emacs.d/lisp -L emacs.d/tests -L emacs.d/site-lisp/org-note \
  -l emacs.d/tests/org-note-org-bridge-todo-test.el \
  --eval "(ert-run-tests-batch-and-exit \"gsmlg-org-note-resolve-todo-target\")"
```

Expected: void-function `gsmlg-org-note-org--resolve-todo-target`.

- [x] **Step 3: Implement resolver**

```elisp
(defun gsmlg-org-note-org--ordered-states ()
  "Return configured active states followed by done states."
  (append gsmlg-org-note-todo-states gsmlg-org-note-done-states))

(defun gsmlg-org-note-org--cycle-state (current direction)
  "Return the next/previous configured state from CURRENT.

DIRECTION is `forward' or `backward'.  Never cycles to an empty state."
  (let* ((states (gsmlg-org-note-org--ordered-states))
         (len (length states))
         (idx (or (cl-position current states :test #'equal) -1))
         (next (pcase direction
                 ('forward (mod (1+ idx) len))
                 ('backward (mod (1- (if (< idx 0) 0 idx)) len))
                 (_ (user-error "Invalid TODO cycle direction")))))
    (nth next states)))

(defun gsmlg-org-note-org--read-fast-todo-key ()
  "Read one fast-selection character for bridge TODO targeting."
  (let* ((prompt
          (mapconcat
           (lambda (pair)
             (format "[%c] %s" (car pair) (cdr pair)))
           gsmlg-org-note-state-fast-keys
           " "))
         (ch (read-char-exclusive (concat "Org Note TODO: " prompt " "))))
    ch))

(defun gsmlg-org-note-org--resolve-fast-key (ch)
  "Resolve character CH through `gsmlg-org-note-state-fast-keys'."
  (let ((state (alist-get ch gsmlg-org-note-state-fast-keys)))
    (unless state
      (user-error "Unknown Org Note TODO fast key"))
    state))

(defun gsmlg-org-note-org--resolve-todo-target
    (arg current-state &optional interactive-p)
  "Resolve Org TODO ARG to a configured server state string.

CURRENT-STATE is the authoritative current keyword.  INTERACTIVE-P is
non-nil for interactive nil calls that may open the fast-key prompt.
Does not compare equality with CURRENT-STATE; callers do that after
preflight."
  (gsmlg-org-note-org--validate-state-configuration
   gsmlg-org-note-todo-states
   gsmlg-org-note-done-states
   gsmlg-org-note-state-fast-keys
   gsmlg-org-note-archive-target)
  (cond
   ((memq arg '(none nextset previousset))
    (user-error "Org Note bridge does not support TODO argument %S" arg))
   ((equal arg "")
    (user-error "Org Note bridge does not support clearing TODO state"))
   ((and (consp arg) (memq (car arg) '(4 16 64)))
    (user-error "Org Note bridge does not support TODO argument %S" arg))
   ((and (integerp arg) (< arg 0))
    (user-error "Org Note bridge does not support TODO repeater cancel"))
   ((eq arg 'done)
    (car gsmlg-org-note-done-states))
   ((and (integerp arg) (> arg 0))
    (let ((states (gsmlg-org-note-org--ordered-states)))
      (unless (<= arg (length states))
        (user-error "Org Note TODO prefix %s is out of range" arg))
      (nth (1- arg) states)))
   ((or (eq arg 0) (eq arg 'right))
    (gsmlg-org-note-org--cycle-state current-state 'forward))
   ((eq arg 'left)
    (gsmlg-org-note-org--cycle-state current-state 'backward))
   ((stringp arg)
    (unless (member arg (gsmlg-org-note-org--ordered-states))
      (user-error "Unknown Org Note TODO state: %s" arg))
    arg)
   ((null arg)
    (let ((fast-enabled
           (memq org-use-fast-todo-selection '(auto t expert))))
      (if (and interactive-p
               fast-enabled
               gsmlg-org-note-state-fast-keys)
          (gsmlg-org-note-org--resolve-fast-key
           (gsmlg-org-note-org--read-fast-todo-key))
        (gsmlg-org-note-org--cycle-state current-state 'forward))))
   (t
    (user-error "Unsupported Org Note TODO argument: %S" arg))))
```

- [x] **Step 4: Run resolver tests — PASS**

- [ ] **Step 5: Commit — SKIP unless user authorizes**

---

### Task 3: Local plain `.org` refuse policy (+ refile/clock/archive stubs)

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org-note-org.el`
- Test: `emacs.d/tests/org-note-org-bridge-todo-test.el`

**Interfaces:**
- Consumes: `gsmlg-org-note-org-enable`, `gsmlg-org-note-org--activated`, feed file helpers, `org-note-document-mode`
- Produces:
  - `(gsmlg-org-note-org--plain-local-org-buffer-p &optional buffer) -> boolean`
  - `(gsmlg-org-note-org--refuse-if-plain-local command-label)` — `user-error` with clear prompt to use Org Note
  - Around-advice helpers installed on activation for: `org-todo`, `org-refile`, `org-archive-subtree` (and common archive entrypoints used by the keybinding surface), `org-clock-in`, `org-clock-out`, `org-clock-cancel`
  - Body editing remains allowed (no advice on self-insert / org-meta-return)
  - Refile/clock/archive advice: refuse-only stubs (real engines are Phases 4–6). TODO advice body is completed in Tasks 5–6; this task installs the plain-local refuse gate used by all of them.

- [x] **Step 1: Failing tests for plain-local detection and refuse**

```elisp
(ert-deftest gsmlg-org-note-plain-local-org-is-detected ()
  (require 'gsmlg-org-note-org)
  (let ((file (make-temp-file "plain" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (should (gsmlg-org-note-org--plain-local-org-buffer-p)))
      (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest gsmlg-org-note-plain-local-todo-refuses ()
  (require 'gsmlg-org-note-org)
  (let ((file (make-temp-file "plain" nil ".org"))
        (gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (insert "* TODO Task\n")
          (goto-char (point-min))
          (should-error (gsmlg-org-note-org--refuse-if-plain-local "TODO")
                        :type 'user-error))
      (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest gsmlg-org-note-plain-local-body-edit-still-allowed ()
  (require 'gsmlg-org-note-org)
  (let ((file (make-temp-file "plain" nil ".org"))
        (gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (insert "* Heading\nbody")
          (should (string-match-p "body" (buffer-string))))
      (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest gsmlg-org-note-refuse-hooks-installed-on-activate ()
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated nil)
        (gsmlg-org-note-org--mutation-hooks-installed nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _)
                 (unless (eq feature 'org-note)
                   (require feature nil t))
                 feature))
              ((symbol-function 'gsmlg-org-note-org--install-todo-keywords)
               (lambda () nil))
              ((symbol-function 'gsmlg-org-apply-path-settings)
               (lambda () nil)))
      (gsmlg-org-note-org-activate)
      (should (advice-member-p #'gsmlg-org-note-org--around-refile #'org-refile))
      (should (advice-member-p #'gsmlg-org-note-org--around-clock-in #'org-clock-in))
      (should (advice-member-p #'gsmlg-org-note-org--around-archive-subtree
                               #'org-archive-subtree)))))
```

- [ ] **Step 2: Run — expect FAIL**

- [x] **Step 3: Implement detection + refuse stubs + cold-start installation**

```elisp
(defvar gsmlg-org-note-org--mutation-hooks-installed nil
  "Non-nil after TODO/refile/archive/clock refuse-or-bridge advice is installed.")

(defconst gsmlg-org-note-org--plain-local-refuse-fmt
  "Org Note bridge refuses %s in plain local .org buffers; use Org Note documents or agenda."
  "user-error format for plain-local refuse.")

(defun gsmlg-org-note-org--plain-local-org-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a plain local Org file outside the bridge."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'org-mode)
         (not (and (fboundp 'org-note-document-mode)
                   (derived-mode-p 'org-note-document-mode)))
         (not (gsmlg-org-note-org--bridge-buffer-p))
         (or buffer-file-name
             (and (boundp 'buffer-file-truename) buffer-file-truename)))))

(defun gsmlg-org-note-org--refuse-if-plain-local (command-label)
  "Signal `user-error' when the current buffer is plain local Org."
  (when (and gsmlg-org-note-org-enable
             gsmlg-org-note-org--activated
             (gsmlg-org-note-org--plain-local-org-buffer-p))
    (user-error gsmlg-org-note-org--plain-local-refuse-fmt command-label)))

(defun gsmlg-org-note-org--around-refile (orig &rest args)
  "Refuse plain-local and feed refile; Phase 4 owns same-document refile."
  (if (not (and gsmlg-org-note-org-enable gsmlg-org-note-org--activated))
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "refile")
    (user-error
     "Org Note same-document refile is not available until Phase 4")))

(defun gsmlg-org-note-org--around-clock-in (orig &rest args)
  "Refuse plain-local clock-in; Phase 5 owns claim."
  (if (not (and gsmlg-org-note-org-enable gsmlg-org-note-org--activated))
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "clock")
    (user-error "Org Note clock claim is not available until Phase 5")))

(defun gsmlg-org-note-org--around-clock-out (orig &rest args)
  "Refuse bridged clock-out stub; Phase 5 owns release."
  (if (not (and gsmlg-org-note-org-enable gsmlg-org-note-org--activated))
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "clock")
    (user-error "Org Note clock release is not available until Phase 5")))

(defun gsmlg-org-note-org--around-clock-cancel (orig &rest args)
  "Refuse bridged clock-cancel stub; Phase 5 owns cancel/release."
  (if (not (and gsmlg-org-note-org-enable gsmlg-org-note-org--activated))
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "clock")
    (user-error "Org Note clock cancel is not available until Phase 5")))

(defun gsmlg-org-note-org--around-archive-subtree (orig &rest args)
  "Refuse plain-local archive; Phase 6 owns archive engines."
  (if (not (and gsmlg-org-note-org-enable gsmlg-org-note-org--activated))
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "archive")
    (user-error "Org Note archive is not available until Phase 6")))

(defun gsmlg-org-note-org--install-mutation-hooks ()
  "Install TODO/refile/archive/clock advice once."
  (unless gsmlg-org-note-org--mutation-hooks-installed
    (setq gsmlg-org-note-org--mutation-hooks-installed t)
    (require 'org)
    (require 'org-refile nil t)
    (require 'org-archive nil t)
    (require 'org-clock nil t)
    (require 'org-agenda nil t)
    (advice-add #'org-todo :around #'gsmlg-org-note-org--around-todo)
    (advice-add #'org-agenda-todo :around #'gsmlg-org-note-org--around-agenda-todo)
    (advice-add #'org-refile :around #'gsmlg-org-note-org--around-refile)
    (advice-add #'org-clock-in :around #'gsmlg-org-note-org--around-clock-in)
    (advice-add #'org-clock-out :around #'gsmlg-org-note-org--around-clock-out)
    (advice-add #'org-clock-cancel :around #'gsmlg-org-note-org--around-clock-cancel)
    (advice-add #'org-archive-subtree
                :around #'gsmlg-org-note-org--around-archive-subtree)))
```

Temporary stubs for TODO advice until Tasks 5–6 replace them:

```elisp
(defun gsmlg-org-note-org--around-todo (orig &rest args)
  "Phase 2 placeholder: refuse plain local; otherwise call ORIG until Task 6."
  (if (not (and gsmlg-org-note-org-enable gsmlg-org-note-org--activated))
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "TODO")
    (apply orig args)))

(defun gsmlg-org-note-org--around-agenda-todo (orig &rest args)
  "Phase 2 placeholder until Task 6 replaces the body."
  (if (not (and gsmlg-org-note-org-enable gsmlg-org-note-org--activated))
      (apply orig args)
    (apply orig args)))
```

Call `gsmlg-org-note-org--install-mutation-hooks` from `gsmlg-org-note-org-activate` after feed hooks / keyword install.

- [x] **Step 4: Run Task 3 tests — PASS**

- [ ] **Step 5: Commit — SKIP unless user authorizes**

---

### Task 4: Frozen transition dispatch + unconditional response validation

**Files:**
- Modify: `emacs.d/site-lisp/org-note/org-note-operation.el`
- Test: `emacs.d/site-lisp/org-note/test/org-note-operation-test.el`

**Interfaces:**
- Consumes: `org-note-operation--freeze-request`, `org-note-operation--dispatch-frozen`, existing transition validators
- Produces:
  - `(org-note-operation--transition-typed-request workspace-id item-id document-id expected-revision target-state &key lease error metadata operation-id) -> typed-request plist`
  - `(org-note-operation--validate-transition-response response workspace-id item-id document-id expected-revision target-state operation-id &optional expected-lease-id expected-kind) -> context`
    - Always required fields from spec § Identified transition attempts, including document revision **strictly greater** than `expected-revision` and item state equal to `target-state`
  - `org-note-operation-transition` public signature unchanged; internally freeze → dispatch → **always** validate (not only when a registered lease was supplied); then reconcile lease when registered

- [x] **Step 1: Write failing tests**

Append to `org-note-operation-test.el`:

```elisp
(ert-deftest org-note-operation-frozen-transition-reuses-bytes ()
  (let ((org-note-actor-id "emacs:test@example")
        (org-note-endpoint "https://example.test/")
        bodies)
    (cl-letf (((symbol-function 'org-note-client-request-raw)
               (lambda (&rest args)
                 (push (plist-get args :body) bodies)
                 (org-note-operation-test--transition-response
                  "workspace-1" "item-1" "document-1" "op-freeze"
                  nil))))
      (let* ((typed
              (org-note-operation--transition-typed-request
               "workspace-1" "item-1" "document-1" 4 "DONE"
               :operation-id "op-freeze"))
             (env (org-note-operation--freeze-request typed))
             (_ (org-note-operation--dispatch-frozen env))
             (_ (org-note-operation--dispatch-frozen env)))
        (should (= (length bodies) 2))
        (should (eq (car bodies) (cadr bodies)))))))

(ert-deftest org-note-operation-transition-validates-without-lease ()
  (let ((org-note-actor-id "emacs:test@example")
        (org-note-endpoint "https://example.test/")
        (org-note-operation--leases (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'org-note-client-request-raw)
               (lambda (&rest _args)
                 (let ((response
                        (org-note-operation-test--transition-response
                         "workspace-1" "item-1" "document-1" "op-1" nil)))
                   ;; Strengthen fixture: revision 5, state DONE
                   (setf (alist-get 'revision
                                    (alist-get 'document
                                               (alist-get 'context
                                                          (alist-get 'data response))))
                         5)
                   (setf (alist-get 'state
                                    (alist-get 'item
                                               (alist-get 'context
                                                          (alist-get 'data response))))
                         "DONE")
                   response))))
      (should
       (org-note-operation-transition
        "workspace-1" "item-1" "document-1" 4 "DONE"
        :operation-id "op-1")))))

(ert-deftest org-note-operation-transition-rejects-non-advancing-revision ()
  (let ((org-note-actor-id "emacs:test@example")
        (org-note-endpoint "https://example.test/"))
    (cl-letf (((symbol-function 'org-note-client-request-raw)
               (lambda (&rest _args)
                 (let ((response
                        (org-note-operation-test--transition-response
                         "workspace-1" "item-1" "document-1" "op-1" nil)))
                   (setf (alist-get 'revision
                                    (alist-get 'document
                                               (alist-get 'context
                                                          (alist-get 'data response))))
                         4)
                   (setf (alist-get 'state
                                    (alist-get 'item
                                               (alist-get 'context
                                                          (alist-get 'data response))))
                         "DONE")
                   response))))
      (should-error
       (org-note-operation-transition
        "workspace-1" "item-1" "document-1" 4 "DONE"
        :operation-id "op-1")
       :type 'org-note-error))))
```

Update `org-note-operation-test--transition-response` so the default fixture includes `(state . "ready")` on the item and document revision `5` (already present) — adjust existing transition tests that assert response shape if they break; keep lease reconciliation tests green by ensuring their stubs include matching `state` and advancing revision.

- [ ] **Step 2: Run package tests — expect FAIL on new tests**

```bash
cd emacs.d/site-lisp/org-note && ./run_tests.sh 2>&1 | rg "frozen-transition|validates-without-lease|non-advancing|FAILED|passed"
```

- [x] **Step 3: Implement typed request builder + strengthened always-on validation + rewrite `org-note-operation-transition`**

Replace / extend around the existing transition helpers:

```elisp
(cl-defun org-note-operation--transition-typed-request
    (workspace-id item-id document-id expected-revision target-state
                  &key lease error metadata operation-id)
  "Return a typed transition request plist ready for freeze."
  (list :method "POST"
        :route (org-note-operation--item-route item-id "/transition")
        :query nil
        :body (org-note-operation--mutation-body
               workspace-id
               (append `((document_id . ,document-id)
                         (expected_document_revision . ,expected-revision)
                         (target_state . ,target-state))
                       (and lease `((lease . ,lease)))
                       (and error `((error . ,error)))
                       `((metadata
                          . ,(or metadata (org-note-client-empty-object)))))
               operation-id)))

(defun org-note-operation--validate-transition-response
    (response workspace-id item-id document-id expected-revision target-state
              operation-id &optional expected-lease-id expected-kind)
  "Validate RESPONSE for a transition and return its context.

Always runs, including when no lease proof was supplied.  Requires document
revision strictly greater than EXPECTED-REVISION and item state equal to
TARGET-STATE."
  (let ((context
         (org-note-operation--validated-transition-context
          response workspace-id item-id document-id operation-id
          expected-lease-id expected-kind))
        (document (org-note-operation--response-value
                   (org-note-operation--response-value
                    (org-note-operation--response-value response 'data)
                    'context)
                   'document))
        (item (org-note-operation--response-value
               (org-note-operation--response-value
                (org-note-operation--response-value response 'data)
                'context)
               'item)))
    (let ((revision (org-note-operation--response-value document 'revision))
          (state (org-note-operation--response-value item 'state)))
      (unless (and (integerp revision)
                   (integerp expected-revision)
                   (> revision expected-revision)
                   (equal state target-state))
        (signal 'org-note-error
                '("Org Note transition response is invalid"))))
    context))

(cl-defun org-note-operation-transition
    (workspace-id item-id document-id expected-revision target-state
                  &key lease error metadata operation-id)
  "Transition ITEM-ID in WORKSPACE-ID and DOCUMENT-ID to TARGET-STATE.

EXPECTED-REVISION identifies the document version.  LEASE and ERROR are
included only when non-nil.  METADATA is encoded as an empty JSON object when
nil.  OPERATION-ID optionally supplies the mutation ID."
  (let* ((request-operation-id
          (or operation-id (org-note-client-new-operation-id)))
         (registered-lease
          (and lease
               (org-note-operation--registered-transition-lease
                workspace-id item-id lease)))
         (typed
          (org-note-operation--transition-typed-request
           workspace-id item-id document-id expected-revision target-state
           :lease lease :error error :metadata metadata
           :operation-id request-operation-id))
         (frozen (org-note-operation--freeze-request typed))
         (response (org-note-operation--dispatch-frozen frozen)))
    (org-note-operation--validate-transition-response
     response workspace-id item-id document-id expected-revision target-state
     request-operation-id
     (and registered-lease
          (org-note-operation-lease-lease-id registered-lease))
     (and registered-lease
          (org-note-operation-lease-kind registered-lease)))
    (when registered-lease
      (org-note-operation--reconcile-transition-lease
       registered-lease response workspace-id item-id document-id
       request-operation-id))
    response))
```

Refactor `org-note-operation--reconcile-transition-lease` to call `org-note-operation--validate-transition-response` (or assume caller already validated) so validation is not skipped on the lease path. Prefer: validate once in `org-note-operation-transition`, then pass already-validated context into reconcile — smallest change that preserves public return value (the raw response).

Update `org-note-operation-test--transition-response` item alist to include `(state . "ready")` by default; lease tests that transition to a specific state must set matching state + advancing revision in their stubs / call sites.

- [x] **Step 4: Run `./run_tests.sh` — all package tests PASS**

- [ ] **Step 5: Commit — SKIP unless user authorizes**

---

### Task 5: Identified-item preflight + frozen transition attempt

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org-note-org.el`
- Test: `emacs.d/tests/org-note-org-bridge-todo-test.el`

**Interfaces:**
- Consumes:
  - `org-note-operation-get-item-context`
  - `org-note-operation-find-lease` / lease proof helpers
  - `org-note-operation--transition-typed-request` / freeze / dispatch / validate (or public `org-note-operation-transition` **only if** bridge can still own committed-before-refresh; prefer calling freeze+dispatch directly with a bridge-owned operation id so retries share one envelope)
  - `org-note-document-id`, `org-note-document-mode`, buffer unmodified check
- Produces:
  - `(gsmlg-org-note-org--origin-item-ids) -> (workspace-id . item-id) or nil` from `ORG_NOTE_WORKSPACE_ID` / `ORG_NOTE_ITEM_ID` at point (feed or document properties only; never title)
  - `(gsmlg-org-note-org--preflight-identified-item workspace-id item-id) -> plist` with `:workspace-id` `:item-id` `:document-id` `:revision` `:state` `:lease-proof`
  - `(gsmlg-org-note-org--attempt-identified-transition target-state origin) -> plist` with `:committed-p` `:operation-id` `:response`; marks committed immediately after validation; refresh failures become “transition succeeded; view stale” without re-dispatch
  - Minimal in-memory ambiguity table `gsmlg-org-note-org--transition-ambiguities` keyed by `(workspace . item)`; further transitions for that key `user-error` fail-closed until same-frozen-wire replay helper clears it. No durable credentialful crash replay (Phase 7 / recovery hardening).

- [x] **Step 1: Failing tests for preflight + success + already-state + failure leaves UI unchanged**

```elisp
(defun gsmlg-org-note-todo-test--context (state revision)
  `((schema_version . 1)
    (data
     . ((context
         . ((workspace . ((id . "ws-1")))
            (document . ((id . "doc-1") (revision . ,revision)))
            (item . ((id . "item-1")
                     (workspace_id . "ws-1")
                     (document_id . "doc-1")
                     (state . ,state)))))))))

(ert-deftest gsmlg-org-note-preflight-uses-context-revision ()
  (require 'gsmlg-org-note-org)
  (cl-letf (((symbol-function 'org-note-operation-get-item-context)
             (lambda (_ws _item)
               (gsmlg-org-note-todo-test--context "TODO" 7)))
            ((symbol-function 'org-note-operation-find-lease)
             (lambda (&rest _) nil)))
    (let ((pre (gsmlg-org-note-org--preflight-identified-item "ws-1" "item-1")))
      (should (equal (plist-get pre :document-id) "doc-1"))
      (should (equal (plist-get pre :revision) 7))
      (should (equal (plist-get pre :state) "TODO")))))

(ert-deftest gsmlg-org-note-transition-already-state-no-dispatch ()
  (require 'gsmlg-org-note-org)
  (let ((dispatched 0))
    (cl-letf (((symbol-function 'org-note-operation-get-item-context)
               (lambda (&rest _)
                 (gsmlg-org-note-todo-test--context "DONE" 3)))
              ((symbol-function 'org-note-operation-find-lease)
               (lambda (&rest _) nil))
              ((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (&rest _)
                 (cl-incf dispatched)
                 nil)))
      (should-error
       (gsmlg-org-note-org--attempt-identified-transition
        "DONE" '(:workspace-id "ws-1" :item-id "item-1"))
       :type 'user-error)
      (should (= dispatched 0)))))

(ert-deftest gsmlg-org-note-transition-success-commits-before-refresh ()
  (require 'gsmlg-org-note-org)
  (let (order)
    (cl-letf (((symbol-function 'org-note-operation-get-item-context)
               (lambda (&rest _)
                 (gsmlg-org-note-todo-test--context "TODO" 3)))
              ((symbol-function 'org-note-operation-find-lease)
               (lambda (&rest _) nil))
              ((symbol-function 'org-note-client-new-operation-id)
               (lambda () "op-bridge-1"))
              ((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (_env)
                 (push 'dispatch order)
                 (let ((response
                        (org-note-operation-test--transition-response
                         "ws-1" "item-1" "doc-1" "op-bridge-1" nil)))
                   (setf (alist-get 'revision
                                    (alist-get 'document
                                               (alist-get 'context
                                                          (alist-get 'data response))))
                         4)
                   (setf (alist-get 'state
                                    (alist-get 'item
                                               (alist-get 'context
                                                          (alist-get 'data response))))
                         "DONE")
                   response)))
              ((symbol-function 'org-note-operation--validate-transition-response)
               (lambda (&rest args)
                 (push 'validate order)
                 (apply (lambda (&rest _args) '((ok . t))) args)))
              ((symbol-function 'gsmlg-org-note-org-refresh-feed)
               (lambda (&rest _)
                 (push 'refresh order)
                 (error "refresh failed"))))
      (let ((result
             (gsmlg-org-note-org--attempt-identified-transition
              "DONE" '(:workspace-id "ws-1" :item-id "item-1"))))
        (should (plist-get result :committed-p))
        (should (equal (reverse order)
                       '(dispatch validate refresh)))
        (should (string-match-p "stale\\|succeeded"
                                (or (plist-get result :message) "")))))))
```

Note: if `org-note-operation-test--transition-response` is not on the bridge test load-path helpers, duplicate a tiny local fixture builder in the todo test file instead of requiring the vendored test file.

- [ ] **Step 2: Run — expect FAIL**

- [x] **Step 3: Implement preflight + attempt + exact in-process ambiguity replay**

```elisp
(defvar gsmlg-org-note-org--transition-ambiguities (make-hash-table :test #'equal)
  "In-memory fail-closed ambiguity records keyed by (workspace . item).")

(defvar gsmlg-org-note-org--frozen-transitions (make-hash-table :test #'equal)
  "In-memory frozen transition envelopes for same-process replay.")

(defun gsmlg-org-note-org--origin-item-ids ()
  "Return (WORKSPACE . ITEM) from Org Note properties at point, or nil."
  (require 'org)
  (let ((workspace (org-entry-get (point) "ORG_NOTE_WORKSPACE_ID" 'selective))
        (item (org-entry-get (point) "ORG_NOTE_ITEM_ID" 'selective)))
    (and (gsmlg-org-note-org--string-or-nil workspace)
         (gsmlg-org-note-org--string-or-nil item)
         (cons workspace item))))

(defun gsmlg-org-note-org--context-field (context &rest keys)
  "Walk symbol-keyed CONTEXT alist by KEYS."
  (let ((node context))
    (dolist (key keys node)
      (setq node (cdr (assq key (if (listp node) node nil)))))))

(defun gsmlg-org-note-org--preflight-identified-item (workspace-id item-id)
  "Run identified-item preflight for WORKSPACE-ID / ITEM-ID."
  (require 'org-note-operation)
  (when (gethash (cons workspace-id item-id)
                 gsmlg-org-note-org--transition-ambiguities)
    (user-error
     "Org Note transition for %s/%s is ambiguous; resolve before retrying"
     workspace-id item-id))
  (when (and (fboundp 'org-note-document-mode)
             (derived-mode-p 'org-note-document-mode)
             (buffer-modified-p))
    (user-error
     "Save or discard Org Note document edits before TODO transition"))
  (let* ((response (org-note-operation-get-item-context workspace-id item-id))
         (data (cdr (assq 'data response)))
         (context (cdr (assq 'context data)))
         (document (cdr (assq 'document context)))
         (item (cdr (assq 'item context)))
         (document-id (cdr (assq 'id document)))
         (revision (cdr (assq 'revision document)))
         (state (cdr (assq 'state item)))
         (item-ws (cdr (assq 'workspace_id item)))
         (item-doc (cdr (assq 'document_id item)))
         (item-id* (cdr (assq 'id item))))
    (unless (and (equal item-ws workspace-id)
                 (equal item-id* item-id)
                 (stringp document-id)
                 (integerp revision)
                 (stringp state))
      (user-error "Org Note item context is invalid for transition"))
    (when (and (fboundp 'org-note-document-mode)
               (derived-mode-p 'org-note-document-mode)
               (boundp 'org-note-document-id)
               (not (equal org-note-document-id document-id)))
      (user-error "Org Note document origin does not match item context"))
    (unless (equal item-doc document-id)
      (user-error "Org Note item context document mismatch"))
    (let* ((lease (org-note-operation-find-lease workspace-id item-id "execution"))
           (proof
            (and lease
                 `((lease_id . ,(org-note-operation-lease-lease-id lease))
                   (kind . ,(org-note-operation-lease-kind lease))
                   (fencing_token
                    . ,(org-note-operation-lease-fencing-token lease))))))
      (list :workspace-id workspace-id
            :item-id item-id
            :document-id document-id
            :revision revision
            :state state
            :lease-proof proof))))

(defun gsmlg-org-note-org--mark-transition-ambiguous (workspace-id item-id
                                                      operation-id frozen)
  "Record an in-memory ambiguity for WORKSPACE-ID/ITEM-ID."
  (puthash (cons workspace-id item-id)
           (list :operation-id operation-id :frozen frozen)
           gsmlg-org-note-org--transition-ambiguities))

(defun gsmlg-org-note-org--attempt-identified-transition (target-state origin)
  "Attempt an identified transition to TARGET-STATE from ORIGIN plist."
  (require 'org-note-operation)
  (let* ((workspace-id (plist-get origin :workspace-id))
         (item-id (plist-get origin :item-id))
         (pre (gsmlg-org-note-org--preflight-identified-item
               workspace-id item-id))
         (current (plist-get pre :state)))
    (when (equal target-state current)
      (user-error "Already %s" current))
    (let* ((operation-id (org-note-client-new-operation-id))
           (typed
            (org-note-operation--transition-typed-request
             workspace-id item-id
             (plist-get pre :document-id)
             (plist-get pre :revision)
             target-state
             :lease (plist-get pre :lease-proof)
             :operation-id operation-id))
           (frozen (org-note-operation--freeze-request typed))
           (committed-p nil)
           response)
      (puthash (cons workspace-id item-id) frozen
               gsmlg-org-note-org--frozen-transitions)
      (condition-case err
          (progn
            (setq response (org-note-operation--dispatch-frozen frozen))
            (org-note-operation--validate-transition-response
             response workspace-id item-id
             (plist-get pre :document-id)
             (plist-get pre :revision)
             target-state
             operation-id)
            (setq committed-p t)
            (remhash (cons workspace-id item-id)
                     gsmlg-org-note-org--transition-ambiguities)
            (condition-case refresh-err
                (progn
                  (gsmlg-org-note-org-refresh-feed t)
                  (list :committed-p t
                        :operation-id operation-id
                        :response response
                        :message nil))
              (error
               (list :committed-p t
                     :operation-id operation-id
                     :response response
                     :message
                     (format
                      "transition succeeded; view stale (%s)"
                      (error-message-string refresh-err))))))
        ((quit error)
         (unless committed-p
           (gsmlg-org-note-org--mark-transition-ambiguous
            workspace-id item-id operation-id frozen))
         (signal (car err) (cdr err)))))))
```

Keep the property: pre-commit errors leave UI unchanged and mark ambiguity fail-closed; post-commit never rolls back metadata and never re-dispatches on refresh failure.

- [x] **Step 4: Run Task 5 tests — PASS**

- [ ] **Step 5: Commit — SKIP unless user authorizes**

---

### Task 6: Intercept `org-todo` and `org-agenda-todo` (single-row)

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org-note-org.el`
- Test: `emacs.d/tests/org-note-org-bridge-todo-test.el`

**Interfaces:**
- Consumes: Tasks 2–5 APIs, Phase 1 feed refresh
- Produces:
  - Final `gsmlg-org-note-org--around-todo` — plain-local refuse; identified path → resolve → attempt; id-less document path calls `gsmlg-org-note-org--attempt-idless-document-todo` from Task 7. Implement Task 6 advice skeleton first (id-less branch may temporarily `user-error`), then Task 7 in the same working tree before declaring Phase 2 done.
  - Final `gsmlg-org-note-org--around-agenda-todo` — refuse active region / marked / bulk; single current row only; never call native `org-agenda-maybe-loop` / postprocessing on success
  - `(gsmlg-org-note-org--agenda-bulk-or-region-todo-p) -> boolean`

- [x] **Step 1: Failing tests**

```elisp
(ert-deftest gsmlg-org-note-agenda-todo-refuses-bulk ()
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (org-agenda-bulk-marked-entries (list (point-marker))))
    (should-error
     (gsmlg-org-note-org--around-agenda-todo (lambda (&rest _) 'native) nil)
     :type 'user-error)))

(ert-deftest gsmlg-org-note-agenda-todo-single-row-uses-transition ()
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (called nil)
        (org-agenda-bulk-marked-entries nil))
    (cl-letf (((symbol-function 'gsmlg-org-note-org--agenda-bulk-or-region-todo-p)
               (lambda () nil))
              ((symbol-function 'gsmlg-org-note-org--agenda-row-origin)
               (lambda ()
                 '(:workspace-id "ws-1" :item-id "item-1" :state "TODO")))
              ((symbol-function 'gsmlg-org-note-org--resolve-todo-target)
               (lambda (&rest _) "DONE"))
              ((symbol-function 'gsmlg-org-note-org--attempt-identified-transition)
               (lambda (target origin)
                 (setq called (list target origin))
                 (list :committed-p t :message nil))))
      (should
       (gsmlg-org-note-org--around-agenda-todo (lambda (&rest _) 'native) nil))
      (should called)
      (should (equal (car called) "DONE")))))

(ert-deftest gsmlg-org-note-todo-identified-heading-dispatches ()
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (called nil))
    (cl-letf (((symbol-function 'gsmlg-org-note-org--plain-local-org-buffer-p)
               (lambda (&optional _) nil))
              ((symbol-function 'gsmlg-org-note-org--origin-item-ids)
               (lambda () (cons "ws-1" "item-1")))
              ((symbol-function 'gsmlg-org-note-org--preflight-identified-item)
               (lambda (&rest _)
                 (list :workspace-id "ws-1" :item-id "item-1"
                       :document-id "doc-1" :revision 1 :state "TODO"
                       :lease-proof nil)))
              ((symbol-function 'gsmlg-org-note-org--resolve-todo-target)
               (lambda (&rest _) "DONE"))
              ((symbol-function 'gsmlg-org-note-org--attempt-identified-transition)
               (lambda (target origin)
                 (setq called (list target origin))
                 (list :committed-p t))))
      (gsmlg-org-note-org--around-todo (lambda (&rest _) (error "native"))
                                       nil)
      (should called))))
```

- [ ] **Step 2: Run — expect FAIL until advice bodies updated**

- [x] **Step 3: Implement final advice**

```elisp
(defun gsmlg-org-note-org--agenda-bulk-or-region-todo-p ()
  "Return non-nil when agenda TODO would be bulk or region scoped."
  (or (and (boundp 'org-agenda-bulk-marked-entries)
           org-agenda-bulk-marked-entries)
      (and (use-region-p)
           (not (eq (region-beginning) (region-end))))
      (and (boundp 'org-agenda-bulk-action)
           org-agenda-bulk-action)))

(defun gsmlg-org-note-org--agenda-row-origin ()
  "Return origin plist for the current agenda row, or signal."
  (require 'org-agenda)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-get-at-bol 'org-marker)))
         (workspace
          (and marker
               (with-current-buffer (marker-buffer marker)
                 (org-entry-get (marker-position marker)
                                "ORG_NOTE_WORKSPACE_ID" 'selective))))
         (item
          (and marker
               (with-current-buffer (marker-buffer marker)
                 (org-entry-get (marker-position marker)
                                "ORG_NOTE_ITEM_ID" 'selective))))
         (state
          (and marker
               (with-current-buffer (marker-buffer marker)
                 (org-get-todo-state)))))
    (unless (and workspace item)
      (user-error
       "Agenda TODO requires Org Note item ids; refusing native mutation"))
    (list :workspace-id workspace
          :item-id item
          :state (or state "TODO"))))

(defun gsmlg-org-note-org--around-todo (orig &rest args)
  "Bridge `org-todo' for plain-local refuse and Org Note mutations."
  (if (not (and gsmlg-org-note-org-enable gsmlg-org-note-org--activated))
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "TODO")
    (let* ((arg (car args))
           (ids (gsmlg-org-note-org--origin-item-ids)))
      (cond
       (ids
        (let* ((pre (gsmlg-org-note-org--preflight-identified-item
                     (car ids) (cdr ids)))
               (target
                (gsmlg-org-note-org--resolve-todo-target
                 arg
                 (plist-get pre :state)
                 (called-interactively-p 'any)))
               (result
                (gsmlg-org-note-org--attempt-identified-transition
                 target
                 (list :workspace-id (car ids) :item-id (cdr ids)))))
          (when (plist-get result :message)
            (message "%s" (plist-get result :message)))
          nil))
       ((and (fboundp 'org-note-document-mode)
             (derived-mode-p 'org-note-document-mode))
        (gsmlg-org-note-org--attempt-idless-document-todo arg)
        nil)
       (t
        (user-error
         "Org Note TODO requires item ids or an Org Note document buffer"))))))

(defun gsmlg-org-note-org--around-agenda-todo (orig &rest args)
  "Bridge single-row `org-agenda-todo'; refuse bulk/region."
  (if (not (and gsmlg-org-note-org-enable gsmlg-org-note-org--activated))
      (apply orig args)
    (when (gsmlg-org-note-org--agenda-bulk-or-region-todo-p)
      (user-error
       "Org Note bridge refuses bulk or region agenda TODO"))
    (let* ((origin (gsmlg-org-note-org--agenda-row-origin))
           (target
            (gsmlg-org-note-org--resolve-todo-target
             (car args)
             (plist-get origin :state)
             (called-interactively-p 'any)))
           (result
            (gsmlg-org-note-org--attempt-identified-transition
             target origin)))
      (when (plist-get result :message)
        (message "%s" (plist-get result :message)))
      ;; Do not call native agenda todo / maybe-loop / line postprocessing.
      nil)))
```

Keep the id-less branch calling `gsmlg-org-note-org--attempt-idless-document-todo`. If that function is not yet defined when Task 6 tests run in isolation, define a one-line stub that signals `user-error` and replace it in Task 7.

- [x] **Step 4: Run Task 6 tests — PASS**

- [ ] **Step 5: Commit — SKIP unless user authorizes**

---

### Task 7: Id-less document TODO via transform-mode document put

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org-note-org.el`
- Test: `emacs.d/tests/org-note-org-bridge-todo-test.el`

**Interfaces:**
- Consumes: `org-note-document-*` buffer locals, `org-note-operation-put-document`, freeze path already used by put-document, Task 2 resolver
- Produces:
  - `(gsmlg-org-note-org--attempt-idless-document-todo arg)` — requires `org-note-document-mode`, unmodified buffer, no item ids at heading; builds proposed source in a temp buffer; freezes PUT; validates `document_revisions[id] > expected`; on success replaces live text only if tick unchanged; **always** messages `document text updated; no item transition` (never claims transition success); never infers item id from title
  - Minimal post-commit conflict: if live buffer diverged during I/O, keep live text, update revision/base-source to confirmed proposal, `user-error`/`message` that remote committed with local in-flight edits (full Ediff gate can be a thin stub that blocks further document TODO until buffer is unmodified again)

- [x] **Step 1: Failing tests**

```elisp
(ert-deftest gsmlg-org-note-idless-todo-puts-document-and-warns ()
  (require 'gsmlg-org-note-org)
  (require 'org-note-document)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (put-calls 0)
        messages)
    (with-temp-buffer
      (org-note-document-mode)
      (setq-local org-note-document-workspace-id "ws-1"
                  org-note-document-id "doc-1"
                  org-note-document-path "notes/a.org"
                  org-note-document-revision 2
                  org-note-document-base-source "* TODO Task\n")
      (insert "* TODO Task\n")
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-note-operation-put-document)
                 (lambda (&rest args)
                   (cl-incf put-calls)
                   (should (string-match-p "\\* DONE Task" (nth 3 args)))
                   '((document_revisions . ((doc-1 . 3))))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (gsmlg-org-note-org--attempt-idless-document-todo 'done)
        (should (= put-calls 1))
        (should (cl-some (lambda (m)
                           (string-match-p
                            "document text updated; no item transition" m))
                         messages))
        (should (string-match-p "\\* DONE Task" (buffer-string)))
        (should-not (buffer-modified-p))))))

(ert-deftest gsmlg-org-note-idless-todo-refuses-modified-buffer ()
  (require 'gsmlg-org-note-org)
  (require 'org-note-document)
  (with-temp-buffer
    (org-note-document-mode)
    (setq-local org-note-document-workspace-id "ws-1"
                org-note-document-id "doc-1"
                org-note-document-path "notes/a.org"
                org-note-document-revision 2
                org-note-document-base-source "* TODO Task\n")
    (insert "* TODO Task\n")
    (set-buffer-modified-p t)
    (should-error (gsmlg-org-note-org--attempt-idless-document-todo nil)
                  :type 'user-error)))

(ert-deftest gsmlg-org-note-idless-todo-never-infers-item-id-from-title ()
  (require 'gsmlg-org-note-org)
  (require 'org-note-document)
  (let ((transition-calls 0))
    (with-temp-buffer
      (org-note-document-mode)
      (setq-local org-note-document-workspace-id "ws-1"
                  org-note-document-id "doc-1"
                  org-note-document-path "notes/a.org"
                  org-note-document-revision 2
                  org-note-document-base-source "* TODO item-1\n")
      (insert "* TODO item-1\n")
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'org-note-operation-transition)
                 (lambda (&rest _)
                   (cl-incf transition-calls)
                   nil))
                ((symbol-function 'org-note-operation-put-document)
                 (lambda (&rest _)
                   '((document_revisions . ((doc-1 . 3))))))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (gsmlg-org-note-org--attempt-idless-document-todo 'done)
        (should (= transition-calls 0))))))
```

- [ ] **Step 2: Run — expect FAIL**

- [x] **Step 3: Implement transform-mode helper + exact in-process ambiguity replay**

```elisp
(defun gsmlg-org-note-org--put-response-revision (response document-id)
  "Return integer revision for DOCUMENT-ID from PUT RESPONSE."
  (let* ((revisions (cdr (assq 'document_revisions response)))
         (value
          (cond
           ((hash-table-p revisions)
            (or (gethash document-id revisions)
                (gethash (intern document-id) revisions)))
           ((listp revisions)
            (or (cdr (assoc document-id revisions))
                (cdr (assq (intern document-id) revisions)))))))
    (unless (integerp value)
      (user-error "Org Note document PUT response revision is invalid"))
    value))

(defun gsmlg-org-note-org--build-todo-transformed-source (arg)
  "Return buffer source after applying TODO ARG at point in a temp clone."
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (pos (point))
        (current (org-get-todo-state)))
    (with-temp-buffer
      (org-mode)
      (insert source)
      (goto-char pos)
      (let ((target
             (gsmlg-org-note-org--resolve-todo-target
              arg (or current "") nil))
            (gsmlg-org-note-org-enable nil)
            (gsmlg-org-note-org--activated nil))
        (when (and current (equal target current))
          (user-error "Already %s" current))
        (org-todo target)
        (buffer-string)))))

(defun gsmlg-org-note-org--attempt-idless-document-todo (arg)
  "Apply id-less document TODO via frozen document PUT (transform mode)."
  (require 'org-note-operation)
  (unless (and (fboundp 'org-note-document-mode)
               (derived-mode-p 'org-note-document-mode))
    (user-error "Id-less TODO requires an Org Note document buffer"))
  (when (gsmlg-org-note-org--origin-item-ids)
    (user-error "Internal error: identified heading reached id-less TODO path"))
  (when (buffer-modified-p)
    (user-error
     "Save or discard edits before id-less Org Note document TODO"))
  (unless (and (gsmlg-org-note-org--string-or-nil org-note-document-workspace-id)
               (gsmlg-org-note-org--string-or-nil org-note-document-id)
               (gsmlg-org-note-org--string-or-nil org-note-document-path)
               (integerp org-note-document-revision))
    (user-error "Org Note document metadata is incomplete"))
  (let* ((origin-source (buffer-substring-no-properties (point-min) (point-max)))
         (origin-tick (buffer-modified-tick))
         (origin-point (point))
         (prior-revision org-note-document-revision)
         (prior-base org-note-document-base-source)
         (proposed (gsmlg-org-note-org--build-todo-transformed-source arg))
         (operation-id (org-note-client-new-operation-id))
         (committed-p nil)
         response new-revision)
    (when (equal proposed origin-source)
      (user-error "Already %s" (or (org-get-todo-state) "")))
    (condition-case err
        (progn
          (setq response
                (org-note-operation-put-document
                 org-note-document-workspace-id
                 org-note-document-id
                 org-note-document-path
                 proposed
                 org-note-document-revision
                 (org-note-operation-lease-proofs org-note-document-id)
                 :operation-id operation-id))
          (setq new-revision
                (gsmlg-org-note-org--put-response-revision
                 response org-note-document-id))
          (unless (> new-revision org-note-document-revision)
            (user-error "Org Note document PUT did not advance revision"))
          (setq committed-p t)
          (setq-local org-note-document-revision new-revision
                      org-note-document-base-source proposed)
          (if (= (buffer-modified-tick) origin-tick)
              (progn
                (erase-buffer)
                (insert proposed)
                (set-buffer-modified-p nil)
                (goto-char (min origin-point (point-max))))
            (set-buffer-modified-p t)
            (message
             "Remote document TODO committed; local in-flight edits preserved"))
          (message "document text updated; no item transition")
          nil)
      (error
       (unless committed-p
         (setq-local org-note-document-revision prior-revision
                     org-note-document-base-source prior-base))
       (signal (car err) (cdr err))))))
```

- [x] **Step 4: Run Task 7 tests + full todo ERT file — PASS**

```bash
emacs -Q --batch -L emacs.d/lisp -L emacs.d/tests -L emacs.d/site-lisp/org-note \
  -l emacs.d/tests/org-note-org-bridge-todo-test.el \
  --eval "(ert-run-tests-batch-and-exit \"^gsmlg-org-note-\")"
```

- [ ] **Step 5: Commit — SKIP unless user authorizes**

---

### Task 8: Phase 2 closeout — suite + self-check notes

**Files:**
- Modify: `emacs.d/docs/architecture.md` (one short paragraph that Phase 2 adds TODO/state/refuse; keep `gsmlg-org-note-org-enable` nil)
- Possibly thin comments in `gsmlg-org-note-org.el` documenting deferred Phase 3–7 items at refuse stubs
- Test: existing suites only (no new product code unless a closeout bugfix)

- [x] **Step 1: Confirm enable default remains nil**

```bash
rg "defcustom gsmlg-org-note-org-enable" -A3 emacs.d/lisp/gsmlg-org-note-org.el
```

Expected: default `nil`.

- [ ] **Step 2: Run vendored + focused + full suite**

```bash
cd emacs.d/site-lisp/org-note && ./run_tests.sh
emacs -Q --batch -L emacs.d/lisp -L emacs.d/tests -L emacs.d/site-lisp/org-note \
  -l emacs.d/tests/org-note-org-bridge-todo-test.el \
  --eval "(ert-run-tests-batch-and-exit \"^gsmlg-org-note-\")"
emacs -Q --batch -L emacs.d/lisp -L emacs.d/tests -L emacs.d/site-lisp/org-note \
  -l emacs.d/tests/org-note-org-bridge-agenda-test.el \
  --eval "(ert-run-tests-batch-and-exit \"^gsmlg-org-note-org-\")"
./run-emacs-tests.sh
```

Expected: PASS, or stop and record out-of-scope failures per `AGENTS.md`.

- [x] **Step 3: Architecture note**

Add a short paragraph under the existing Org Note bridge section: Phase 2 owns atomic TODO state configuration, plain-local refuse for TODO/refile/archive/clock, identified frozen transitions, and id-less document keyword put with explicit non-transition messaging. Capture/refile/clock/archive engines remain later phases. Bridge stays disabled by default.

- [ ] **Step 4: Commit — SKIP unless user authorizes**

---

## Spec coverage (self-review)

| Spec requirement | Task |
| --- | --- |
| Local plain `.org` refuse TODO/refile/archive/clock; body edit OK | Task 3 |
| `todo-states` / `done-states` / `fast-keys` / archive-target + atomic `apply-state-configuration` + live recompute | Task 1 |
| Identified item mutation preflight | Task 5 |
| Identified transition freeze/dispatch + unconditional validation + committed-before-refresh | Tasks 4–5 |
| Full-document transform mode for id-less TODO + warn | Task 7 |
| TODO §1 keywords on activation | Task 1 |
| TODO §2 identified path + lease proof | Task 5 |
| TODO §3 pure target resolver + Already STATE | Tasks 2, 5 |
| TODO §4–5 agenda todo single-row / bulk refuse | Task 6 |
| TODO §6 plain local refuse | Task 3 |
| TODO §7 id-less document put warn | Task 7 |
| TODO §8 clock/archive id requirement (refuse stubs only) | Task 3 (engines deferred) |
| TODO §9 unknown state clear error | Task 2 |
| TODO §10 pre-dispatch unchanged; ambiguity rules minimal fail-closed | Task 5 |
| Testing: operable states, transition success/failure, plain refuse, agenda intercept | Tasks 1, 3, 5, 6, 8 |
| Ambiguous transition/document TODO same-wire replay without fresh preflight/id | Tasks 5, 7 |
| Out of scope Capture/refile/clock/archive engines / multi-process locks | Documented; refuse stubs only |

## Placeholder / consistency notes (self-review)

- No TBD/TODO placeholders remain as work instructions; deferred product work is explicitly Phase 3–7 with refuse stubs.
- Public names: `gsmlg-org-note-apply-state-configuration`, `gsmlg-org-note-todo-states`, `gsmlg-org-note-done-states`, `gsmlg-org-note-state-fast-keys`, `gsmlg-org-note-archive-target`.
- Vendored: `org-note-operation--transition-typed-request`, `org-note-operation--validate-transition-response`, freeze+dispatch used by public `org-note-operation-transition`.
- Bridge attempt APIs: `gsmlg-org-note-org--preflight-identified-item`, `gsmlg-org-note-org--attempt-identified-transition`, `gsmlg-org-note-org--attempt-idless-document-todo`.
- Commit steps remain SKIP unless the user authorizes.
- `gsmlg-org-note-org-enable` remains nil by default.

## Validation record (2026-09-10)

- Vendored Org Note suite: 234/234 passed.
- Focused Phase 2 TODO bridge suite: 35/35 passed using `-l` from the
  repository root.
- Focused agenda bridge suite: 13/13 passed using `-l` from the repository
  root.
- `git diff --check`: passed.
- Full `./run-emacs-tests.sh`: not passed. The isolated fresh-start phase
  exited 255 before bridge checks with `Elpaca failed to prepare:
  paredit-everywhere, paredit; inspect *elpaca-log*`; installer tests had
  already passed 9/9. This is a baseline dependency/bootstrap environment
  blocker and is outside Phase 2 scope.
- `./lint-emacs-config.sh`: not passed. Reused package data failed its lock
  assertion before lint/checkdoc because `emacs-duskmoon-theme` was at
  `728f6f76631e0b151ed5abe92581d5429ffa1038`, while the committed lock expects
  `96f16ce51abec81bfde90c347477c083e3ff9dbb`. No dependency state was changed.
- External same-operation-id service integration gates: not run. These remain
  release gates, so default enablement stays nil.
- Historical red-run steps above remain unchecked where no retained execution
  evidence exists; the 2026-09-10 ambiguity/cold-start/typed-dispatch repair
  regressions were observed failing before their implementations and passing
  afterward.
