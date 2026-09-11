;;; gsmlg-org-note-org.el --- Org Note Org bridge -*- lexical-binding: t; -*-

;;; Commentary:
;; First-party bridge between Org entrypoints and Org Note.  Cold-start
;; around-advice on Org agenda producers and `org-capture' is installed without
;; requiring Org Note or performing network I/O.  The first command
;; invocation after explicit development enablement loads Org Note, activates
;; the bridge, and owns the generated agenda feed so `org-agenda-files' is
;; feed-only.

;;; Code:

(require 'cl-lib)
(require 'gsmlg-paths)

(declare-function org-note-operation-query-agenda "org-note-operation"
                  (&rest keyword-arguments))
(declare-function org-note-operation-get-item-context "org-note-operation"
                  (workspace-id item-id))
(declare-function org-note-operation-find-lease "org-note-operation"
                  (workspace-id item-id kind))
(declare-function org-note-operation-lease-lease-id "org-note-operation" (lease))
(declare-function org-note-operation-lease-kind "org-note-operation" (lease))
(declare-function org-note-operation-lease-fencing-token "org-note-operation"
                  (lease))
(declare-function org-note-operation--transition-typed-request
                  "org-note-operation"
                  (workspace-id item-id document-id expected-revision
                                target-state &rest keyword-arguments))
(declare-function org-note-operation--freeze-request "org-note-operation"
                  (typed-request))
(declare-function org-note-operation--dispatch-frozen "org-note-operation"
                  (frozen-envelope))
(declare-function org-note-operation--validate-transition-response
                  "org-note-operation"
                  (response workspace-id item-id document-id expected-revision
                            target-state operation-id &optional expected-lease-id
                            expected-kind))
(declare-function org-note-operation--registered-transition-lease
                  "org-note-operation"
                  (workspace-id item-id proof))
(declare-function org-note-operation--reconcile-transition-lease
                  "org-note-operation"
                  (registered-lease context workspace-id item-id))
(declare-function org-note-operation-lease-proofs "org-note-operation"
                  (document-id))
(declare-function org-note-operation--path-segment "org-note-operation"
                  (identifier))
(declare-function org-note-operation--mutation-body "org-note-operation"
                  (workspace-id fields &optional operation-id))
(declare-function org-note-client-new-operation-id "org-note-client" ())
(declare-function org-note-client-empty-object "org-note-client" ())
(declare-function org-note-item-context "org-note" (workspace-id item-id))
(declare-function org-note-configure-agenda-workspaces "org-note" ())
(declare-function org-note-validation-page-cursor "org-note-validation"
                  (cursor))
(declare-function org-note-validation-bounded-pager-state "org-note-validation"
                  (&rest keyword-arguments))
(declare-function org-note-validation-bounded-pager-fold "org-note-validation"
                  (state page-fetcher))
(declare-function org-agenda-goto "org-agenda" (&optional highlight))
(declare-function org-entry-get "org" (pom property &optional literal selective))
(declare-function org-get-at-bol "org" (prop))
(declare-function org-get-todo-state "org" ())
(declare-function org-set-regexps-and-options "org" (&optional tags-only))
(declare-function org-todo "org" (&optional arg))
(defvar org-agenda-files)
(defvar org-done-keywords)
(defvar org-not-done-keywords)
(defvar org-todo-key-alist)
(defvar org-todo-key-trigger)
(defvar org-todo-keywords)
(defvar org-todo-keywords-1)
(declare-function gsmlg-org-apply-path-settings "gsmlg-org" ())
(defvar org-use-fast-todo-selection)
(defvar org-note-document-workspace-id)
(defvar org-note-document-id)
(defvar org-note-document-path)
(defvar org-note-document-revision)
(defvar org-note-document-base-source)

(defgroup gsmlg-org-note-org nil
  "Org Note bridge for Org agenda and capture."
  :group 'gsmlg)

(defcustom gsmlg-org-note-org-enable nil
  "Enable the phased Org Note Org bridge.

Keep this nil for normal use until every release-gated bridge phase and
integration gate in the approved design is complete.  Developers may enable
it explicitly while implementing and testing a phase."
  :type 'boolean
  :group 'gsmlg-org-note-org)

(defun gsmlg-org-note-org--state-string-valid-p (value)
  "Return non-nil when VALUE is a valid Org Note TODO state string."
  (and (stringp value)
       (not (string-empty-p value))
       (not (string-match-p "[[:space:]\n\r\t\f\v|()]" value))))

(defun gsmlg-org-note-org--fast-key-char-valid-p (key)
  "Return non-nil when KEY is a printable non-reserved Org fast-selection char."
  (and (characterp key)
       (not (memq key '(?! ?@ ?/ ?\s ?\t ?\n ?\r ?\f ?\v)))
       (>= key 33)
       (<= key 126)))

(defun gsmlg-org-note-org--format-keyword (state fast-keys)
  "Return STATE, or STATE(key) when FAST-KEYS maps a character to STATE.

FAST-KEYS uses bridge polarity (CHAR . STATE).  Embedding the key in the
keyword string lets Org parse the explicit mapping during temporary setup."
  (let ((entry (rassoc state fast-keys)))
    (if entry
        (format "%s(%c)" state (car entry))
      state)))

(defun gsmlg-org-note-org--fast-keys-to-org-alist (fast-keys)
  "Convert bridge FAST-KEYS (CHAR . STATE) to Org `org-todo-key-alist' polarity.

Org stores (STATE . CHAR).  Bridge defcustom
`gsmlg-org-note-state-fast-keys' keeps (CHAR . STATE); convert only at
the precompute/publish/apply-buffer boundary."
  (mapcar (lambda (pair) (cons (cdr pair) (car pair))) fast-keys))

(defun gsmlg-org-note-org--validate-state-configuration
    (todo-states done-states fast-keys archive-target)
  "Validate the four-field state tuple and return precomputed keyword tables.

TODO-STATES and DONE-STATES are the active and completed state lists.
FAST-KEYS maps selection characters to states, and ARCHIVE-TARGET is the
completed state used for archive operations.
Signals `user-error' without mutating defcustoms or live buffers.
`:key-alist' in the returned plist is Org-native (STATE . CHAR)."
  (require 'org)
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
          (bridge-keys nil))
      (dolist (entry fast-keys)
        (unless (and (consp entry)
                     (gsmlg-org-note-org--fast-key-char-valid-p (car entry))
                     (member (cdr entry) all))
          (user-error "Org Note state fast keys are invalid"))
        (when (memq (car entry) seen-keys)
          (user-error "Org Note state fast keys must be unique"))
        (push (car entry) seen-keys)
        (push (cons (car entry) (cdr entry)) bridge-keys))
      (setq bridge-keys (nreverse bridge-keys))
      (let* ((sequence
              (append '("sequence")
                      (mapcar (lambda (state)
                                (gsmlg-org-note-org--format-keyword
                                 state bridge-keys))
                              todo-states)
                      '("|")
                      (mapcar (lambda (state)
                                (gsmlg-org-note-org--format-keyword
                                 state bridge-keys))
                              done-states)))
             (keywords (list sequence))
             (prior-keywords (default-value 'org-todo-keywords))
             (built-alist nil)
             (built-trigger nil))
        ;; Org reads `(default-value 'org-todo-keywords)' inside
        ;; `org-set-regexps-and-options'; buffer-local setq is ignored.
        (unwind-protect
            (progn
              (setq-default org-todo-keywords keywords)
              (with-temp-buffer
                (delay-mode-hooks (org-mode))
                (org-set-regexps-and-options)
                ;; Prove Org accepts the union and every explicit mapping.
                ;; Org may auto-assign keys for unmapped states; reject
                ;; missing/remapped explicit keys, then strip extras.
                (dolist (pair bridge-keys)
                  (let* ((ch (car pair))
                         (state (cdr pair))
                         (found (assoc state org-todo-key-alist)))
                    (unless (member state org-todo-keywords-1)
                      (user-error "Org Note fast key target %s is not a TODO keyword"
                                  state))
                    (unless (and found (eq (cdr found) ch))
                      (user-error "Org Note fast-key map failed Org round-trip"))))
                ;; Normalized Org-native map: explicit mappings only.
                ;; Empty bridge map installs no trigger.
                (setq built-alist
                      (gsmlg-org-note-org--fast-keys-to-org-alist bridge-keys)
                      built-trigger (and built-alist t))
                (dolist (pair bridge-keys)
                  (unless (eq (cdr (assoc (cdr pair) built-alist)) (car pair))
                    (user-error "Org Note fast-key map failed Org round-trip")))
                (list :sequence sequence
                      :keywords keywords
                      :key-alist (copy-sequence built-alist)
                      :key-trigger built-trigger
                      :todo-states (copy-sequence todo-states)
                      :done-states (copy-sequence done-states)
                      :fast-keys (copy-tree fast-keys)
                      :archive-target archive-target)))
          (setq-default org-todo-keywords prior-keywords))))))

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
  "Install PRECOMPUTED keyword tables in the current Org buffer.

`:key-alist' must already be Org-native (STATE . CHAR)."
  (when (derived-mode-p 'org-mode)
    (setq-local org-todo-keywords (plist-get precomputed :keywords))
    (org-set-regexps-and-options)
    ;; Overwrite Org auto-assigned extras with the normalized explicit map.
    (setq-local org-todo-key-alist (copy-sequence (plist-get precomputed :key-alist))
                org-todo-key-trigger (plist-get precomputed :key-trigger))))

(defun gsmlg-org-note-org--recompute-live-buffers (precomputed)
  "Recompute keyword tables in every live bridge Org buffer from PRECOMPUTED."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (gsmlg-org-note-org--bridge-buffer-p)
        (gsmlg-org-note-org--apply-keywords-in-buffer precomputed)))))

(defun gsmlg-org-note-org--publish-keyword-defaults (precomputed)
  "Publish PRECOMPUTED keyword tables as Org defaults and buffer locals.

`org-set-regexps-and-options' is a no-op outside `org-mode' and reads
`(default-value \\='org-todo-keywords)', so install must set defaults and
recompute inside a temporary Org buffer.  `:key-alist' is Org-native
\=(STATE . CHAR); bridge (CHAR . STATE) polarity stays on the defcustom."
  (let ((keywords (plist-get precomputed :keywords))
        (key-alist (copy-sequence (plist-get precomputed :key-alist)))
        (key-trigger (plist-get precomputed :key-trigger)))
    (setq-default org-todo-keywords keywords)
    (setq org-todo-keywords keywords)
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (org-set-regexps-and-options)
      (setq-default org-todo-keywords-1 (copy-sequence org-todo-keywords-1)
                    org-done-keywords (copy-sequence org-done-keywords)
                    org-not-done-keywords (copy-sequence org-not-done-keywords)
                    org-todo-key-alist (copy-sequence key-alist)
                    org-todo-key-trigger key-trigger))
    (setq-default org-todo-key-alist (copy-sequence key-alist)
                  org-todo-key-trigger key-trigger)
    (setq org-todo-key-alist (copy-sequence key-alist)
          org-todo-key-trigger key-trigger)))

(defun gsmlg-org-note-org--install-todo-keywords ()
  "Install the validated current state configuration into Org."
  (require 'org)
  (let ((precomputed
         (gsmlg-org-note-org--validate-state-configuration
          gsmlg-org-note-todo-states
          gsmlg-org-note-done-states
          gsmlg-org-note-state-fast-keys
          gsmlg-org-note-archive-target)))
    (gsmlg-org-note-org--publish-keyword-defaults precomputed)
    (gsmlg-org-note-org--recompute-live-buffers precomputed)
    precomputed))

(defun gsmlg-org-note-apply-state-configuration
    (todo-states done-states fast-keys archive-target)
  "Atomically commit the four Org Note TODO configuration fields.

TODO-STATES and DONE-STATES are the active and completed state lists.
FAST-KEYS maps selection characters to states, and ARCHIVE-TARGET is the
completed state used for archive operations.
Validates once, precomputes keyword tables, commits all values, then
recomputes live bridge buffers.  Any failure restores the prior
defcustom values and republishes/recomputes the prior keyword tables in
defaults and live bridge buffers."
  (let* ((prior-todo gsmlg-org-note-todo-states)
         (prior-done gsmlg-org-note-done-states)
         (prior-keys gsmlg-org-note-state-fast-keys)
         (prior-archive gsmlg-org-note-archive-target)
         (prior-precomputed
          (gsmlg-org-note-org--validate-state-configuration
           prior-todo prior-done prior-keys prior-archive))
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
          (gsmlg-org-note-org--publish-keyword-defaults precomputed)
          (gsmlg-org-note-org--recompute-live-buffers precomputed)
          t)
      (error
       (setq gsmlg-org-note-todo-states prior-todo
             gsmlg-org-note-done-states prior-done
             gsmlg-org-note-state-fast-keys prior-keys
             gsmlg-org-note-archive-target prior-archive)
       (condition-case restore-err
           (progn
             (gsmlg-org-note-org--publish-keyword-defaults prior-precomputed)
             (gsmlg-org-note-org--recompute-live-buffers prior-precomputed))
         (error
          (signal 'error
                  (list
                   (format
                    "Org Note state apply failed (%s); restore also failed (%s)"
                    (error-message-string err)
                    (error-message-string restore-err))))))
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
  "Optional alist mapping unique characters to configured TODO states.

Entries use bridge polarity (CHAR . STATE).  Installation converts to
Org-native `org-todo-key-alist' polarity (STATE . CHAR)."
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

(with-eval-after-load 'org
  (when gsmlg-org-note-org-enable
    (gsmlg-org-note-org--install-todo-keywords)))

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

(defconst gsmlg-org-note-org--feed-tag "ORGNOTE"
  "Tag marking generated Org Note feed headings.")

(defconst gsmlg-org-note-org--feed-schema-version 1
  "Schema version written into generated agenda feed headers.")

(defconst gsmlg-org-note-org--agenda-page-limit 100
  "Explicit page size for exhaustive Org Note agenda view fetches.")
(defvar gsmlg-org-note-org--feed-file
  (gsmlg-cache-file "org-note-agenda-feed.org")
  "Generated Org file exposing Org Note scheduled and deadline items.

This is the last-good snapshot path for the current process.  Phase 1
uses a single-process write.  Multi-process publication reservation
locks (spec blockers 69, 74, 79) are deferred to Phase 7.")

(defvar gsmlg-org-note-org--selected-feed-file nil
  "Feed path currently selected for `org-agenda-files'.

May be the last-good snapshot or an endpoint-keyed empty feed.")

(defvar gsmlg-org-note-org--last-workspace-ids nil
  "Workspace IDs used for the current feed snapshot.")

(defvar gsmlg-org-note-org--refresh-active nil
  "Non-nil while `gsmlg-org-note-org-refresh-feed' is running.")

(defvar gsmlg-org-note-org--guards-installed nil
  "Non-nil after cold-start advice has been installed.")

(defvar gsmlg-org-note-org--activated nil
  "Non-nil after `gsmlg-org-note-org-activate' has completed.")

(defvar gsmlg-org-note-org--activating nil
  "Reentrancy guard while activating the bridge.")

(defvar gsmlg-org-note-org--feed-hooks-installed nil
  "Non-nil after feed refresh and goto advice are installed.")

(defvar gsmlg-org-note-org--mutation-hooks-installed nil
  "Non-nil after TODO/refile/archive/clock refuse-or-bridge advice is installed.")

(defvar gsmlg-org-note-org--transition-ambiguities (make-hash-table :test #'equal)
  "In-memory fail-closed ambiguity records keyed by (workspace . item).")

(defvar gsmlg-org-note-org--frozen-transitions (make-hash-table :test #'equal)
  "In-memory frozen transition envelopes for same-process replay.")

(defvar gsmlg-org-note-org--document-ambiguities (make-hash-table :test #'equal)
  "In-memory fail-closed document PUT ambiguity records keyed by document-id.")

(defvar gsmlg-org-note-org--agenda-command-active nil
  "Non-nil while the outermost guarded Agenda producer is running.")

(defconst gsmlg-org-note-org--plain-local-refuse-fmt
  "Org Note bridge refuses %s in plain local .org buffers; use Org Note documents or agenda."
  "User-error format for plain-local refuse.")

(defconst gsmlg-org-note-org--agenda-entrypoints
  '(org-agenda
    org-agenda-list
    org-todo-list
    org-tags-view
    org-search-view
    org-agenda-list-stuck-projects
    org-occur-in-agenda-files
    org-store-agenda-views
    org-agenda-redo
    org-agenda-redo-all)
  "Public Agenda producers guarded against cold-start local-file access.")

;;;###autoload
(defun gsmlg-org-note-org-install-guards ()
  "Install inert around-advice on Org agenda and capture entrypoints.

This must not load Org Note or perform network I/O."
  (unless gsmlg-org-note-org--guards-installed
    (setq gsmlg-org-note-org--guards-installed t)
    (dolist (command gsmlg-org-note-org--agenda-entrypoints)
      (autoload command "org-agenda" nil t)
      (advice-add command :around #'gsmlg-org-note-org--around-agenda))
    (autoload 'org-agenda-files "org" nil nil)
    (advice-add #'org-agenda-files
                :around #'gsmlg-org-note-org--around-agenda-files)
    (autoload 'org-capture "org-capture" nil t)
    (advice-add #'org-capture :around #'gsmlg-org-note-org--around-capture)
    (gsmlg-org-note-org--install-mutation-hooks)))

(defun gsmlg-org-note-org-feed-file ()
  "Return the selected Org Note agenda feed path."
  (or gsmlg-org-note-org--selected-feed-file
      gsmlg-org-note-org--feed-file))

(defun gsmlg-org-note-org-agenda-files ()
  "Return agenda files for the active Org Note bridge.

The list contains exactly the selected feed path (or empty-feed path)."
  (list (gsmlg-org-note-org-feed-file)))

;;;###autoload
(defun gsmlg-org-note-org-activate ()
  "Activate the Org Note Org bridge once.

May `(require \\='org-note)'.  Idempotent under a reentrancy guard.
Applies feed-only `org-agenda-files' when path settings are available.
Does not truncate a valid last-good snapshot."
  (unless gsmlg-org-note-org-enable
    (user-error "Org Note Org bridge is release-gated and disabled"))
  (unless (or gsmlg-org-note-org--activated
              gsmlg-org-note-org--activating)
    (setq gsmlg-org-note-org--activating t)
    (unwind-protect
        (progn
          (require 'org-note)
          (require 'org)
          (gsmlg-org-note-org--install-todo-keywords)
          (gsmlg-org-note-org--install-feed-hooks)
          (gsmlg-org-note-org--install-mutation-hooks)
          (setq gsmlg-org-note-org--activated t)
          (when (fboundp #'gsmlg-org-apply-path-settings)
            (gsmlg-org-apply-path-settings)))
      (setq gsmlg-org-note-org--activating nil))))

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
  "Signal `user-error' for COMMAND-LABEL in a plain local Org buffer."
  (when (and gsmlg-org-note-org-enable
             gsmlg-org-note-org--activated
             (gsmlg-org-note-org--plain-local-org-buffer-p))
    (user-error gsmlg-org-note-org--plain-local-refuse-fmt command-label)))

(defun gsmlg-org-note-org--origin-item-ids ()
  "Return (WORKSPACE . ITEM) from Org Note properties at point, or nil.

Reads only `ORG_NOTE_WORKSPACE_ID' / `ORG_NOTE_ITEM_ID' (feed or document
properties).  Never infers identity from the headline title."
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
  "Run identified-item preflight for WORKSPACE-ID / ITEM-ID.

Returns a plist with `:workspace-id' `:item-id' `:document-id' `:revision'
`:state' `:lease-proof'.  Signals `user-error' fail-closed on ambiguity,
unsaved document edits, or invalid/mismatched context."
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
         (data (gsmlg-org-note-org--context-field response 'data))
         (context (gsmlg-org-note-org--context-field data 'context))
         (document (gsmlg-org-note-org--context-field context 'document))
         (item (gsmlg-org-note-org--context-field context 'item))
         (document-id (gsmlg-org-note-org--context-field document 'id))
         (revision (gsmlg-org-note-org--context-field document 'revision))
         (state (gsmlg-org-note-org--context-field item 'state))
         (item-ws (gsmlg-org-note-org--context-field item 'workspace_id))
         (item-doc (gsmlg-org-note-org--context-field item 'document_id))
         (item-id* (gsmlg-org-note-org--context-field item 'id)))
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
    (let* ((lease (org-note-operation-find-lease workspace-id item-id
                                                 "execution"))
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
                                                      operation-id frozen
                                                      &rest properties)
  "Record an in-memory ambiguity for WORKSPACE-ID and ITEM-ID.

Store OPERATION-ID, FROZEN request data, and additional PROPERTIES."
  (puthash (cons workspace-id item-id)
           (append (list :operation-id operation-id :frozen frozen
                         :workspace-id workspace-id :item-id item-id)
                   properties)
           gsmlg-org-note-org--transition-ambiguities))

(defun gsmlg-org-note-org--mark-document-ambiguous (document-id operation-id
                                                   frozen &rest properties)
  "Record an in-memory document PUT ambiguity for DOCUMENT-ID.

Store OPERATION-ID, FROZEN request data, and additional PROPERTIES."
  (puthash document-id
           (append (list :operation-id operation-id :frozen frozen)
                   properties)
           gsmlg-org-note-org--document-ambiguities))

(defun gsmlg-org-note-org--finish-transition-attempt (record response)
  "Validate and finish the transition attempt in RECORD using RESPONSE."
  (let* ((workspace-id (plist-get record :workspace-id))
         (item-id (plist-get record :item-id))
         (registered-lease (plist-get record :registered-lease))
         (context
          (org-note-operation--validate-transition-response
           response workspace-id item-id
           (plist-get record :document-id)
           (plist-get record :expected-revision)
           (plist-get record :target-state)
           (plist-get record :operation-id)
           (and registered-lease
                (org-note-operation-lease-lease-id registered-lease))
           (and registered-lease
                (org-note-operation-lease-kind registered-lease)))))
    (remhash (cons workspace-id item-id)
             gsmlg-org-note-org--transition-ambiguities)
    (remhash (cons workspace-id item-id)
             gsmlg-org-note-org--frozen-transitions)
    (condition-case refresh-err
        (progn
          (when registered-lease
            (org-note-operation--reconcile-transition-lease
             registered-lease context workspace-id item-id))
          (gsmlg-org-note-org-refresh-feed t)
          (list :committed-p t
                :operation-id (plist-get record :operation-id)
                :response response
                :message nil))
      ((quit error)
       (list :committed-p t
             :operation-id (plist-get record :operation-id)
             :response response
             :message
             (format "transition succeeded; view stale (%s)"
                     (error-message-string refresh-err)))))))

(defun gsmlg-org-note-org-retry-ambiguous-transition (workspace-id item-id)
  "Replay the in-process ambiguous transition for WORKSPACE-ID and ITEM-ID."
  (interactive "sWorkspace ID: \nsItem ID: ")
  (require 'org-note-operation)
  (let* ((key (cons workspace-id item-id))
         (record (gethash key gsmlg-org-note-org--transition-ambiguities)))
    (unless (and record (plist-get record :frozen))
      (user-error "No replayable Org Note transition for %s/%s"
                  workspace-id item-id))
    (gsmlg-org-note-org--finish-transition-attempt
     record
     (org-note-operation--dispatch-frozen (plist-get record :frozen)))))

(defun gsmlg-org-note-org--attempt-identified-transition (target-state origin)
  "Attempt an identified transition to TARGET-STATE from ORIGIN plist.

ORIGIN must include `:workspace-id' and `:item-id'.  Marks committed after
successful validation and before refresh.  Refresh failures return
`:committed-p' with a stale-view message and never re-dispatch.  Pre-commit
errors mark the item ambiguous fail-closed and re-signal."
  (require 'org-note-operation)
  (let* ((workspace-id (plist-get origin :workspace-id))
         (item-id (plist-get origin :item-id))
         (pre (gsmlg-org-note-org--preflight-identified-item
               workspace-id item-id))
         (current (plist-get pre :state)))
    (when (equal target-state current)
      (user-error "Already %s" current))
    (let* ((operation-id (org-note-client-new-operation-id))
           (lease-proof (plist-get pre :lease-proof))
           (registered-lease
            (and lease-proof
                 (org-note-operation--registered-transition-lease
                  workspace-id item-id lease-proof)))
           (typed
            (org-note-operation--transition-typed-request
             workspace-id item-id
             (plist-get pre :document-id)
             (plist-get pre :revision)
             target-state
             :lease lease-proof
             :operation-id operation-id))
           (frozen (org-note-operation--freeze-request typed))
           (record
            (list :operation-id operation-id :frozen frozen
                  :workspace-id workspace-id :item-id item-id
                  :document-id (plist-get pre :document-id)
                  :expected-revision (plist-get pre :revision)
                  :target-state target-state
                  :registered-lease registered-lease))
           (committed-p nil)
           response)
      (puthash (cons workspace-id item-id) frozen
               gsmlg-org-note-org--frozen-transitions)
      (condition-case err
          (progn
            (setq response (org-note-operation--dispatch-frozen frozen))
            (setq committed-p t)
            (gsmlg-org-note-org--finish-transition-attempt record response))
        ((quit error)
         (unless committed-p
           (puthash (cons workspace-id item-id) record
                    gsmlg-org-note-org--transition-ambiguities))
         (signal (car err) (cdr err)))))))

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
                 (org-entry-get (marker-position marker) "TODO")))))
    (unless (and workspace item)
      (user-error
       "Agenda TODO requires Org Note item ids; refusing native mutation"))
    (list :workspace-id workspace
          :item-id item
          :state (or state "TODO"))))

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

(defun gsmlg-org-note-org--finish-document-todo-attempt (document-id record
                                                                     response)
  "Validate RESPONSE and finish DOCUMENT-ID TODO attempt described by RECORD."
  (let ((new-revision
         (gsmlg-org-note-org--put-response-revision response document-id))
        (expected-revision (plist-get record :expected-revision))
        (buffer (plist-get record :buffer))
        (proposed (plist-get record :proposed-source)))
    (unless (> new-revision expected-revision)
      (user-error "Org Note document PUT did not advance revision"))
    (remhash document-id gsmlg-org-note-org--document-ambiguities)
    (condition-case local-err
        (progn
          (unless (buffer-live-p buffer)
            (error "Org Note document buffer for %s is no longer live"
                   document-id))
          (with-current-buffer buffer
            (unless (equal org-note-document-id document-id)
              (error "Org Note document recovery buffer identity changed"))
            (setq-local org-note-document-revision new-revision
                        org-note-document-base-source proposed)
            (if (= (buffer-modified-tick) (plist-get record :origin-tick))
                (progn
                  (erase-buffer)
                  (insert proposed)
                  (set-buffer-modified-p nil)
                  (goto-char
                   (min (plist-get record :origin-point) (point-max))))
              (set-buffer-modified-p t)
              (message
               "Remote document TODO committed; local in-flight edits preserved")))
          (message "document text updated; no item transition")
          nil)
      ((quit error)
       (list :committed-p t
             :message
             (format "document TODO succeeded; local update failed (%s)"
                     (error-message-string local-err)))))))

(defun gsmlg-org-note-org-retry-ambiguous-document-todo (document-id)
  "Replay the in-process ambiguous TODO PUT for DOCUMENT-ID."
  (interactive "sDocument ID: ")
  (require 'org-note-operation)
  (let ((record (gethash document-id
                         gsmlg-org-note-org--document-ambiguities)))
    (unless (and record (plist-get record :frozen))
      (user-error "No replayable Org Note document TODO for %s" document-id))
    (gsmlg-org-note-org--finish-document-todo-attempt
     document-id record
     (org-note-operation--dispatch-frozen (plist-get record :frozen)))))

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
  "Apply TODO ARG via frozen document PUT for an id-less heading."
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
  (when (gethash org-note-document-id gsmlg-org-note-org--document-ambiguities)
    (user-error
     "Org Note document PUT for %s is ambiguous; resolve before retrying"
     org-note-document-id))
  (let* ((origin-source (buffer-substring-no-properties (point-min) (point-max)))
         (origin-tick (buffer-modified-tick))
         (origin-point (point))
         (workspace-id org-note-document-workspace-id)
         (document-id org-note-document-id)
         (document-path org-note-document-path)
         (expected-revision org-note-document-revision)
         (proposed (gsmlg-org-note-org--build-todo-transformed-source arg))
         (operation-id (org-note-client-new-operation-id))
         (lease-proofs (org-note-operation-lease-proofs document-id))
         (typed
          (list :method "PUT"
                :route (format "/api/org/documents/%s"
                               (org-note-operation--path-segment document-id))
                :query nil
                :body (org-note-operation--mutation-body
                       workspace-id
                       (append
                        `((path . ,document-path)
                          (source . ,proposed)
                          (expected_revision . ,expected-revision))
                        `((lease_proofs
                           . ,(or lease-proofs
                                  (org-note-client-empty-object)))))
                       operation-id)
                :response-validator
                (lambda (response)
                  (let ((revision
                         (gsmlg-org-note-org--put-response-revision
                          response document-id)))
                    (unless (> revision expected-revision)
                      (user-error
                       "Org Note document PUT did not advance revision"))))))
         (frozen (org-note-operation--freeze-request typed))
         (record
          (list :operation-id operation-id :frozen frozen
                :buffer (current-buffer) :origin-tick origin-tick
                :origin-point origin-point :origin-source origin-source
                :proposed-source proposed :expected-revision expected-revision))
         (dispatched-p nil)
         (committed-p nil)
         response)
    (when (equal proposed origin-source)
      (user-error "Already %s" (or (org-get-todo-state) "")))
    (condition-case err
        (progn
          (setq dispatched-p t)
          (setq response (org-note-operation--dispatch-frozen frozen))
          (prog1
              (gsmlg-org-note-org--finish-document-todo-attempt
               document-id record response)
            (setq committed-p t)))
      ((quit error)
       (cond
        (committed-p
         ;; Post-commit UI/recovery failure: keep confirmed metadata.
         nil)
        (dispatched-p
         ;; Post-dispatch non-validated outcome is ambiguous; never pretend
         ;; the mutation did not happen by restoring prior revision/base.
         (puthash document-id record
                  gsmlg-org-note-org--document-ambiguities))
        ;; Pre-dispatch: leave buffer metadata unchanged.
        )
       (signal (car err) (cdr err))))))

(defun gsmlg-org-note-org--around-todo (orig &rest args)
  "Call ORIG with ARGS or bridge `org-todo' to an Org Note mutation."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
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
          t))
       ((and (fboundp 'org-note-document-mode)
             (derived-mode-p 'org-note-document-mode))
        (gsmlg-org-note-org--attempt-idless-document-todo arg)
        t)
       (t
        (user-error
         "Org Note TODO requires item ids or an Org Note document buffer"))))))

(defun gsmlg-org-note-org--around-agenda-todo (orig &rest args)
  "Call ORIG with ARGS or bridge a single-row `org-agenda-todo'."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (when (gsmlg-org-note-org--agenda-bulk-or-region-todo-p)
      (user-error
       "Org Note bridge refuses bulk or region agenda TODO"))
    (let* ((origin (gsmlg-org-note-org--agenda-row-origin))
           (pre (gsmlg-org-note-org--preflight-identified-item
                 (plist-get origin :workspace-id)
                 (plist-get origin :item-id)))
           (target
            (gsmlg-org-note-org--resolve-todo-target
             (car args)
             (plist-get pre :state)
             (called-interactively-p 'any)))
           (result
            (gsmlg-org-note-org--attempt-identified-transition
             target
             (list :workspace-id (plist-get origin :workspace-id)
                   :item-id (plist-get origin :item-id)))))
      (when (plist-get result :message)
        (message "%s" (plist-get result :message)))
      ;; Do not call native agenda todo / maybe-loop / line postprocessing.
      t)))

(defun gsmlg-org-note-org--around-refile (orig &rest args)
  "Call ORIG with ARGS or refuse refile until Phase 4."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "refile")
    (user-error
     "Org Note same-document refile is not available until Phase 4")))

(defun gsmlg-org-note-org--around-clock-in (orig &rest args)
  "Call ORIG with ARGS or refuse clock-in until Phase 5."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "clock")
    (user-error "Org Note clock claim is not available until Phase 5")))

(defun gsmlg-org-note-org--around-clock-out (orig &rest args)
  "Call ORIG with ARGS or refuse clock-out until Phase 5."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "clock")
    (user-error "Org Note clock release is not available until Phase 5")))

(defun gsmlg-org-note-org--around-clock-cancel (orig &rest args)
  "Call ORIG with ARGS or refuse clock-cancel until Phase 5."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "clock")
    (user-error "Org Note clock cancel is not available until Phase 5")))

(defun gsmlg-org-note-org--around-archive-subtree (orig &rest args)
  "Call ORIG with ARGS or refuse archive operations until Phase 6.
Shared around advice for org-archive entrypoints."
  (when (and gsmlg-org-note-org-enable
             (not gsmlg-org-note-org--activated))
    (gsmlg-org-note-org-activate))
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (gsmlg-org-note-org--refuse-if-plain-local "archive")
    (user-error "Org Note archive is not available until Phase 6")))

(defun gsmlg-org-note-org--install-mutation-hooks ()
  "Install TODO/refile/archive/clock advice once."
  (unless gsmlg-org-note-org--mutation-hooks-installed
    (setq gsmlg-org-note-org--mutation-hooks-installed t)
    (autoload 'org-todo "org" nil t)
    (autoload 'org-agenda-todo "org-agenda" nil t)
    (autoload 'org-refile "org-refile" nil t)
    (autoload 'org-clock-in "org-clock" nil t)
    (autoload 'org-clock-out "org-clock" nil t)
    (autoload 'org-clock-cancel "org-clock" nil t)
    (advice-add #'org-todo :around #'gsmlg-org-note-org--around-todo)
    (advice-add #'org-agenda-todo :around #'gsmlg-org-note-org--around-agenda-todo)
    (advice-add #'org-refile :around #'gsmlg-org-note-org--around-refile)
    (advice-add #'org-clock-in :around #'gsmlg-org-note-org--around-clock-in)
    (advice-add #'org-clock-out :around #'gsmlg-org-note-org--around-clock-out)
    (advice-add #'org-clock-cancel :around #'gsmlg-org-note-org--around-clock-cancel)
    (dolist (entry '((org-archive-subtree . "org-archive")
                     (org-toggle-archive-tag . "org-archive")
                     (org-archive-to-archive-sibling . "org-archive")
                     (org-archive-set-tag . "org-archive")))
      (autoload (car entry) (cdr entry) nil t)
      (advice-add (car entry) :around
                  #'gsmlg-org-note-org--around-archive-subtree))))

(defun gsmlg-org-note-org--install-feed-hooks ()
  "Install feed refresh and agenda-goto advice once."
  (unless gsmlg-org-note-org--feed-hooks-installed
    (setq gsmlg-org-note-org--feed-hooks-installed t)
    (advice-add #'org-agenda-goto :around #'gsmlg-org-note-org--goto)))

(defun gsmlg-org-note-org--around-agenda (orig &rest args)
  "Guard an Agenda producer before calling ORIG with ARGS."
  (if (or (not gsmlg-org-note-org-enable)
          gsmlg-org-note-org--agenda-command-active)
      (apply orig args)
    (let ((gsmlg-org-note-org--agenda-command-active t))
      (require 'org-note)
      (gsmlg-org-note-org-activate)
      (gsmlg-org-note-org--refresh-before-agenda)
      (let ((org-agenda-files (gsmlg-org-note-org-agenda-files)))
        (apply orig args)))))

(defun gsmlg-org-note-org--around-agenda-files (orig &rest args)
  "Return only the bridge feed while active, otherwise call ORIG with ARGS.

This lower-level guard defeats native restrictions and custom Agenda command
bindings that dynamically replace `org-agenda-files' after entrypoint advice."
  (if (and gsmlg-org-note-org-enable
           gsmlg-org-note-org--activated)
      (gsmlg-org-note-org-agenda-files)
    (apply orig args)))

(defun gsmlg-org-note-org--around-capture (orig &rest args)
  "Activate the bridge then call ORIG with ARGS.

Phase 3 owns real capture staging; this stub only cold-starts."
  (if (not gsmlg-org-note-org-enable)
      (apply orig args)
    (require 'org-note)
    (gsmlg-org-note-org-activate)
    (apply orig args)))

(defun gsmlg-org-note-org--symbol-alist-p (value)
  "Return non-nil when VALUE is a nonempty alist."
  (and (listp value) (cl-every #'consp value)))

(defun gsmlg-org-note-org--string-or-nil (value)
  "Return VALUE when it is a nonempty string."
  (and (stringp value) (not (string-empty-p value)) value))

(defun gsmlg-org-note-org--workspace-ids ()
  "Return configured Org Note agenda workspace IDs, or nil."
  (when (boundp 'org-note-agenda-workspace-ids)
    (let ((ids org-note-agenda-workspace-ids))
      (when (and (listp ids) ids)
        (let ((copy (copy-sequence ids)))
          (when (and copy (cl-every #'gsmlg-org-note-org--string-or-nil copy))
            copy))))))

(defun gsmlg-org-note-org--timestamp-raw (timestamp)
  "Return the Org timestamp text from Org Note TIMESTAMP, or nil."
  (when (gsmlg-org-note-org--symbol-alist-p timestamp)
    (gsmlg-org-note-org--string-or-nil (alist-get 'raw timestamp))))

(defun gsmlg-org-note-org--item-id (item)
  "Return the validated item ID from ITEM."
  (gsmlg-org-note-org--string-or-nil (alist-get 'id item)))

(defun gsmlg-org-note-org--item-key (item)
  "Return a deduplication key for ITEM."
  (let ((workspace (gsmlg-org-note-org--string-or-nil
                    (alist-get 'workspace_id item)))
        (id (gsmlg-org-note-org--item-id item)))
    (when (and workspace id)
      (cons workspace id))))

(defun gsmlg-org-note-org--item-less-p (left right)
  "Return non-nil when LEFT sorts before RIGHT by canonical identity."
  (let ((left-key (gsmlg-org-note-org--item-key left))
        (right-key (gsmlg-org-note-org--item-key right)))
    (or (string< (car left-key) (car right-key))
        (and (equal (car left-key) (car right-key))
             (string< (cdr left-key) (cdr right-key))))))

(defun gsmlg-org-note-org--item-tags (item)
  "Return a `:TAG:' suffix string for ITEM."
  (let ((tags (alist-get 'tags item)))
    (when (and (listp tags) tags)
      (mapconcat #'identity
                 (cl-remove-if-not #'gsmlg-org-note-org--string-or-nil tags)
                 ":"))))

(defun gsmlg-org-note-org--item-headline (item)
  "Return an Org headline line for ITEM."
  (let* ((state (or (gsmlg-org-note-org--string-or-nil
                     (alist-get 'state item))
                    "TODO"))
         (title (or (gsmlg-org-note-org--string-or-nil
                     (alist-get 'title item))
                    "Org Note item"))
         (priority (gsmlg-org-note-org--string-or-nil
                    (alist-get 'priority item)))
         (tags (gsmlg-org-note-org--item-tags item))
         (tag-string
          (mapconcat #'identity
                     (delq nil
                           (list gsmlg-org-note-org--feed-tag tags))
                     ":")))
    (format "* %s%s %s :%s:"
            state
            (if priority (format " [#%s]" priority) "")
            (replace-regexp-in-string "[\n\r\t]+" " " title)
            tag-string)))

(defun gsmlg-org-note-org--item-properties (item)
  "Return Org property drawer lines for ITEM."
  (let ((workspace (gsmlg-org-note-org--string-or-nil
                    (alist-get 'workspace_id item)))
        (id (gsmlg-org-note-org--item-id item)))
    (when (and workspace id)
      (format ":PROPERTIES:\n:ORG_NOTE_WORKSPACE_ID: %s\n:ORG_NOTE_ITEM_ID: %s\n:END:\n"
              workspace id))))

(defun gsmlg-org-note-org--item-timestamp-lines (item view)
  "Return scheduled/deadline lines for ITEM and agenda VIEW."
  (let ((scheduled (gsmlg-org-note-org--timestamp-raw
                    (alist-get 'scheduled item)))
        (deadline (gsmlg-org-note-org--timestamp-raw
                   (alist-get 'deadline item))))
    (delq nil
          (list
           (when (and scheduled (memq view '(scheduled all)))
             (format "SCHEDULED: %s" scheduled))
           (when (and deadline (memq view '(upcoming_deadline all)))
             (format "DEADLINE: %s" deadline))))))

(defun gsmlg-org-note-org--item-text (item view)
  "Return Org text for ITEM in agenda VIEW."
  (let ((lines (append (list (gsmlg-org-note-org--item-headline item))
                       (gsmlg-org-note-org--item-timestamp-lines item view)
                       (list (gsmlg-org-note-org--item-properties item)))))
    (mapconcat #'identity (delq nil lines) "\n")))

(defun gsmlg-org-note-org--page-items (response)
  "Return item alists from an Org Note agenda page RESPONSE."
  (unless (gsmlg-org-note-org--symbol-alist-p response)
    (error "Org Note agenda page is malformed"))
  (let ((raw-items (alist-get 'items response)))
    (unless (listp raw-items)
      (error "Org Note agenda page items are malformed"))
    (mapcar (lambda (row)
              (let ((item (alist-get 'item row)))
                (unless (gsmlg-org-note-org--symbol-alist-p item)
                  (error "Org Note agenda row item is malformed"))
                item))
            raw-items)))

(defun gsmlg-org-note-org--page-next-cursor (response)
  "Return the next cursor from Org Note agenda page RESPONSE.

Nil ends pagination.  An empty string fails closed as `org-note-error'."
  (require 'org-note-validation)
  (org-note-validation-page-cursor (alist-get 'next_cursor response)))

(defun gsmlg-org-note-org--fetch-view-items (workspace-ids view)
  "Fetch all agenda items for VIEW across WORKSPACE-IDS.

Uses the shared bounded pager so empty or repeated cursors, repeated
row identities, and page/row/request/time budgets fail closed.  Failures
propagate to `gsmlg-org-note-org-refresh-feed' for last-good handling."
  (require 'org-note-validation)
  (let ((state (org-note-validation-bounded-pager-state
                :limit gsmlg-org-note-org--agenda-page-limit)))
    (org-note-validation-bounded-pager-fold
     state
     (lambda (cursor)
       (let* ((response (org-note-operation-query-agenda
                         :workspace-ids workspace-ids
                         :view view
                         :cursor cursor
                         :limit gsmlg-org-note-org--agenda-page-limit))
              (items (gsmlg-org-note-org--page-items response))
              (next (gsmlg-org-note-org--page-next-cursor response)))
         (list :rows items :next-cursor next))))))
(defun gsmlg-org-note-org--write-feed (contents &optional path)
  "Write CONTENTS to PATH or the last-good Org Note agenda feed file.

Phase 1 uses a direct single-process `write-region'.  Cross-process
publication reservation locks, nonce-checked release, and acquire-
before-fetch ordering (spec blockers 69, 74, 79) are deferred to
Phase 7."
  (let ((target (or path gsmlg-org-note-org--feed-file)))
    (gsmlg-ensure-parent-directory target)
    (unless (and (file-readable-p target)
                 (with-temp-buffer
                   (insert-file-contents target)
                   (equal contents (buffer-string))))
      (write-region contents nil target nil 'silent))
    target))

(defun gsmlg-org-note-org--empty-feed-contents (&optional workspace-ids)
  "Return the contents of an empty Org Note agenda feed.

When WORKSPACE-IDS is non-nil, embed matching schema metadata."
  (concat
   (format "#+TITLE: Org Note Agenda Feed\n#+FILETAGS: %s\n"
           gsmlg-org-note-org--feed-tag)
   (format "#+ORG_NOTE_FEED_SCHEMA: %s\n"
           gsmlg-org-note-org--feed-schema-version)
   (when workspace-ids
     (format "#+ORG_NOTE_WORKSPACE_IDS: %s\n"
             (mapconcat #'identity workspace-ids " ")))))

(defun gsmlg-org-note-org--endpoint-identity ()
  "Return the canonical Org Note endpoint string used for empty feeds."
  (if (and (boundp 'org-note-endpoint)
           (stringp org-note-endpoint)
           (not (string-empty-p org-note-endpoint)))
      org-note-endpoint
    "default"))

(defun gsmlg-org-note-org--empty-feed-file ()
  "Return an endpoint-keyed empty feed path that does not clobber last-good."
  (let* ((endpoint (gsmlg-org-note-org--endpoint-identity))
         (digest (secure-hash 'sha256 endpoint)))
    (gsmlg-cache-file
     (format "org-note-agenda-empty-%s.org" (substring digest 0 16)))))

(defun gsmlg-org-note-org--select-feed (path)
  "Record PATH as the selected agenda feed and return it."
  (setq gsmlg-org-note-org--selected-feed-file path)
  path)

(defun gsmlg-org-note-org--parse-feed-keyword (contents keyword)
  "Return the value of KEYWORD from feed CONTENTS, or nil."
  (when (string-match
         (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote keyword))
         contents)
    (string-trim (match-string 1 contents))))

(defun gsmlg-org-note-org--last-good-matches-p (workspace-ids)
  "Return non-nil when last-good matches schema and WORKSPACE-IDS."
  (and (file-readable-p gsmlg-org-note-org--feed-file)
       (with-temp-buffer
         (insert-file-contents gsmlg-org-note-org--feed-file)
         (let* ((text (buffer-string))
                (schema (gsmlg-org-note-org--parse-feed-keyword
                         text "ORG_NOTE_FEED_SCHEMA"))
                (ids (gsmlg-org-note-org--parse-feed-keyword
                      text "ORG_NOTE_WORKSPACE_IDS"))
                (parsed-ids (and ids (split-string ids nil t))))
           (and (equal schema
                       (number-to-string
                        gsmlg-org-note-org--feed-schema-version))
                (equal parsed-ids workspace-ids))))))

(defun gsmlg-org-note-org--offer-last-good-or-abort (workspace-ids err)
  "Offer matching last-good for WORKSPACE-IDS after ERR, or signal."
  (let ((prompt
         (format
          "Org Note agenda refresh failed (%s). Use last-good snapshot? "
          (error-message-string err))))
    (unless (and (gsmlg-org-note-org--last-good-matches-p workspace-ids)
                 (yes-or-no-p prompt))
      (signal (car err) (cdr err)))
    (setq gsmlg-org-note-org--last-workspace-ids
          (copy-sequence workspace-ids))
    (gsmlg-org-note-org--select-feed gsmlg-org-note-org--feed-file)))

(defun gsmlg-org-note-org--fetch-views (workspace-ids)
  "Fetch scheduled and upcoming_deadline items for WORKSPACE-IDS.

Return a cons (SCHEDULED . DEADLINES)."
  (cons (gsmlg-org-note-org--fetch-view-items workspace-ids 'scheduled)
        (gsmlg-org-note-org--fetch-view-items
         workspace-ids 'upcoming_deadline)))

(defun gsmlg-org-note-org--build-feed-contents (workspace-ids)
  "Return Org feed contents for WORKSPACE-IDS."
  (let* ((views (gsmlg-org-note-org--fetch-views workspace-ids))
         (scheduled (car views))
         (deadlines (cdr views))
         (seen (make-hash-table :test #'equal))
         (items nil))
    (dolist (item (append scheduled deadlines))
      (let ((key (gsmlg-org-note-org--item-key item)))
        (when (and key (not (gethash key seen)))
          (puthash key t seen)
          (push item items))))
    (setq items (sort items #'gsmlg-org-note-org--item-less-p))
    (if items
        (concat (gsmlg-org-note-org--empty-feed-contents workspace-ids)
                (mapconcat (lambda (item)
                             (gsmlg-org-note-org--item-text item 'all))
                           items "\n\n")
                "\n")
      (gsmlg-org-note-org--empty-feed-contents workspace-ids))))

(defun gsmlg-org-note-org--ensure-workspaces ()
  "Return workspace IDs, prompting to configure when unset.

On cancel or still-empty selection, return nil.
Noninteractive sessions skip configure (treat as cancel) so batch
and ERT never block on a minibuffer."
  (or (gsmlg-org-note-org--workspace-ids)
      (progn
        (when (and (not noninteractive)
                   (fboundp #'org-note-configure-agenda-workspaces))
          (condition-case nil
              (org-note-configure-agenda-workspaces)
            (quit nil)))
        (gsmlg-org-note-org--workspace-ids))))

(defun gsmlg-org-note-org--use-empty-feed ()
  "Write and select the endpoint-keyed empty feed without clobbering last-good."
  (let ((empty (gsmlg-org-note-org--empty-feed-file)))
    (setq gsmlg-org-note-org--last-workspace-ids nil)
    (gsmlg-org-note-org--write-feed
     (gsmlg-org-note-org--empty-feed-contents)
     empty)
    (gsmlg-org-note-org--select-feed empty)))

(defun gsmlg-org-note-org-refresh-feed (&optional force)
  "Refresh the generated Org Note agenda feed file.

When FORCE is nil and workspace selection is unchanged, reuse the
existing snapshot.  Unset workspaces trigger configure-on-empty; cancel
selects an endpoint-keyed empty feed without overwriting last-good.
Pre-rename failures offer a matching last-good snapshot via
`yes-or-no-p', or abort."
  (unless gsmlg-org-note-org--refresh-active
    (setq gsmlg-org-note-org--refresh-active t)
    (unwind-protect
        (let ((workspace-ids (gsmlg-org-note-org--ensure-workspaces)))
          (cond
           ((null workspace-ids)
            (gsmlg-org-note-org--use-empty-feed))
           ((and (not force)
                 (equal workspace-ids gsmlg-org-note-org--last-workspace-ids)
                 (file-readable-p gsmlg-org-note-org--feed-file))
            (gsmlg-org-note-org--select-feed gsmlg-org-note-org--feed-file))
           (t
            (condition-case err
                (progn
                  (gsmlg-org-note-org--write-feed
                   (gsmlg-org-note-org--build-feed-contents workspace-ids))
                  (setq gsmlg-org-note-org--last-workspace-ids
                        (copy-sequence workspace-ids))
                  (gsmlg-org-note-org--select-feed
                   gsmlg-org-note-org--feed-file))
              (error
               (gsmlg-org-note-org--offer-last-good-or-abort
                workspace-ids err))))))
      (setq gsmlg-org-note-org--refresh-active nil)))
  (gsmlg-org-note-org-feed-file))

(defun gsmlg-org-note-org--refresh-before-agenda (&rest _)
  "Refresh the Org Note feed before building an agenda buffer."
  (require 'org-note)
  (gsmlg-org-note-org-refresh-feed))

(defun gsmlg-org-note-org--goto (orig-fun &optional highlight)
  "Open Org Note item context or call ORIG-FUN with HIGHLIGHT."
  (let* ((marker (org-get-at-bol 'org-marker))
         (workspace (and marker
                         (with-current-buffer (marker-buffer marker)
                           (org-entry-get (marker-position marker)
                                          "ORG_NOTE_WORKSPACE_ID"
                                          'selective))))
         (item (and marker
                    (with-current-buffer (marker-buffer marker)
                      (org-entry-get (marker-position marker)
                                     "ORG_NOTE_ITEM_ID"
                                     'selective)))))
    (if (and workspace item)
        (org-note-item-context workspace item)
      (funcall orig-fun highlight))))

(provide 'gsmlg-org-note-org)
;;; gsmlg-org-note-org.el ends here
