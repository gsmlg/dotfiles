;;; org-note-org-bridge-todo-test.el --- Phase 2 TODO bridge tests -*- lexical-binding: t; -*-

;;; Commentary:
;; State configuration, refuse policy, TODO transitions, agenda todo intercept.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)

(declare-function org-note-document-mode "org-note" ())
(declare-function org-note-operation--make-lease "org-note-operation"
                  (&rest keyword-arguments))

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

(defconst gsmlg-org-note-todo-test--org-state-variables
  '(org-todo-keywords
    org-todo-kwd-alist
    org-todo-key-alist
    org-todo-key-trigger
    org-todo-keywords-1
    org-done-keywords
    org-todo-heads
    org-todo-sets
    org-todo-log-states
    org-not-done-keywords
    org-todo-regexp
    org-not-done-regexp
    org-not-done-heading-regexp
    org-todo-line-regexp
    org-complex-heading-regexp
    org-complex-heading-regexp-format
    org-todo-line-tags-regexp)
  "Org TODO defaults and derived buffer-local tables changed by state tests.")

(defun gsmlg-org-note-todo-test--with-preserved-org-state (body)
  "Call BODY and restore Org TODO defaults and live buffer state afterward."
  (let ((defaults
         (mapcar (lambda (variable)
                   (cons variable (copy-tree (default-value variable))))
                 gsmlg-org-note-todo-test--org-state-variables))
        (buffers
         (mapcar
          (lambda (buffer)
            (cons
             buffer
             (with-current-buffer buffer
               (mapcar
                (lambda (variable)
                  (list variable
                        (local-variable-p variable)
                        (copy-tree (symbol-value variable))))
                gsmlg-org-note-todo-test--org-state-variables))))
          (buffer-list))))
    (unwind-protect
        (funcall body)
      (dolist (entry defaults)
        (set-default (car entry) (cdr entry)))
      (dolist (entry buffers)
        (when (buffer-live-p (car entry))
          (with-current-buffer (car entry)
            (dolist (state (cdr entry))
              (if (nth 1 state)
                  (set (make-local-variable (car state)) (nth 2 state))
                (kill-local-variable (car state))))))))))

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
  (gsmlg-org-note-todo-test--with-preserved-org-state
   (lambda ()
     (let ((gsmlg-org-note-todo-states '("TODO" "RUNNING"))
           (gsmlg-org-note-done-states '("DONE"))
           (gsmlg-org-note-state-fast-keys nil)
           (gsmlg-org-note-archive-target "DONE"))
       (gsmlg-org-note-apply-state-configuration
        '("NEXT" "WAITING") '("FINISHED")
        '((?n . "NEXT") (?f . "FINISHED")) "FINISHED")
       (should (equal gsmlg-org-note-todo-states '("NEXT" "WAITING")))
       (should (equal gsmlg-org-note-done-states '("FINISHED")))
       (should (equal gsmlg-org-note-state-fast-keys
                      '((?n . "NEXT") (?f . "FINISHED"))))
       (should (equal gsmlg-org-note-archive-target "FINISHED"))
       (should
        (member '("sequence" "NEXT(n)" "WAITING" "|" "FINISHED(f)")
                (mapcar #'identity org-todo-keywords)))
       ;; Org-native polarity on installed tables; bridge defcustom stays inverse.
       (should (eq (cdr (assoc "NEXT" org-todo-key-alist)) ?n))
       (should (eq (cdr (assoc "FINISHED" org-todo-key-alist)) ?f))
       (should (null (assoc "WAITING" org-todo-key-alist)))
       (should org-todo-key-trigger)))))

(ert-deftest gsmlg-org-note-apply-state-configuration-rolls-back-on-recompute-failure ()
  (require 'gsmlg-org-note-org)
  (gsmlg-org-note-todo-test--with-preserved-org-state
   (lambda ()
     (let* ((gsmlg-org-note-todo-states '("TODO"))
            (gsmlg-org-note-done-states '("DONE"))
            (gsmlg-org-note-state-fast-keys nil)
            (gsmlg-org-note-archive-target "DONE")
            (prior (gsmlg-org-note-org--validate-state-configuration
                    '("TODO") '("DONE") nil "DONE"))
            (calls 0)
            (real-recompute
             (symbol-function 'gsmlg-org-note-org--recompute-live-buffers))
            (probe (get-buffer-create "*Org Note rollback probe*")))
       (unwind-protect
           (progn
          (gsmlg-org-note-org--publish-keyword-defaults prior)
          (with-current-buffer probe
            (delay-mode-hooks (org-mode))
            (gsmlg-org-note-org--apply-keywords-in-buffer prior)
            (should (member "TODO" org-todo-keywords-1))
            (should (member "DONE" org-done-keywords)))
          (cl-letf (((symbol-function 'gsmlg-org-note-org--recompute-live-buffers)
                     (lambda (precomputed)
                       (cl-incf calls)
                       (if (= calls 1)
                           (error "Simulated recompute failure")
                         (funcall real-recompute precomputed)))))
            (should-error
             (gsmlg-org-note-apply-state-configuration
              '("A") '("B") nil "B")))
          (should (equal gsmlg-org-note-todo-states '("TODO")))
          (should (equal gsmlg-org-note-done-states '("DONE")))
          (should (equal gsmlg-org-note-state-fast-keys nil))
          (should (equal gsmlg-org-note-archive-target "DONE"))
          (should (= calls 2))
          (should (member "TODO" (default-value 'org-todo-keywords-1)))
          (should (not (member "A" (default-value 'org-todo-keywords-1))))
          (should (member "DONE" (default-value 'org-done-keywords)))
          (should (not (member "B" (default-value 'org-done-keywords))))
          (with-current-buffer probe
            (should (member "TODO" org-todo-keywords-1))
            (should (not (member "A" org-todo-keywords-1)))
            (should (member "DONE" org-done-keywords))
            (should (not (member "B" org-done-keywords)))))
         (when (buffer-live-p probe)
           (kill-buffer probe)))))))

(ert-deftest gsmlg-org-note-apply-state-configuration-surfaces-restore-failure ()
  "Rollback restore failure is not swallowed by `ignore-errors'."
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-todo-states '("TODO"))
        (gsmlg-org-note-done-states '("DONE"))
        (gsmlg-org-note-state-fast-keys nil)
        (gsmlg-org-note-archive-target "DONE")
        (err nil))
    (cl-letf (((symbol-function 'gsmlg-org-note-org--recompute-live-buffers)
               (lambda (_precomputed)
                 (error "Simulated recompute failure")))
              ((symbol-function 'gsmlg-org-note-org--publish-keyword-defaults)
               (lambda (_precomputed)
                 (error "Simulated restore publish failure"))))
      (setq err
            (should-error
             (gsmlg-org-note-apply-state-configuration
              '("A") '("B") nil "B"))))
    (should (equal gsmlg-org-note-todo-states '("TODO")))
    (should (equal gsmlg-org-note-done-states '("DONE")))
    (should (string-match-p
             "restore also failed"
             (error-message-string err)))))

(ert-deftest gsmlg-org-note-state-defaults-make-org-todo-operable ()
  (require 'gsmlg-org-note-org)
  (gsmlg-org-note-todo-test--with-preserved-org-state
   (lambda ()
     (gsmlg-org-note-org--install-todo-keywords)
     (should (member "TODO" org-todo-keywords-1))
     (should (member "RUNNING" org-todo-keywords-1))
     (should (member "DONE" org-done-keywords))
     (should (null org-todo-key-trigger))
     (should (null org-todo-key-alist)))))

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
        (gsmlg-org-note-org--mutation-hooks-installed nil)
        (orig-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest args)
                 (unless (eq feature 'org-note)
                   (apply orig-require feature args))
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

(ert-deftest gsmlg-org-note-refuse-hooks-advise-extra-archive-commands ()
  "Extra archive entrypoints share --around-archive-subtree when bound."
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-org--mutation-hooks-installed nil))
    (gsmlg-org-note-org--install-mutation-hooks)
    (dolist (cmd '(org-toggle-archive-tag
                   org-archive-to-archive-sibling
                   org-archive-set-tag))
      (when (fboundp cmd)
        (should (advice-member-p #'gsmlg-org-note-org--around-archive-subtree
                                 cmd))))))

(defun gsmlg-org-note-todo-test--context (state revision)
  "Return a get-item-context response with STATE and document REVISION."
  `((schema_version . 1)
    (data
     . ((context
         . ((workspace . ((id . "ws-1")))
            (document . ((id . "doc-1") (revision . ,revision)))
            (item . ((id . "item-1")
                     (workspace_id . "ws-1")
                     (document_id . "doc-1")
                     (state . ,state)))))))))

(defun gsmlg-org-note-todo-test--transition-response
    (workspace-id item-id document-id operation-id &optional state revision)
  "Return a local transition response fixture.

The response identifies WORKSPACE-ID, ITEM-ID, DOCUMENT-ID, and OPERATION-ID.
Optional STATE and REVISION override the fixture defaults."
  `((schema_version . 1)
    (workspace_id . ,workspace-id)
    (operation_id . ,operation-id)
    (event_ids . ["event-1"])
    (data
     . ((context
         . ((workspace . ((id . ,workspace-id)))
            (workspace_revision . 5)
            (document . ((id . ,document-id)
                         (revision . ,(or revision 5))))
            (item . ((id . ,item-id)
                     (workspace_id . ,workspace-id)
                     (document_id . ,document-id)
                     (state . ,(or state "ready"))))
            (lease . nil)))))))

(ert-deftest gsmlg-org-note-preflight-uses-context-revision ()
  (require 'gsmlg-org-note-org)
  (require 'org-note-operation)
  (clrhash gsmlg-org-note-org--transition-ambiguities)
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
  (require 'org-note-operation)
  (clrhash gsmlg-org-note-org--transition-ambiguities)
  (clrhash gsmlg-org-note-org--frozen-transitions)
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
  (require 'org-note-operation)
  (clrhash gsmlg-org-note-org--transition-ambiguities)
  (clrhash gsmlg-org-note-org--frozen-transitions)
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
                 (gsmlg-org-note-todo-test--transition-response
                  "ws-1" "item-1" "doc-1" "op-bridge-1" "DONE" 4)))
              ((symbol-function 'org-note-operation--validate-transition-response)
               (lambda (&rest args)
                 (push 'validate order)
                 (apply (lambda (&rest _args) '((ok . t))) args)))
              ((symbol-function 'gsmlg-org-note-org-refresh-feed)
               (lambda (&rest _)
                 (push 'refresh order)
                 (error "Refresh failed"))))
      (let ((result
             (gsmlg-org-note-org--attempt-identified-transition
              "DONE" '(:workspace-id "ws-1" :item-id "item-1"))))
        (should (plist-get result :committed-p))
        (should (equal (reverse order)
                       '(dispatch validate refresh)))
        (should (string-match-p "stale\\|succeeded"
                                (or (plist-get result :message) "")))
        (should-not (gethash (cons "ws-1" "item-1")
                             gsmlg-org-note-org--frozen-transitions))))))

(ert-deftest gsmlg-org-note-transition-includes-lease-proof ()
  "Registered lease proof is frozen into the request; validate+reconcile match public transition."
  (require 'gsmlg-org-note-org)
  (require 'org-note-operation)
  (clrhash gsmlg-org-note-org--transition-ambiguities)
  (clrhash gsmlg-org-note-org--frozen-transitions)
  (let* ((fake-lease
          (org-note-operation--make-lease
           :workspace-id "ws-1"
           :item-id "item-1"
           :document-id "doc-1"
           :kind "execution"
           :lease-id "lease-1"
           :fencing-token "fence-1"
           :expires-at (+ (float-time) 3600)
           :heartbeat-p nil))
         (expected-proof
          '((lease_id . "lease-1")
            (kind . "execution")
            (fencing_token . "fence-1")))
         captured-typed
         captured-validate
         reconcile-called
         (orig-typed
          (symbol-function 'org-note-operation--transition-typed-request)))
    (cl-letf (((symbol-function 'org-note-operation-get-item-context)
               (lambda (&rest _)
                 (gsmlg-org-note-todo-test--context "TODO" 3)))
              ((symbol-function 'org-note-operation-find-lease)
               (lambda (&rest _) fake-lease))
              ((symbol-function 'org-note-client-new-operation-id)
               (lambda () "op-lease-1"))
              ((symbol-function 'org-note-operation--transition-typed-request)
               (lambda (&rest args)
                 (let ((typed (apply orig-typed args)))
                   (setq captured-typed typed)
                   typed)))
              ((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (_env)
                 (gsmlg-org-note-todo-test--transition-response
                  "ws-1" "item-1" "doc-1" "op-lease-1" "DONE" 4)))
              ((symbol-function 'org-note-operation--validate-transition-response)
               (lambda (&rest args)
                 (setq captured-validate args)
                 '((ok . t))))
              ((symbol-function 'org-note-operation--reconcile-transition-lease)
               (lambda (registered context ws item)
                 (setq reconcile-called
                       (list registered context ws item))
                 nil))
              ((symbol-function 'gsmlg-org-note-org-refresh-feed)
               (lambda (&rest _) nil)))
      (let ((result
             (gsmlg-org-note-org--attempt-identified-transition
              "DONE" '(:workspace-id "ws-1" :item-id "item-1"))))
        (should (plist-get result :committed-p))
        (should (equal (alist-get 'lease (plist-get captured-typed :body))
                       expected-proof))
        (should (equal (nth 7 captured-validate) "lease-1"))
        (should (equal (nth 8 captured-validate) "execution"))
        (should reconcile-called)
        (should (eq (car reconcile-called) fake-lease))
        (should-not (gethash (cons "ws-1" "item-1")
                             gsmlg-org-note-org--frozen-transitions))))))

(ert-deftest gsmlg-org-note-transition-failure-leaves-ui-unchanged ()
  "Pre-commit dispatch failure marks ambiguity and skips refresh."
  (require 'gsmlg-org-note-org)
  (require 'org-note-operation)
  (clrhash gsmlg-org-note-org--transition-ambiguities)
  (clrhash gsmlg-org-note-org--frozen-transitions)
  (let ((refreshed 0))
    (cl-letf (((symbol-function 'org-note-operation-get-item-context)
               (lambda (&rest _)
                 (gsmlg-org-note-todo-test--context "TODO" 3)))
              ((symbol-function 'org-note-operation-find-lease)
               (lambda (&rest _) nil))
              ((symbol-function 'org-note-client-new-operation-id)
               (lambda () "op-fail-1"))
              ((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (&rest _)
                 (error "Dispatch failed")))
              ((symbol-function 'gsmlg-org-note-org-refresh-feed)
               (lambda (&rest _)
                 (cl-incf refreshed)
                 nil)))
      (should-error
       (gsmlg-org-note-org--attempt-identified-transition
        "DONE" '(:workspace-id "ws-1" :item-id "item-1")))
      (should (= refreshed 0))
      (should (gethash (cons "ws-1" "item-1")
                       gsmlg-org-note-org--transition-ambiguities))
      (should-error
       (gsmlg-org-note-org--preflight-identified-item "ws-1" "item-1")
       :type 'user-error))))

(ert-deftest gsmlg-org-note-transition-ambiguity-retries-exact-attempt ()
  "Retry an ambiguous transition without a fresh preflight or operation id."
  (require 'gsmlg-org-note-org)
  (require 'org-note-operation)
  (let* ((key (cons "ws-1" "item-1"))
         (frozen (list :method "POST"
                       :body (encode-coding-string "frozen" 'binary)))
         (response
          (gsmlg-org-note-todo-test--transition-response
           "ws-1" "item-1" "doc-1" "op-1" "DONE" 3))
         (dispatches nil)
         (preflight-calls 0)
         (operation-id-calls 0))
    (clrhash gsmlg-org-note-org--transition-ambiguities)
    (puthash key
             (list :operation-id "op-1" :frozen frozen
                   :workspace-id "ws-1" :item-id "item-1"
                   :document-id "doc-1" :expected-revision 2
                   :target-state "DONE" :registered-lease nil)
             gsmlg-org-note-org--transition-ambiguities)
    (cl-letf (((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (envelope)
                 (push envelope dispatches)
                 response))
              ((symbol-function 'gsmlg-org-note-org--preflight-identified-item)
               (lambda (&rest _)
                 (cl-incf preflight-calls)))
              ((symbol-function 'org-note-client-new-operation-id)
               (lambda ()
                 (cl-incf operation-id-calls)
                 "fresh-op"))
              ((symbol-function 'gsmlg-org-note-org-refresh-feed)
               (lambda (&rest _) t)))
      (let ((result
             (gsmlg-org-note-org-retry-ambiguous-transition
              "ws-1" "item-1")))
        (should (plist-get result :committed-p))
        (should (equal dispatches (list frozen)))
        (should (eq (car dispatches) frozen))
        (should (= preflight-calls 0))
        (should (= operation-id-calls 0))
        (should-not (gethash key gsmlg-org-note-org--transition-ambiguities))))))

(ert-deftest gsmlg-org-note-transition-post-commit-quit-is-not-replayable ()
  "A quit during reconciliation cannot turn confirmed success ambiguous."
  (require 'gsmlg-org-note-org)
  (require 'org-note-operation)
  (let* ((key (cons "ws-1" "item-1"))
         (fake-lease
          (org-note-operation--make-lease
           :workspace-id "ws-1" :item-id "item-1" :document-id "doc-1"
           :kind "execution" :lease-id "lease-1" :fencing-token "fence-1"
           :expires-at (+ (float-time) 3600) :heartbeat-p nil))
         (dispatches 0))
    (clrhash gsmlg-org-note-org--transition-ambiguities)
    (clrhash gsmlg-org-note-org--frozen-transitions)
    (cl-letf (((symbol-function 'org-note-operation-get-item-context)
               (lambda (&rest _)
                 (gsmlg-org-note-todo-test--context "TODO" 3)))
              ((symbol-function 'org-note-operation-find-lease)
               (lambda (&rest _) fake-lease))
              ((symbol-function 'org-note-client-new-operation-id)
               (lambda () "op-quit-1"))
              ((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (&rest _)
                 (cl-incf dispatches)
                 (gsmlg-org-note-todo-test--transition-response
                  "ws-1" "item-1" "doc-1" "op-quit-1" "DONE" 4)))
              ((symbol-function 'org-note-operation--reconcile-transition-lease)
               (lambda (&rest _)
                 (signal 'quit nil))))
      (let ((result
             (gsmlg-org-note-org--attempt-identified-transition
              "DONE" '(:workspace-id "ws-1" :item-id "item-1"))))
        (should (plist-get result :committed-p))
        (should (string-match-p "view stale" (plist-get result :message)))
        (should-not (gethash key gsmlg-org-note-org--transition-ambiguities))
        (should-not (gethash key gsmlg-org-note-org--frozen-transitions))
        (should-error
         (gsmlg-org-note-org-retry-ambiguous-transition "ws-1" "item-1")
         :type 'user-error)
        (should (= dispatches 1))))))

(ert-deftest gsmlg-org-note-direct-todo-cold-start-refuses-before-native-mutation ()
  "Enabled direct TODO activates and refuses local mutation on first call."
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated nil)
        (activation-calls 0)
        (native-calls 0))
    (cl-letf (((symbol-function 'gsmlg-org-note-org-activate)
               (lambda ()
                 (cl-incf activation-calls)
                 (setq gsmlg-org-note-org--activated t)))
              ((symbol-function 'gsmlg-org-note-org--plain-local-org-buffer-p)
               (lambda (&optional _) t)))
      (should-error
       (gsmlg-org-note-org--around-todo
        (lambda (&rest _)
          (cl-incf native-calls))
        'done)
       :type 'user-error)
      (should (= activation-calls 1))
      (should (= native-calls 0)))))

(ert-deftest gsmlg-org-note-agenda-todo-refuses-bulk ()
  (require 'gsmlg-org-note-org)
  (require 'org-agenda)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (org-agenda-bulk-marked-entries (list (point-marker))))
    (should-error
     (gsmlg-org-note-org--around-agenda-todo (lambda (&rest _) 'native) nil)
     :type 'user-error)))

(ert-deftest gsmlg-org-note-agenda-todo-refuses-region ()
  (require 'gsmlg-org-note-org)
  (require 'org-agenda)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (org-agenda-bulk-marked-entries nil))
    (with-temp-buffer
      (insert "ab")
      (goto-char (point-min))
      (set-mark (point-max))
      (activate-mark)
      (should-error
       (gsmlg-org-note-org--around-agenda-todo (lambda (&rest _) 'native) nil)
       :type 'user-error))))

(ert-deftest gsmlg-org-note-agenda-todo-refuses-bulk-action ()
  (require 'gsmlg-org-note-org)
  (require 'org-agenda)
  ;; `org-agenda-bulk-action' is a command; bind a dynamic value to exercise
  ;; the refuse arm that checks (boundp 'org-agenda-bulk-action).
  (defvar org-agenda-bulk-action)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (org-agenda-bulk-marked-entries nil)
        (org-agenda-bulk-action t))
    (should-error
     (gsmlg-org-note-org--around-agenda-todo (lambda (&rest _) 'native) nil)
     :type 'user-error)))

(ert-deftest gsmlg-org-note-agenda-row-origin-reads-marker-state ()
  "Origin :state follows the agenda marker heading, not buffer point."
  (require 'gsmlg-org-note-org)
  (require 'org-agenda)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Heading A\n"
            ":PROPERTIES:\n"
            ":ORG_NOTE_WORKSPACE_ID: ws-a\n"
            ":ORG_NOTE_ITEM_ID: item-a\n"
            ":END:\n"
            "* DONE Heading B\n"
            ":PROPERTIES:\n"
            ":ORG_NOTE_WORKSPACE_ID: ws-b\n"
            ":ORG_NOTE_ITEM_ID: item-b\n"
            ":END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\* DONE Heading B")
    (beginning-of-line)
    (let ((marker-b (copy-marker (point))))
      (goto-char (point-min))
      (re-search-forward "^\\* TODO Heading A")
      (beginning-of-line)
      (should (equal (org-get-todo-state) "TODO"))
      (with-temp-buffer
        (insert "  agenda row for B\n")
        (goto-char (point-min))
        (put-text-property (line-beginning-position) (line-end-position)
                           'org-hd-marker marker-b)
        (let ((origin (gsmlg-org-note-org--agenda-row-origin)))
          (should (equal (plist-get origin :workspace-id) "ws-b"))
          (should (equal (plist-get origin :item-id) "item-b"))
          (should (equal (plist-get origin :state) "DONE")))))))

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
                 (list :committed-p t :message nil))))
      (should
       (gsmlg-org-note-org--around-agenda-todo (lambda (&rest _) 'native) nil))
      (should called)
      (should (equal (car called) "DONE")))))

(ert-deftest gsmlg-org-note-agenda-todo-resolves-from-preflight-state ()
  "Agenda TODO cycles from validated preflight state, not feed presentation."
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (resolved-from nil)
        (called nil)
        (org-agenda-bulk-marked-entries nil))
    (cl-letf (((symbol-function 'gsmlg-org-note-org--agenda-bulk-or-region-todo-p)
               (lambda () nil))
              ((symbol-function 'gsmlg-org-note-org--agenda-row-origin)
               (lambda ()
                 ;; Stale feed presentation claims TODO while server is DONE.
                 '(:workspace-id "ws-1" :item-id "item-1" :state "TODO")))
              ((symbol-function 'gsmlg-org-note-org--preflight-identified-item)
               (lambda (&rest _)
                 (list :workspace-id "ws-1" :item-id "item-1"
                       :document-id "doc-1" :revision 4 :state "DONE"
                       :lease-proof nil)))
              ((symbol-function 'gsmlg-org-note-org--resolve-todo-target)
               (lambda (_arg current &optional _interactive)
                 (setq resolved-from current)
                 "TODO"))
              ((symbol-function 'gsmlg-org-note-org--attempt-identified-transition)
               (lambda (target origin)
                 (setq called (list target origin))
                 (list :committed-p t :message nil))))
      (should
       (gsmlg-org-note-org--around-agenda-todo
        (lambda (&rest _) 'native) nil))
      (should (equal resolved-from "DONE"))
      (should (equal (car called) "TODO")))))

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
      (gsmlg-org-note-org--around-todo (lambda (&rest _) (error "Native"))
                                       nil)
      (should called))))

(ert-deftest gsmlg-org-note-idless-todo-puts-document-and-warns ()
  (require 'gsmlg-org-note-org)
  (require 'org-note-document)
  (require 'org-note-operation)
  (clrhash gsmlg-org-note-org--document-ambiguities)
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
      (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (&rest _)
                   (cl-incf put-calls)
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
        (should-not (buffer-modified-p))
        (should (= org-note-document-revision 3))))))

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
  (require 'org-note-operation)
  (clrhash gsmlg-org-note-org--document-ambiguities)
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
                ((symbol-function 'org-note-operation-lease-proofs)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (&rest _)
                   '((document_revisions . ((doc-1 . 3))))))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (gsmlg-org-note-org--attempt-idless-document-todo 'done)
        (should (= transition-calls 0))))))

(ert-deftest gsmlg-org-note-idless-todo-post-dispatch-validation-marks-ambiguous ()
  "Post-dispatch validation failure must not pretend rollback; block retry."
  (require 'gsmlg-org-note-org)
  (require 'org-note-document)
  (require 'org-note-operation)
  (clrhash gsmlg-org-note-org--document-ambiguities)
  (let ((prior-base "* TODO Task\n")
        (dispatched 0))
    (with-temp-buffer
      (org-note-document-mode)
      (setq-local org-note-document-workspace-id "ws-1"
                  org-note-document-id "doc-1"
                  org-note-document-path "notes/a.org"
                  org-note-document-revision 2
                  org-note-document-base-source prior-base)
      (insert prior-base)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-note-client-new-operation-id)
                 (lambda () "op-idless-1"))
                ((symbol-function 'org-note-operation-lease-proofs)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (_env)
                   (cl-incf dispatched)
                   ;; Dispatch returned, but revision validation fails.
                   '((document_revisions . ((doc-1 . "bad")))))))
        (should-error
         (gsmlg-org-note-org--attempt-idless-document-todo 'done)
         :type 'user-error)
        (should (= dispatched 1))
        ;; Do not restore prior revision/base as if the mutation never happened.
        (should (= org-note-document-revision 2))
        (should (equal org-note-document-base-source prior-base))
        (should (gethash "doc-1" gsmlg-org-note-org--document-ambiguities))
        (should (equal
                 (plist-get
                  (gethash "doc-1" gsmlg-org-note-org--document-ambiguities)
                  :operation-id)
                 "op-idless-1"))
        ;; Further id-less TODO is blocked fail-closed.
        (should-error
         (gsmlg-org-note-org--attempt-idless-document-todo 'done)
         :type 'user-error)
        (should (= dispatched 1))))))

(ert-deftest gsmlg-org-note-document-ambiguity-retries-exact-attempt ()
  "Retry an ambiguous document TODO with its frozen envelope and operation id."
  (require 'gsmlg-org-note-org)
  (require 'org-note-document)
  (let ((frozen (list :method "PUT"
                      :body (encode-coding-string "frozen-put" 'binary)))
        (dispatches nil)
        (operation-id-calls 0))
    (clrhash gsmlg-org-note-org--document-ambiguities)
    (with-temp-buffer
      (org-note-document-mode)
      (setq-local org-note-document-workspace-id "ws-1"
                  org-note-document-id "doc-1"
                  org-note-document-path "notes/a.org"
                  org-note-document-revision 2
                  org-note-document-base-source "* TODO Task\n")
      (insert "* TODO Task\n")
      (set-buffer-modified-p nil)
      (puthash "doc-1"
               (list :operation-id "op-put" :frozen frozen
                     :buffer (current-buffer) :origin-tick (buffer-modified-tick)
                     :origin-point (point-min) :origin-source (buffer-string)
                     :proposed-source "* DONE Task\n"
                     :expected-revision 2)
               gsmlg-org-note-org--document-ambiguities)
      (cl-letf (((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (envelope)
                   (push envelope dispatches)
                   '((document_revisions . ((doc-1 . 3))))))
                ((symbol-function 'org-note-client-new-operation-id)
                 (lambda ()
                   (cl-incf operation-id-calls)
                   "fresh-op")))
        (gsmlg-org-note-org-retry-ambiguous-document-todo "doc-1")
        (should (equal dispatches (list frozen)))
        (should (eq (car dispatches) frozen))
        (should (= operation-id-calls 0))
        (should (= org-note-document-revision 3))
        (should (equal (buffer-string) "* DONE Task\n"))
        (should-not (gethash "doc-1"
                             gsmlg-org-note-org--document-ambiguities))))))

(provide 'org-note-org-bridge-todo-test)
;;; org-note-org-bridge-todo-test.el ends here
