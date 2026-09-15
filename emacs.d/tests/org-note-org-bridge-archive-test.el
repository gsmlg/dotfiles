;;; org-note-org-bridge-archive-test.el --- Archive bridge tests -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'org-note)
(require 'gsmlg-org-note-org)

(defun gsmlg-org-note-org-archive-test--ensure-runtime ()
  "Load the real Org Note runtime after another test unloads it."
  (require 'org-note)
  (unless (and (fboundp 'org-note--document-lifecycle-context)
               (fboundp 'org-note--document-lifecycle-from-context))
    (error "Org Note document lifecycle helpers are unavailable")))

(defmacro gsmlg-org-note-org-archive-test--with-document (&rest body)
  "Evaluate BODY in a bridge-enabled disposable Org Note document buffer."
  (declare (indent 0) (debug t))
  `(let ((gsmlg-org-note-org-enable t)
         (gsmlg-org-note-org--activated t)
         (gsmlg-state-directory (make-temp-file "org-note-archive-state-" t))
         (gsmlg-org-note-org--archive-ambiguities
          (make-hash-table :test #'equal)))
     (unwind-protect
         (with-temp-buffer
           (gsmlg-org-note-org-archive-test--ensure-runtime)
           (gsmlg-org-note-org-install-guards)
           (org-note-document-mode)
           (setq-local org-note-document-workspace-id "workspace-1"
                       org-note-document-id "document-1"
                       org-note-document-path "notes/one.org"
                       org-note-document-revision 4
                       org-note-document-base-source "* TODO Heading\n")
           ,@body)
       (delete-directory gsmlg-state-directory t))))

(ert-deftest gsmlg-org-note-org-archive-list-paginates-all-pages ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'org-note-operation-list-documents)
               (lambda (_workspace &rest args)
                 (let ((cursor (plist-get args :cursor)))
                   (push cursor calls)
                   (if cursor
                       '((documents . (((id . "doc-2") (path . "b.org")
                                        (revision . 4) (archived_at . 20)))) )
                     '((documents . (((id . "doc-1") (path . "a.org")
                                      (revision . 3) (archived_at . 10))))
                       (next_cursor . "opaque")))))))
      (let ((rows (gsmlg-org-note-org--archive-list-documents "ws")))
        (should (= (length rows) 2))
        (should (= (length calls) 2))))))

(ert-deftest gsmlg-org-note-org-archive-row-requires-positive-timestamp ()
  (should-error
   (gsmlg-org-note-org--archive-row
    '(((id . "doc") (path . "x.org") (revision . 2) (archived_at . 0)))
    "doc" "x.org")))

(ert-deftest gsmlg-org-note-org-archive-row-requires-unique-id-and-path ()
  (should-error
   (gsmlg-org-note-org--archive-row
    '(((id . "doc") (path . "x.org") (revision . 2) (archived_at . 1))
      ((id . "doc") (path . "x.org") (revision . 3) (archived_at . 2)))
    "doc" "x.org")))

(ert-deftest gsmlg-org-note-org-archive-row-rejects-id-at-wrong-path ()
  "An exact document id at another path cannot verify archive commit."
  (should-error
   (gsmlg-org-note-org--archive-row
    '(((id . "doc") (path . "other.org") (revision . 2) (archived_at . 1)))
    "doc" "x.org")))

(ert-deftest gsmlg-org-note-org-archive-subtree-routes-identified-item-only ()
  "An identified document heading must not imply whole-document archive."
  (gsmlg-org-note-org-archive-test--with-document
    (insert "* TODO Heading\n:PROPERTIES:\n:ORG_NOTE_WORKSPACE_ID: workspace-1\n:ORG_NOTE_ITEM_ID: item-1\n:END:\n")
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (let ((transition nil)
          (document-archives 0))
      (cl-letf (((symbol-function 'gsmlg-org-note-org--archive-document)
                 (lambda ()
                   (cl-incf document-archives)))
                ((symbol-function 'gsmlg-org-note-org--attempt-identified-transition)
                 (lambda (target origin)
                   (setq transition (list target origin))
                   (list :committed-p t))))
        (org-archive-subtree)
        (should (equal transition
                       (list gsmlg-org-note-archive-target
                             '(:workspace-id "workspace-1" :item-id "item-1"))))
        (should (= document-archives 0))))))

(ert-deftest gsmlg-org-note-org-archive-subtree-refuses-idless-heading ()
  "An id-less document heading must not archive the entire document."
  (gsmlg-org-note-org-archive-test--with-document
    (insert "* TODO Unidentified\n")
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (let ((transitions 0)
          (document-archives 0))
      (cl-letf (((symbol-function 'gsmlg-org-note-org--archive-document)
                 (lambda ()
                   (cl-incf document-archives)))
                ((symbol-function 'gsmlg-org-note-org--attempt-identified-transition)
                 (lambda (&rest _)
                   (cl-incf transitions))))
        (should-error (org-archive-subtree)
                      :type 'user-error)
        (should (= transitions 0))
        (should (= document-archives 0))))))

(ert-deftest gsmlg-org-note-org-agenda-archive-entrypoint-routes-only-one-row ()
  "The shared Agenda archive entrypoint must route one identified row."
  (require 'org-agenda)
  (gsmlg-org-note-org-install-guards)
  (with-temp-buffer
    (let ((gsmlg-org-note-org-enable t)
          (gsmlg-org-note-org--activated t)
          (org-agenda-buffer-name (buffer-name))
          transition)
      (cl-letf (((symbol-function 'gsmlg-org-note-org--origin-item-ids)
                 (lambda () (cons "workspace-1" "item-1")))
                ((symbol-function 'gsmlg-org-note-org--attempt-identified-transition)
                 (lambda (target origin)
                   (setq transition (list target origin))
                   (list :committed-p t))))
        (org-agenda-archive-with ?a nil)
        (should (equal transition
                       (list gsmlg-org-note-archive-target
                             '(:workspace-id "workspace-1"
                               :item-id "item-1"))))))))

(ert-deftest gsmlg-org-note-org-agenda-archive-refuses-marked-row-before-dispatch ()
  "Marked Agenda archive must fail before an identified transition is sent."
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (org-agenda-bulk-marked-entries '(marker))
        (transitions 0))
    (cl-letf (((symbol-function 'gsmlg-org-note-org--plain-local-org-buffer-p)
               (lambda (&optional _) nil))
              ((symbol-function 'gsmlg-org-note-org--origin-item-ids)
               (lambda () (cons "workspace-1" "item-1")))
              ((symbol-function 'gsmlg-org-note-org--attempt-identified-transition)
               (lambda (&rest _)
                 (cl-incf transitions))))
      (should-error (gsmlg-org-note-org--around-archive-subtree #'ignore)
                    :type 'user-error)
      (should (= transitions 0)))))

(ert-deftest gsmlg-org-note-org-agenda-bulk-entrypoint-refuses-before-native ()
  "Direct Agenda bulk entrypoint refuses before reading or dispatching actions."
  (require 'org-agenda)
  (gsmlg-org-note-org-install-guards)
  (with-temp-buffer
    (let ((gsmlg-org-note-org-enable t)
          (org-agenda-buffer-name (buffer-name))
          (native-calls 0))
      (cl-letf (((symbol-function 'read-char-exclusive)
                 (lambda () (ert-fail "bulk action prompt was reached"))))
        (should-error
         (gsmlg-org-note-org--around-agenda-bulk-action
          (lambda (&rest _)
            (cl-incf native-calls)))
         :type 'user-error)
        (should (= native-calls 0))))))

(ert-deftest gsmlg-org-note-org-archive-pager-rebinds-frozen-endpoint-per-page ()
  "Every reconciliation page starts from the frozen endpoint binding."
  (let ((org-note-endpoint "https://global.example")
        endpoints)
    (cl-letf (((symbol-function 'org-note-operation-list-documents)
               (lambda (_workspace &rest args)
                 (push org-note-endpoint endpoints)
                 (if (plist-get args :cursor)
                     '((documents . ()))
                   (setq org-note-endpoint "https://changed.example")
                   '((documents . (((id . "other") (path . "x")
                                    (revision . 1) (archived_at . 1))))
                     (next_cursor . "next"))))))
      (gsmlg-org-note-org--archive-list-documents
       "workspace-1" "https://frozen.example")
      (should (equal (nreverse endpoints)
                     '("https://frozen.example" "https://frozen.example"))))))

(ert-deftest gsmlg-org-note-org-archive-reconciliation-keeps-frozen-endpoint ()
  "Archive POST and reconciliation must keep the endpoint frozen before POST."
  (gsmlg-org-note-org-archive-test--with-document
    (insert "* TODO Heading\n")
    (set-buffer-modified-p nil)
    (let ((org-note-endpoint "https://archive-a.example")
          endpoints)
      (cl-letf (((symbol-function 'org-note-client-new-operation-id)
                 (lambda () "archive-operation-1"))
                ((symbol-function 'org-note-operation--freeze-request)
                 (lambda (_)
                   (list :endpoint org-note-endpoint :url org-note-endpoint)))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (_)
                   (setq org-note-endpoint "https://archive-b.example")
                   '((revision . 5))))
                ((symbol-function 'org-note-operation-list-documents)
                 (lambda (&rest _)
                   (push org-note-endpoint endpoints)
                   '((documents . (((id . "document-1")
                                    (path . "notes/one.org")
                                    (revision . 5)
                                    (archived_at . 1)))))))
                ((symbol-function 'org-note-document--kill-buffer-safely)
                 #'ignore))
        (gsmlg-org-note-org--archive-document)
        (should (equal endpoints '("https://archive-a.example")))))))

(ert-deftest gsmlg-org-note-org-archive-postcommit-divergence-preserves-buffer ()
  "A post-commit archive edit must retain a read-only recovery buffer."
  (let ((state-directory (make-temp-file "org-note-archive-state-" t))
        (buffer (generate-new-buffer " *org-note-archive-divergence*")))
    (unwind-protect
        (with-current-buffer buffer
          (gsmlg-org-note-org-archive-test--ensure-runtime)
          (org-note-document-mode)
          (setq-local org-note-document-workspace-id "workspace-1"
                      org-note-document-id "document-1"
                      org-note-document-path "notes/one.org"
                      org-note-document-revision 4
                      org-note-document-base-source "* TODO Heading\n")
          (insert "* TODO Heading\n")
          (set-buffer-modified-p nil)
          (let ((gsmlg-org-note-org-enable t)
                (gsmlg-org-note-org--activated t)
                (org-note-endpoint "https://archive-a.example")
                (gsmlg-state-directory state-directory)
                (gsmlg-org-note-org--archive-ambiguities
                 (make-hash-table :test #'equal)))
            (cl-letf (((symbol-function 'org-note-client-new-operation-id)
                       (lambda () "archive-operation-1"))
                      ((symbol-function 'org-note-operation--freeze-request)
                       (lambda (_)
                         (list :endpoint org-note-endpoint
                               :url org-note-endpoint)))
                      ((symbol-function 'org-note-operation--dispatch-frozen)
                       (lambda (_)
                         (let ((inhibit-modification-hooks t)
                               (inhibit-read-only t))
                           (goto-char (point-max))
                           (insert "changed during dispatch\n"))
                         '((revision . 5))))
                      ((symbol-function 'org-note-operation-list-documents)
                       (lambda (&rest _)
                         '((documents . (((id . "document-1")
                                          (path . "notes/one.org")
                                          (revision . 5)
                                          (archived_at . 1))))))))
              (gsmlg-org-note-org--archive-document)
              (should (buffer-live-p buffer))
              (should buffer-read-only)
              (should (string-match-p "changed during dispatch" (buffer-string))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer))))
      (delete-directory state-directory t))))

(ert-deftest gsmlg-org-note-org-document-archive-confirms-unsaved-draft ()
  "Explicit document archive retains the native two-confirmation contract."
  (gsmlg-org-note-org-archive-test--with-document
    (insert "draft\n")
    (let (prompts lifecycle)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt prompts)
                   t))
                ((symbol-function 'gsmlg-org-note-org--archive-document)
                 (lambda (context)
                   (setq lifecycle context)
                   'archived)))
        (should (eq 'archived
                    (gsmlg-org-note-org--around-document-archive #'ignore)))
        (should (= (length prompts) 2))
        (should (equal (nth 0 lifecycle) "workspace-1"))
        (should (equal (nth 1 lifecycle) "document-1"))))))

(ert-deftest gsmlg-org-note-org-archive-persists-before-dispatch ()
  "Prepared and dispatched markers precede the archive POST."
  (gsmlg-org-note-org-archive-test--with-document
    (insert "source\n")
    (set-buffer-modified-p nil)
    (let ((marker-write
           (symbol-function 'gsmlg-org-note-org--noncapture-marker-write))
          states dispatched)
      (cl-letf (((symbol-function 'org-note-client-new-operation-id)
                 (lambda () "archive-order-1"))
                ((symbol-function 'gsmlg-org-note-org--noncapture-marker-write)
                 (lambda (record)
                   (push (plist-get record :state) states)
                   (funcall marker-write record)))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (frozen)
                   (setq dispatched frozen)
                   (should (equal (car states) "dispatched"))
                   '((revision . 5))))
                ((symbol-function 'org-note-operation-list-documents)
                 (lambda (&rest _)
                   '((documents . (((id . "document-1")
                                    (path . "notes/one.org")
                                    (revision . 5)
                                    (archived_at . 1)))))))
                ((symbol-function 'org-note-document--kill-buffer-safely)
                 #'ignore))
        (should (equal '((revision . 5))
                       (gsmlg-org-note-org--archive-document)))
        (should dispatched)
        (should (equal (seq-take (nreverse states) 2)
                       '("prepared" "dispatched")))))))

(ert-deftest gsmlg-org-note-org-archive-409-skips-list-and-cleans-locally ()
  "Validated stale revision is definitive and performs no reconciliation GET."
  (gsmlg-org-note-org-archive-test--with-document
    (insert "source\n")
    (set-buffer-modified-p nil)
    (let ((list-calls 0)
          states)
      (cl-letf (((symbol-function 'org-note-client-new-operation-id)
                 (lambda () "archive-conflict-1"))
                ((symbol-function 'gsmlg-org-note-org--noncapture-marker-write)
                 (lambda (record)
                   (push (plist-get record :state) states)))
                ((symbol-function 'gsmlg-org-note-org--noncapture-marker-delete)
                 #'ignore)
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (_)
                   (signal 'org-note-http-error
                           '((:status 409 :code "stale_revision")))))
                ((symbol-function 'org-note-operation-list-documents)
                 (lambda (&rest _)
                   (cl-incf list-calls))))
        (should-error (gsmlg-org-note-org--archive-document)
                      :type 'org-note-http-error)
        (should (= list-calls 0))
        (should (member "definitive-noncommit-pending-cleanup" states))
        (should-not buffer-read-only)
        (should-not (gethash "document-1"
                             gsmlg-org-note-org--archive-ambiguities))))))

(ert-deftest gsmlg-org-note-org-archive-409-cleanup-retry-stays-local ()
  "A failed 409 cleanup retry never repeats POST or reconciliation."
  (gsmlg-org-note-org-archive-test--with-document
    (insert "source\n")
    (set-buffer-modified-p nil)
    (let ((deletes 0)
          (dispatches 0)
          (lists 0))
      (cl-letf (((symbol-function 'org-note-client-new-operation-id)
                 (lambda () "archive-conflict-cleanup-1"))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (_)
                   (cl-incf dispatches)
                   (signal 'org-note-http-error
                           '((:status 409 :code stale_revision)))))
                ((symbol-function 'org-note-operation-list-documents)
                 (lambda (&rest _)
                   (cl-incf lists)))
                ((symbol-function 'gsmlg-org-note-org--noncapture-marker-delete)
                 (lambda (_)
                   (cl-incf deletes)
                   (when (= deletes 1)
                     (error "Injected local cleanup failure")))))
        (should-error (gsmlg-org-note-org--archive-document))
        (should (eq (plist-get
                     (gethash "document-1"
                              gsmlg-org-note-org--archive-ambiguities)
                     :state)
                    'definitive-noncommit-pending-cleanup))
        (gsmlg-org-note-org-retry-archive-cleanup "document-1")
        (should (= dispatches 1))
        (should (= lists 0))
        (should (= deletes 2))))))

(ert-deftest gsmlg-org-note-org-archive-ambiguity-replays-exact-envelope ()
  "Ambiguous archive recovery reuses the exact frozen request and operation id."
  (gsmlg-org-note-org-archive-test--with-document
    (insert "source\n")
    (set-buffer-modified-p nil)
    (let (first replay)
      (cl-letf (((symbol-function 'org-note-client-new-operation-id)
                 (lambda () "archive-replay-1"))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (frozen)
                   (setq first frozen)
                   (signal 'org-note-transport-error '("uncertain")))))
        (should-error (gsmlg-org-note-org--archive-document)
                      :type 'org-note-transport-error))
      (should buffer-read-only)
      (cl-letf (((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (frozen)
                   (setq replay frozen)
                   '((revision . 5))))
                ((symbol-function 'org-note-operation-list-documents)
                 (lambda (&rest _)
                   '((documents . (((id . "document-1")
                                    (path . "notes/one.org")
                                    (revision . 5)
                                    (archived_at . 1)))))))
                ((symbol-function 'org-note-document--kill-buffer-safely)
                 #'ignore))
        (gsmlg-org-note-org-retry-ambiguous-archive "document-1"))
      (should (eq first replay)))))

(ert-deftest gsmlg-org-note-org-archive-cleanup-retry-never-resends ()
  "Committed archive cleanup retry performs no second POST or list request."
  (gsmlg-org-note-org-archive-test--with-document
    (insert "source\n")
    (set-buffer-modified-p nil)
    (let ((marker-delete
           (symbol-function 'gsmlg-org-note-org--noncapture-marker-delete))
          (deletes 0)
          (dispatches 0)
          (lists 0))
      (cl-letf (((symbol-function 'org-note-client-new-operation-id)
                 (lambda () "archive-cleanup-1"))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (_)
                   (cl-incf dispatches)
                   '((revision . 5))))
                ((symbol-function 'org-note-operation-list-documents)
                 (lambda (&rest _)
                   (cl-incf lists)
                   '((documents . (((id . "document-1")
                                    (path . "notes/one.org")
                                    (revision . 5)
                                    (archived_at . 1)))))))
                ((symbol-function 'org-note-document--kill-buffer-safely)
                 #'ignore)
                ((symbol-function 'gsmlg-org-note-org--noncapture-marker-delete)
                 (lambda (operation-id)
                   (cl-incf deletes)
                   (if (= deletes 1)
                       (error "Injected cleanup failure")
                     (funcall marker-delete operation-id)))))
        (should (equal '((revision . 5))
                       (gsmlg-org-note-org--archive-document)))
        (gsmlg-org-note-org-retry-archive-cleanup "document-1")
        (should (= dispatches 1))
        (should (= lists 1))
        (should-not (gethash "document-1"
                             gsmlg-org-note-org--archive-ambiguities))))))

(ert-deftest gsmlg-org-note-org-archive-divergence-requires-explicit-discard ()
  "Committed divergent text is cleaned only after explicit confirmation."
  (with-temp-buffer
    (let* ((gsmlg-org-note-org--archive-ambiguities
            (make-hash-table :test #'equal))
           (record (list :state 'committed-local-divergence
                         :document-id "document-1"))
           cleanup)
      (setq-local gsmlg-org-note-org--archive-recovery-state record)
      (setq buffer-read-only t)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'gsmlg-org-note-org--archive-cleanup-committed)
                 (lambda (resolved)
                   (setq cleanup resolved))))
        (gsmlg-org-note-org-discard-archived-buffer)
        (should (eq (plist-get cleanup :state) 'committed-pending-cleanup))
        (should (plist-get cleanup :origins-verified))
        (should-not buffer-read-only)
        (should-not gsmlg-org-note-org--archive-recovery-state)))))

(provide 'org-note-org-bridge-archive-test)
;;; org-note-org-bridge-archive-test.el ends here
