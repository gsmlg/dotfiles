;;; org-note-org-bridge-clock-test.el --- Clock bridge tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'gsmlg-org-note-org)
(require 'org-clock)
(require 'org-note-operation)

(defun gsmlg-org-note-clock-test--claim-response (revision)
  "Return a structurally valid claim response at REVISION."
  `((schema_version . 1) (actor_id . "actor") (workspace_id . "ws")
    (operation_id . "op") (lease_id . "lease")
    (fencing_token . "token") (expires_at . 4102444800)
    (event_ids . ["event"])
    (context . ((workspace . ((id . "ws") (workspace_revision . 1)))
                (document . ((id . "doc") (path . "x.org") (revision . ,revision)))
                (item . ((id . "item") (workspace_id . "ws") (document_id . "doc")))
                (lease . ((id . "lease") (workspace_id . "ws")
                          (work_item_id . "item") (attempt_id . "attempt")
                          (kind . "execution") (actor_id . "actor")
                          (acquired_at . 1) (last_heartbeat_at . 1)
                          (expires_at . 4102444800) (status . "active")))))))

(ert-deftest gsmlg-org-note-clock-claim-validates-revision ()
  "A claim response older than preflight is rejected."
  (let ((org-note-actor-id "actor"))
    (should-error
     (gsmlg-org-note-org--clock-claim-response-validator
      (gsmlg-org-note-clock-test--claim-response 2)
      "ws" "item" "doc" 3 "execution" "op")
     :type 'org-note-error)))

(ert-deftest gsmlg-org-note-clock-claim-ambiguity-replays-frozen-wire ()
  "An ambiguous claim can be replayed with its original operation id."
  (let ((gsmlg-org-note-org--clock-ambiguities (make-hash-table :test #'equal))
        (gsmlg-org-note-org--clock-presentation nil)
        (gsmlg-org-note-org--clock-mode-line-installed nil)
        (global-mode-string nil)
        (org-note-actor-id "actor")
        (calls 0))
    (puthash "op" (list :action 'claim :operation-id "op" :workspace-id "ws"
                         :item-id "item" :document-id "doc" :expected-revision 1
                         :kind "execution" :frozen '(:body "wire"))
             gsmlg-org-note-org--clock-ambiguities)
    (cl-letf (((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (wire) (cl-incf calls) (should (equal wire '(:body "wire")))
                 (gsmlg-org-note-clock-test--claim-response 1)))
              ((symbol-function 'org-note-operation--validate-claim-response)
               (lambda (&rest _) t))
              ((symbol-function 'org-note-operation-register-claim)
               (lambda (&rest _) 'lease)))
      (gsmlg-org-note-org-retry-ambiguous-clock "op")
      (should (= calls 1))
      (should-not (gethash "op" gsmlg-org-note-org--clock-ambiguities))
      (should (equal (plist-get gsmlg-org-note-org--clock-presentation :item-id)
                     "item")))))

(ert-deftest gsmlg-org-note-clock-unresolved-attempt-blocks-new-clock-in ()
  "Any unresolved clock attempt blocks a later clock mutation."
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (gsmlg-org-note-org--clock-ambiguities (make-hash-table :test #'equal)))
    (puthash "pending" '(:action release) gsmlg-org-note-org--clock-ambiguities)
    (should-error
     (gsmlg-org-note-org--around-clock-in (lambda (&rest _) 'native))
     :type 'user-error)))

(ert-deftest gsmlg-org-note-agenda-clock-in-routes-marker-free-row ()
  "Agenda clock-in routes the current identified row without native markers."
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (gsmlg-org-note-org--clock-presentation nil)
        (gsmlg-org-note-org--clock-ambiguities (make-hash-table :test #'equal))
        native-called routed)
    (cl-letf (((symbol-function 'gsmlg-org-note-org--agenda-feed-buffer-p)
               (lambda () t))
              ((symbol-function 'gsmlg-org-note-org--agenda-row-origin)
               (lambda () '(:workspace-id "ws" :item-id "item" :state "TODO")))
              ((symbol-function 'gsmlg-org-note-org--clock-in)
               (lambda (ids) (setq routed ids))))
      (gsmlg-org-note-org--around-agenda-clock-in
       (lambda (&rest _) (setq native-called t)))
      (should (equal routed '("ws" . "item")))
      (should-not native-called))))

(ert-deftest gsmlg-org-note-agenda-clock-in-refuses-missing-provenance ()
  "Agenda clock-in rejects incomplete provenance before clock dispatch."
  (let ((gsmlg-org-note-org-enable t) clock-called native-called)
    (cl-letf (((symbol-function 'gsmlg-org-note-org--agenda-feed-buffer-p)
               (lambda () t))
              ((symbol-function 'gsmlg-org-note-org--agenda-row-origin)
               (lambda () '(:workspace-id "ws" :state "TODO")))
              ((symbol-function 'gsmlg-org-note-org--clock-in)
               (lambda (&rest _) (setq clock-called t))))
      (should-error
       (gsmlg-org-note-org--around-agenda-clock-in
        (lambda (&rest _) (setq native-called t)))
       :type 'user-error)
      (should-not clock-called)
      (should-not native-called))))

(defun gsmlg-org-note-clock-test--lease (&optional lease-id)
  "Return a bridge test lease with optional LEASE-ID."
  (org-note-operation--make-lease
   :workspace-id "ws" :item-id "item" :document-id "doc"
   :kind "execution" :lease-id (or lease-id "lease")
   :fencing-token "token" :expires-at 4102444800))

(defun gsmlg-org-note-clock-test--presentation (&optional lease-id)
  "Return an active bridge presentation with optional LEASE-ID."
  (list :workspace-id "ws" :item-id "item" :document-id "doc"
        :kind "execution" :title "Task" :started-at 1
        :lease (gsmlg-org-note-clock-test--lease lease-id)))

(ert-deftest gsmlg-org-note-clock-out-uses-global-presentation-identity ()
  "Clock-out from a non-Org buffer uses only the active presentation ids."
  (let* ((gsmlg-org-note-org-enable t)
         (gsmlg-org-note-org--activated t)
         (gsmlg-org-note-org--clock-ambiguities (make-hash-table :test #'equal))
         (gsmlg-org-note-org--clock-mode-line-installed nil)
         (global-mode-string nil)
         (lease (gsmlg-org-note-clock-test--lease))
         (gsmlg-org-note-org--clock-presentation
          (gsmlg-org-note-clock-test--presentation))
         built dispatched)
    (cl-letf (((symbol-function 'gsmlg-org-note-org--clock-reconcile)
               (lambda () gsmlg-org-note-org--clock-presentation))
              ((symbol-function 'org-note-operation-find-lease)
               (lambda (workspace item kind)
                 (should (equal (list workspace item kind)
                                '("ws" "item" "execution")))
                 lease))
              ((symbol-function 'gsmlg-org-note-org--preflight-identified-item)
               (lambda (workspace item)
                 (should (equal (cons workspace item) '("ws" . "item")))
                 '(:document-id "doc" :revision 7 :state "running")))
              ((symbol-function 'org-note-client-new-operation-id)
               (lambda () "release-op"))
              ((symbol-function 'org-note-operation--build-frozen-release)
               (lambda (&rest args) (setq built args) 'release-record))
              ((symbol-function 'org-note-operation--dispatch-frozen-release)
               (lambda (record) (setq dispatched record) 'post-result)))
      (with-temp-buffer
        (should (eq (gsmlg-org-note-org--around-clock-out
                     (lambda (&rest _) 'native))
                    'post-result)))
      (should (equal (cl-subseq built 0 7)
                     '("ws" "item" "doc" 7 "lease" "execution" "token")))
      (should (eq dispatched 'release-record))
      (should-not gsmlg-org-note-org--clock-presentation))))

(ert-deftest gsmlg-org-note-clock-reconcile-clears-replaced-lease ()
  "A replaced registered lease clears, rather than adopts, the presentation."
  (let ((gsmlg-org-note-org--clock-presentation
         (gsmlg-org-note-clock-test--presentation "original")))
    (cl-letf (((symbol-function 'org-note-operation-find-lease)
               (lambda (&rest _) (gsmlg-org-note-clock-test--lease "replacement"))))
      (should-error (gsmlg-org-note-org--clock-reconcile) :type 'user-error)
      (should-not gsmlg-org-note-org--clock-presentation))))

(ert-deftest gsmlg-org-note-clock-display-and-menu-reconcile-first ()
  "Display and menu clear stale presentation before rendering or prompting."
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--clock-presentation
         (gsmlg-org-note-clock-test--presentation))
        (gsmlg-org-note-org--clock-mode-line-installed nil)
        prompted)
    (cl-letf (((symbol-function 'org-note-operation-find-lease)
               (lambda (&rest _) nil))
              ((symbol-function 'read-multiple-choice)
               (lambda (&rest _) (setq prompted t))))
      (should-not (gsmlg-org-note-org--clock-display))
      (should-not gsmlg-org-note-org--clock-presentation)
      (setq gsmlg-org-note-org--clock-presentation
            (gsmlg-org-note-clock-test--presentation))
      (should-error
       (gsmlg-org-note-org--around-clock-menu (lambda () 'native))
       :type 'user-error)
      (should-not prompted)
      (should-not gsmlg-org-note-org--clock-presentation))))

(ert-deftest gsmlg-org-note-clock-display-preserves-unrelated-mode-line-state ()
  "Bridge presentation owns one segment and preserves existing mode-line state."
  (let ((global-mode-string '(unrelated-segment))
        (org-mode-line-string 'native-clock-state)
        (gsmlg-org-note-org--clock-mode-line-installed nil)
        (gsmlg-org-note-org--clock-presentation nil))
    (gsmlg-org-note-org--clock-register-presentation
     '(:workspace-id "ws" :item-id "item" :document-id "doc"
       :kind "execution" :operation-id "op" :title "Task" :started-at 1)
     'response (gsmlg-org-note-clock-test--lease))
    (should (equal global-mode-string
                   (list 'unrelated-segment
                         gsmlg-org-note-org--clock-mode-line-segment)))
    (should (eq org-mode-line-string 'native-clock-state))
    (gsmlg-org-note-org--clock-clear-presentation)
    (should (equal global-mode-string '(unrelated-segment)))
    (should (eq org-mode-line-string 'native-clock-state))))

(ert-deftest gsmlg-org-note-clock-in-refuses-native-and-second-clocks ()
  "Clock-in refuses legacy native state and a second bridge clock."
  (let ((gsmlg-org-note-org--clock-ambiguities (make-hash-table :test #'equal))
        (org-clock-current-task "native"))
    (should-error (gsmlg-org-note-org--clock-in '("ws" . "item"))
                  :type 'user-error))
  (let ((gsmlg-org-note-org--clock-ambiguities (make-hash-table :test #'equal))
        (org-clock-current-task nil)
        (org-clock-marker nil)
        (gsmlg-org-note-org--clock-presentation
         (gsmlg-org-note-clock-test--presentation)))
    (cl-letf (((symbol-function 'gsmlg-org-note-org--clock-reconcile)
               (lambda () gsmlg-org-note-org--clock-presentation)))
      (should-error (gsmlg-org-note-org--clock-in '("ws" . "other"))
                    :type 'user-error))))

(ert-deftest gsmlg-org-note-clock-release-recovery-clears-pending-state ()
  "Exact release recovery clears ambiguity and presentation after validation."
  (let ((gsmlg-org-note-org--clock-ambiguities (make-hash-table :test #'equal))
        (gsmlg-org-note-org--clock-presentation
         (gsmlg-org-note-clock-test--presentation))
        dispatched)
    (puthash "release-op"
             '(:action release :operation-id "release-op"
               :release-record frozen-release)
             gsmlg-org-note-org--clock-ambiguities)
    (cl-letf (((symbol-function 'org-note-operation--dispatch-frozen-release)
               (lambda (record) (setq dispatched record) 'post-result)))
      (gsmlg-org-note-org-retry-ambiguous-clock "release-op")
      (should (eq dispatched 'frozen-release))
      (should-not (gethash "release-op" gsmlg-org-note-org--clock-ambiguities))
      (should-not gsmlg-org-note-org--clock-presentation))))

(ert-deftest gsmlg-org-note-clock-release-recovery-reuses-pending-post ()
  "Bridge recovery reconciles a committed release without another POST."
  (let* ((org-note-endpoint "https://a.example")
         (gsmlg-org-note-org-enable t)
         (org-note-actor-id "actor")
         (org-note-operation--leases (make-hash-table :test #'equal))
         (org-note-operation--pending-releases (make-hash-table :test #'equal))
         (gsmlg-org-note-org--clock-ambiguities (make-hash-table :test #'equal))
         (lease (gsmlg-org-note-clock-test--lease))
         (gsmlg-org-note-org--clock-presentation
          (list :workspace-id "ws" :item-id "item" :document-id "doc"
                :kind "execution" :title "Task" :started-at 1 :lease lease))
         (post-result '((released . t)))
         (post-count 0)
         (get-count 0))
    (puthash (org-note-operation--lease-key "ws" "item" "execution")
             lease org-note-operation--leases)
    (cl-letf (((symbol-function 'org-note-client-new-operation-id)
               (lambda () "release-op"))
              ((symbol-function 'gsmlg-org-note-org--preflight-identified-item)
               (lambda (_workspace-id _item-id)
                 (setq org-note-endpoint "https://b.example")
                 '(:document-id "doc" :revision 3 :state "running")))
              ((symbol-function 'org-note-client-request-raw)
               (lambda (&rest args)
                 (if (equal (plist-get args :method) "POST")
                     (progn
                       (cl-incf post-count)
                       (should (string-prefix-p "https://a.example/"
                                                (plist-get args :url)))
                       post-result)
                   (cl-incf get-count)
                   (if (= get-count 1)
                       (signal 'org-note-transport-error '("GET failed"))
                     '((workspace . ((id . "ws")))
                       (workspace_revision . 4)
                       (document . ((id . "doc") (path . "x.org")
                                    (revision . 3)))
                       (item . ((id . "item") (workspace_id . "ws")
                                (document_id . "doc") (state . "running")))
                       (lease . nil)))))))
      (should-error
       (gsmlg-org-note-org--around-clock-out (lambda (&rest _) 'native))
       :type 'org-note-transport-error)
      (should (eq (gsmlg-org-note-org-retry-ambiguous-clock "release-op")
                  post-result))
      (should (= post-count 1))
      (should (= get-count 2))
      (should-not (gethash "release-op" gsmlg-org-note-org--clock-ambiguities))
      (should-not gsmlg-org-note-org--clock-presentation))))

(ert-deftest gsmlg-org-note-clock-menu-limits-bridge-actions ()
  "The bridge clock menu dispatches only its three supported actions."
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--clock-presentation
         (gsmlg-org-note-clock-test--presentation))
        choices called)
    (cl-letf (((symbol-function 'gsmlg-org-note-org--clock-reconcile)
               (lambda () gsmlg-org-note-org--clock-presentation))
              ((symbol-function 'read-multiple-choice)
               (lambda (_prompt offered) (setq choices offered) '(?g "goto")))
              ((symbol-function 'call-interactively)
               (lambda (command) (setq called command))))
      (gsmlg-org-note-org--around-clock-menu (lambda () 'native))
      (should (equal (mapcar #'car choices) '(?g ?o ?c)))
      (should (eq called #'org-clock-goto)))))

(ert-deftest gsmlg-org-note-pomodoro-refuses-bridge-agenda ()
  "Pomodoro never starts native clocking from a bridge Agenda."
  (let ((gsmlg-org-note-org-enable t) native-called)
    (cl-letf (((symbol-function 'gsmlg-org-note-org--agenda-feed-buffer-p)
               (lambda () t)))
      (should-error
       (gsmlg-org-note-org--around-pomodoro
        (lambda (&rest _) (setq native-called t)))
       :type 'user-error)
      (should-not native-called))))

(provide 'org-note-org-bridge-clock-test)
;;; org-note-org-bridge-clock-test.el ends here
