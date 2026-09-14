;;; org-note-org-bridge-clock-test.el --- Clock bridge tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'gsmlg-org-note-org)
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

(provide 'org-note-org-bridge-clock-test)
;;; org-note-org-bridge-clock-test.el ends here
