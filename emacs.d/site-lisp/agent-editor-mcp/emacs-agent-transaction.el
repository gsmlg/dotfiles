;;; emacs-agent-transaction.el --- Atomic runtime edits -*- lexical-binding: t; -*-

;;; Commentary:

;; Validate every document before activating coordinated buffer change groups.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'emacs-agent-changeset)
(require 'emacs-agent-document)
(require 'emacs-agent-project)
(require 'emacs-agent-request)
(require 'emacs-agent-runtime)
(require 'emacs-agent-transform)

(cl-defstruct (emacs-agent-transaction-item
               (:constructor emacs-agent-transaction-item--make))
  target path document expected-revision before after operation diff)

(cl-defstruct (emacs-agent-transaction-plan
               (:constructor emacs-agent-transaction-plan--make))
  runtime items)

(defun emacs-agent-transaction--field (object key)
  "Read KEY from OBJECT."
  (emacs-agent-document--field object key))

(defun emacs-agent-transaction--replace-in-string (text edit path)
  "Apply exact replacement EDIT to TEXT for PATH."
  (let* ((old (emacs-agent-transaction--field edit 'old_text))
         (new (emacs-agent-transaction--field edit 'new_text))
         (replace-all
          (eq (emacs-agent-transaction--field edit 'replace_all) t))
         (expected
          (emacs-agent-transaction--field edit 'expected_occurrences))
         matches)
    (unless (and (stringp old) (not (string-empty-p old))
                 (stringp new))
      (emacs-agent-signal 'invalid_argument :path path :field 'edits))
    (let ((start 0))
      (while (string-match (regexp-quote old) text start)
        (push (cons (match-beginning 0) (match-end 0)) matches)
        (setq start (match-end 0))))
    (setq matches (nreverse matches))
    (let ((count (length matches)))
      (when (zerop count)
        (emacs-agent-signal
         'expected_text_mismatch :path path :actual_occurrences 0))
      (when (and expected (/= expected count))
        (emacs-agent-signal
         'occurrence_count_mismatch :path path
         :expected_occurrences expected :actual_occurrences count))
      (when (and (not replace-all) (> count 1))
        (emacs-agent-signal
         'ambiguous_text_match :path path :actual_occurrences count))
      (with-temp-buffer
        (insert text)
        (dolist (match
                 (reverse (if replace-all matches (list (car matches)))))
          (delete-region (1+ (car match)) (1+ (cdr match)))
          (goto-char (1+ (car match)))
          (insert new))
        (buffer-string)))))

(defun emacs-agent-transaction--assert-text-result (path text)
  "Reject unsafe resulting TEXT for canonical PATH."
  (when (string-match-p (string 0) text)
    (emacs-agent-signal
     'unsupported_document_type :path path :reason 'binary))
  (when (> (string-bytes text)
           emacs-agent-policy-maximum-document-bytes)
    (emacs-agent-signal 'document_too_large :path path))
  text)

(defun emacs-agent-transaction--replace-plan
    (runtime target expected-revision edits)
  "Plan exact EDITS for TARGET in RUNTIME."
  (let* ((path (emacs-agent-resolved-target-canonical-path target))
         (document (emacs-agent-document-open runtime target))
         (_ (emacs-agent-document-reconcile document))
         (revision (emacs-agent-document-revision document))
         (before
          (with-current-buffer (emacs-agent-document-buffer document)
            (save-restriction
              (widen)
              (buffer-substring-no-properties
               (point-min) (point-max)))))
         (after before))
    (unless (equal revision expected-revision)
      (emacs-agent-signal
       'revision_conflict :path path
       :expected_revision expected-revision
       :current_revision revision :requires_reread t))
    (unless (and (listp edits) edits)
      (emacs-agent-signal 'invalid_argument :path path :field 'edits))
    (dolist (edit edits)
      (setq after
            (emacs-agent-transaction--replace-in-string
             after edit path)))
    (emacs-agent-transaction--assert-text-result path after)
    (emacs-agent-transaction-item--make
     :target target
     :path path
     :document document :expected-revision revision
     :before before :after after :operation 'replace
     :diff (emacs-agent-changeset--diff-text path before after))))

;;;###autoload
(defun emacs-agent-transaction-plan (runtime documents)
  "Validate and plan atomic DOCUMENTS edits in RUNTIME."
  (unless (emacs-agent-runtime-p runtime)
    (emacs-agent-signal 'runtime_not_started))
  (unless (and (listp documents) documents)
    (emacs-agent-signal 'invalid_argument :field 'documents))
  (let (paths items)
    (dolist (entry documents)
      (let* ((path (emacs-agent-transaction--field entry 'path))
             (raw-project-id
              (emacs-agent-transaction--field entry 'project_id))
             (project-id
              (and (stringp raw-project-id) raw-project-id))
             (revision
              (emacs-agent-transaction--field entry 'expected_revision))
             (patch (emacs-agent-transaction--field entry 'patch))
             (edits (emacs-agent-transaction--field entry 'edits))
             item)
        (unless (and (stringp path) (stringp revision))
          (emacs-agent-signal 'invalid_argument :field 'documents))
        (when (and raw-project-id
                   (not (or (stringp raw-project-id)
                            (eq raw-project-id :false))))
          (emacs-agent-signal
           'invalid_argument :field 'project_id))
        (let* ((target
                (emacs-agent-project-resolve-target
                 runtime path :project-id project-id))
               (canonical
                (emacs-agent-resolved-target-canonical-path target)))
          (when (member canonical paths)
            (emacs-agent-signal 'invalid_argument
                                :path canonical
                                :reason 'duplicate_document))
          (push canonical paths)
          (setq
           item
           (if patch
               (let ((plan
                      (emacs-agent-transform-plan-patch
                       runtime target revision patch)))
                 (emacs-agent-transaction--assert-text-result
                  canonical
                  (emacs-agent-transform-plan-after plan))
                 (emacs-agent-transaction-item--make
                  :target target
                  :path (emacs-agent-transform-plan-path plan)
                  :document (emacs-agent-transform-plan-document plan)
                  :expected-revision revision
                  :before (emacs-agent-transform-plan-before plan)
                  :after (emacs-agent-transform-plan-after plan)
                  :operation 'patch
                  :diff (emacs-agent-transform-plan-diff plan)))
             (emacs-agent-transaction--replace-plan
              runtime target revision edits)))
          (push item items))))
    (emacs-agent-transaction-plan--make
     :runtime runtime :items (nreverse items))))

(defun emacs-agent-transaction--validate-current (item)
  "Revalidate transaction ITEM immediately before mutation."
  (let* ((document (emacs-agent-transaction-item-document item))
         (_
          (emacs-agent-edit--revalidate-target
           (emacs-agent-document-runtime document)
           (emacs-agent-transaction-item-target item)))
         (_ (emacs-agent-document-reconcile document))
         (revision (emacs-agent-document-revision document))
         (content
          (with-current-buffer (emacs-agent-document-buffer document)
            (save-restriction
              (widen)
              (buffer-substring-no-properties
               (point-min) (point-max))))))
    (unless (and
             (equal revision
                    (emacs-agent-transaction-item-expected-revision item))
             (equal content (emacs-agent-transaction-item-before item)))
      (emacs-agent-signal
       'revision_conflict
       :path (emacs-agent-transaction-item-path item)
       :expected_revision
       (emacs-agent-transaction-item-expected-revision item)
       :current_revision revision :requires_reread t))
    (with-current-buffer (emacs-agent-document-buffer document)
      (when buffer-read-only
        (emacs-agent-signal
         'path_denied :path (emacs-agent-transaction-item-path item)
         :reason 'read-only)))
    item))

(defun emacs-agent-transaction--item-modified-p (item)
  "Return non-nil when transaction ITEM would change document content."
  (not
   (equal
    (emacs-agent-transaction-item-before item)
    (emacs-agent-transaction-item-after item))))

(defun emacs-agent-transaction--item-current-content (item)
  "Return the authoritative current text for transaction ITEM."
  (emacs-agent-document--buffer-content
   (emacs-agent-document-buffer
    (emacs-agent-transaction-item-document item))))

(defun emacs-agent-transaction--item-effective-modified-p (item)
  "Return non-nil when ITEM's current text differs from its before-image."
  (not
   (equal
    (emacs-agent-transaction-item-before item)
    (emacs-agent-transaction--item-current-content item))))

(defun emacs-agent-transaction--preview-result (plan)
  "Return dry-run result for PLAN."
  (let ((diff
         (mapconcat
          #'emacs-agent-transaction-item-diff
          (emacs-agent-transaction-plan-items plan) "")))
    (list
     :old_revision nil :new_revision nil
     :applied nil :checkpointed nil
     :modified
     (seq-some
      #'emacs-agent-transaction--item-modified-p
      (emacs-agent-transaction-plan-items plan))
     :changeset_id nil
     :diff diff :truncated nil :diff_truncated nil
     :documents
     (mapcar
      (lambda (item)
        (append
         (emacs-agent-document-output-fields
          (emacs-agent-transaction-item-target item))
         (list
          :old_revision
          (emacs-agent-transaction-item-expected-revision item)
          :new_revision
          (emacs-agent-transaction-item-expected-revision item)
          :applied nil :modified
          (emacs-agent-transaction--item-modified-p item)
          :checkpointed nil
          :diff (emacs-agent-transaction-item-diff item)
          :truncated nil :diff_truncated nil)))
      (emacs-agent-transaction-plan-items plan)))))

(defun emacs-agent-transaction--record
    (plan checkpointed request-context)
  "Record applied PLAN with CHECKPOINTED state and REQUEST-CONTEXT."
  (let* ((runtime (emacs-agent-transaction-plan-runtime plan))
         (items (emacs-agent-transaction-plan-items plan))
         (changeset
          (emacs-agent-changeset-record
           runtime
           :request-id
           (and request-context
                (emacs-agent-request-id request-context))
           :agent-identity
           (and request-context
                (emacs-agent-request-client-info request-context))
           :operations
           (mapcar
            (lambda (item)
              (list :type (emacs-agent-transaction-item-operation item)
                    :path (emacs-agent-transaction-item-path item)))
            items)
           :touched-documents
           (mapcar #'emacs-agent-transaction-item-path items)
           :base-revisions
           (mapcar
            (lambda (item)
              (cons
               (emacs-agent-transaction-item-path item)
               (emacs-agent-transaction-item-expected-revision item)))
            items)
           :final-revisions
           (mapcar
            (lambda (item)
              (cons
               (emacs-agent-transaction-item-path item)
               (emacs-agent-document-revision
                (emacs-agent-transaction-item-document item))))
            items)
           :before-snapshots
           (mapcar
            (lambda (item)
              (cons
               (emacs-agent-transaction-item-path item)
               (list :exists t
                     :content
                     (emacs-agent-transaction-item-before item))))
            items)
           :checkpoint-state (and checkpointed 'checkpointed)
           :unified-diff
           (mapconcat
            (lambda (item)
              (emacs-agent-changeset--diff-text
               (emacs-agent-transaction-item-path item)
               (emacs-agent-transaction-item-before item)
               (with-current-buffer
                   (emacs-agent-document-buffer
                    (emacs-agent-transaction-item-document item))
                 (save-restriction
                   (widen)
                   (buffer-substring-no-properties
                    (point-min) (point-max))))))
            items ""))))
    changeset))

;;;###autoload
(defun emacs-agent-transaction-apply
    (plan &optional dry-run checkpoint request-context)
  "Apply PLAN atomically across buffers.
DRY-RUN only validates.  CHECKPOINT saves after the buffer transaction.
REQUEST-CONTEXT attributes the resulting runtime change set."
  (unless (emacs-agent-transaction-plan-p plan)
    (signal 'wrong-type-argument
            (list 'emacs-agent-transaction-plan plan)))
  (let* ((items (emacs-agent-transaction-plan-items plan))
         (planned-changed-items
          (seq-filter
           #'emacs-agent-transaction--item-modified-p items)))
    (dolist (item items)
      (emacs-agent-transaction--validate-current item))
    (if dry-run
        (emacs-agent-transaction--preview-result plan)
      (let ((runtime (emacs-agent-transaction-plan-runtime plan)))
        (emacs-agent-runtime-enqueue-mutation
         runtime
         (lambda ()
           (let (groups checkpoint-error checkpointed-paths changeset
                        changeset-id checkpointing-item
                        effective-changed-items)
             (condition-case error-data
                 (progn
                   (dolist (item items)
                     (emacs-agent-transaction--validate-current item))
                   (dolist (item planned-changed-items)
                     (let ((buffer
                            (emacs-agent-document-buffer
                             (emacs-agent-transaction-item-document item))))
                       (with-current-buffer buffer
                         (let ((group (prepare-change-group)))
                           (activate-change-group group)
                           (push (cons buffer group) groups)
                           (save-restriction
                             (widen)
                             (erase-buffer)
                             (insert
                              (emacs-agent-transaction-item-after
                               item)))))))
                   (dolist (entry groups)
                     (with-current-buffer (car entry)
                       (accept-change-group (cdr entry)))))
               (error
                (dolist (entry groups)
                  (when (buffer-live-p (car entry))
                    (with-current-buffer (car entry)
                      (ignore-errors
                        (cancel-change-group (cdr entry))))))
                (signal (car error-data) (cdr error-data))))
             (when checkpoint
               (condition-case error-data
                   (dolist (item items)
                     (setq checkpointing-item item)
                     (let ((document
                            (emacs-agent-transaction-item-document item)))
                       (emacs-agent-edit--revalidate-target
                        runtime
                        (emacs-agent-transaction-item-target item))
                       (emacs-agent-document-checkpoint document)
                       (push (emacs-agent-transaction-item-path item)
                             checkpointed-paths)))
                 (error
                  (when checkpointing-item
                    (setf
                     (emacs-agent-document-degraded
                      (emacs-agent-transaction-item-document
                       checkpointing-item))
                     t))
                  (setq checkpointed-paths
                        (nreverse checkpointed-paths))
                  (let ((details
                         (and (eq (car error-data) 'emacs-agent-error)
                              (emacs-agent-error-details error-data))))
                    (setq checkpoint-error
                          (append
                           (list
                            :code 'checkpoint_failed
                            :message (error-message-string error-data)
                            :checkpointed_paths
                            (copy-sequence checkpointed-paths))
                           (when details
                             (list
                              :path (plist-get details :path)
                              :partial_completion
                              (and
                               (or checkpointed-paths
                                   (plist-get
                                    details :partial_completion))
                               t)
                              :reconciliation_required
                              (and
                               (plist-get
                                details :reconciliation_required)
                               t))))))
                  (setf (emacs-agent-runtime-health-state runtime)
                        'degraded))))
             (setq effective-changed-items
                   (seq-filter
                    #'emacs-agent-transaction--item-effective-modified-p
                    items))
             (when effective-changed-items
               (setq changeset
                     (emacs-agent-transaction--record
                      (emacs-agent-transaction-plan--make
                       :runtime runtime :items effective-changed-items)
                      (and checkpoint (not checkpoint-error))
                      request-context))
               (setq changeset-id
                     (emacs-agent-changeset-changeset-id changeset)))
             (list
              :old_revision nil
              :new_revision nil
              :applied t
              :checkpointed
              (and checkpoint items (not checkpoint-error) t)
              :modified (and effective-changed-items t)
              :changeset_id changeset-id
              :diff
              (if changeset-id
                  (emacs-agent-changeset-diff runtime changeset-id)
                "")
              :truncated nil
              :diff_truncated nil
              :checkpoint_error checkpoint-error
              :documents
              (mapcar
               (lambda (item)
                 (let ((document
                        (emacs-agent-transaction-item-document item)))
                   (append
                    (emacs-agent-document-output-fields
                     (emacs-agent-transaction-item-target item))
                    (list
                     :old_revision
                     (emacs-agent-transaction-item-expected-revision item)
                     :new_revision
                     (emacs-agent-document-revision document)
                     :applied t
                     :checkpointed
                     (and checkpoint
                          (member
                           (emacs-agent-transaction-item-path item)
                           checkpointed-paths)
                          t)
                     :modified
                     (emacs-agent-transaction--item-effective-modified-p
                      item)
                     :diff
                     (emacs-agent-changeset--diff-text
                      (emacs-agent-transaction-item-path item)
                      (emacs-agent-transaction-item-before item)
                      (with-current-buffer
                          (emacs-agent-document-buffer document)
                        (save-restriction
                          (widen)
                          (buffer-substring-no-properties
                           (point-min) (point-max)))))
                     :truncated nil :diff_truncated nil))))
               items)))))))))

(provide 'emacs-agent-transaction)
;;; emacs-agent-transaction.el ends here
