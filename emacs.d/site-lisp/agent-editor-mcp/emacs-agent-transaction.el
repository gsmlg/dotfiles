;;; emacs-agent-transaction.el --- Atomic workspace edits -*- lexical-binding: t; -*-

;;; Commentary:

;; Validate every document before activating coordinated buffer change groups.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'emacs-agent-changeset)
(require 'emacs-agent-document)
(require 'emacs-agent-request)
(require 'emacs-agent-transform)
(require 'emacs-agent-workspace)

(cl-defstruct (emacs-agent-transaction-item
               (:constructor emacs-agent-transaction-item--make))
  path document expected-revision before after operation diff)

(cl-defstruct (emacs-agent-transaction-plan
               (:constructor emacs-agent-transaction-plan--make))
  workspace items)

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

(defun emacs-agent-transaction--replace-plan
    (workspace path expected-revision edits)
  "Plan exact EDITS for PATH in WORKSPACE."
  (let* ((document (emacs-agent-document-open workspace path))
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
    (emacs-agent-transaction-item--make
     :path (emacs-agent-document-relative-path document)
     :document document :expected-revision revision
     :before before :after after :operation 'replace
     :diff (emacs-agent-changeset--diff-text path before after))))

;;;###autoload
(defun emacs-agent-transaction-plan (workspace documents)
  "Validate and plan atomic DOCUMENTS edits in WORKSPACE."
  (unless (and (listp documents) documents)
    (emacs-agent-signal 'invalid_argument :field 'documents))
  (let (paths items)
    (dolist (entry documents)
      (let* ((path (emacs-agent-transaction--field entry 'path))
             (revision
              (emacs-agent-transaction--field entry 'expected_revision))
             (patch (emacs-agent-transaction--field entry 'patch))
             (edits (emacs-agent-transaction--field entry 'edits))
             item)
        (unless (and (stringp path) (stringp revision))
          (emacs-agent-signal 'invalid_argument :field 'documents))
        (when (member path paths)
          (emacs-agent-signal 'invalid_argument
                              :path path :reason 'duplicate_document))
        (push path paths)
        (setq
         item
         (if patch
             (let ((plan
                    (emacs-agent-transform-plan-patch
                     workspace path revision patch)))
               (emacs-agent-transaction-item--make
                :path (emacs-agent-transform-plan-path plan)
                :document (emacs-agent-transform-plan-document plan)
                :expected-revision revision
                :before (emacs-agent-transform-plan-before plan)
                :after (emacs-agent-transform-plan-after plan)
                :operation 'patch
                :diff (emacs-agent-transform-plan-diff plan)))
           (emacs-agent-transaction--replace-plan
            workspace path revision edits)))
        (push item items)))
    (emacs-agent-transaction-plan--make
     :workspace workspace :items (nreverse items))))

(defun emacs-agent-transaction--validate-current (item)
  "Revalidate transaction ITEM immediately before mutation."
  (let* ((document (emacs-agent-transaction-item-document item))
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
    (lambda (item)
      (not (equal (emacs-agent-transaction-item-before item)
                  (emacs-agent-transaction-item-after item))))
    (emacs-agent-transaction-plan-items plan))
   :changeset_id nil
   :diff diff :truncated nil :diff_truncated nil
   :documents
   (mapcar
    (lambda (item)
      (list
       :path (emacs-agent-transaction-item-path item)
       :old_revision
       (emacs-agent-transaction-item-expected-revision item)
       :new_revision
       (emacs-agent-transaction-item-expected-revision item)
       :applied nil :modified
       (not (equal (emacs-agent-transaction-item-before item)
                   (emacs-agent-transaction-item-after item)))
       :checkpointed nil
       :diff (emacs-agent-transaction-item-diff item)
       :truncated nil :diff_truncated nil))
    (emacs-agent-transaction-plan-items plan)))))

(defun emacs-agent-transaction--record
    (plan checkpointed request-context)
  "Record applied PLAN with CHECKPOINTED state and REQUEST-CONTEXT."
  (let* ((workspace (emacs-agent-transaction-plan-workspace plan))
         (items (emacs-agent-transaction-plan-items plan))
         (changeset
          (emacs-agent-changeset-record
           workspace
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
REQUEST-CONTEXT attributes the resulting workspace change set."
  (unless (emacs-agent-transaction-plan-p plan)
    (signal 'wrong-type-argument
            (list 'emacs-agent-transaction-plan plan)))
  (dolist (item (emacs-agent-transaction-plan-items plan))
    (emacs-agent-transaction--validate-current item))
  (if dry-run
      (emacs-agent-transaction--preview-result plan)
    (let ((workspace (emacs-agent-transaction-plan-workspace plan)))
      (emacs-agent-workspace-enqueue-mutation
       workspace
       (lambda ()
         (let (groups checkpoint-error checkpointed-paths changeset
                     checkpointing-item)
           (condition-case error-data
               (progn
                 (dolist (item (emacs-agent-transaction-plan-items plan))
                   (emacs-agent-transaction--validate-current item)
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
                            (emacs-agent-transaction-item-after item)))))))
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
                 (dolist (item
                          (emacs-agent-transaction-plan-items plan))
                   (setq checkpointing-item item)
                   (let ((document
                          (emacs-agent-transaction-item-document item)))
                     (with-current-buffer
                         (emacs-agent-document-buffer document)
                       (save-buffer))
                     (setf
                      (emacs-agent-document-disk-fingerprint document)
                      (emacs-agent-document--disk-fingerprint
                       (emacs-agent-document-canonical-path document)))
                     (push (emacs-agent-transaction-item-path item)
                           checkpointed-paths)))
               (error
                (when checkpointing-item
                  (setf
                   (emacs-agent-document-degraded
                    (emacs-agent-transaction-item-document
                     checkpointing-item))
                   t))
                (setq checkpoint-error
                      (list
                       :code 'checkpoint_failed
                       :message (error-message-string error-data)
                       :checkpointed_paths
                       (nreverse checkpointed-paths)))
                (setf (emacs-agent-workspace-health-state workspace)
                      'degraded))))
           (setq changeset
                 (emacs-agent-transaction--record
                  plan (and checkpoint (not checkpoint-error))
                  request-context))
           (list
            :old_revision nil
            :new_revision nil
            :applied t
            :checkpointed
            (and checkpoint (not checkpoint-error))
            :modified t
            :changeset_id
            (emacs-agent-changeset-changeset-id changeset)
            :diff
            (emacs-agent-changeset-diff
             workspace
             (emacs-agent-changeset-changeset-id changeset))
            :truncated nil
            :diff_truncated nil
            :checkpoint_error checkpoint-error
            :documents
            (mapcar
             (lambda (item)
               (let ((document
                      (emacs-agent-transaction-item-document item)))
                 (list
                  :path (emacs-agent-transaction-item-path item)
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
                  (not
                   (equal
                    (emacs-agent-transaction-item-before item)
                    (emacs-agent-transaction-item-after item)))
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
                  :truncated nil :diff_truncated nil)))
             (emacs-agent-transaction-plan-items plan)))))))))

(provide 'emacs-agent-transaction)
;;; emacs-agent-transaction.el ends here
