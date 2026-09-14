;;; org-note-org-bridge-save-test.el --- Bridge document save tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org-note)
(require 'gsmlg-paths)
(require 'gsmlg-org-note-org)

(defmacro gsmlg-org-note-org-save-test--with-buffer (&rest body)
  "Evaluate BODY in a bridge-enabled disposable document buffer."
  (declare (indent 0) (debug t))
  `(let ((gsmlg-org-note-org-enable t)
         (gsmlg-org-note-org--activated t)
         (gsmlg-state-directory (make-temp-file "org-note-save-state-" t))
         (gsmlg-org-note-org--document-ambiguities
          (make-hash-table :test #'equal)))
     (unwind-protect
         (with-temp-buffer
           (org-note-document-mode)
          (setq-local org-note-document-workspace-id "workspace-1"
                       org-note-document-id "document-1"
                       org-note-document-path "notes/one.org"
                       org-note-document-revision 4
                       org-note-document-base-source "old\n")
          (insert "new\n")
          ,@body)
       (delete-directory gsmlg-state-directory t))))

(ert-deftest gsmlg-org-note-org-save-uses-frozen-attempt ()
  "A bridge-enabled save dispatches one frozen request and cleans its marker."
  (gsmlg-org-note-org-save-test--with-buffer
    (let ((installed-p
           (advice-member-p #'gsmlg-org-note-org--around-document-save-remote
                            #'org-note-document--save-remote))
          (marker-write
           (symbol-function 'gsmlg-org-note-org--noncapture-marker-write))
          dispatched states)
      (unless installed-p
        (advice-add #'org-note-document--save-remote :around
                    #'gsmlg-org-note-org--around-document-save-remote))
      (unwind-protect
          (cl-letf (((symbol-function 'org-note-operation-put-document)
                     (lambda (&rest _)
                       (ert-fail "legacy PUT path was called")))
                    ((symbol-function 'org-note-operation-lease-proofs)
                     (lambda (_) nil))
                    ((symbol-function 'org-note-client-new-operation-id)
                     (lambda () "save-operation-1"))
                    ((symbol-function
                      'gsmlg-org-note-org--noncapture-marker-write)
                     (lambda (record)
                       (push (plist-get record :state) states)
                       (funcall marker-write record)))
                    ((symbol-function 'org-note-operation--dispatch-frozen)
                     (lambda (frozen)
                       (setq dispatched frozen)
                       (should
                        (equal "dispatched"
                               (alist-get
                                'state
                                (gsmlg-org-note-org--noncapture-marker-read
                                 (gsmlg-org-note-org--noncapture-marker-file
                                  "save-operation-1")))))
                       '((document_revisions . (("document-1" . 5)))))))
            (narrow-to-region (point-min) (max (point-min) (1- (point-max))))
            (org-note-document-save)
            (widen)
            (should dispatched)
            (should (equal (nreverse states) '("prepared" "dispatched")))
            (let ((body (json-parse-string
                         (decode-coding-string (plist-get dispatched :body)
                                               'utf-8)
                         :object-type 'alist)))
              (should (equal (alist-get 'operation_id body)
                             "save-operation-1"))
              (should (equal (alist-get 'source body) "new\n")))
            (should (= org-note-document-revision 5))
            (should (equal org-note-document-base-source "new\n"))
            (should-not (buffer-modified-p))
            (should-not
             (directory-files
              (gsmlg-org-note-org--noncapture-marker-directory)
              nil "\\`[^.]")))
        (unless installed-p
          (advice-remove #'org-note-document--save-remote
                         #'gsmlg-org-note-org--around-document-save-remote))))))

(ert-deftest gsmlg-org-note-org-save-preserves-in-flight-edits ()
  "A validated save keeps later synchronous edits modified."
  (gsmlg-org-note-org-save-test--with-buffer
    (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
               (lambda (_) nil))
              ((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (_)
                 (goto-char (point-max))
                 (insert "later\n")
                 '((document_revisions . (("document-1" . 5)))))))
      (gsmlg-org-note-org--around-document-save-remote #'ignore)
      (should (= org-note-document-revision 5))
      (should (equal org-note-document-base-source "new\n"))
      (should (equal (buffer-string) "new\nlater\n"))
      (should (buffer-modified-p))
      (should-not org-note-document--conflict))))

(ert-deftest gsmlg-org-note-org-save-ambiguity-replays-identical-envelope ()
  "An ambiguous save blocks replacement and replays its exact envelope."
  (gsmlg-org-note-org-save-test--with-buffer
    (let (first-frozen replay-frozen)
      (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                 (lambda (_) nil))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (frozen)
                   (setq first-frozen frozen)
                   (signal 'org-note-transport-error '("uncertain")))))
        (should-error
         (gsmlg-org-note-org--around-document-save-remote #'ignore)
         :type 'org-note-transport-error))
      (should (= org-note-document-revision 4))
      (should (equal org-note-document-base-source "old\n"))
      (let* ((record (gethash "document-1"
                              gsmlg-org-note-org--document-ambiguities))
             (operation-id (plist-get record :operation-id))
             (file (gsmlg-org-note-org--noncapture-marker-file operation-id)))
        (should (eq first-frozen (plist-get record :frozen)))
        (should (equal "ambiguous"
                       (alist-get 'state
                                  (gsmlg-org-note-org--noncapture-marker-read file))))
        (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                   (lambda (_) (ert-fail "a blocked save requested new proofs")))
                  ((symbol-function 'org-note-operation--dispatch-frozen)
                   (lambda (_) (ert-fail "a blocked save dispatched"))))
          (should-error
           (gsmlg-org-note-org--around-document-save-remote #'ignore)
           :type 'user-error))
        (cl-letf (((symbol-function 'org-note-operation--dispatch-frozen)
                   (lambda (frozen)
                     (setq replay-frozen frozen)
                     '((document_revisions . (("document-1" . 5)))))))
          (gsmlg-org-note-org-retry-ambiguous-document-save "document-1"))
        (should (eq first-frozen replay-frozen))
        (should (= org-note-document-revision 5))
        (should (equal org-note-document-base-source "new\n"))
        (should-not (file-exists-p file))))))

(ert-deftest gsmlg-org-note-org-save-recovered-marker-blocks-dispatch ()
  "A recovered unresolved marker gates a related document save."
  (gsmlg-org-note-org-save-test--with-buffer
    (let ((gsmlg-org-note-org--noncapture-recovery-required
           '(((endpoint . "https://agent-note.gsmlg.net")
              (document_id . "document-1")
              (operation_id . "recovered-op")))))
      (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                 (lambda (_) nil))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (_) (ert-fail "recovered save dispatched"))))
        (should-error
         (gsmlg-org-note-org--around-document-save-remote #'ignore)
         :type 'user-error)))))

(ert-deftest gsmlg-org-note-org-save-cancels-pre-dispatch-divergence ()
  "A source change before dispatch cancels the prepared save marker."
  (gsmlg-org-note-org-save-test--with-buffer
    (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
               (lambda (_)
                 (goto-char (point-max))
                 (insert "before dispatch\n")
                 nil))
              ((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (_) (ert-fail "divergent save dispatched"))))
      (should-error
       (gsmlg-org-note-org--around-document-save-remote #'ignore)
       :type 'user-error)
      (should (= org-note-document-revision 4))
      (should (equal org-note-document-base-source "old\n"))
      (should (equal (buffer-string) "new\nbefore dispatch\n"))
      (should-not
       (directory-files
        (gsmlg-org-note-org--noncapture-marker-directory)
        nil "\\`[^.]")))))

(ert-deftest gsmlg-org-note-org-save-cleans-definitive-noncommit ()
  "A stale-revision refusal preserves local state and removes its marker."
  (gsmlg-org-note-org-save-test--with-buffer
    (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
               (lambda (_) nil))
              ((symbol-function 'org-note-operation--dispatch-frozen)
               (lambda (_)
                 (signal 'org-note-http-error
                         '((:status 409 :code "stale_revision"))))))
      (should-error
       (gsmlg-org-note-org--around-document-save-remote #'ignore)
       :type 'org-note-http-error)
      (should (= org-note-document-revision 4))
      (should (equal org-note-document-base-source "old\n"))
      (should (equal (buffer-string) "new\n"))
      (should (buffer-modified-p))
      (should-not (gethash "document-1"
                           gsmlg-org-note-org--document-ambiguities))
      (should-not
       (directory-files
        (gsmlg-org-note-org--noncapture-marker-directory)
        nil "\\`[^.]")))))

(ert-deftest gsmlg-org-note-org-save-postcommit-cleanup-never-resends ()
  "A post-commit cleanup retry performs no second remote dispatch."
  (gsmlg-org-note-org-save-test--with-buffer
    (let ((marker-delete
           (symbol-function 'gsmlg-org-note-org--noncapture-marker-delete))
          (delete-calls 0)
          (dispatches 0))
      (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                 (lambda (_) nil))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (_)
                   (setq dispatches (1+ dispatches))
                   '((document_revisions . (("document-1" . 5))))))
                ((symbol-function 'gsmlg-org-note-org--noncapture-marker-delete)
                 (lambda (operation-id)
                   (setq delete-calls (1+ delete-calls))
                   (if (= delete-calls 1)
                       (error "Injected marker cleanup failure")
                     (funcall marker-delete operation-id)))))
        (should-error
         (gsmlg-org-note-org--around-document-save-remote #'ignore))
        (should (= org-note-document-revision 5))
        (should (equal org-note-document-base-source "new\n"))
        (should-error
         (gsmlg-org-note-org-retry-ambiguous-document-save "document-1")
         :type 'user-error)
        (gsmlg-org-note-org-retry-document-save-cleanup "document-1")
        (should (= dispatches 1))
        (should-not (gethash "document-1"
                             gsmlg-org-note-org--document-ambiguities))))))

(ert-deftest gsmlg-org-note-org-save-disabled-passes-through ()
  "A disabled bridge preserves the vendored save implementation."
  (let ((gsmlg-org-note-org-enable nil)
        called)
    (should
     (eq 'native
         (gsmlg-org-note-org--around-document-save-remote
          (lambda (&rest args)
            (setq called args)
            'native)
          'one 'two)))
    (should (equal called '(one two)))))

(provide 'org-note-org-bridge-save-test)
;;; org-note-org-bridge-save-test.el ends here
