;;; org-note-org-bridge-refile-test.el --- Phase 4 refile tests -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'org)
(require 'org-note)
(require 'gsmlg-org-note-org)

(ert-deftest gsmlg-org-note-org-refile-moves-subtree-with-marker-target ()
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (response '((document_revisions . (("doc-1" . 2)))))
        (sent nil))
    (with-temp-buffer
      (org-note-document-mode)
      (setq-local org-note-document-id "doc-1"
                  org-note-document-workspace-id "ws-1"
                  org-note-document-path "inbox.org"
                  org-note-document-revision 1)
      (insert "* Source\nsource body\n* Target\n** Existing\n")
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (search-forward "Source")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Target"))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (frozen)
                   (setq sent frozen)
                   response))
                ((symbol-function 'org-note-operation-lease-proofs)
                 (lambda (_) nil)))
        (gsmlg-org-note-org--around-refile #'ignore)
        (should sent)
        (should (= org-note-document-revision 2))
        (should (equal (buffer-string)
                       "* Target\n** Existing\n* Source\nsource body\n"))))))

(ert-deftest gsmlg-org-note-org-refile-refuses-dirty-buffer ()
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t))
    (with-temp-buffer
      (org-note-document-mode)
      (setq-local org-note-document-id "doc-1"
                  org-note-document-workspace-id "ws-1"
                  org-note-document-path "inbox.org"
                  org-note-document-revision 1)
      (insert "* Source\n* Target\n")
      (should (buffer-modified-p))
      (should-error (gsmlg-org-note-org--around-refile #'ignore)
                    :type 'user-error))))

(ert-deftest gsmlg-org-note-org-refile-409-is-definitive-noncommit ()
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (gsmlg-org-note-org--document-ambiguities (make-hash-table :test #'equal)))
    (with-temp-buffer
      (org-note-document-mode)
      (setq-local org-note-document-id "doc-1"
                  org-note-document-workspace-id "ws-1"
                  org-note-document-path "inbox.org"
                  org-note-document-revision 1)
      (insert "* Source\n* Target\n")
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (search-forward "Source")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Target"))
                ((symbol-function 'org-note-operation--dispatch-frozen)
                 (lambda (_frozen)
                   (signal 'org-note-http-error
                           '((:status 409 :code "stale_revision")))))
                ((symbol-function 'org-note-operation-lease-proofs)
                 (lambda (_) nil)))
        (should-error (gsmlg-org-note-org--around-refile #'ignore)
                      :type 'org-note-http-error)
        (should-not (gethash "doc-1" gsmlg-org-note-org--document-ambiguities))
        (should-not (buffer-modified-p))))))

(provide 'org-note-org-bridge-refile-test)
;;; org-note-org-bridge-refile-test.el ends here
