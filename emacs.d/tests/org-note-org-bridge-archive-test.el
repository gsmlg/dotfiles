;;; org-note-org-bridge-archive-test.el --- Archive bridge tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'gsmlg-org-note-org)

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

(provide 'org-note-org-bridge-archive-test)
;;; org-note-org-bridge-archive-test.el ends here
