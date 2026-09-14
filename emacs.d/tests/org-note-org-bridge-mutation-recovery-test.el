;;; org-note-org-bridge-mutation-recovery-test.el --- Marker tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'gsmlg-paths)
(require 'gsmlg-org-note-org)

(ert-deftest gsmlg-org-note-noncapture-marker-roundtrip-and-secret-rejection ()
  (let ((gsmlg-state-directory (make-temp-file "org-note-state-" t)))
    (unwind-protect
        (let* ((op "op-test-1")
               (record (list :operation-kind "transition" :purpose "todo"
                             :state "prepared" :operation-id op
                             :endpoint "https://agent-note.gsmlg.net"
                             :workspace-id "w" :item-id "i"
                             :expected-revision 1 :target-state "DONE"))
               (file (gsmlg-org-note-org--noncapture-marker-write record)))
          (should (file-exists-p file))
          (should (= (logand (file-modes file) #o077) 0))
          (should (equal "prepared"
                         (alist-get 'state
                                    (gsmlg-org-note-org--noncapture-marker-read file))))
          (should-error
           (gsmlg-org-note-org--noncapture-marker-write
            (append record '(:source "secret")))
           :type 'user-error)
          (gsmlg-org-note-org-cancel-noncapture-mutation op)
          (should-not (file-exists-p file)))
      (delete-directory gsmlg-state-directory t))))

(ert-deftest gsmlg-org-note-noncapture-marker-corruption-quarantines ()
  (let ((gsmlg-state-directory (make-temp-file "org-note-state-" t)))
    (unwind-protect
        (let* ((file (gsmlg-org-note-org--noncapture-marker-file "bad-1" t)))
          (with-temp-file file (insert "{}"))
          (should-error (gsmlg-org-note-org--noncapture-marker-read file))
          (should (file-exists-p (concat file ".quarantine"))))
      (delete-directory gsmlg-state-directory t))))

(provide 'org-note-org-bridge-mutation-recovery-test)
;;; org-note-org-bridge-mutation-recovery-test.el ends here
