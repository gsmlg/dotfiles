;;; org-note-org-bridge-phase7-test.el --- Phase 7 hardening tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-org-note-org)
(defvar org-note-endpoint)

(ert-deftest gsmlg-org-note-org-phase7-default-disabled ()
  (should-not gsmlg-org-note-org-enable))

(ert-deftest gsmlg-org-note-org-phase7-publication-key-isolated ()
  (let* ((org-note-endpoint "https://agent-note.example/a")
         (a (gsmlg-org-note-org--publication-reservation-directory '("one")))
         (b (gsmlg-org-note-org--publication-reservation-directory '("two")))
         (c (gsmlg-org-note-org--publication-reservation-directory '("three"))))
    (should-not (equal a b))
    (should-not (equal a c))))

(ert-deftest gsmlg-org-note-org-phase7-owner-start-token-field ()
  (let ((gsmlg-org-note-org--capture-reservation nil)
        (root (make-temp-file "org-note-phase7-" t)))
    (cl-letf (((symbol-function 'gsmlg-state-file)
               (lambda (&rest parts) (expand-file-name (car parts) root)))
              ((symbol-function 'org-note-client-new-operation-id)
               (lambda () "phase7-nonce")))
      (gsmlg-org-note-org--capture-reservation-acquire)
      (unwind-protect
          (let* ((file (expand-file-name "owner.json"
                                         (plist-get gsmlg-org-note-org--capture-reservation
                                                    :directory)))
                 (owner (json-parse-string
                         (with-temp-buffer
                           (insert-file-contents-literally file)
                           (buffer-string))
                         :object-type 'alist)))
            (should (integerp (alist-get 'pid owner)))
            (should (stringp (alist-get 'nonce owner)))
            (should (assq 'start_token owner)))
        (gsmlg-org-note-org--capture-reservation-release)))))

(ert-deftest gsmlg-org-note-org-phase7-ownerless-grace-refuses ()
  (let* ((gsmlg-org-note-org--capture-reservation nil)
         (gsmlg-org-note-org-reservation-ownerless-grace 60.0)
         (root (make-temp-file "org-note-phase7-" t))
         (directory (expand-file-name "org-note/capture.lock" root)))
    (make-directory directory t)
    (cl-letf (((symbol-function 'gsmlg-state-file)
               (lambda (&rest _parts) (concat directory "/"))))
      (should-error (gsmlg-org-note-org--capture-reservation-acquire)
                    :type 'user-error))))

(provide 'org-note-org-bridge-phase7-test)
;;; org-note-org-bridge-phase7-test.el ends here
