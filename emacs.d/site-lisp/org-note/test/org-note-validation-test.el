;;; org-note-validation-test.el --- Tests for Org Note validation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for endpoint validation and bounded pagination primitives.

;;; Code:

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest org-note-validation-rejects-endpoint-userinfo-and-query ()
  (require 'org-note-validation)
  (should-error (org-note-validation-canonical-endpoint
                 "https://user:pass@example.com/api")
                :type 'org-note-error)
  (should-error (org-note-validation-canonical-endpoint
                 "https://example.com/api?token=1")
                :type 'org-note-error)
  (should (string-match-p "\\`https://example.com"
                          (org-note-validation-canonical-endpoint
                           "https://example.com/api/"))))

(ert-deftest org-note-validation-pager-rejects-repeated-cursor ()
  (require 'org-note-validation)
  (let ((state (org-note-validation-bounded-pager-state :limit 10 :max-pages 5))
        (page 0)
        message)
    (setq message
          (condition-case err
              (progn
                (org-note-validation-bounded-pager-fold
                 state
                 (lambda (_cursor)
                   (cl-incf page)
                   (pcase page
                     (1 (list :rows '(("id" . "a")) :next-cursor "c1"))
                     (2 (list :rows '(("id" . "b")) :next-cursor "c2"))
                     (_ (list :rows '(("id" . "c")) :next-cursor "c1")))))
                nil)
            (org-note-error (car (cdr err)))))
    (should (equal "Org Note pagination repeated an opaque cursor" message))))

(ert-deftest org-note-validation-pager-rejects-stuck-cursor ()
  (require 'org-note-validation)
  (let ((state (org-note-validation-bounded-pager-state :limit 10 :max-pages 5))
        message)
    (setq message
          (condition-case err
              (progn
                (org-note-validation-bounded-pager-fold
                 state
                 (lambda (cursor)
                   (list :rows `(("id" . ,(or cursor "page-1")))
                         :next-cursor (or cursor "c1"))))
                nil)
            (org-note-error (car (cdr err)))))
    (should (equal "Org Note pagination cursor is stuck" message))))

(ert-deftest org-note-validation-page-cursor-accepts-nil-and-nonempty-string ()
  (require 'org-note-validation)
  (should (eq (org-note-validation-page-cursor nil) nil))
  (should (equal (org-note-validation-page-cursor "opaque+cursor") "opaque+cursor"))
  (should-error (org-note-validation-page-cursor "") :type 'org-note-error)
  (should-error (org-note-validation-page-cursor 42) :type 'org-note-error))

(ert-deftest org-note-validation-endpoint-bound-read-context-builds-urls ()
  (require 'org-note-validation)
  (let* ((endpoint (org-note-validation-canonical-endpoint "https://notes.example/api/"))
         (context (org-note-validation-endpoint-bound-read-context endpoint))
         (builder (alist-get 'url-builder context)))
    (should (equal (alist-get 'endpoint context) endpoint))
    (should (functionp builder))
    (should (equal (funcall builder "/items" '((limit . 10)))
                   "https://notes.example/api/items?limit=10"))))

(ert-deftest org-note-validation-pager-step-completes-with-nil-next-cursor ()
  (require 'org-note-validation)
  (let ((state (org-note-validation-bounded-pager-state :limit 10 :max-pages 5))
        (calls 0))
    (cl-multiple-value-bind (rows done-p)
        (org-note-validation-bounded-pager-step
         state
         (lambda (_cursor)
           (cl-incf calls)
           (list :rows '(("id" . "a")) :next-cursor nil)))
      (should (equal rows '(("id" . "a"))))
      (should done-p)
      (should (= calls 1)))))

(ert-deftest org-note-validation-pager-fold-collects-multi-page-rows ()
  (require 'org-note-validation)
  (let ((state (org-note-validation-bounded-pager-state :limit 10 :max-pages 5))
        (calls 0)
        rows)
    (setq rows
          (org-note-validation-bounded-pager-fold
           state
           (lambda (cursor)
             (cl-incf calls)
             (pcase cursor
               ((pred null) (list :rows '(("id" . "a")) :next-cursor "cursor-1"))
               ("cursor-1" (list :rows '(("id" . "b")) :next-cursor "cursor-2"))
               ("cursor-2" (list :rows '(("id" . "c")) :next-cursor nil))))))
    (should (equal rows '(("id" . "a") ("id" . "b") ("id" . "c"))))
    (should (= calls 3)))

(ert-deftest org-note-validation-pager-rejects-repeated-row-id ()
  (require 'org-note-validation)
  (let ((state (org-note-validation-bounded-pager-state :limit 10 :max-pages 5))
        (page 0)
        message)
    (setq message
          (condition-case err
              (progn
                (org-note-validation-bounded-pager-fold
                 state
                 (lambda (_cursor)
                   (cl-incf page)
                   (list :rows '(("id" . "same"))
                         :next-cursor (format "page-%d" page)))))
                nil)
            (org-note-error (car (cdr err)))))
    (should (equal "Org Note pagination repeated a row identity" message))))

(provide 'org-note-validation-test)

;;; org-note-validation-test.el ends here
