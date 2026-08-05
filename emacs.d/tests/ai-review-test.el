;;; ai-review-test.el --- Offline AI proposal apply tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Stale detection, apply, and transactional Apply All without network.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-ai-context)
(require 'gsmlg-ai-session)
(require 'gsmlg-ai-tools)
(require 'gsmlg-ai-review)

(defun gsmlg-ai-test--edit-session-from-buffer (buffer content proposed)
  "Create an edit session for BUFFER from CONTENT to PROPOSED."
  (with-current-buffer buffer
    (erase-buffer)
    (insert content)
    (let* ((file
            (gsmlg-ai-snapshot--create
             :id "f1"
             :display-path (or buffer-file-name (buffer-name))
             :canonical-file buffer-file-name
             :source-kind 'live-buffer
             :source-buffer buffer
             :source-buffer-tick (buffer-chars-modified-tick)
             :source-content-hash (gsmlg-ai-context--hash content)
             :source-file-attributes
             (and buffer-file-name (file-attributes buffer-file-name))
             :original-content content
             :proposed-content proposed
             :proposal-revision 1
             :operation 'modify
             :editable-p t
             :remote-p nil
             :apply-status 'pending
             :conflict-reason nil))
           (session
            (gsmlg-ai-session--create
             :id "s-apply"
             :kind 'edit
             :state 'ready
             :user-prompt "edit"
             :system-directive "edit"
             :context-id "c"
             :files (list file)
             :creation-root default-directory
             :backend-summary "stub"
             :request-buffer nil
             :tool-call-count 1
             :tool-token nil
             :revision-round 0
             :model-summary "done"
             :warnings nil
             :errors nil
             :created-at 0
             :updated-at 0)))
      (setq gsmlg-ai-session--active session)
      session)))

(ert-deftest gsmlg-ai-review-apply-does-not-save ()
  "Applying updates the buffer, leaves it modified, and does not save."
  (let ((file (make-temp-file "gsmlg-ai-apply-" nil ".txt" "original\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (gsmlg-ai-test--edit-session-from-buffer
           (current-buffer) "original\n" "applied\n")
          (gsmlg-ai-review--apply-one
           (car (gsmlg-ai-session-files gsmlg-ai-session--active)))
          (should (equal (buffer-string) "applied\n"))
          (should (buffer-modified-p))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "original\n"))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer)))
      (setq gsmlg-ai-session--active nil)
      (delete-file file))))

(ert-deftest gsmlg-ai-review-refuses-stale ()
  "Apply refuses when the source buffer changed after snapshot."
  (let ((file (make-temp-file "gsmlg-ai-stale-" nil ".txt" "original\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (gsmlg-ai-test--edit-session-from-buffer
           (current-buffer) "original\n" "proposed\n")
          (goto-char (point-max))
          (insert "user-edit")
          (should (gsmlg-ai-review-stale-p
                   (car (gsmlg-ai-session-files gsmlg-ai-session--active))))
          (should-error
           (gsmlg-ai-review--apply-one
            (car (gsmlg-ai-session-files gsmlg-ai-session--active))))
          (should (string-match-p "user-edit" (buffer-string)))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer)))
      (setq gsmlg-ai-session--active nil)
      (delete-file file))))

(ert-deftest gsmlg-ai-review-apply-all-rolls-back ()
  "Apply All rolls back when a later file fails preflight mid-flight."
  (let* ((root (make-temp-file "gsmlg-ai-all-" t))
         (file-a (expand-file-name "a.txt" root))
         (file-b (expand-file-name "b.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file file-a (insert "A\n"))
          (with-temp-file file-b (insert "B\n"))
          (let* ((buf-a (find-file-noselect file-a))
                 (buf-b (find-file-noselect file-b))
                 (snap-a
                  (gsmlg-ai-snapshot--create
                   :id "a" :display-path file-a :canonical-file file-a
                   :source-kind 'live-buffer :source-buffer buf-a
                   :source-buffer-tick (buffer-chars-modified-tick buf-a)
                   :source-content-hash (gsmlg-ai-context--hash "A\n")
                   :source-file-attributes (file-attributes file-a)
                   :original-content "A\n" :proposed-content "A2\n"
                   :proposal-revision 1 :operation 'modify :editable-p t
                   :remote-p nil :apply-status 'pending :conflict-reason nil))
                 (snap-b
                  (gsmlg-ai-snapshot--create
                   :id "b" :display-path file-b :canonical-file file-b
                   :source-kind 'live-buffer :source-buffer buf-b
                   :source-buffer-tick (buffer-chars-modified-tick buf-b)
                   :source-content-hash (gsmlg-ai-context--hash "B\n")
                   :source-file-attributes (file-attributes file-b)
                   :original-content "B\n" :proposed-content "B2\n"
                   :proposal-revision 1 :operation 'modify :editable-p t
                   :remote-p nil :apply-status 'pending :conflict-reason nil)))
            (setq gsmlg-ai-session--active
                  (gsmlg-ai-session--create
                   :id "all" :kind 'edit :state 'ready :user-prompt "x"
                   :system-directive "x" :context-id "c"
                   :files (list snap-a snap-b) :creation-root root
                   :backend-summary "stub" :request-buffer nil
                   :tool-call-count 1 :tool-token nil :revision-round 0
                   :model-summary "s" :warnings nil :errors nil
                   :created-at 0 :updated-at 0))
            (with-current-buffer buf-b
              (setq buffer-read-only t))
            (should-error (gsmlg-ai-review-apply-all))
            (with-current-buffer buf-a
              (should (equal (buffer-string) "A\n"))
              (should-not
               (eq (gsmlg-ai-snapshot-apply-status snap-a) 'applied)))
            (with-current-buffer buf-b
              (setq buffer-read-only nil)
              (should (equal (buffer-string) "B\n")))
            (kill-buffer buf-a)
            (kill-buffer buf-b)))
      (setq gsmlg-ai-session--active nil)
      (delete-directory root t))))

(ert-deftest gsmlg-ai-review-new-file-stays-unsaved ()
  "Applying a staged new file opens a modified buffer without creating disk."
  (let* ((root (make-temp-file "gsmlg-ai-new-" t))
         (path (expand-file-name "fresh.txt" root)))
    (unwind-protect
        (let* ((file
                (gsmlg-ai-snapshot--create
                 :id "n1" :display-path path :canonical-file path
                 :source-kind 'staged-new :source-buffer nil
                 :source-buffer-tick nil
                 :source-content-hash (gsmlg-ai-context--hash "")
                 :source-file-attributes nil
                 :original-content "" :proposed-content "brand-new\n"
                 :proposal-revision 1 :operation 'create :editable-p t
                 :remote-p nil :apply-status 'pending :conflict-reason nil)))
          (setq gsmlg-ai-session--active
                (gsmlg-ai-session--create
                 :id "new" :kind 'edit :state 'ready :user-prompt "x"
                 :system-directive "x" :context-id "c" :files (list file)
                 :creation-root root :backend-summary "stub"
                 :request-buffer nil :tool-call-count 1 :tool-token nil
                 :revision-round 0 :model-summary "s" :warnings nil
                 :errors nil :created-at 0 :updated-at 0))
          (gsmlg-ai-review--apply-one file)
          (should (buffer-live-p (gsmlg-ai-snapshot-source-buffer file)))
          (with-current-buffer (gsmlg-ai-snapshot-source-buffer file)
            (should (equal (buffer-string) "brand-new\n"))
            (should (buffer-modified-p))
            (should-not (file-exists-p path))
            (set-buffer-modified-p nil)
            (kill-buffer (current-buffer))))
      (setq gsmlg-ai-session--active nil)
      (delete-directory root t))))

(provide 'ai-review-test)
;;; ai-review-test.el ends here
