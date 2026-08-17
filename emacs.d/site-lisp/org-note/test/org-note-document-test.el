;;; org-note-document-test.el --- Tests for Org Note documents -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for editable Org Note document buffers.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-note-document)

(defun org-note-document-test--response (id workspace-id path source revision)
  "Return a document response for ID in WORKSPACE-ID at PATH."
  `((id . ,id)
    (workspace_id . ,workspace-id)
    (path . ,path)
    (source . ,source)
    (content_hash . ,(copy-sequence "content-hash"))
    (revision . ,(if (integerp revision) revision 1))))

(defun org-note-document-test--buffer (source &optional keep-kill-query)
  "Return an Org Note document buffer containing SOURCE.

KEEP-KILL-QUERY preserves the mode's query function for kill lifecycle tests."
  (let ((buffer (generate-new-buffer " *org-note-document-test*")))
    (with-current-buffer buffer
      (org-note-document-mode)
      (insert source)
      (setq-local org-note-document-workspace-id "workspace-a"
                  org-note-document-id "document-a"
                  org-note-document-path "notes/example.org"
                  org-note-document-revision 1
                  org-note-document-content-hash "content-hash"
                  org-note-document-base-source "base source"
                  org-note-document--conflict 'existing-conflict)
      (unless keep-kill-query
        (setq-local kill-buffer-query-functions nil))
      (set-buffer-modified-p t))
    buffer))

(ert-deftest org-note-document-open-populates-an-org-buffer ()
  "Opening a document uses its response source and metadata exactly."
  (let (buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-document)
                   (lambda (_workspace-id _document-id)
                     (org-note-document-test--response
                      "document-a" "workspace-a" "notes/example.org"
                      "* Heading\nBody\n" 1))))
          (setq buffer (org-note-document-open "workspace-a" "document-a"))
          (with-current-buffer buffer
            (should (derived-mode-p 'org-note-document-mode))
            (should (derived-mode-p 'org-mode))
            (should (equal (buffer-string) "* Heading\nBody\n"))
            (should (equal org-note-document-workspace-id "workspace-a"))
            (should (equal org-note-document-id "document-a"))
            (should (equal org-note-document-path "notes/example.org"))
            (should (= org-note-document-revision 1))
            (should (equal org-note-document-content-hash "content-hash"))
            (should (equal org-note-document-base-source "* Heading\nBody\n"))
            (should-not org-note-document--conflict)
            (should-not (buffer-modified-p))
            (should-not buffer-file-name)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest org-note-document-open-reuses-an-exact-document-buffer ()
  "Opening an already-live workspace/document pair does not fetch again."
  (let ((calls 0)
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-document)
                   (lambda (_workspace-id _document-id)
                    (setq calls (1+ calls))
                    (org-note-document-test--response
                      "document-reuse" "workspace-reuse" "notes/example.org"
                      "body" 1))))
          (setq buffer
                (org-note-document-open "workspace-reuse" "document-reuse"))
          (should (eq buffer
                      (org-note-document-open
                       "workspace-reuse" "document-reuse")))
          (should (= calls 1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest org-note-document-open-keeps-workspaces-with-the-same-id-distinct ()
  "Documents with the same ID in distinct workspaces are fetched separately."
  (let ((calls 0)
        first second)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-document)
                   (lambda (workspace-id document-id)
                     (setq calls (1+ calls))
                     (org-note-document-test--response
                      document-id workspace-id "notes/shared.org"
                      workspace-id 1))))
          (setq first (org-note-document-open "workspace-a" "document-a")
                second (org-note-document-open "workspace-b" "document-a"))
          (should-not (eq first second))
          (should (= calls 2))
          (with-current-buffer second
            (should (equal org-note-document-workspace-id "workspace-b"))))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest org-note-document-open-keeps-same-path-documents-distinct ()
  "Documents with the same path but distinct IDs receive distinct buffers."
  (let (first second)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-document)
                   (lambda (workspace-id document-id)
                     (org-note-document-test--response
                      document-id workspace-id "notes/shared.org"
                      document-id 1))))
          (setq first (org-note-document-open "workspace-a" "document-a")
                second (org-note-document-open "workspace-a" "document-b"))
          (should-not (eq first second))
          (should (equal (buffer-name first) "*Org Note: notes/shared.org*"))
          (should (equal (buffer-name second) "*Org Note: notes/shared.org*<2>")))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest org-note-document-mode-remaps-save-buffer ()
  "The document mode remaps `save-buffer' to its remote save command."
  (should (eq (lookup-key org-note-document-mode-map [remap save-buffer])
              #'org-note-document-save)))

(ert-deftest org-note-document-open-cleans-up-after-population-error ()
  "A failed population leaves no newly-created document buffer behind."
  (cl-letf (((symbol-function 'org-note-operation-get-document)
             (lambda (_workspace-id _document-id)
               (org-note-document-test--response
                "document-a" "workspace-a" "notes/fail.org" "body" 1)))
            ((symbol-function 'org-note-document--populate-buffer)
             (lambda (_buffer _response)
               (error "population failed"))))
    (should-error (org-note-document-open "workspace-a" "document-a"))
    (should-not
     (cl-find-if (lambda (buffer)
                   (string-prefix-p "*Org Note: notes/fail.org*"
                                    (buffer-name buffer)))
                 (buffer-list)))))

(ert-deftest org-note-document-load-is-inert ()
  "Loading the document module creates neither requests nor timers."
  (let* ((emacs (concat invocation-directory invocation-name))
         (directory (file-name-directory (locate-library "org-note-document")))
         (output (generate-new-buffer " *org-note-document-load-test*"))
         (form
          (format
           "(progn (require 'cl-lib) (require 'url) (let ((requests 0) (timers 0)) (cl-letf (((symbol-function 'url-retrieve-synchronously) (lambda (&rest _arguments) (setq requests (1+ requests)))) ((symbol-function 'url-retrieve) (lambda (&rest _arguments) (setq requests (1+ requests)))) ((symbol-function 'run-at-time) (lambda (&rest _arguments) (setq timers (1+ timers)))) ((symbol-function 'run-with-timer) (lambda (&rest _arguments) (setq timers (1+ timers))))) (require 'org-note-document)) (unless (and (= requests 0) (= timers 0)) (kill-emacs 1))))")))
    (unwind-protect
        (should (zerop (call-process emacs nil output nil
                                     "-Q" "--batch" "-L" directory
                                     "--eval" form)))
      (kill-buffer output))))

(ert-deftest org-note-document-source-strips-text-properties ()
  "Document source has no text properties even when the buffer does."
  (let ((buffer (org-note-document-test--buffer
                 (propertize "body" 'face 'bold))))
    (unwind-protect
        (with-current-buffer buffer
          (let ((source (org-note-document--source)))
            (should (equal source "body"))
            (should-not (text-properties-at 1 source))))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-sends-full-source-and-updates-an-alist-revision ()
  "Saving sends exact widened text and commits confirmed alist revision data."
  (let ((buffer (org-note-document-test--buffer "top\nbody\nbottom\n"))
        call)
    (unwind-protect
        (with-current-buffer buffer
          (goto-char 6)
          (narrow-to-region 5 10)
          (let ((point (point))
                (minimum (point-min))
                (maximum (point-max))
                (proofs (make-hash-table :test #'equal)))
            (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                       (lambda (document-id)
                         (should (equal document-id "document-a"))
                         proofs))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest arguments)
                         (setq call arguments)
                         `((document_revisions
                            . (("document-a" . 2)))))))
              (org-note-document-save))
            (should (equal call
                           (list "workspace-a" "document-a" "notes/example.org"
                                 "top\nbody\nbottom\n" 1 proofs)))
            (should (equal org-note-document-revision 2))
            (should (equal org-note-document-base-source "top\nbody\nbottom\n"))
            (should-not org-note-document--conflict)
            (should-not (buffer-modified-p))
            (should (= (point) point))
            (should (= (point-min) minimum))
            (should (= (point-max) maximum))))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-accepts-a-hash-table-revision-map ()
  "Saving accepts JSON objects decoded as hash tables with symbol keys."
  (let ((buffer (org-note-document-test--buffer "body"))
        (revisions (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash 'document-a 2 revisions)
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                       (lambda (_document-id) nil))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments)
                         `((document_revisions . ,revisions)))))
              (org-note-document-save))
            (should (equal org-note-document-revision 2))))
      (kill-buffer buffer))))

(ert-deftest org-note-document-response-revision-accepts-all-key-types ()
  "Revision maps accept alists and hash tables with string or symbol keys."
  (let ((hash-string (make-hash-table :test #'equal))
        (hash-symbol (make-hash-table :test #'equal)))
    (puthash "document-a" 4 hash-string)
    (puthash 'document-a 5 hash-symbol)
    (dolist (case (list (cons (list (cons "document-a" 2)) 2)
                        (cons (list (cons 'document-a 3)) 3)
                        (cons hash-string 4)
                        (cons hash-symbol 5)))
      (should
       (equal (org-note-document--response-revision
               `((document_revisions . ,(car case))) "document-a")
              (cdr case))))))

(ert-deftest org-note-document-save-rejects-a-response-without-revision ()
  "An unconfirmed response preserves all local document state."
  (let ((buffer (org-note-document-test--buffer "body")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                     (lambda (_document-id) nil))
                    ((symbol-function 'org-note-operation-put-document)
                     (lambda (&rest _arguments) '((document_revisions . nil)))))
            (should-error (org-note-document-save)
                          :type 'org-note-response-error))
          (should (equal (buffer-string) "body\n"))
          (should (equal org-note-document-revision 1))
          (should (equal org-note-document-base-source "base source"))
          (should (eq org-note-document--conflict 'existing-conflict))
          (should (buffer-modified-p)))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-preserves-state-after-an-operation-error ()
  "An operation error leaves document text, metadata, and view state intact."
  (let ((buffer (org-note-document-test--buffer "top\nbody\nbottom\n")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char 6)
          (narrow-to-region 5 10)
          (let ((point (point))
                (minimum (point-min))
                (maximum (point-max)))
            (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                       (lambda (_document-id) nil))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments)
                         (signal 'org-note-error '("operation failed")))))
              (should-error (org-note-document-save) :type 'org-note-error))
            (should (equal (buffer-string) "body\n"))
            (should (equal org-note-document-revision 1))
            (should (equal org-note-document-base-source "base source"))
            (should (eq org-note-document--conflict 'existing-conflict))
            (should (buffer-modified-p))
            (should (= (point) point))
            (should (= (point-min) minimum))
            (should (= (point-max) maximum))))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-keeps-committed-state-after-quit ()
  "A quit after server confirmation leaves confirmed metadata intact."
  (let ((buffer (org-note-document-test--buffer "top\nbody\nbottom\n")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char 6)
          (narrow-to-region 5 10)
          (let ((point (point))
                (minimum (point-min))
                (maximum (point-max))
                (calls 0)
                (original (symbol-function 'set-buffer-modified-p)))
            (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                       (lambda (_document-id) nil))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments)
                         '((document_revisions
                            . (("document-a" . 2))))))
                      ((symbol-function 'set-buffer-modified-p)
                       (lambda (modified-p)
                         (setq calls (1+ calls))
                         (if (= calls 1)
                             (signal 'quit nil)
                           (funcall original modified-p)))))
              (should (condition-case nil
                          (progn
                            (org-note-document-save)
                            nil)
                        (quit t))))
            (should (equal org-note-document-revision 2))
            (should (equal org-note-document-base-source "top\nbody\nbottom\n"))
            (should-not org-note-document--conflict)
            (should (buffer-modified-p))
            (should (= (point) point))
            (should (= (point-min) minimum))
            (should (= (point-max) maximum))))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-validates-mode-and-metadata-first ()
  "Invalid document buffers do not make save operations or change state."
  (let ((calls 0))
    (cl-letf (((symbol-function 'org-note-operation-put-document)
               (lambda (&rest _arguments)
                 (setq calls (1+ calls)))))
      (with-temp-buffer
        (org-mode)
        (insert "body")
        (set-buffer-modified-p t)
        (should-error (org-note-document-save) :type 'user-error)
        (should (equal (buffer-string) "body"))
        (should (buffer-modified-p)))
      (dolist (variable '(org-note-document-workspace-id
                          org-note-document-id
                          org-note-document-path
                          org-note-document-revision))
        (let ((buffer (org-note-document-test--buffer "top\nbody\nbottom\n")))
          (unwind-protect
              (with-current-buffer buffer
                (goto-char 6)
                (narrow-to-region 5 10)
                (let ((point (point))
                      (minimum (point-min))
                      (maximum (point-max)))
                  (set variable nil)
                  (should-error (org-note-document-save) :type 'user-error)
                  (should (equal (buffer-string) "body\n"))
                  (should (equal org-note-document-base-source "base source"))
                  (should (eq org-note-document--conflict 'existing-conflict))
                  (should (buffer-modified-p))
                  (should (= (point) point))
                  (should (= (point-min) minimum))
                  (should (= (point-max) maximum))))
            (kill-buffer buffer)))))
    (should (= calls 0))))

(ert-deftest org-note-document-mode-integrates-with-save-lifecycle ()
  "Every normal Emacs save path runs matching hooks around one PUT."
  (dolist (entry '(direct offered key))
    (let ((buffer (org-note-document-test--buffer "body"))
          (calls 0)
          events)
      (unwind-protect
          (with-current-buffer buffer
            (add-hook 'before-save-hook
                      (lambda () (push 'before events)) nil t)
            (add-hook 'after-save-hook
                      (lambda () (push 'after events)) t t)
            (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                       (lambda (_document-id) nil))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments)
                         (setq calls (1+ calls))
                         (push 'write events)
                         '((document_revisions
                            . (("document-a" . 2))))))
                      ((symbol-function 'read-file-name)
                       (lambda (&rest _arguments)
                         (error "Local file selection is forbidden"))))
              (pcase entry
                ('direct (save-buffer))
                ('offered
                 (save-some-buffers
                  t (lambda () (eq (current-buffer) buffer))))
                ('key
                 (save-window-excursion
                   (pop-to-buffer buffer)
                   (execute-kbd-macro (kbd "C-x C-s")))))
              (should (= calls 1))
              (should (equal (nreverse events) '(before write after)))
              (should-not (buffer-modified-p))
              (should buffer-offer-save)
              (should (memq #'org-note-document--write-contents
                            write-contents-functions))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(ert-deftest org-note-document-standard-saves-preserve-in-flight-edits ()
  "Direct and offered saves retain edits made during the remote request."
  (dolist (entry '(direct offered))
    (let ((buffer (org-note-document-test--buffer "body")))
      (unwind-protect
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                       (lambda (_document-id) nil))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments)
                         (goto-char (point-max))
                         (insert " later")
                         '((document_revisions . (("document-a" . 2)))))))
              (pcase entry
                ('direct (save-buffer))
                ('offered
                 (save-some-buffers
                  t (lambda () (eq (current-buffer) buffer)))))
              (should (string-suffix-p " later" (buffer-string)))
              (should (buffer-modified-p))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(ert-deftest org-note-document-kill-query-respects-modification-state ()
  "Modified document buffers require a safe explicit discard confirmation."
  (let ((unmodified (org-note-document-test--buffer "body" t))
        (vetoed (org-note-document-test--buffer "body" t))
        (confirmed (org-note-document-test--buffer "body" t))
        prompt)
    (unwind-protect
        (progn
          (with-current-buffer unmodified
            (set-buffer-modified-p nil))
          (should (kill-buffer unmodified))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (text)
                       (setq prompt text)
                       nil)))
            (should-not (kill-buffer vetoed)))
          (should (buffer-live-p vetoed))
          (should (equal prompt "Discard unsaved Org Note changes? "))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_text) t)))
            (should (kill-buffer confirmed)))
          (should-not (buffer-live-p confirmed)))
      (dolist (buffer (list unmodified vetoed confirmed))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(ert-deftest org-note-document-save-keeps-in-flight-edits-after-success ()
  "Edits made while PUT runs retain modified text after a confirmed save."
  (let ((buffer (org-note-document-test--buffer "body")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                     (lambda (_document-id) nil))
                    ((symbol-function 'org-note-operation-put-document)
                     (lambda (&rest _arguments)
                       (goto-char (point-max))
                       (insert " later")
                       '((document_revisions
                          . (("document-a" . 2)))))))
            (org-note-document-save))
          (should (string-suffix-p " later" (buffer-string)))
          (should (equal org-note-document-revision 2))
          (should (equal org-note-document-base-source "body\n"))
          (should-not org-note-document--conflict)
          (should (buffer-modified-p)))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-keeps-in-flight-edits-after-failure ()
  "Failed PUTs retain edits made after a request sent clean source."
  (let ((buffer (org-note-document-test--buffer "body")))
    (unwind-protect
        (with-current-buffer buffer
          (set-buffer-modified-p nil)
          (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                     (lambda (_document-id) nil))
                    ((symbol-function 'org-note-operation-put-document)
                     (lambda (&rest _arguments)
                       (goto-char (point-max))
                       (insert " later")
                       (signal 'org-note-error '("PUT failed")))))
            (should-error (org-note-document--save-remote)
                          :type 'org-note-error))
          (should (string-suffix-p " later" (buffer-string)))
          (should (equal org-note-document-revision 1))
          (should (equal org-note-document-base-source "base source"))
          (should (eq org-note-document--conflict 'existing-conflict))
          (should (buffer-modified-p)))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-keeps-committed-state-after-message-error ()
  "A local message error after commit cannot restore stale metadata."
  (let ((buffer (org-note-document-test--buffer "body")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                     (lambda (_document-id) nil))
                    ((symbol-function 'org-note-operation-put-document)
                     (lambda (&rest _arguments)
                       '((document_revisions
                          . (("document-a" . 2))))))
                    ((symbol-function 'message)
                     (lambda (&rest _arguments)
                       (error "message failed"))))
            (should-error (org-note-document-save)))
          (should (equal org-note-document-revision 2))
          (should (equal org-note-document-base-source "body\n"))
          (should-not org-note-document--conflict))
      (kill-buffer buffer))))

(ert-deftest org-note-document-open-rejects-invalid-responses ()
  "Malformed or crossed document responses are rejected without buffers."
  (dolist (case '((workspace_id . "workspace-b")
                  (workspace_id . "")
                  (workspace_id . 1)
                  (id . "document-b")
                  (id . "")
                  (id . 1)
                  (path . "")
                  (path . 1)
                  (source . 1)
                  (content_hash . "")
                  (content_hash . 1)
                  (revision . -1)
                  (revision . "1")))
    (let ((response (org-note-document-test--response
                     "document-a" "workspace-a" "notes/invalid.org"
                     "body" 1)))
      (setf (alist-get (car case) response) (cdr case))
      (cl-letf (((symbol-function 'org-note-operation-get-document)
                 (lambda (_workspace-id _document-id) response)))
        (should-error (org-note-document-open "workspace-a" "document-a")
                      :type 'org-note-response-error)
        (should-not
         (org-note-document--find-buffer "workspace-a" "document-a"))))))

(ert-deftest org-note-document-open-preserves-display-error-during-cleanup ()
  "Failed display cleanup suppresses hooks and preserves the display error."
  (let ((kill-buffer-hook (list (lambda () (error "kill hook ran"))))
        (kill-buffer-query-functions (list (lambda () (error "query ran"))))
        error-data)
    (cl-letf (((symbol-function 'org-note-operation-get-document)
               (lambda (_workspace-id _document-id)
                 (org-note-document-test--response
                  "document-a" "workspace-a" "notes/display.org" "body" 1)))
              ((symbol-function 'pop-to-buffer)
               (lambda (_buffer &rest _arguments)
                 (error "display failed"))))
      (condition-case error-value
          (org-note-document-open "workspace-a" "document-a")
        (error (setq error-data error-value))))
    (should (equal error-data '(error "display failed")))
    (should-not (org-note-document--find-buffer "workspace-a" "document-a"))))

(ert-deftest org-note-document-open-cleanup-cannot-mask-original-error ()
  "A cleanup failure does not replace the original display error."
  (let (error-data buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'org-note-operation-get-document)
                   (lambda (_workspace-id _document-id)
                     (org-note-document-test--response
                      "document-a" "workspace-a" "notes/cleanup.org" "body" 1)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (_buffer &rest _arguments)
                     (error "display failed")))
                  ((symbol-function 'kill-buffer)
                   (lambda (&rest _arguments)
                     (error "cleanup failed"))))
          (condition-case error-value
              (org-note-document-open "workspace-a" "document-a")
            (error (setq error-data error-value)))
          (setq buffer
                (org-note-document--find-buffer "workspace-a" "document-a"))
          (should (equal error-data '(error "display failed")))
          (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local kill-buffer-query-functions nil
                      kill-buffer-hook nil))
        (kill-buffer buffer)))))

(ert-deftest org-note-document-open-validates-request-identifiers ()
  "Open rejects invalid requested identifiers before making a GET request."
  (dolist (arguments '(("" "document-a")
                       (nil "document-a")
                       (1 "document-a")
                       ("workspace-a" "")
                       ("workspace-a" nil)
                       ("workspace-a" 1)))
    (let ((calls 0))
      (cl-letf (((symbol-function 'org-note-operation-get-document)
                 (lambda (&rest _arguments)
                   (setq calls (1+ calls)))))
        (should-error (apply #'org-note-document-open arguments)
                      :type 'user-error)
        (should (= calls 0))))))

(ert-deftest org-note-document-save-requires-well-typed-metadata ()
  "Save rejects invalid metadata before making a PUT request."
  (dolist (case '((org-note-document-workspace-id . "")
                  (org-note-document-workspace-id . 1)
                  (org-note-document-id . "")
                  (org-note-document-id . 1)
                  (org-note-document-path . "")
                  (org-note-document-path . 1)
                  (org-note-document-revision . -1)
                  (org-note-document-revision . "1")))
    (let ((buffer (org-note-document-test--buffer "body"))
          (calls 0))
      (unwind-protect
          (with-current-buffer buffer
            (set (car case) (cdr case))
            (cl-letf (((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments)
                         (setq calls (1+ calls)))))
              (should-error (org-note-document-save) :type 'user-error)
              (should (= calls 0))))
        (kill-buffer buffer)))))

(ert-deftest org-note-document-save-rejects-invalid-response-revisions ()
  "Malformed PUT revisions never become authoritative local metadata."
  (dolist (revision '(nil "" "2" -1))
    (let ((buffer (org-note-document-test--buffer "body")))
      (unwind-protect
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                       (lambda (_document-id) nil))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments)
                         `((document_revisions
                            . (("document-a" . ,revision)))))))
              (should-error (org-note-document-save)
                            :type 'org-note-response-error))
            (should (= org-note-document-revision 1))
            (should (equal org-note-document-base-source "base source")))
        (kill-buffer buffer)))))

(ert-deftest org-note-document-save-records-a-validated-stale-conflict ()
  "A stale normal save fetches one latest response without changing local state."
  (let ((buffer (org-note-document-test--buffer "top\nbody\nbottom\n"))
        (put-calls 0)
        (get-calls 0)
        get-arguments
        remote)
    (unwind-protect
        (with-current-buffer buffer
          (goto-char 6)
          (narrow-to-region 5 10)
          (let ((source (org-note-document--source))
                (point (point))
                (minimum (point-min))
                (maximum (point-max)))
            (setq remote
                  (org-note-document-test--response
                   "document-a" "workspace-a" "notes/example.org"
                   "* Remote\n" 2))
            (let (error-data)
              (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                       (lambda (_document-id) nil))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments)
                         (setq put-calls (1+ put-calls))
                         (signal 'org-note-http-error
                                 '((:status 409 :code stale_revision
                                    :message "stale" :details nil
                                    :retryable nil)))))
                      ((symbol-function 'org-note-operation-get-document)
                       (lambda (&rest arguments)
                         (setq get-calls (1+ get-calls)
                               get-arguments arguments)
                         remote)))
                (condition-case error-value
                    (org-note-document-save)
                  (org-note-http-error (setq error-data error-value))))
              (should (equal error-data
                             '(org-note-http-error
                               (:status 409 :code stale_revision
                                :message "stale" :details nil
                                :retryable nil)))))
            (should (= put-calls 1))
            (should (= get-calls 1))
            (should (equal get-arguments '("workspace-a" "document-a")))
            (should (equal (org-note-document--source) source))
            (should (= (point) point))
            (should (= (point-min) minimum))
            (should (= (point-max) maximum))
            (should (= org-note-document-revision 1))
            (should (equal org-note-document-content-hash "content-hash"))
            (should (equal org-note-document-base-source "base source"))
            (should (eq org-note-document--conflict remote))
            (should (buffer-modified-p))))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-does-not-fetch-for-non-stale-errors ()
  "Only a stale revision HTTP error may trigger a latest-document GET."
  (let ((buffer (org-note-document-test--buffer "body"))
        (get-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                     (lambda (_document-id) nil))
                    ((symbol-function 'org-note-operation-put-document)
                     (lambda (&rest _arguments)
                       (signal 'org-note-error '(failed))))
                    ((symbol-function 'org-note-operation-get-document)
                     (lambda (&rest _arguments)
                       (setq get-calls (1+ get-calls)))))
            (should-error (org-note-document-save) :type 'org-note-error))
          (should (= get-calls 0)))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-does-not-fetch-for-non-stale-http-errors ()
  "A non-stale HTTP error leaves the previous conflict cache untouched."
  (let ((buffer (org-note-document-test--buffer "body"))
        (get-calls 0)
        error-data)
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                     (lambda (_document-id) nil))
                    ((symbol-function 'org-note-operation-put-document)
                     (lambda (&rest _arguments)
                       (signal 'org-note-http-error
                               '((:status 409 :code other_revision
                                  :message "other" :details nil
                                  :retryable nil)))))
                    ((symbol-function 'org-note-operation-get-document)
                     (lambda (&rest _arguments)
                       (setq get-calls (1+ get-calls)))))
            (condition-case error-value
                (org-note-document-save)
              (org-note-http-error (setq error-data error-value))))
          (should (equal error-data
                         '(org-note-http-error
                           (:status 409 :code other_revision
                            :message "other" :details nil
                            :retryable nil))))
          (should (= get-calls 0))
          (should (eq org-note-document--conflict 'existing-conflict)))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-records-string-stale-revision-errors ()
  "A string stale revision code follows the same conflict cache path."
  (let ((buffer (org-note-document-test--buffer "body"))
        (get-calls 0)
        remote)
    (unwind-protect
        (with-current-buffer buffer
          (setq remote (org-note-document-test--response
                        "document-a" "workspace-a" "notes/example.org"
                        "remote" 2))
          (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                     (lambda (_document-id) nil))
                    ((symbol-function 'org-note-operation-put-document)
                     (lambda (&rest _arguments)
                       (signal 'org-note-http-error
                               '((:status 409 :code "stale_revision"
                                  :message "stale" :details nil
                                  :retryable nil)))))
                    ((symbol-function 'org-note-operation-get-document)
                     (lambda (&rest _arguments)
                       (setq get-calls (1+ get-calls))
                       remote)))
            (should-error (org-note-document-save) :type 'org-note-http-error))
          (should (= get-calls 1))
          (should (eq org-note-document--conflict remote)))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-preserves-state-when-stale-fetch-is-bad ()
  "A failed or invalid stale fetch re-signals stale without changing local state."
  (dolist (latest
           (list (lambda () (signal 'org-note-error '(latest-failed)))
                 (lambda ()
                   (org-note-document-test--response
                    "document-a" "workspace-a" "" "remote" 2))
                 (lambda ()
                   (org-note-document-test--response
                    "document-b" "workspace-a" "notes/example.org" "remote" 2))))
    (let ((buffer (org-note-document-test--buffer "top\nbody\nbottom\n")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char 6)
            (narrow-to-region 5 10)
            (let ((source (org-note-document--source))
                  (point (point))
                  (minimum (point-min))
                  (maximum (point-max)))
              (let (error-data)
                (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                         (lambda (_document-id) nil))
                        ((symbol-function 'org-note-operation-put-document)
                         (lambda (&rest _arguments)
                           (signal 'org-note-http-error
                                   '((:status 409 :code stale_revision
                                      :message "stale" :details nil
                                      :retryable nil)))))
                        ((symbol-function 'org-note-operation-get-document)
                         (lambda (&rest _arguments) (funcall latest))))
                  (condition-case error-value
                      (org-note-document-save)
                    (org-note-http-error (setq error-data error-value))))
                (should (equal error-data
                               '(org-note-http-error
                                 (:status 409 :code stale_revision
                                  :message "stale" :details nil
                                  :retryable nil)))))
              (should (equal (org-note-document--source) source))
              (should (= (point) point))
              (should (= (point-min) minimum))
              (should (= (point-max) maximum))
              (should (= org-note-document-revision 1))
              (should (equal org-note-document-content-hash "content-hash"))
              (should (equal org-note-document-base-source "base source"))
              (should (eq org-note-document--conflict 'existing-conflict))
              (should (buffer-modified-p))))
        (kill-buffer buffer)))))

(ert-deftest org-note-document-compare-latest-uses-cached-clean-remote-buffer ()
  "Comparing uses a clean, read-only non-file Org buffer without a request."
  (let ((buffer (org-note-document-test--buffer "local"))
        remote collision
        ediff-arguments
        (operation-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local org-note-document--conflict
                      (org-note-document-test--response
                       "document-a" "workspace-a" "notes/example.org"
                       "* Remote\n" 2))
          (setq collision
                (generate-new-buffer "*Org Note Remote: notes/example.org r2*"))
          (require 'ediff)
          (cl-letf (((symbol-function 'ediff-buffers)
                     (lambda (&rest arguments)
                       (setq ediff-arguments arguments)))
                    ((symbol-function 'org-note-operation-get-document)
                     (lambda (&rest _arguments)
                       (setq operation-calls (1+ operation-calls))))
                    ((symbol-function 'org-note-operation-put-document)
                     (lambda (&rest _arguments)
                       (setq operation-calls (1+ operation-calls)))))
            (setq remote (org-note-document-compare-latest)))
          (should (equal ediff-arguments (list buffer remote)))
          (should (= operation-calls 0))
          (should-not (eq remote collision))
          (with-current-buffer remote
            (should (derived-mode-p 'org-mode))
            (should (equal (buffer-string) "* Remote\n"))
            (should buffer-read-only)
            (should-not buffer-file-name)
            (should-not (buffer-modified-p))
            (should (string-prefix-p "*Org Note Remote: notes/example.org r2*"
                                     (buffer-name remote)))))
      (dolist (candidate (list remote collision buffer))
        (when (buffer-live-p candidate)
          (with-current-buffer candidate
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer candidate))))))

(ert-deftest org-note-document-compare-latest-discards-remote-on-ediff-error ()
  "An Ediff error without a control buffer re-signals and discards remote."
  (let ((buffer (org-note-document-test--buffer "local")) remote error-data)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local org-note-document--conflict
                      (org-note-document-test--response
                       "document-a" "workspace-a" "notes/example.org"
                       "remote" 2))
          (require 'ediff)
          (cl-letf (((symbol-function 'ediff-buffers)
                     (lambda (_local remote-buffer)
                       (setq remote remote-buffer)
                       (signal 'error '(ediff-failed)))))
            (condition-case error-value
                (org-note-document-compare-latest)
              (error (setq error-data error-value))))
          (should (equal error-data '(error ediff-failed)))
          (should-not (buffer-live-p remote)))
      (dolist (candidate (list remote buffer))
        (when (buffer-live-p candidate)
          (with-current-buffer candidate
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer candidate))))))

(ert-deftest org-note-document-compare-latest-ignores-preexisting-ediff-control ()
  "A pre-existing Ediff control buffer never takes ownership on an error."
  (let ((buffer (org-note-document-test--buffer "local"))
        control remote existing error-data)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local org-note-document--conflict
                      (org-note-document-test--response
                       "document-a" "workspace-a" "notes/example.org"
                       "remote" 2))
          (setq control (generate-new-buffer " *org-note-ediff-control*"))
          (setq existing (generate-new-buffer " *org-note-ediff-existing*"))
          (with-current-buffer control
            (setq-local org-note-document--ediff-remote-buffer existing))
          (require 'ediff)
          (let ((ediff-control-buffer control))
            (cl-letf (((symbol-function 'ediff-buffers)
                       (lambda (_local remote-buffer)
                         (setq remote remote-buffer)
                         (signal 'error '(ediff-failed)))))
              (condition-case error-value
                  (org-note-document-compare-latest)
                (error (setq error-data error-value)))))
          (should (equal error-data '(error ediff-failed)))
          (should-not (buffer-live-p remote))
          (with-current-buffer control
            (should (eq org-note-document--ediff-remote-buffer existing)))
      (dolist (candidate (list remote existing control buffer))
        (when (buffer-live-p candidate)
          (with-current-buffer candidate
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer candidate)))))))

(ert-deftest org-note-document-compare-latest-owns-new-ediff-control-on-error ()
  "A newly established Ediff control buffer owns cleanup after an error."
  (let ((buffer (org-note-document-test--buffer "local"))
        control remote error-data)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local org-note-document--conflict
                      (org-note-document-test--response
                       "document-a" "workspace-a" "notes/example.org"
                       "remote" 2))
          (setq control (generate-new-buffer " *org-note-ediff-control*"))
          (require 'ediff)
          (let ((ediff-control-buffer nil))
            (cl-letf (((symbol-function 'ediff-buffers)
                       (lambda (_local remote-buffer)
                         (setq remote remote-buffer
                               ediff-control-buffer control)
                         (signal 'error '(ediff-failed)))))
              (condition-case error-value
                  (org-note-document-compare-latest)
                (error (setq error-data error-value)))))
          (should (equal error-data '(error ediff-failed)))
          (should (buffer-live-p remote))
          (with-current-buffer control
            (run-hooks 'ediff-after-quit-hook-internal))
          (should-not (buffer-live-p remote)))
      (dolist (candidate (list remote control buffer))
        (when (buffer-live-p candidate)
          (with-current-buffer candidate
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer candidate))))))

(ert-deftest org-note-document-conflict-commands-require-mode-and-conflict ()
  "Conflict commands reject buffers outside the document conflict state."
  (with-temp-buffer
    (dolist (command '(org-note-document-compare-latest
                       org-note-document-reload
                       org-note-document-rebase))
      (should-error (funcall command) :type 'user-error)))
  (let ((buffer (org-note-document-test--buffer "body")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local org-note-document--conflict nil)
          (dolist (command '(org-note-document-compare-latest
                             org-note-document-reload
                             org-note-document-rebase))
            (should-error (funcall command) :type 'user-error)))
      (kill-buffer buffer))))

(ert-deftest org-note-document-reload-requires-confirmation-and-uses-conflict ()
  "Reload either leaves local state alone or replaces it from cached remote data."
  (let ((buffer (org-note-document-test--buffer "local text"))
        (put-calls 0)
        (get-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (let ((conflict (org-note-document-test--response
                           "document-a" "workspace-a" "notes/new.org"
                           "remote text" 2)))
            (setq-local org-note-document--conflict conflict)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
              (should-error (org-note-document-reload) :type 'user-error))
            (should (equal (buffer-string) "local text"))
            (should (eq org-note-document--conflict conflict))
            (should (buffer-modified-p))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments) (setq put-calls (1+ put-calls))))
                      ((symbol-function 'org-note-operation-get-document)
                       (lambda (&rest _arguments) (setq get-calls (1+ get-calls)))))
              (org-note-document-reload))
            (should (equal (buffer-string) "remote text"))
            (should (equal org-note-document-path "notes/new.org"))
            (should (= org-note-document-revision 2))
            (should (equal org-note-document-workspace-id "workspace-a"))
            (should (equal org-note-document-id "document-a"))
            (should (equal org-note-document-content-hash "content-hash"))
            (should (equal org-note-document-base-source "remote text"))
            (should-not org-note-document--conflict)
            (should-not (buffer-modified-p))
            (should (= put-calls 0))
            (should (= get-calls 0))))
      (kill-buffer buffer))))

(ert-deftest org-note-document-rebase-preserves-local-view-and-remains-modified ()
  "Rebase changes only the expected remote metadata and retains local edits."
  (let ((buffer (org-note-document-test--buffer "top\nbody\nbottom\n"))
        (put-calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char 6)
          (narrow-to-region 5 10)
          (let ((conflict (org-note-document-test--response
                           "document-a" "workspace-a" "notes/new.org"
                           "remote text" 2))
                (source (org-note-document--source))
                (point (point))
                (minimum (point-min))
                (maximum (point-max)))
            (setq-local org-note-document--conflict conflict)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
              (should-error (org-note-document-rebase) :type 'user-error))
            (should (eq org-note-document--conflict conflict))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments) (setq put-calls (1+ put-calls)))))
              (org-note-document-rebase))
            (should (equal (org-note-document--source) source))
            (should (= (point) point))
            (should (= (point-min) minimum))
            (should (= (point-max) maximum))
            (should (equal org-note-document-path "notes/new.org"))
            (should (= org-note-document-revision 2))
            (should (equal org-note-document-content-hash "content-hash"))
            (should (equal org-note-document-base-source "remote text"))
            (should-not org-note-document--conflict)
            (should (buffer-modified-p))
            (should (= put-calls 0))))
      (kill-buffer buffer))))

(ert-deftest org-note-document-rebase-confirms-even-when-clean ()
  "A clean buffer still requires confirmation before rebasing its save base."
  (let ((buffer (org-note-document-test--buffer "local")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((conflict (org-note-document-test--response
                           "document-a" "workspace-a" "notes/new.org"
                           "remote" 2)))
            (setq-local org-note-document--conflict conflict)
            (set-buffer-modified-p nil)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
              (should-error (org-note-document-rebase) :type 'user-error))
            (should (eq org-note-document--conflict conflict))
            (should (= org-note-document-revision 1))
            (should-not (buffer-modified-p))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (org-note-document-rebase))
            (should (= org-note-document-revision 2))
            (should-not org-note-document--conflict)
            (should (buffer-modified-p))))
      (kill-buffer buffer))))

(ert-deftest org-note-document-save-caches-a-moved-path-for-reload-and-rebase ()
  "A stale response may move a document path for subsequent local recovery."
  (let ((buffer (org-note-document-test--buffer "local"))
        (rebase-buffer (org-note-document-test--buffer "local"))
        (remote (org-note-document-test--response
                 "document-a" "workspace-a" "notes/moved.org" "remote" 2)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'org-note-operation-lease-proofs)
                       (lambda (_document-id) nil))
                      ((symbol-function 'org-note-operation-put-document)
                       (lambda (&rest _arguments)
                         (signal 'org-note-http-error
                                 '((:status 409 :code stale_revision
                                    :message "stale" :details nil
                                    :retryable nil)))))
                      ((symbol-function 'org-note-operation-get-document)
                       (lambda (&rest _arguments) remote)))
              (should-error (org-note-document-save) :type 'org-note-http-error))
            (should (eq org-note-document--conflict remote))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (org-note-document-reload))
            (should (equal org-note-document-path "notes/moved.org")))
          (with-current-buffer rebase-buffer
            (setq-local org-note-document--conflict remote)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (org-note-document-rebase))
            (should (equal org-note-document-path "notes/moved.org"))))
      (dolist (candidate (list buffer rebase-buffer))
        (when (buffer-live-p candidate)
          (kill-buffer candidate))))))

(ert-deftest org-note-document-compare-latest-cleans-remote-on-ediff-quit ()
  "Ediff quit cleanup removes only the remote comparison buffer once."
  (let ((buffer (org-note-document-test--buffer "local")) control remote)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local org-note-document--conflict
                      (org-note-document-test--response
                       "document-a" "workspace-a" "notes/example.org"
                       "remote" 2))
          (setq control (generate-new-buffer " *org-note-ediff-control*"))
          (require 'ediff)
          (let ((ediff-control-buffer control))
            (cl-letf (((symbol-function 'ediff-buffers)
                       (lambda (_local _remote) nil)))
              (setq remote (org-note-document-compare-latest))))
          (with-current-buffer control
            (run-hooks 'ediff-after-quit-hook-internal)
            (should-not org-note-document--ediff-remote-buffer)
            (should-not (memq #'org-note-document--cleanup-ediff-remote-buffer
                              ediff-after-quit-hook-internal)))
          (should-not (buffer-live-p remote)))
      (dolist (candidate (list remote control buffer))
        (when (buffer-live-p candidate)
          (with-current-buffer candidate
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer candidate))))))

(ert-deftest org-note-document-compare-latest-cleans-partial-setup-errors ()
  "Remote setup errors leave no partial comparison buffer behind."
  (let ((buffer (org-note-document-test--buffer "local")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local org-note-document--conflict
                      (org-note-document-test--response
                       "document-a" "workspace-a" "notes/setup.org" "remote" 2))
          (cl-letf (((symbol-function 'org-mode)
                     (lambda () (error "remote setup failed"))))
            (should-error (org-note-document-compare-latest)))
          (should-not
           (cl-find-if (lambda (candidate)
                         (string-prefix-p "*Org Note Remote: notes/setup.org r2*"
                                          (buffer-name candidate)))
                       (buffer-list))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest org-note-document-reload-preserves-narrowing-and-rolls-back-errors ()
  "Reload keeps the view range and atomically rejects replacement hook failures."
  (let ((buffer (org-note-document-test--buffer "top\nbody\nbottom\n")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char 6)
          (narrow-to-region 5 10)
          (let ((conflict (org-note-document-test--response
                           "document-a" "workspace-a" "notes/example.org"
                           "top\nremote\nbottom\n" 2))
                (source (org-note-document--source))
                (point (point))
                (minimum (point-min))
                (maximum (point-max)))
            (setq-local org-note-document--conflict conflict)
            (add-hook 'after-change-functions
                      (lambda (&rest _arguments) (error "reload hook failed"))
                      nil t)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (should-error (org-note-document-reload)))
            (should (equal (org-note-document--source) source))
            (should (= (point) point))
            (should (= (point-min) minimum))
            (should (= (point-max) maximum))
            (should (eq org-note-document--conflict conflict))
            (should (= org-note-document-revision 1))
            (should (buffer-modified-p))
            (remove-hook 'after-change-functions
                         (car after-change-functions) t)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (org-note-document-reload))
            (should (equal (org-note-document--source) "top\nremote\nbottom\n"))
            (should (= (point-min) minimum))
            (should (= (point-max) maximum))
            (should (= (point) point))
            (should-not (buffer-modified-p))))
      (kill-buffer buffer))))

(defun org-note-document-test--http-response-buffer (status body)
  "Return an HTTP response buffer with STATUS and JSON BODY."
  (let ((buffer (generate-new-buffer " *org-note-http-response*")))
    (with-current-buffer buffer
      (insert (format "HTTP/1.1 %s Test\r\nContent-Type: application/json\r\n\r\n"
                      status))
      (setq-local url-http-response-status status
                  url-http-end-of-headers (point))
      (insert body)
      (goto-char (point-min)))
    buffer))

(ert-deftest org-note-document-save-real-client-caches-a-stale-conflict ()
  "A real client 409 followed by GET caches the latest document response."
  (let ((buffer (org-note-document-test--buffer "local"))
        (calls 0))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (lambda (&rest _arguments)
                       (setq calls (1+ calls))
                       (if (= calls 1)
                           (org-note-document-test--http-response-buffer
                            409
                            "{\"error\":{\"code\":\"stale_revision\",\"message\":\"stale\",\"details\":null,\"retryable\":false}}")
                         (org-note-document-test--http-response-buffer
                          200
                          "{\"workspace_id\":\"workspace-a\",\"id\":\"document-a\",\"path\":\"notes/example.org\",\"source\":\"remote\",\"content_hash\":\"remote-hash\",\"revision\":2}")))))
            (should-error (org-note-document-save) :type 'org-note-http-error))
          (should (= calls 2))
          (should (equal (org-note-document--response-value
                          org-note-document--conflict 'source)
                         "remote")))
      (kill-buffer buffer))))

(ert-deftest org-note-document-compare-latest-cold-loads-ediff-before-snapshot ()
  "Compare loads Ediff before reading its control buffer variable."
  (let* ((emacs (concat invocation-directory invocation-name))
         (directory (file-name-directory (locate-library "org-note-document")))
         (output (generate-new-buffer " *org-note-document-cold-ediff*"))
         (form
          "(progn
              (require 'org-note-document)
              (when (featurep 'ediff) (kill-emacs 2))
              (let ((original-require (symbol-function 'require)) remote buffer)
                (cl-letf (((symbol-function 'require)
                           (lambda (feature &optional filename noerror)
                             (let ((result (funcall original-require feature filename noerror)))
                               (when (eq feature 'ediff)
                                 (fset 'ediff-buffers (lambda (&rest _arguments) nil)))
                               result))))
                  (setq buffer (generate-new-buffer \" *org-note-cold*\"))
                  (with-current-buffer buffer
                    (org-note-document-mode)
                    (insert \"local\")
                    (setq-local org-note-document-workspace-id \"workspace-a\"
                                org-note-document-id \"document-a\"
                                org-note-document-path \"notes/example.org\"
                                org-note-document-revision 1
                                org-note-document--conflict
                                '((id . \"document-a\")
                                  (workspace_id . \"workspace-a\")
                                  (path . \"notes/example.org\")
                                  (source . \"remote\")
                                  (content_hash . \"hash\")
                                  (revision . 2)))
                    (setq remote (org-note-document-compare-latest)))
                  (unless (buffer-live-p remote) (kill-emacs 3))
                  (dolist (candidate (list remote buffer))
                    (when (buffer-live-p candidate)
                      (with-current-buffer candidate
                        (setq-local kill-buffer-query-functions nil))
                      (kill-buffer candidate))))))"))
    (unwind-protect
        (should (zerop (call-process emacs nil output nil
                                     "-Q" "--batch" "-L" directory
                                     "--eval" form)))
      (kill-buffer output))))

(provide 'org-note-document-test)

;;; org-note-document-test.el ends here
