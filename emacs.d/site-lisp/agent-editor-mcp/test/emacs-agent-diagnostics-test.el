;;; emacs-agent-diagnostics-test.el --- Diagnostics tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-agent-diagnostics)
(require 'emacs-agent-project)
(require 'flymake)

;;; Code:

(defun emacs-agent-diagnostics-test--flymake-backend (report-fn &rest _args)
  "Report one deterministic diagnostic through REPORT-FN."
  (funcall
   report-fn
   (list
    (flymake-make-diagnostic
     (current-buffer) (point-min) (min (1+ (point-min)) (point-max))
     :warning "Flymake warning"))))

(defun emacs-agent-diagnostics-test--pending-flymake-backend
    (_report-fn &rest _args)
  "Leave a Flymake run pending to test bounded waiting.")

(defun emacs-agent-diagnostics-test--target
    (runtime project-id path)
  "Resolve PATH in PROJECT-ID for test RUNTIME."
  (emacs-agent-project-resolve-target
   runtime path :project-id project-id))

(defmacro emacs-agent-diagnostics-test-with-files (files &rest body)
  "Evaluate BODY in a temporary project populated with FILES.
FILES is an alist mapping relative file names to contents."
  (declare (indent 1))
  `(let* ((root (make-temp-file "agent-diagnostics-" t))
          (runtime (emacs-agent-runtime-create))
          project-id)
     (unwind-protect
         (progn
           (dolist (file ,files)
             (let ((path (expand-file-name (car file) root)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path
                 (insert (cdr file)))))
           (setq project-id
                 (plist-get
                  (emacs-agent-project-open runtime root)
                  :project_id))
           (ignore project-id)
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file root)
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (emacs-agent-runtime-clear runtime)
       (delete-directory root t))))

(ert-deftest emacs-agent-diagnostics-elisp-sees-unsaved-buffer ()
  (emacs-agent-diagnostics-test-with-files
   '(("broken.el" . "(message \"disk\")\n"))
   (let* ((document (emacs-agent-document-open runtime
                                               (emacs-agent-diagnostics-test--target
                                                runtime project-id "broken.el")))
          (buffer (emacs-agent-document-buffer document)))
     (with-current-buffer buffer
       (erase-buffer)
       (insert "(message \"unsaved\"\n"))
     (let* ((revision (emacs-agent-document-revision document))
            (result
             (emacs-agent-document-diagnostics
              runtime
              (emacs-agent-diagnostics-test--target
               runtime project-id "broken.el")
              :expected-revision revision
              :sources '("parser")))
            (diagnostic (car (plist-get result :diagnostics))))
       (should (equal (plist-get result :document_revision) revision))
       (should (equal (plist-get result :diagnostics_revision) revision))
       (should-not (plist-get result :stale))
       (should (equal (plist-get diagnostic :source) "parser"))
       (should (equal (plist-get diagnostic :severity) "error"))
       (should (stringp (plist-get diagnostic :message)))
       (should (equal
                (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "broken.el" root))
                  (buffer-string))
                "(message \"disk\")\n"))))))

(ert-deftest emacs-agent-diagnostics-json-parser-reports-invalid-json ()
  (emacs-agent-diagnostics-test-with-files
   '(("broken.json" . "{\"disk\": true}\n"))
   (let* ((document (emacs-agent-document-open runtime
                                               (emacs-agent-diagnostics-test--target
                                                runtime project-id "broken.json")))
          (buffer (emacs-agent-document-buffer document)))
     (with-current-buffer buffer
       (erase-buffer)
       (insert "{\"unsaved\": ]}\n"))
     (let* ((result
             (emacs-agent-document-diagnostics
              runtime
              (emacs-agent-diagnostics-test--target
               runtime project-id "broken.json") :sources '("parser")))
            (diagnostic (car (plist-get result :diagnostics))))
       (should (= (length (plist-get result :diagnostics)) 1))
       (should (equal (plist-get diagnostic :source) "parser"))
       (should (equal (plist-get diagnostic :code) "invalid_json"))
       (should (plist-get diagnostic :range))))))

(ert-deftest emacs-agent-diagnostics-supports-direct-absolute-target ()
  (emacs-agent-diagnostics-test-with-files
   '(("broken.json" . "{\"broken\": ]}\n"))
   (let* ((path (expand-file-name "broken.json" root))
          (target (emacs-agent-project-resolve-target runtime path))
          (result
           (emacs-agent-document-diagnostics
            runtime target :sources '("parser"))))
     (should (equal (plist-get result :path) (file-truename path)))
     (should-not (plist-get result :project_id))
     (should-not (plist-get result :relative_path))
     (should (= (length (plist-get result :diagnostics)) 1)))))

(ert-deftest emacs-agent-diagnostics-rejects-stale-expected-revision ()
  (emacs-agent-diagnostics-test-with-files
   '(("valid.el" . "(message \"ok\")\n"))
   (let ((condition
          (should-error
           (emacs-agent-document-diagnostics
            runtime
            (emacs-agent-diagnostics-test--target
             runtime project-id "valid.el")
            :expected-revision "rev:stale"
            :sources '("parser"))
           :type 'emacs-agent-error)))
     (should (eq (emacs-agent-error-code condition) 'revision_conflict))
     (should (plist-get
              (emacs-agent-error-details condition)
              :requires_reread)))))

(ert-deftest emacs-agent-diagnostics-reports-unavailable-provider ()
  (emacs-agent-diagnostics-test-with-files
   '(("valid.el" . "(message \"ok\")\n"))
   (let ((condition
          (should-error
           (emacs-agent-document-diagnostics
            runtime
            (emacs-agent-diagnostics-test--target
             runtime project-id "valid.el") :sources '("eglot"))
           :type 'emacs-agent-error)))
     (should
      (eq (emacs-agent-error-code condition) 'capability_unavailable))
     (should
      (equal (plist-get (emacs-agent-error-details condition) :source)
             "eglot")))))

(ert-deftest emacs-agent-diagnostics-collects-enabled-flymake ()
  (emacs-agent-diagnostics-test-with-files
   '(("file.txt" . "content\n"))
   (let* ((document (emacs-agent-document-open runtime
                                               (emacs-agent-diagnostics-test--target
                                                runtime project-id "file.txt")))
          (buffer (emacs-agent-document-buffer document)))
     (with-current-buffer buffer
       (setq-local
        flymake-diagnostic-functions
        '(emacs-agent-diagnostics-test--flymake-backend))
       (flymake-mode 1)
       (flymake-start nil t))
     (let* ((result
             (emacs-agent-document-diagnostics
              runtime
              (emacs-agent-diagnostics-test--target
               runtime project-id "file.txt")
              :sources '("flymake")
              :wait-ms 250))
            (diagnostic (car (plist-get result :diagnostics))))
       (should (equal (plist-get diagnostic :source) "flymake"))
       (should (equal (plist-get diagnostic :severity) "warning"))
       (should (equal (plist-get diagnostic :message) "Flymake warning"))
       (should (plist-get diagnostic :range))))))

(ert-deftest emacs-agent-diagnostics-bounds-provider-wait ()
  (emacs-agent-diagnostics-test-with-files
   '(("file.txt" . "content\n"))
   (let* ((document (emacs-agent-document-open runtime
                                               (emacs-agent-diagnostics-test--target
                                                runtime project-id "file.txt")))
          (buffer (emacs-agent-document-buffer document)))
     (with-current-buffer buffer
       (setq-local
        flymake-diagnostic-functions
        '(emacs-agent-diagnostics-test--pending-flymake-backend))
       (flymake-mode 1)
       (flymake-start nil t))
     (let* ((started (float-time))
            (result
             (emacs-agent-document-diagnostics
              runtime
              (emacs-agent-diagnostics-test--target
               runtime project-id "file.txt")
              :sources '("flymake")
              :wait-ms 10)))
       (should (plist-get result :pending))
       (should (< (- (float-time) started) 0.5))))))

(ert-deftest emacs-agent-diagnostics-marks-result-stale-after-buffer-change ()
  (emacs-agent-diagnostics-test-with-files
   '(("file.txt" . "content\n"))
   (let* ((document (emacs-agent-document-open runtime
                                               (emacs-agent-diagnostics-test--target
                                                runtime project-id "file.txt")))
          (buffer (emacs-agent-document-buffer document)))
     (with-current-buffer buffer
       (setq-local
        flymake-diagnostic-functions
        '(emacs-agent-diagnostics-test--pending-flymake-backend))
       (flymake-mode 1)
       (flymake-start nil t))
     (run-at-time
      0.005 nil
      (lambda ()
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "changed\n"))))
     (let ((result
            (emacs-agent-document-diagnostics
             runtime
             (emacs-agent-diagnostics-test--target
              runtime project-id "file.txt")
             :sources '("flymake")
             :wait-ms 40)))
       (should (plist-get result :stale))
       (should-not
        (equal (plist-get result :diagnostics_revision)
               (plist-get result :document_revision)))))))

(ert-deftest emacs-agent-project-diagnostics-aggregates-selected-files ()
  (emacs-agent-diagnostics-test-with-files
   '(("broken.el" . "(message \"broken\"\n")
     ("broken.json" . "{\"broken\": ]}\n")
     ("ignored.el" . "(message \"ignored\"\n"))
   (let* ((result
           (emacs-agent-project-diagnostics
            runtime project-id
            :paths '("broken.el" "broken.json")
            :sources '("parser")))
          (summary (plist-get result :summary))
          (diagnostics (plist-get result :diagnostics)))
     (should (= (plist-get result :document_count) 2))
     (should (= (plist-get summary :error) 2))
     (should (= (length diagnostics) 2))
     (should
      (equal (sort (mapcar (lambda (item) (plist-get item :path))
                           diagnostics)
                   #'string<)
             (sort
              (mapcar
               (lambda (path)
                 (file-truename (expand-file-name path root)))
               '("broken.el" "broken.json"))
              #'string<)))
     (should
      (cl-every
       (lambda (item)
         (and (equal (plist-get item :project_id) project-id)
              (stringp (plist-get item :relative_path))))
       diagnostics)))))

(ert-deftest emacs-agent-project-diagnostics-filters-and-paginates-files ()
  (emacs-agent-diagnostics-test-with-files
   '(("a.el" . "(message \"a\"\n")
     ("b.el" . "(message \"b\"\n")
     ("ignored.el" . "(message \"ignored\"\n")
     ("data.json" . "{\"ok\": true}\n"))
   (let* ((first
           (emacs-agent-project-diagnostics
            runtime project-id
            :include-globs '("*.el")
            :exclude-globs '("ignored*")
            :sources '("parser")
            :limit 1))
          (second
           (emacs-agent-project-diagnostics
            runtime project-id
            :include-globs '("*.el")
            :exclude-globs '("ignored*")
            :sources '("parser")
            :limit 1
            :cursor (plist-get first :next_cursor))))
     (should (= (plist-get first :document_count) 1))
     (should (stringp (plist-get first :next_cursor)))
     (should (= (plist-get second :document_count) 1))
     (should-not (plist-get second :next_cursor))
     (should
      (equal
       (sort
        (list (plist-get (car (plist-get first :documents)) :path)
              (plist-get (car (plist-get second :documents)) :path))
        #'string<)
       (sort
        (mapcar
         (lambda (path)
           (file-truename (expand-file-name path root)))
         '("a.el" "b.el"))
        #'string<))))))

(ert-deftest emacs-agent-project-diagnostics-requires-known-project ()
  (let ((runtime (emacs-agent-runtime-create)))
    (unwind-protect
        (dolist (project-id '(nil "project_missing"))
          (condition-case error-data
              (progn
                (emacs-agent-project-diagnostics
                 runtime project-id)
                (ert-fail "Expected project_not_found"))
            (emacs-agent-error
             (should
              (eq (emacs-agent-error-code error-data)
                  'project_not_found)))))
      (emacs-agent-runtime-clear runtime))))

(provide 'emacs-agent-diagnostics-test)
;;; emacs-agent-diagnostics-test.el ends here
