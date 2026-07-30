;;; emacs-agent-search-test.el --- Project search tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for explicit project file discovery and text search.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'emacs-agent-project)
(require 'emacs-agent-runtime)
(require 'emacs-agent-search)

(defmacro emacs-agent-search-test--with-projects (&rest body)
  "Evaluate BODY with a runtime and two registered temporary projects."
  (declare (indent 0) (debug t))
  `(let* ((first-root
           (make-temp-file "emacs-agent-search-first-" t))
          (second-root
           (make-temp-file "emacs-agent-search-second-" t))
          (runtime (emacs-agent-runtime-create))
          (first-info
           (emacs-agent-project-open runtime first-root))
          (second-info
           (emacs-agent-project-open runtime second-root))
          (first-id (plist-get first-info :project_id))
          (second-id (plist-get second-info :project_id))
          (emacs-agent-search-cursors
           (make-hash-table :test #'equal))
          (emacs-agent-search-processes
           (make-hash-table :test #'eq)))
     (unwind-protect
         (progn
           (ignore first-id second-id)
           ,@body)
       (maphash
        (lambda (process _)
          (when (process-live-p process)
            (delete-process process))
          (when (buffer-live-p (process-buffer process))
            (kill-buffer (process-buffer process))))
        emacs-agent-search-processes)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when
               (or
                (file-in-directory-p
                 (expand-file-name file) first-root)
                (file-in-directory-p
                 (expand-file-name file) second-root))
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (emacs-agent-runtime-clear runtime)
       (delete-directory first-root t)
       (delete-directory second-root t))))

(defun emacs-agent-search-test--relative-paths (page)
  "Return relative paths from result PAGE."
  (mapcar
   (lambda (item)
     (plist-get item :relative_path))
   (plist-get page :results)))

(ert-deftest emacs-agent-project-files-uses-explicit-project-root ()
  (emacs-agent-search-test--with-projects
    (write-region "first\n" nil
                  (expand-file-name "first.el" first-root))
    (write-region "second\n" nil
                  (expand-file-name "second.el" second-root))
    (let ((original-directory default-directory)
          (page
           (emacs-agent-project-files runtime first-id)))
      (should (equal (emacs-agent-search-test--relative-paths page)
                     '("first.el")))
      (let ((item (car (plist-get page :results))))
        (should
         (equal
          (plist-get item :path)
          (file-truename
           (expand-file-name "first.el" first-root))))
        (should (equal (plist-get item :project_id) first-id))
        (should (equal (plist-get item :relative_path) "first.el")))
      (should (equal default-directory original-directory)))))

(ert-deftest emacs-agent-project-files-preserves-filtered-pagination ()
  (emacs-agent-search-test--with-projects
    (write-region "a\n" nil (expand-file-name "a.el" first-root))
    (write-region "b\n" nil (expand-file-name "b.el" first-root))
    (write-region "c\n" nil (expand-file-name "c.txt" first-root))
    (let* ((first
            (emacs-agent-project-files
             runtime first-id
             :include-globs '("*.el")
             :max-results 1))
           (cursor (plist-get first :next_cursor))
           (second
            (emacs-agent-project-files
             runtime first-id
             :include-globs '("*.el")
             :max-results 1
             :cursor cursor)))
      (should (equal (emacs-agent-search-test--relative-paths first)
                     '("a.el")))
      (should cursor)
      (should (equal (emacs-agent-search-test--relative-paths second)
                     '("b.el")))
      (should-not (plist-get second :next_cursor)))))

(ert-deftest emacs-agent-project-files-cursor-is-project-bound ()
  (emacs-agent-search-test--with-projects
    (write-region "a\n" nil (expand-file-name "a.el" first-root))
    (write-region "b\n" nil (expand-file-name "b.el" first-root))
    (let* ((first
            (emacs-agent-project-files
             runtime first-id :max-results 1))
           (cursor (plist-get first :next_cursor)))
      (should cursor)
      (should-error
       (emacs-agent-project-files
        runtime second-id :max-results 1 :cursor cursor)
       :type 'emacs-agent-invalid-cursor))))

(ert-deftest emacs-agent-project-files-requires-registered-project ()
  (emacs-agent-search-test--with-projects
    (should-error
     (emacs-agent-project-files runtime "project_missing")
     :type 'emacs-agent-error)))

(ert-deftest emacs-agent-project-search-is-root-scoped-and-canonical ()
  (emacs-agent-search-test--with-projects
    (write-region "needle first\n" nil
                  (expand-file-name "first.el" first-root))
    (write-region "needle second\n" nil
                  (expand-file-name "second.el" second-root))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) nil)))
      (let* ((page
              (emacs-agent-project-search
               runtime first-id "needle"))
             (items (plist-get page :results))
             (item (car items)))
        (should (= (length items) 1))
        (should
         (equal (plist-get item :path)
                (file-truename
                 (expand-file-name "first.el" first-root))))
        (should (equal (plist-get item :project_id) first-id))
        (should (equal (plist-get item :relative_path) "first.el"))
        (should (equal (plist-get item :source) "disk"))
        (should (eq (plist-get item :modified) :false))))))

(ert-deftest emacs-agent-project-search-dirty-buffer-shadows-disk ()
  (emacs-agent-search-test--with-projects
    (let* ((absolute (expand-file-name "dirty.el" first-root))
           (buffer nil))
      (write-region "disk-only\n" nil absolute)
      (setq buffer (find-file-noselect absolute))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "buffer-only needle\n"))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_program) nil)))
        (let* ((page
                (emacs-agent-project-search
                 runtime first-id "needle"))
               (items (plist-get page :results))
               (item (car items)))
          (should (= (length items) 1))
          (should (equal (plist-get item :path)
                         (file-truename absolute)))
          (should (equal (plist-get item :project_id) first-id))
          (should (equal (plist-get item :relative_path) "dirty.el"))
          (should (equal (plist-get item :source) "buffer"))
          (should (plist-get item :modified))
          (should (string-prefix-p
                   "rev:" (plist-get item :revision))))
        (should-not
         (plist-get
          (emacs-agent-project-search
           runtime first-id "disk-only")
          :results))))))

(ert-deftest emacs-agent-project-search-keeps-asynchronous-rg ()
  (skip-unless (executable-find "rg"))
  (emacs-agent-search-test--with-projects
    (write-region "async needle\n" nil
                  (expand-file-name "async.el" first-root))
    (let (result error-data)
      (let ((process
             (emacs-agent-project-search
              runtime first-id "needle"
              :callback
              (lambda (value failure)
                (setq result value
                      error-data failure)))))
        (should (processp process))
        (while (and (not result)
                    (not error-data)
                    (process-live-p process))
          (accept-process-output process 0.1))
        (unless (or result error-data)
          (accept-process-output process 0.1))
        (should-not error-data)
        (let ((item (car (plist-get result :results))))
          (should
           (equal
            (plist-get item :path)
            (file-truename
             (expand-file-name "async.el" first-root))))
          (should (equal (plist-get item :project_id) first-id))
          (should (equal (plist-get item :relative_path)
                         "async.el")))))))

(ert-deftest emacs-agent-search-clear-cancels-runtime-processes ()
  (emacs-agent-search-test--with-projects
    (let* ((other-runtime (emacs-agent-runtime-create))
           (first-buffer
            (generate-new-buffer " *emacs-agent-search-clear-first*"))
           (second-buffer
            (generate-new-buffer " *emacs-agent-search-clear-second*"))
           (first-process
            (make-pipe-process
             :name "emacs-agent-search-clear-first"
             :buffer first-buffer
             :noquery t))
           (second-process
            (make-pipe-process
             :name "emacs-agent-search-clear-second"
             :buffer second-buffer
             :noquery t)))
      (unwind-protect
          (progn
            (puthash first-process runtime emacs-agent-search-processes)
            (puthash second-process other-runtime
                     emacs-agent-search-processes)
            (should (emacs-agent-search-clear runtime))
            (should-not
             (gethash first-process emacs-agent-search-processes))
            (should-not (process-live-p first-process))
            (should-not (buffer-live-p first-buffer))
            (should
             (eq
              (gethash second-process emacs-agent-search-processes)
              other-runtime))
            (should (process-live-p second-process))
            (should (buffer-live-p second-buffer))
            (should (emacs-agent-search-clear))
            (should-not
             (gethash second-process emacs-agent-search-processes))
            (should-not (process-live-p second-process))
            (should-not (buffer-live-p second-buffer)))
        (dolist (process (list first-process second-process))
          (when (process-live-p process)
            (delete-process process)))
        (dolist (buffer (list first-buffer second-buffer))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(provide 'emacs-agent-search-test)
;;; emacs-agent-search-test.el ends here
