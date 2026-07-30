;;; emacs-agent-transform-test.el --- High-level transform tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-agent-transform)
(require 'emacs-agent-project)

;;; Code:

(defun emacs-agent-transform-test--append-before-save ()
  "Append deterministic text once before saving the current buffer."
  (remove-hook 'before-save-hook
               #'emacs-agent-transform-test--append-before-save t)
  (goto-char (point-max))
  (insert "formatted by hook\n"))

(defmacro emacs-agent-transform-test-with-file (content &rest body)
  "Evaluate BODY with a manual-save runtime containing CONTENT."
  (declare (indent 1))
  `(let* ((root (make-temp-file "agent-transform-" t))
          (path (expand-file-name "file.txt" root))
          (runtime
           (emacs-agent-runtime-create :save-policy 'manual))
          target)
     (unwind-protect
         (progn
           (with-temp-file path (insert ,content))
           (setq target
                 (emacs-agent-project-resolve-target runtime path))
           ,@body)
       (when-let* ((buffer (get-file-buffer path)))
         (set-buffer-modified-p nil)
         (kill-buffer buffer))
       (emacs-agent-runtime-clear runtime)
       (delete-directory root t))))

(ert-deftest emacs-agent-transform-replaces-exact-unique-text ()
  (emacs-agent-transform-test-with-file "before old after\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (result
            (emacs-agent-transform-replace
             runtime target revision "old" "new")))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "before new after\n")))
      (should (plist-get result :applied))
      (should (= (plist-get result :match_count) 1))
      (should (string-match-p "^-before old after"
                              (plist-get result :diff))))))

(ert-deftest emacs-agent-transform-returns-project-relative-output-fields ()
  (emacs-agent-transform-test-with-file "before old after\n"
    (ignore target)
    (let* ((opened (emacs-agent-project-open runtime root))
           (project-id (plist-get opened :project_id))
           (project-target
            (emacs-agent-project-resolve-target
             runtime "file.txt" :project-id project-id))
           (document
            (emacs-agent-document-open runtime project-target))
           (revision (emacs-agent-document-revision document))
           (result
            (emacs-agent-transform-replace
             runtime project-target revision "old" "new")))
      (should (equal (plist-get result :path)
                     (file-truename path)))
      (should (equal (plist-get result :project_id)
                     project-id))
      (should (equal (plist-get result :relative_path)
                     "file.txt")))))

(ert-deftest emacs-agent-transform-replace-dry-run-preserves-buffer-and-revision ()
  (emacs-agent-transform-test-with-file "a λ\told 😀\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (result
            (emacs-agent-transform-replace
             runtime target revision "old" "new" :dry-run t)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "a λ\told 😀\n")))
      (should-not (plist-get result :applied))
      (should (equal (plist-get result :old_revision) revision))
      (should (equal (plist-get result :new_revision) revision))
      (should
       (equal (emacs-agent-document-revision document) revision))
      (should (string-match-p "new" (plist-get result :diff))))))

(ert-deftest emacs-agent-transform-replace-rejects-ambiguous-match-atomically ()
  (emacs-agent-transform-test-with-file "old and old\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-transform-replace
        runtime target revision "old" "new")
       :type 'emacs-agent-error)
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "old and old\n"))))))

(ert-deftest emacs-agent-transform-replace-all-enforces-occurrence-count ()
  (emacs-agent-transform-test-with-file "old old\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-transform-replace
        runtime target revision "old" "new"
        :replace-all t :expected-occurrences 3)
       :type 'emacs-agent-error)
      (should
       (equal
        (with-current-buffer (emacs-agent-document-buffer document)
          (buffer-string))
        "old old\n"))
      (let ((result
             (emacs-agent-transform-replace
              runtime target revision "old" "new"
              :replace-all t :expected-occurrences 2)))
        (should (= (plist-get result :match_count) 2))
        (should
         (equal
          (with-current-buffer (emacs-agent-document-buffer document)
            (buffer-string))
          "new new\n"))))))

(ert-deftest emacs-agent-transform-applies-strict-unified-patch ()
  (emacs-agent-transform-test-with-file "one\ntwo\nthree\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (canonical (file-truename path))
           (patch
            (concat
             "--- " canonical "\n"
             "+++ " canonical "\n"
             "@@ -1,3 +1,3 @@\n"
             " one\n"
             "-two\n"
             "+second\n"
             " three\n"))
           (result
            (emacs-agent-transform-apply-patch
             runtime target revision patch)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "one\nsecond\nthree\n")))
      (should (plist-get result :applied))
      (should (= (length (plist-get result :ranges)) 1))
      (should (string-match-p "^+second" (plist-get result :diff))))))

(ert-deftest emacs-agent-transform-patch-dry-run-matches-applied-diff ()
  (emacs-agent-transform-test-with-file "one\ntwo\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (patch "@@ -1,2 +1,2 @@\n one\n-two\n+second\n")
           (preview
            (emacs-agent-transform-apply-patch
             runtime target revision patch :dry-run t)))
      (should-not (plist-get preview :applied))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "one\ntwo\n")))
      (let ((applied
             (emacs-agent-transform-apply-patch
              runtime target revision patch)))
        (should (equal (plist-get preview :diff)
                       (plist-get applied :diff)))))))

(ert-deftest emacs-agent-transform-replace-dry-run-matches-applied-diff ()
  (emacs-agent-transform-test-with-file "前缀\t旧😀\n后缀\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (preview
            (emacs-agent-transform-replace
             runtime target revision "旧😀" "新🧪" :dry-run t)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "前缀\t旧😀\n后缀\n")))
      (let ((applied
             (emacs-agent-transform-replace
              runtime target revision "旧😀" "新🧪")))
        (should (equal (plist-get preview :diff)
                       (plist-get applied :diff)))
        (should
         (equal
          (with-current-buffer (emacs-agent-document-buffer document)
            (buffer-string))
          "前缀\t新🧪\n后缀\n"))))))

(ert-deftest emacs-agent-transform-no-op-reports-before-save-hook-change ()
  (emacs-agent-transform-test-with-file "old\n"
    (let* ((document
            (emacs-agent-document-open
             runtime target))
           (revision
            (emacs-agent-document-revision
             document))
           result)
      (with-current-buffer
          (emacs-agent-document-buffer document)
        (set-buffer-modified-p t)
        (add-hook
         'before-save-hook
         #'emacs-agent-transform-test--append-before-save
         nil t))
      (setq result
            (emacs-agent-transform-replace
             runtime target revision
             "old" "old" :checkpoint t))
      (should (plist-get result :applied))
      (should (plist-get result :modified))
      (should (plist-get result :checkpointed))
      (should (stringp
               (plist-get result :changeset_id)))
      (should
       (string-match-p
        "^+formatted by hook"
        (plist-get result :diff)))
      (should
       (equal
        (with-current-buffer
            (emacs-agent-document-buffer document)
          (buffer-string))
        "old\nformatted by hook\n"))
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
        "old\nformatted by hook\n")))))

(ert-deftest emacs-agent-transform-replace-preserves-crlf-coding ()
  (let* ((root (make-temp-file "agent-transform-crlf-" t))
         (path (expand-file-name "file.txt" root))
         (runtime
          (emacs-agent-runtime-create :save-policy 'immediate))
         target)
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-file-coding-system 'utf-8-dos)
            (insert "标题\t旧😀\n第二行\n")
            (write-region (point-min) (point-max) path))
          (setq target
                (emacs-agent-project-resolve-target runtime path))
          (let* ((document
                  (emacs-agent-document-open runtime target))
                 (revision (emacs-agent-document-revision document)))
            (emacs-agent-transform-replace
             runtime target revision "旧😀" "新🧪" :checkpoint t)
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally path)
              (should (string-match-p
                       (regexp-quote
                        (encode-coding-string
                         "标题\t新🧪\r\n第二行\r\n" 'utf-8-unix t))
                       (buffer-string))))))
      (when-let* ((buffer (get-file-buffer path)))
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (emacs-agent-runtime-clear runtime)
      (delete-directory root t))))

(ert-deftest emacs-agent-transform-rejects-patch-path-escape ()
  (emacs-agent-transform-test-with-file "before\n"
    (let* ((document (emacs-agent-document-open runtime target))
           (revision (emacs-agent-document-revision document))
           (canonical (file-truename path))
           (patch
            (concat
             "--- " canonical "\n"
             "+++ "
             (expand-file-name
              "escaped.txt"
              (file-name-directory root))
             "\n"
             "@@ -1 +1 @@\n"
             "-before\n"
             "+after\n")))
      (should-error
       (emacs-agent-transform-apply-patch
        runtime target revision patch)
       :type 'emacs-agent-error)
      (should-not
       (file-exists-p
        (expand-file-name "escaped.txt" (file-name-directory root))))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "before\n"))))))

(provide 'emacs-agent-transform-test)
;;; emacs-agent-transform-test.el ends here
