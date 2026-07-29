;;; emacs-agent-transform-test.el --- High-level transform tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-agent-transform)
(require 'emacs-agent-workspace)

(defmacro emacs-agent-transform-test-with-file (content &rest body)
  "Evaluate BODY with a manual-save workspace containing CONTENT."
  (declare (indent 1))
  `(let* ((root (make-temp-file "agent-transform-" t))
          (path (expand-file-name "file.txt" root))
          (workspace nil))
     (unwind-protect
         (progn
           (with-temp-file path (insert ,content))
           (setq workspace
                 (emacs-agent-workspace-create
                  root :workspace-id
                  (format "test-%s" (secure-hash 'sha256 root))
                  :save-policy 'manual))
           ,@body)
       (when-let* ((buffer (get-file-buffer path)))
         (set-buffer-modified-p nil)
         (kill-buffer buffer))
       (when workspace
         (remhash (emacs-agent-workspace-workspace-id workspace)
                  emacs-agent-workspace-registry))
       (delete-directory root t))))

(ert-deftest emacs-agent-transform-replaces-exact-unique-text ()
  (emacs-agent-transform-test-with-file "before old after\n"
    (let* ((document (emacs-agent-document-open workspace "file.txt"))
           (revision (emacs-agent-document-revision document))
           (result
            (emacs-agent-transform-replace
             workspace "file.txt" revision "old" "new")))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "before new after\n")))
      (should (plist-get result :applied))
      (should (= (plist-get result :match_count) 1))
      (should (string-match-p "^-before old after"
                              (plist-get result :diff))))))

(ert-deftest emacs-agent-transform-replace-dry-run-preserves-buffer-and-revision ()
  (emacs-agent-transform-test-with-file "a λ\told 😀\n"
    (let* ((document (emacs-agent-document-open workspace "file.txt"))
           (revision (emacs-agent-document-revision document))
           (result
            (emacs-agent-transform-replace
             workspace "file.txt" revision "old" "new" :dry-run t)))
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
    (let* ((document (emacs-agent-document-open workspace "file.txt"))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-transform-replace
        workspace "file.txt" revision "old" "new")
       :type 'emacs-agent-error)
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "old and old\n"))))))

(ert-deftest emacs-agent-transform-replace-all-enforces-occurrence-count ()
  (emacs-agent-transform-test-with-file "old old\n"
    (let* ((document (emacs-agent-document-open workspace "file.txt"))
           (revision (emacs-agent-document-revision document)))
      (should-error
       (emacs-agent-transform-replace
        workspace "file.txt" revision "old" "new"
        :replace-all t :expected-occurrences 3)
       :type 'emacs-agent-error)
      (should
       (equal
        (with-current-buffer (emacs-agent-document-buffer document)
          (buffer-string))
        "old old\n"))
      (let ((result
             (emacs-agent-transform-replace
              workspace "file.txt" revision "old" "new"
              :replace-all t :expected-occurrences 2)))
        (should (= (plist-get result :match_count) 2))
        (should
         (equal
          (with-current-buffer (emacs-agent-document-buffer document)
            (buffer-string))
          "new new\n"))))))

(ert-deftest emacs-agent-transform-applies-strict-unified-patch ()
  (emacs-agent-transform-test-with-file "one\ntwo\nthree\n"
    (let* ((document (emacs-agent-document-open workspace "file.txt"))
           (revision (emacs-agent-document-revision document))
           (patch
            (concat "--- a/file.txt\n"
                    "+++ b/file.txt\n"
                    "@@ -1,3 +1,3 @@\n"
                    " one\n"
                    "-two\n"
                    "+second\n"
                    " three\n"))
           (result
            (emacs-agent-transform-apply-patch
             workspace "file.txt" revision patch)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "one\nsecond\nthree\n")))
      (should (plist-get result :applied))
      (should (= (length (plist-get result :ranges)) 1))
      (should (string-match-p "^+second" (plist-get result :diff))))))

(ert-deftest emacs-agent-transform-patch-dry-run-matches-applied-diff ()
  (emacs-agent-transform-test-with-file "one\ntwo\n"
    (let* ((document (emacs-agent-document-open workspace "file.txt"))
           (revision (emacs-agent-document-revision document))
           (patch "@@ -1,2 +1,2 @@\n one\n-two\n+second\n")
           (preview
            (emacs-agent-transform-apply-patch
             workspace "file.txt" revision patch :dry-run t)))
      (should-not (plist-get preview :applied))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "one\ntwo\n")))
      (let ((applied
             (emacs-agent-transform-apply-patch
              workspace "file.txt" revision patch)))
        (should (equal (plist-get preview :diff)
                       (plist-get applied :diff)))))))

(ert-deftest emacs-agent-transform-replace-dry-run-matches-applied-diff ()
  (emacs-agent-transform-test-with-file "前缀\t旧😀\n后缀\n"
    (let* ((document (emacs-agent-document-open workspace "file.txt"))
           (revision (emacs-agent-document-revision document))
           (preview
            (emacs-agent-transform-replace
             workspace "file.txt" revision "旧😀" "新🧪" :dry-run t)))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "前缀\t旧😀\n后缀\n")))
      (let ((applied
             (emacs-agent-transform-replace
              workspace "file.txt" revision "旧😀" "新🧪")))
        (should (equal (plist-get preview :diff)
                       (plist-get applied :diff)))
        (should
         (equal
          (with-current-buffer (emacs-agent-document-buffer document)
            (buffer-string))
          "前缀\t新🧪\n后缀\n"))))))

(ert-deftest emacs-agent-transform-replace-preserves-crlf-coding ()
  (let* ((root (make-temp-file "agent-transform-crlf-" t))
         (path (expand-file-name "file.txt" root))
         workspace)
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-file-coding-system 'utf-8-dos)
            (insert "标题\t旧😀\n第二行\n")
            (write-region (point-min) (point-max) path))
          (setq workspace
                (emacs-agent-workspace-create
                 root :workspace-id
                 (format "test-%s" (secure-hash 'sha256 root))
                 :save-policy 'immediate))
          (let* ((document
                  (emacs-agent-document-open workspace "file.txt"))
                 (revision (emacs-agent-document-revision document)))
            (emacs-agent-transform-replace
             workspace "file.txt" revision "旧😀" "新🧪" :checkpoint t)
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
      (when workspace
        (remhash (emacs-agent-workspace-workspace-id workspace)
                 emacs-agent-workspace-registry))
      (delete-directory root t))))

(ert-deftest emacs-agent-transform-rejects-patch-path-escape ()
  (emacs-agent-transform-test-with-file "before\n"
    (let* ((document (emacs-agent-document-open workspace "file.txt"))
           (revision (emacs-agent-document-revision document))
           (patch
            (concat "--- a/file.txt\n"
                    "+++ b/../escaped.txt\n"
                    "@@ -1 +1 @@\n"
                    "-before\n"
                    "+after\n")))
      (should-error
       (emacs-agent-transform-apply-patch
        workspace "file.txt" revision patch)
       :type 'emacs-agent-error)
      (should-not
       (file-exists-p
        (expand-file-name "escaped.txt" (file-name-directory root))))
      (with-current-buffer (emacs-agent-document-buffer document)
        (should (equal (buffer-string) "before\n"))))))

(provide 'emacs-agent-transform-test)
;;; emacs-agent-transform-test.el ends here
