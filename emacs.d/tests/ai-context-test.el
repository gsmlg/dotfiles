;;; ai-context-test.el --- Offline AI context tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Context, snapshot, and fallback behavior without network access.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-ai-context)

(defun gsmlg-ai-test--with-temp-file (content fn)
  "Call FN with a temporary file containing CONTENT."
  (let ((file (make-temp-file "gsmlg-ai-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert content))
          (funcall fn file))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest gsmlg-ai-context-captures-unsaved-buffer ()
  "Snapshots use live unsaved buffer text, not disk."
  (gsmlg-ai-context-clear-all t)
  (gsmlg-ai-test--with-temp-file
   "disk\n"
   (lambda (file)
     (with-current-buffer (find-file-noselect file)
       (erase-buffer)
       (insert "unsaved-live\n")
       (gsmlg-ai-context-add-current-buffer)
       (let ((snap (car (gsmlg-ai-context-snapshot-entries
                         (gsmlg-ai-context-current-entries)))))
         (should (equal (gsmlg-ai-snapshot-original-content snap)
                        "unsaved-live\n")))
       (set-buffer-modified-p nil)
       (kill-buffer (current-buffer))))))

(ert-deftest gsmlg-ai-context-snapshot-widens ()
  "Snapshots widen before capturing buffer text."
  (gsmlg-ai-context-clear-all t)
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (narrow-to-region (point-min) 4)
    (gsmlg-ai-context-add-current-buffer)
    (let ((snap (car (gsmlg-ai-context-snapshot-entries
                      (gsmlg-ai-context-current-entries)))))
      (should (equal (gsmlg-ai-snapshot-original-content snap)
                     "one\ntwo\nthree\n")))))

(ert-deftest gsmlg-ai-context-rejects-binary ()
  "NUL bytes are rejected."
  (gsmlg-ai-context-clear-all t)
  (with-temp-buffer
    (insert "a\0b")
    (should-error (gsmlg-ai-context-add-current-buffer))))

(ert-deftest gsmlg-ai-context-deduplicates-files ()
  "File-backed entries are deduplicated by identity."
  (gsmlg-ai-context-clear-all t)
  (gsmlg-ai-test--with-temp-file
   "same\n"
   (lambda (file)
     (gsmlg-ai-context-add-files (list file file))
     (should (= 1 (length (gsmlg-ai-context-current-entries)))))))

(ert-deftest gsmlg-ai-context-region-is-read-only ()
  "Region entries are not editable mutation targets."
  (gsmlg-ai-context-clear-all t)
  (with-temp-buffer
    (insert "alpha beta")
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (gsmlg-ai-context-add-current-region)
    (should-not
     (gsmlg-ai-context-entry-editable-p
      (car (gsmlg-ai-context-current-entries))))))

(provide 'ai-context-test)
;;; ai-context-test.el ends here
