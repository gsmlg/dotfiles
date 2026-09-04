;;; org-note-agenda-test.el --- Org Note agenda integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for Org Note data inside Org agenda via the bridge module.

;;; Code:

(require 'cl-lib)
(require 'ert)

(declare-function gsmlg-org-note-org-agenda-files "gsmlg-org-note-org" ())
(declare-function gsmlg-org-note-org-refresh-feed "gsmlg-org-note-org"
                  (&optional force))
(declare-function gsmlg-org-note-org--item-headline "gsmlg-org-note-org" (item))
(declare-function gsmlg-org-note-org--goto "gsmlg-org-note-org"
                  (orig-fun &optional highlight))

(unless (require 'gsmlg-paths nil t)
  (defvar gsmlg-cache-directory
    (file-name-as-directory
     (make-temp-file "gsmlg-org-note-agenda-cache-" t)))
  (defun gsmlg-cache-file (name)
    (expand-file-name name gsmlg-cache-directory))
  (defun gsmlg-ensure-parent-directory (file)
    (make-directory (file-name-directory file) t)
    file)
  (provide 'gsmlg-paths))

(cl-letf (((symbol-function #'use-package-ensure-elpa) #'ignore))
  (require 'gsmlg-paths)
  (require 'org-note)
  (require 'gsmlg-org-note-org))

(defun org-note-agenda-test--item (id workspace-id &optional title)
  "Return a minimal Org Note item alist."
  `((id . ,id)
    (workspace_id . ,workspace-id)
    (document_id . "document-a")
    (item_type . "task")
    (title . ,(or title "Ship console"))
    (state . "RUNNING")
    (priority . "A")
    (scheduled . ((raw . "<2026-08-13 Thu 09:00>")))
    (deadline . ((raw . "<2026-08-14 Fri 18:00>")))
    (tags . ("ops"))
    (requires_review . t)
    (created_at . 1786500000)))

(ert-deftest gsmlg-org-note-agenda-feed-includes-scheduled-and-deadline-items ()
  "The generated feed should expose Org Note scheduled and deadline items."
  (require 'org-note)
  (let ((feed-file (make-temp-file "gsmlg-org-note-feed-" nil ".org"))
        (query-count 0)
        (saved-feed-file gsmlg-org-note-org--feed-file)
        (saved-selected gsmlg-org-note-org--selected-feed-file)
        (saved-workspaces (and (boundp 'org-note-agenda-workspace-ids)
                               org-note-agenda-workspace-ids))
        (saved-last-workspaces gsmlg-org-note-org--last-workspace-ids)
        (saved-query-fn (symbol-function 'org-note-operation-query-agenda)))
    (unwind-protect
        (progn
          (setq gsmlg-org-note-org--feed-file feed-file
                gsmlg-org-note-org--selected-feed-file nil
                org-note-agenda-workspace-ids '("workspace-a")
                gsmlg-org-note-org--last-workspace-ids nil)
          (fset 'org-note-operation-query-agenda
                (lambda (&key workspace-ids view &rest _args)
                  (cl-incf query-count)
                  `((items . (((item . ,(org-note-agenda-test--item
                                          "item-a"
                                          (car workspace-ids))))))
                    (next_cursor . nil))))
          (gsmlg-org-note-org-refresh-feed t)
          (should (= query-count 2))
          (should (file-readable-p feed-file))
          (with-temp-buffer
            (insert-file-contents feed-file)
            (goto-char (point-min))
            (should (search-forward "SCHEDULED: <2026-08-13 Thu 09:00>" nil t))
            (goto-char (point-min))
            (should (search-forward "DEADLINE: <2026-08-14 Fri 18:00>" nil t))
            (goto-char (point-min))
            (should (search-forward ":ORGNOTE:ops:" nil t))
            (goto-char (point-min))
            (should (search-forward "ORG_NOTE_WORKSPACE_ID: workspace-a" nil t))))
      (fset 'org-note-operation-query-agenda saved-query-fn)
      (setq gsmlg-org-note-org--feed-file saved-feed-file
            gsmlg-org-note-org--selected-feed-file saved-selected
            gsmlg-org-note-org--last-workspace-ids saved-last-workspaces)
      (when (boundp 'org-note-agenda-workspace-ids)
        (setq org-note-agenda-workspace-ids saved-workspaces))
      (when (file-readable-p feed-file)
        (delete-file feed-file)))))

(ert-deftest gsmlg-org-note-org-agenda-files-exclude-local-sources ()
  "Bridge agenda files must be feed-only and ignore local sources."
  (let ((local-file (make-temp-file "gsmlg-org-local-" nil ".org"))
        (feed-file (make-temp-file "gsmlg-org-note-feed-" nil ".org"))
        (saved-feed-file gsmlg-org-note-org--feed-file)
        (saved-selected gsmlg-org-note-org--selected-feed-file))
    (unwind-protect
        (progn
          (setq gsmlg-org-note-org--feed-file feed-file
                gsmlg-org-note-org--selected-feed-file nil)
          (should (equal (gsmlg-org-note-org-agenda-files)
                         (list feed-file)))
          (should-not (member local-file (gsmlg-org-note-org-agenda-files))))
      (setq gsmlg-org-note-org--feed-file saved-feed-file
            gsmlg-org-note-org--selected-feed-file saved-selected)
      (delete-file local-file)
      (delete-file feed-file))))

(ert-deftest gsmlg-org-note-agenda-goto-opens-item-context ()
  "Agenda goto should open Org Note item context for feed entries."
  (let ((called nil))
    (cl-letf (((symbol-function 'org-note-item-context)
               (lambda (workspace-id item-id)
                 (setq called (list workspace-id item-id))))
              ((symbol-function 'org-agenda-goto) #'ignore))
      (with-temp-buffer
        (org-mode)
        (insert "* RUNNING Ship console :ORGNOTE:\n"
                ":PROPERTIES:\n"
                ":ORG_NOTE_WORKSPACE_ID: workspace-a\n"
                ":ORG_NOTE_ITEM_ID: item-a\n"
                ":END:\n")
        (org-back-to-heading t)
        (let ((marker (copy-marker (point))))
          (with-current-buffer (get-buffer-create "*Org Agenda Test*")
            (org-mode)
            (insert "  feed:RUNNING Ship console\n")
            (put-text-property (point-min) (point-max) 'org-marker marker)
            (goto-char (point-min))
            (gsmlg-org-note-org--goto #'org-agenda-goto)
            (should (equal called '("workspace-a" "item-a")))))))))

(ert-deftest gsmlg-org-note-agenda-skip-tag-matches-feed ()
  "Generated feed entries should use the ORGNOTE tag."
  (should (string-match-p ":ORGNOTE:"
                          (gsmlg-org-note-org--item-headline
                           (org-note-agenda-test--item "item-a" "workspace-a")))))

(provide 'org-note-agenda-test)
;;; org-note-agenda-test.el ends here
