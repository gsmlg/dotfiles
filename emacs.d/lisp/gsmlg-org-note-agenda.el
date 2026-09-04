;;; gsmlg-org-note-agenda.el --- Compatibility shim for Org Note agenda -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin compatibility require.  Feed ownership and refresh live in
;; `gsmlg-org-note-org'.  Callers should migrate to that module.

;;; Code:

(require 'gsmlg-org-note-org)

(defalias 'gsmlg-org-note-agenda-feed-file #'gsmlg-org-note-org-feed-file)
(defalias 'gsmlg-org-note-agenda-refresh-feed #'gsmlg-org-note-org-refresh-feed)
(defalias 'gsmlg-org-note-agenda--item-headline #'gsmlg-org-note-org--item-headline)
(defalias 'gsmlg-org-note-agenda--item-text #'gsmlg-org-note-org--item-text)
(defalias 'gsmlg-org-note-agenda--goto #'gsmlg-org-note-org--goto)

(defvaralias 'gsmlg-org-note-agenda--feed-file 'gsmlg-org-note-org--feed-file)
(defvaralias 'gsmlg-org-note-agenda--last-workspace-ids
  'gsmlg-org-note-org--last-workspace-ids)

(defun gsmlg-org-note-agenda-expanded-files (_source)
  "Return feed-only agenda files for compatibility callers.

SOURCE is ignored; the bridge no longer appends local agenda sources."
  (gsmlg-org-note-org-agenda-files))

(defun gsmlg-org-note-agenda-activate ()
  "Compatibility wrapper around `gsmlg-org-note-org-activate'."
  (gsmlg-org-note-org-activate))

(provide 'gsmlg-org-note-agenda)
;;; gsmlg-org-note-agenda.el ends here
