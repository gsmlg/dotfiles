;;; gsmlg-elfeed.el --- Elfeed configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure Elfeed with the tracked Org feed list and XDG-backed data.
;; Package declarations live in `gsmlg-app-packages' so Elpaca can realize the
;; lock during core startup.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)

(declare-function elfeed-goodies/setup "elfeed-goodies" ())
(declare-function elfeed-org "elfeed-org" ())
(defvar elfeed-db-directory)
(defvar rmh-elfeed-org-files)

(defgroup gsmlg-elfeed nil
  "GSMLG Elfeed settings."
  :group 'elfeed)

(defun gsmlg-elfeed--set-file (symbol value)
  "Set SYMBOL to normalized file VALUE."
  (set-default symbol (expand-file-name value))
  (when (featurep 'gsmlg-elfeed)
    (gsmlg-elfeed-apply-settings)))

(defun gsmlg-elfeed--set-directory (symbol value)
  "Set SYMBOL to normalized directory VALUE."
  (set-default symbol
               (file-name-as-directory (expand-file-name value)))
  (when (featurep 'gsmlg-elfeed)
    (gsmlg-elfeed-apply-settings)))

(defcustom gsmlg-elfeed-feed-file
  (expand-file-name "elfeed.org" gsmlg-config-directory)
  "Tracked Org file containing Elfeed subscriptions."
  :type 'file
  :set #'gsmlg-elfeed--set-file
  :group 'gsmlg-elfeed)

(defcustom gsmlg-elfeed-database-directory
  (file-name-as-directory
   (expand-file-name "elfeed/" gsmlg-data-directory))
  "Directory containing Elfeed's database."
  :type 'directory
  :set #'gsmlg-elfeed--set-directory
  :group 'gsmlg-elfeed)

(defun gsmlg-elfeed-apply-settings ()
  "Apply Elfeed file and data directory settings."
  (make-directory gsmlg-elfeed-database-directory t)
  (setq elfeed-db-directory gsmlg-elfeed-database-directory
        rmh-elfeed-org-files (list gsmlg-elfeed-feed-file)))

(use-package elfeed
  :ensure nil
  :commands elfeed
  :init
  (gsmlg-elfeed-apply-settings))

(use-package elfeed-goodies
  :ensure nil
  :after elfeed
  :config
  (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure nil
  :after elfeed
  :config
  (gsmlg-elfeed-apply-settings)
  (elfeed-org))

(provide 'gsmlg-elfeed)
;;; gsmlg-elfeed.el ends here
