;;; org-note-org-bridge-agenda-test.el --- Org Note Org bridge tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for Org Note Org bridge cold-start advice and activation.

;;; Code:

(require 'cl-lib)
(require 'ert)

(declare-function gsmlg-org-note-org-install-guards "gsmlg-org-note-org" ())
(declare-function gsmlg-org-note-org-activate "gsmlg-org-note-org" ())
(declare-function gsmlg-org-note-org-feed-file "gsmlg-org-note-org" ())
(declare-function gsmlg-org-note-org-agenda-files "gsmlg-org-note-org" ())
(declare-function gsmlg-org-note-org-refresh-feed "gsmlg-org-note-org"
                  (&optional force))
(declare-function gsmlg-org-note-org--around-agenda "gsmlg-org-note-org"
                  (orig &rest args))
(declare-function gsmlg-org-note-org--around-agenda-files "gsmlg-org-note-org"
                  (orig &rest args))
(declare-function gsmlg-org-note-org--around-capture "gsmlg-org-note-org"
                  (orig &rest args))
(declare-function gsmlg-org-note-org--fetch-view-items "gsmlg-org-note-org"
                  (workspace-ids view))
(declare-function gsmlg-org-apply-path-settings "gsmlg-org" ())

(unless (require 'gsmlg-paths nil t)
  (defvar gsmlg-cache-directory
    (file-name-as-directory
     (make-temp-file "gsmlg-org-note-org-cache-" t)))
  (defun gsmlg-cache-file (name)
    (expand-file-name name gsmlg-cache-directory))
  (defun gsmlg-ensure-parent-directory (file)
    (make-directory (file-name-directory file) t)
    file)
  (provide 'gsmlg-paths))

(defun org-note-org-bridge-test--unload-org-note ()
  "Unload Org Note when it is already provided."
  (when (featurep 'org-note)
    (unload-feature 'org-note t)))

(defun org-note-org-bridge-test--with-stub-org-note (body)
  "Call BODY with a stub `org-note' feature on `load-path'."
  (let ((temp-dir (make-temp-file "org-note-stub-" t))
        (saved-load-path load-path))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "org-note.el" temp-dir)
            (insert ";;; org-note.el --- stub -*- lexical-binding: t; -*-\n"
                    "(defvar org-note-agenda-workspace-ids nil)\n"
                    "(defvar org-note-endpoint nil)\n"
                    "(defun org-note-configure-agenda-workspaces ()\n"
                    "  \"Stub: leave agenda workspaces unset.\"\n"
                    "  (interactive)\n"
                    "  nil)\n"
                    "(provide 'org-note)\n"))
          (setq load-path (cons temp-dir load-path))
          (org-note-org-bridge-test--unload-org-note)
          (funcall body))
      (setq load-path saved-load-path)
      (org-note-org-bridge-test--unload-org-note)
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest gsmlg-org-note-org-install-guards-is-inert ()
  "Installing guards must not load Org Note."
  (org-note-org-bridge-test--unload-org-note)
  (require 'org-agenda nil t)
  (require 'gsmlg-org-note-org)
  (when (boundp 'gsmlg-org-note-org--guards-installed)
    (setq gsmlg-org-note-org--guards-installed nil))
  (dolist (command gsmlg-org-note-org--agenda-entrypoints)
    (advice-remove command #'gsmlg-org-note-org--around-agenda))
  (advice-remove #'org-agenda-files
                 #'gsmlg-org-note-org--around-agenda-files)
  (advice-remove #'org-capture #'gsmlg-org-note-org--around-capture)
  (gsmlg-org-note-org-install-guards)
  (should-not (featurep 'org-note))
  (dolist (command gsmlg-org-note-org--agenda-entrypoints)
    (should (advice-member-p #'gsmlg-org-note-org--around-agenda command)))
  (should (advice-member-p #'gsmlg-org-note-org--around-agenda-files
                           #'org-agenda-files))
  (should (advice-member-p #'gsmlg-org-note-org--around-capture #'org-capture)))

(ert-deftest gsmlg-org-note-org-around-agenda-requires-org-note-once ()
  "First agenda invocation loads Org Note then calls the original command."
  (org-note-org-bridge-test--with-stub-org-note
   (lambda ()
     (require 'gsmlg-org-note-org)
     (when (boundp 'gsmlg-org-note-org--activated)
       (setq gsmlg-org-note-org--activated nil))
     (when (boundp 'gsmlg-org-note-org--activating)
       (setq gsmlg-org-note-org--activating nil))
     (should-not (featurep 'org-note))
     (let ((gsmlg-org-note-org-enable t)
           (orig-calls 0)
           (require-count 0)
           (orig-require (symbol-function 'require)))
       (cl-letf (((symbol-function 'require)
                  (lambda (feature &rest args)
                    (when (and (eq feature 'org-note)
                               (not (featurep 'org-note)))
                      (cl-incf require-count))
                    (apply orig-require feature args))))
         (should (eq 'agenda-ok
                     (gsmlg-org-note-org--around-agenda
                      (lambda (&rest _args)
                        (cl-incf orig-calls)
                        'agenda-ok)
                      nil)))
         (should (featurep 'org-note))
         (should (= require-count 1))
         (should (= orig-calls 1))
         (should (eq 'agenda-ok
                     (gsmlg-org-note-org--around-agenda
                      (lambda (&rest _args)
                        (cl-incf orig-calls)
                        'agenda-ok)
                      nil)))
         (should (= orig-calls 2))
         (should (featurep 'org-note)))))))

(ert-deftest gsmlg-org-note-org-agenda-files-are-feed-only ()
  "When the bridge is active, org-agenda-files must be feed-only."
  (org-note-org-bridge-test--with-stub-org-note
   (lambda ()
     (cl-letf (((symbol-function #'use-package-ensure-elpa) #'ignore))
       (require 'gsmlg-org)
       (require 'gsmlg-org-note-org))
     (when (boundp 'gsmlg-org-note-org--activated)
       (setq gsmlg-org-note-org--activated nil))
     (when (boundp 'gsmlg-org-note-org--activating)
       (setq gsmlg-org-note-org--activating nil))
     (let ((gsmlg-org-note-org-enable t)
           (local (make-temp-file "local" nil ".org"))
           (saved-agenda-files gsmlg-org-agenda-files)
           (saved-org-agenda-files (and (boundp 'org-agenda-files)
                                        org-agenda-files)))
       (unwind-protect
           (progn
             (setq gsmlg-org-agenda-files local)
             (gsmlg-org-note-org-activate)
             (gsmlg-org-apply-path-settings)
             (should (equal org-agenda-files
                            (list (gsmlg-org-note-org-feed-file))))
             (should-not (member local org-agenda-files)))
         (setq gsmlg-org-agenda-files saved-agenda-files)
         (when (boundp 'org-agenda-files)
           (setq org-agenda-files saved-org-agenda-files))
         (when (boundp 'gsmlg-org-note-org--activated)
           (setq gsmlg-org-note-org--activated nil))
         (delete-file local))))))

(ert-deftest gsmlg-org-note-org-agenda-files-guard-rejects-overrides ()
  "Restrictions and custom command bindings cannot replace the bridge feed."
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-org-enable t)
        (gsmlg-org-note-org--activated t)
        (gsmlg-org-note-org--selected-feed-file "/tmp/org-note-feed.org")
        (org-agenda-files '("/tmp/local.org"))
        (original-restriction (get 'org-agenda-files 'org-restrict)))
    (unwind-protect
        (progn
          (put 'org-agenda-files 'org-restrict '("/tmp/restricted.org"))
          (should
           (equal
            (gsmlg-org-note-org--around-agenda-files
             (lambda (&rest _args) org-agenda-files) nil 'ifmode)
            '("/tmp/org-note-feed.org"))))
      (put 'org-agenda-files 'org-restrict original-restriction))))

(ert-deftest gsmlg-org-note-org-disabled-entrypoint-is-pass-through ()
  "The release gate leaves normal Org behavior untouched while disabled."
  (require 'gsmlg-org-note-org)
  (let ((gsmlg-org-note-org-enable nil)
        (called nil))
    (should
     (eq 'local-agenda
         (gsmlg-org-note-org--around-agenda
          (lambda (&rest _args)
            (setq called t)
            'local-agenda))))
    (should called)))

(ert-deftest gsmlg-org-note-org-feed-order-is-stable ()
  "Equivalent API rows produce identical feed bytes regardless of order."
  (require 'gsmlg-org-note-org)
  (let ((first '((id . "item-b") (workspace_id . "workspace-a")
                 (title . "B") (state . "TODO")))
        (second '((id . "item-a") (workspace_id . "workspace-a")
                  (title . "A") (state . "TODO")))
        forward reverse)
    (cl-letf (((symbol-function 'gsmlg-org-note-org--fetch-views)
               (lambda (_workspace-ids) (cons (list first second) nil))))
      (setq forward
            (gsmlg-org-note-org--build-feed-contents '("workspace-a"))))
    (cl-letf (((symbol-function 'gsmlg-org-note-org--fetch-views)
               (lambda (_workspace-ids) (cons (list second first) nil))))
      (setq reverse
            (gsmlg-org-note-org--build-feed-contents '("workspace-a"))))
    (should (equal forward reverse))))

(ert-deftest gsmlg-org-note-org-identical-feed-is-not-rewritten ()
  "Writing identical feed bytes should be a no-op."
  (require 'gsmlg-org-note-org)
  (let ((feed-file (make-temp-file "gsmlg-org-note-feed-" nil ".org"))
        (contents "#+TITLE: Stable\n")
        (writes 0))
    (unwind-protect
        (progn
          (with-temp-file feed-file (insert contents))
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _args) (cl-incf writes))))
            (should (equal (gsmlg-org-note-org--write-feed
                            contents feed-file)
                           feed-file))
            (should (= writes 0))))
      (delete-file feed-file))))

(ert-deftest gsmlg-org-note-org-refresh-asks-on-failure ()
  "Pre-rename refresh failure prompts before aborting."
  (require 'gsmlg-org-note-org)
  (let* ((feed-file (make-temp-file "gsmlg-org-note-feed-" nil ".org"))
         (asked nil)
         (saved-feed gsmlg-org-note-org--feed-file)
         (saved-last gsmlg-org-note-org--last-workspace-ids)
         (saved-workspaces (and (boundp 'org-note-agenda-workspace-ids)
                                org-note-agenda-workspace-ids))
         (saved-selected (and (boundp 'gsmlg-org-note-org--selected-feed-file)
                              gsmlg-org-note-org--selected-feed-file)))
    (unwind-protect
        (progn
          (setq gsmlg-org-note-org--feed-file feed-file
                gsmlg-org-note-org--selected-feed-file feed-file
                gsmlg-org-note-org--last-workspace-ids '("workspace-a")
                org-note-agenda-workspace-ids '("workspace-a"))
          (with-temp-file feed-file
            (insert "#+TITLE: Org Note Agenda Feed\n"
                    "#+ORG_NOTE_FEED_SCHEMA: 1\n"
                    "#+ORG_NOTE_WORKSPACE_IDS: workspace-a\n"
                    "* TODO Cached :ORGNOTE:\n"))
          (cl-letf (((symbol-function 'gsmlg-org-note-org--fetch-views)
                     (lambda (&rest _) (error "network")))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt) (setq asked prompt) nil)))
            (should-error (gsmlg-org-note-org-refresh-feed t))
            (should asked)))
      (setq gsmlg-org-note-org--feed-file saved-feed
            gsmlg-org-note-org--last-workspace-ids saved-last
            gsmlg-org-note-org--selected-feed-file saved-selected)
      (when (boundp 'org-note-agenda-workspace-ids)
        (setq org-note-agenda-workspace-ids saved-workspaces))
      (when (file-readable-p feed-file)
        (delete-file feed-file)))))

(ert-deftest gsmlg-org-note-org-refresh-uses-last-good-on-yes ()
  "Matching last-good is reused when the user confirms after failure."
  (require 'gsmlg-org-note-org)
  (let* ((feed-file (make-temp-file "gsmlg-org-note-feed-" nil ".org"))
         (original
          (concat "#+TITLE: Org Note Agenda Feed\n"
                  "#+ORG_NOTE_FEED_SCHEMA: 1\n"
                  "#+ORG_NOTE_WORKSPACE_IDS: workspace-a\n"
                  "* TODO Cached :ORGNOTE:\n"))
         (asked nil)
         (writes 0)
         (saved-feed gsmlg-org-note-org--feed-file)
         (saved-last gsmlg-org-note-org--last-workspace-ids)
         (saved-workspaces (and (boundp 'org-note-agenda-workspace-ids)
                                org-note-agenda-workspace-ids))
         (saved-selected (and (boundp 'gsmlg-org-note-org--selected-feed-file)
                              gsmlg-org-note-org--selected-feed-file)))
    (unwind-protect
        (progn
          (setq gsmlg-org-note-org--feed-file feed-file
                gsmlg-org-note-org--selected-feed-file feed-file
                gsmlg-org-note-org--last-workspace-ids '("workspace-a")
                org-note-agenda-workspace-ids '("workspace-a"))
          (with-temp-file feed-file (insert original))
          (cl-letf (((symbol-function 'gsmlg-org-note-org--fetch-views)
                     (lambda (&rest _) (error "network")))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt) (setq asked prompt) t))
                    ((symbol-function 'write-region)
                     (lambda (&rest _)
                       (cl-incf writes)
                       (error "must not rewrite last-good"))))
            (should (equal (gsmlg-org-note-org-refresh-feed t) feed-file))
            (should asked)
            (should (= writes 0))
            (with-temp-buffer
              (insert-file-contents feed-file)
              (should (equal (buffer-string) original)))))
      (setq gsmlg-org-note-org--feed-file saved-feed
            gsmlg-org-note-org--last-workspace-ids saved-last
            gsmlg-org-note-org--selected-feed-file saved-selected)
      (when (boundp 'org-note-agenda-workspace-ids)
        (setq org-note-agenda-workspace-ids saved-workspaces))
      (when (file-readable-p feed-file)
        (delete-file feed-file)))))

(ert-deftest gsmlg-org-note-org-refresh-configure-on-empty ()
  "Unset workspaces: interactive configures; noninteractive cancels to empty."
  (require 'gsmlg-org-note-org)
  (let* ((feed-file (make-temp-file "gsmlg-org-note-feed-" nil ".org"))
         (last-good
          (concat "#+TITLE: Org Note Agenda Feed\n"
                  "#+ORG_NOTE_FEED_SCHEMA: 1\n"
                  "#+ORG_NOTE_WORKSPACE_IDS: workspace-a\n"
                  "* TODO Keep :ORGNOTE:\n"))
         (configured nil)
         (saved-feed gsmlg-org-note-org--feed-file)
         (saved-last gsmlg-org-note-org--last-workspace-ids)
         (saved-workspaces (and (boundp 'org-note-agenda-workspace-ids)
                                org-note-agenda-workspace-ids))
         (saved-selected (and (boundp 'gsmlg-org-note-org--selected-feed-file)
                              gsmlg-org-note-org--selected-feed-file))
         (saved-endpoint (and (boundp 'org-note-endpoint) org-note-endpoint)))
    (unwind-protect
        (progn
          (setq gsmlg-org-note-org--feed-file feed-file
                gsmlg-org-note-org--selected-feed-file feed-file
                gsmlg-org-note-org--last-workspace-ids '("workspace-a")
                org-note-agenda-workspace-ids nil
                org-note-endpoint "https://example.test/")
          (with-temp-file feed-file (insert last-good))
          (cl-letf (((symbol-function 'org-note-configure-agenda-workspaces)
                     (lambda ()
                       (setq configured t)
                       nil)))
            ;; Batch / ERT: skip configure → empty feed, keep last-good.
            (let ((noninteractive t))
              (let ((path (gsmlg-org-note-org-refresh-feed t)))
                (should-not configured)
                (should (file-readable-p path))
                (should-not (equal path feed-file))
                (with-temp-buffer
                  (insert-file-contents feed-file)
                  (should (equal (buffer-string) last-good)))
                (with-temp-buffer
                  (insert-file-contents path)
                  (should (search-forward "Org Note Agenda Feed" nil t)))))
            ;; Interactive: configure is offered; cancel → empty feed.
            (setq configured nil
                  gsmlg-org-note-org--selected-feed-file feed-file
                  gsmlg-org-note-org--last-workspace-ids '("workspace-a")
                  org-note-agenda-workspace-ids nil)
            (let ((noninteractive nil))
              (let ((path (gsmlg-org-note-org-refresh-feed t)))
                (should configured)
                (should (file-readable-p path))
                (should-not (equal path feed-file))
                (with-temp-buffer
                  (insert-file-contents feed-file)
                  (should (equal (buffer-string) last-good)))))))
      (setq gsmlg-org-note-org--feed-file saved-feed
            gsmlg-org-note-org--last-workspace-ids saved-last
            gsmlg-org-note-org--selected-feed-file saved-selected)
      (when (boundp 'org-note-agenda-workspace-ids)
        (setq org-note-agenda-workspace-ids saved-workspaces))
      (when (boundp 'org-note-endpoint)
        (setq org-note-endpoint saved-endpoint))
      (when (file-readable-p feed-file)
        (delete-file feed-file)))))

(ert-deftest gsmlg-org-note-org-fetch-rejects-empty-next-cursor ()
  "Empty next_cursor must fail closed as org-note-error (no hang)."
  (require 'gsmlg-org-note-org)
  (require 'org-note-validation)
  (cl-letf (((symbol-function 'org-note-operation-query-agenda)
             (lambda (&rest _)
               '((items . ())
                 (next_cursor . "")))))
    (should-error (gsmlg-org-note-org--fetch-view-items
                   '("workspace-a") 'scheduled)
                  :type 'org-note-error)))

(ert-deftest gsmlg-org-note-org-fetch-rejects-page-budget ()
  "Exhaustive agenda paging must fail closed when the page budget is hit."
  (require 'gsmlg-org-note-org)
  (require 'org-note-validation)
  (let ((page 0)
        (make-state (symbol-function
                     'org-note-validation-bounded-pager-state)))
    (cl-letf (((symbol-function 'org-note-validation-bounded-pager-state)
               (cl-function
                (lambda (&key limit &allow-other-keys)
                  (funcall make-state :limit limit :max-pages 2))))
              ((symbol-function 'org-note-operation-query-agenda)
               (lambda (&rest _)
                 (cl-incf page)
                 `((items . (((item . ((id . ,(format "item-%d" page))
                                      (workspace_id . "workspace-a")
                                      (title . "Task")
                                      (state . "TODO"))))))
                   (next_cursor . ,(format "cursor-%d" page))))))
      (should-error (gsmlg-org-note-org--fetch-view-items
                     '("workspace-a") 'scheduled)
                    :type 'org-note-error)
      (should (= page 2)))))
(ert-deftest gsmlg-org-note-org-refresh-offers-last-good-on-empty-cursor ()
  "Empty next_cursor during refresh uses the last-good failure path."
  (require 'gsmlg-org-note-org)
  (require 'org-note-validation)
  (let* ((feed-file (make-temp-file "gsmlg-org-note-feed-" nil ".org"))
         (original
          (concat "#+TITLE: Org Note Agenda Feed\n"
                  "#+ORG_NOTE_FEED_SCHEMA: 1\n"
                  "#+ORG_NOTE_WORKSPACE_IDS: workspace-a\n"
                  "* TODO Cached :ORGNOTE:\n"))
         (asked nil)
         (saved-feed gsmlg-org-note-org--feed-file)
         (saved-last gsmlg-org-note-org--last-workspace-ids)
         (saved-workspaces (and (boundp 'org-note-agenda-workspace-ids)
                                org-note-agenda-workspace-ids))
         (saved-selected (and (boundp 'gsmlg-org-note-org--selected-feed-file)
                              gsmlg-org-note-org--selected-feed-file)))
    (unwind-protect
        (progn
          (setq gsmlg-org-note-org--feed-file feed-file
                gsmlg-org-note-org--selected-feed-file feed-file
                gsmlg-org-note-org--last-workspace-ids '("workspace-a")
                org-note-agenda-workspace-ids '("workspace-a"))
          (with-temp-file feed-file (insert original))
          (cl-letf (((symbol-function 'org-note-operation-query-agenda)
                     (lambda (&rest _)
                       '((items . ())
                         (next_cursor . ""))))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt) (setq asked prompt) t)))
            (should (equal (gsmlg-org-note-org-refresh-feed t) feed-file))
            (should asked)
            (with-temp-buffer
              (insert-file-contents feed-file)
              (should (equal (buffer-string) original)))))
      (setq gsmlg-org-note-org--feed-file saved-feed
            gsmlg-org-note-org--last-workspace-ids saved-last
            gsmlg-org-note-org--selected-feed-file saved-selected)
      (when (boundp 'org-note-agenda-workspace-ids)
        (setq org-note-agenda-workspace-ids saved-workspaces))
      (when (file-readable-p feed-file)
        (delete-file feed-file)))))

(provide 'org-note-org-bridge-agenda-test)
;;; org-note-org-bridge-agenda-test.el ends here
