;;; gsmlg-org.el --- Org workflow configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure the GSMLG Org workflow while keeping host-specific paths
;; customizable.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)

(declare-function org-agenda-skip-entry-if "org-agenda" (&rest conditions))
(declare-function org-agenda-skip-subtree-if "org-agenda" (&rest conditions))
(declare-function org-agenda-redo "org-agenda" ())
(declare-function org-agenda-remove-restriction-lock "org-agenda" ())
(declare-function org-agenda "org-agenda" (&optional arg keys restriction))
(declare-function org-add-note "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function org-cycle "org-cycle" (&optional arg))
(declare-function org-down-element "org" ())
(declare-function org-fold-show-entry "org-fold" ())
(declare-function org-kill-note-or-show-branches "org" ())
(declare-function org-mac-grab-link "org-mac-link" ())
(declare-function org-modern-mode "org-modern" (&optional arg))
(declare-function org-pomodoro "org-pomodoro" ())
(declare-function org-refile "org-refile" (&optional goto default-buffer rfloc msg))
(declare-function org-save-all-org-buffers "org" ())
(declare-function org-store-link "ol" (&optional arg interactive))
(declare-function org-up-element "org" ())
(declare-function outline-hide-other "outline" ())
(declare-function org-babel-do-load-languages "ob-core"
                  (sym value))
(declare-function org-clock-goto "org-clock" (&optional select))
(declare-function org-clock-menu "org-clock" ())
(declare-function org-clock-persistence-insinuate "org-clock" ())
(defvar org-agenda-mode-map)
(defvar org-agenda-sticky)
(defvar org-clock-mode-line-map)
(defvar org-mode-map)
(defvar org-plantuml-exec-mode)
(defvar org-plantuml-jar-path)

(defgroup gsmlg-org nil
  "GSMLG Org workflow."
  :group 'org)

(defun gsmlg-org--set-directory (symbol value)
  "Set SYMBOL to normalized directory VALUE and refresh Org paths."
  (let ((directory (file-name-as-directory (expand-file-name value))))
    (set-default symbol directory)
    (when (boundp 'org-directory)
      (setopt org-directory directory))
    (when (fboundp #'gsmlg-org-refresh-capture-templates)
      (gsmlg-org-refresh-capture-templates))
    (when (and (featurep 'gsmlg-org)
               (fboundp #'gsmlg-org-apply-path-settings))
      (gsmlg-org-apply-path-settings))))

(defcustom gsmlg-org-directory
  (file-name-as-directory (expand-file-name "~/Documents/org/"))
  "Directory containing personal Org files."
  :type 'directory
  :set #'gsmlg-org--set-directory
  :group 'gsmlg-org)

(defun gsmlg-org--normalize-file-source (value)
  "Expand agenda file source VALUE."
  (cond
   ((stringp value) (expand-file-name value))
   ((listp value) (mapcar #'expand-file-name value))
   (t value)))

(defun gsmlg-org--set-file-source (symbol value)
  "Set SYMBOL to normalized agenda file source VALUE."
  (set-default symbol (gsmlg-org--normalize-file-source value))
  (when (featurep 'gsmlg-org)
    (gsmlg-org-apply-path-settings)))

(defun gsmlg-org--set-optional-directory (symbol value)
  "Set SYMBOL to optional normalized directory VALUE."
  (set-default symbol
               (and value
                    (file-name-as-directory (expand-file-name value))))
  (when (featurep 'gsmlg-org)
    (gsmlg-org-apply-path-settings)))

(defun gsmlg-org--set-optional-file (symbol value)
  "Set SYMBOL to optional normalized file VALUE."
  (set-default symbol (and value (expand-file-name value)))
  (when (featurep 'gsmlg-org)
    (gsmlg-org-apply-path-settings)))

(defcustom gsmlg-org-agenda-files
  (expand-file-name ".agenda_files" gsmlg-org-directory)
  "File source used to populate `org-agenda-files'."
  :type '(choice (const :tag "No agenda files" nil)
                 (file :tag "Agenda file or file list")
                 (repeat :tag "Agenda files" file))
  :set #'gsmlg-org--set-file-source
  :group 'gsmlg-org)

(defcustom gsmlg-org-mobile-directory "/Volumes/org.gsmlg.org/"
  "Optional directory used by Org Mobile.
The directory need not be mounted while Emacs starts."
  :type '(choice (const :tag "Disabled" nil) directory)
  :set #'gsmlg-org--set-optional-directory
  :group 'gsmlg-org)

(defcustom gsmlg-org-plantuml-jar-path nil
  "Optional path to a PlantUML jar.
When nil or unreadable, Org may use a PlantUML executable from PATH."
  :type '(choice (const :tag "Use PATH" nil) file)
  :set #'gsmlg-org--set-optional-file
  :group 'gsmlg-org)

(defun gsmlg-org--set-babel-languages (symbol value)
  "Set SYMBOL to VALUE and reapply available Babel languages when loaded."
  (set-default symbol value)
  (when (and (featurep 'gsmlg-org)
             (fboundp #'gsmlg-org-configure-babel))
    (gsmlg-org-configure-babel)))

(defcustom gsmlg-org-babel-languages
  '((ditaa . t)
    (dot . t)
    (emacs-lisp . t)
    (gnuplot . t)
    (haskell . nil)
    (latex . t)
    (ledger . t)
    (ocaml . nil)
    (octave . t)
    (plantuml . t)
    (python . t)
    (ruby . t)
    (screen . nil)
    (shell . t)
    (sql . nil)
    (sqlite . t))
  "Org Babel languages requested by the GSMLG workflow.
Enabled entries are loaded only when their ob-LANGUAGE library exists."
  :type '(alist :key-type symbol :value-type boolean)
  :set #'gsmlg-org--set-babel-languages
  :group 'gsmlg-org)

(defun gsmlg-org-refresh-capture-templates ()
  "Rebuild capture templates from `gsmlg-org-directory'."
  (setopt org-capture-templates
          `(("t" "todo" entry
             (file ,(expand-file-name "todo.org" gsmlg-org-directory))
             "* NEXT %?\n%U\n"
             :clock-resume t)
            ("n" "note" entry
             (file ,(expand-file-name "note.org" gsmlg-org-directory))
             "* %? :NOTE:\n%U\n%a\n"
             :clock-resume t)
            ("b" "bookmark" entry
             (file ,(expand-file-name "bookmark.org" gsmlg-org-directory))
             "* %? \n%U\n"
             :clock-resume t))))

(defun gsmlg-org-apply-plantuml-settings ()
  "Select a readable PlantUML jar or the environment executable."
  (let ((jar (and gsmlg-org-plantuml-jar-path
                  (file-readable-p gsmlg-org-plantuml-jar-path)
                  gsmlg-org-plantuml-jar-path)))
    (setq org-plantuml-jar-path jar
          org-plantuml-exec-mode (if jar 'jar 'plantuml))))

(defun gsmlg-org-apply-path-settings ()
  "Apply customizable Org path settings."
  (require 'org-mobile)
  (setopt org-directory gsmlg-org-directory
          org-agenda-files gsmlg-org-agenda-files
          org-mobile-directory gsmlg-org-mobile-directory
          org-mobile-inbox-for-pull
          (expand-file-name "from-mobile.org" gsmlg-org-directory))
  (gsmlg-org-apply-plantuml-settings)
  (with-eval-after-load 'ob-plantuml
    (gsmlg-org-apply-plantuml-settings))
  (gsmlg-org-refresh-capture-templates))

(defun gsmlg-org-agenda-skip-nottodo-next ()
  "Skip agenda entries that are held, waiting, or not NEXT actions."
  (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
      (org-agenda-skip-entry-if 'nottodo '("NEXT"))))

(defun gsmlg-org-agenda-skip-nottodo-todo ()
  "Skip agenda entries that are projects or not standalone TODO tasks."
  (or (org-agenda-skip-subtree-if
       'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
      (org-agenda-skip-subtree-if 'nottodo '("TODO"))))

(defun gsmlg-org-agenda-skip-nottodo-hold ()
  "Skip agenda entries that are waiting or not on HOLD."
  (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
      (org-agenda-skip-entry-if 'nottodo '("HOLD"))))

(defun gsmlg-org-hide-other ()
  "Show the current Org subtree while hiding other headings."
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (outline-hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun gsmlg-org-widen ()
  "Remove an agenda restriction, or widen the current Org buffer."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(defun gsmlg-org-capture-frame ()
  "Create a compact frame and start `org-capture'.
This command retains the entrypoint used by the user's Alfred workflow."
  (interactive)
  (let ((frame (make-frame '((name . "remember")
                             (width . 80)
                             (height . 16)
                             (top . 400)
                             (left . 300)))))
    (select-frame-set-input-focus frame)
    (org-capture)))

(defun gsmlg-org-show-clock-in-header-line ()
  "Refresh the Org clock displayed in the default header line."
  (force-mode-line-update t))

(defun gsmlg-org-hide-clock-from-header-line ()
  "Refresh the default header line after the Org clock disappears."
  (force-mode-line-update t))

(defalias 'gsmlg/org-hide-other #'gsmlg-org-hide-other)
(defalias 'bh/widen #'gsmlg-org-widen)
(defalias 'make-orgcapture-frame #'gsmlg-org-capture-frame)
(defalias 'gsmlg/show-org-clock-in-header-line
  #'gsmlg-org-show-clock-in-header-line)
(defalias 'gsmlg/hide-org-clock-from-header-line
  #'gsmlg-org-hide-clock-from-header-line)

(defun gsmlg-org-configure-macos-keys ()
  "Configure optional macOS Org bindings."
  (when (eq system-type 'darwin)
    (keymap-unset org-mode-map "M-h")
    (when (fboundp #'org-mac-grab-link)
      (keymap-set org-mode-map "C-c g" #'org-mac-grab-link))))

(defun gsmlg-org-configure-keys-and-speed-commands ()
  "Configure Org keys, speed commands, hooks, and clock display."
  (keymap-global-set "C-c l" #'org-store-link)
  (keymap-global-set "C-c a" #'org-agenda)
  (keymap-global-set "C-c c" #'org-capture)
  (keymap-set org-mode-map "C-M-<up>" #'org-up-element)
  (keymap-set org-mode-map "C-M-<down>" #'org-down-element)
  (keymap-set org-agenda-mode-map "P" #'org-pomodoro)
  (keymap-set org-clock-mode-line-map
              "<header-line> <mouse-2>" #'org-clock-goto)
  (keymap-set org-clock-mode-line-map
              "<header-line> <mouse-1>" #'org-clock-menu)
  (setopt
   org-use-speed-commands t
   org-speed-commands
   '(("h" . gsmlg-org-hide-other)
     ("k" . org-kill-note-or-show-branches)
     ("q" . org-agenda)
     ("s" . org-save-all-org-buffers)
     ("w" . org-refile)
     ("z" . org-add-note)
     ("J" . org-clock-goto)
     ("P" . org-pomodoro)
     ("W" . gsmlg-org-widen)))
  (add-hook 'org-agenda-after-show-hook #'org-fold-show-entry)
  (add-hook 'org-agenda-mode-hook #'hl-line-mode)
  (add-hook 'org-clock-in-hook #'gsmlg-org-show-clock-in-header-line)
  (add-hook 'org-clock-out-hook #'gsmlg-org-hide-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook #'gsmlg-org-hide-clock-from-header-line)
  (gsmlg-org-configure-macos-keys))

(defun gsmlg-org-configure-agenda ()
  "Configure the preserved GTD agenda views."
  (let ((active-project-match "-INBOX/PROJECT"))
    (setopt
     org-stuck-projects `(,active-project-match ("NEXT") nil "")
     org-agenda-compact-blocks t
     org-agenda-sticky t
     org-agenda-start-on-weekday nil
     org-agenda-span 'day
     org-agenda-include-diary nil
     org-agenda-sorting-strategy
     '((agenda habit-down time-up user-defined-up effort-up category-keep)
       (todo category-up effort-up)
       (tags category-up effort-up)
       (search category-up))
     org-agenda-window-setup 'current-window
     org-agenda-custom-commands
     `(("N" "Notes" tags "NOTE"
        ((org-agenda-overriding-header "Notes")
         (org-tags-match-list-sublevels t)))
       ("g" "GTD"
        ((agenda "" nil)
         (tags "INBOX"
               ((org-agenda-overriding-header "Inbox")
                (org-tags-match-list-sublevels nil)))
         (stuck ""
                ((org-agenda-overriding-header "Stuck Projects")
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-tags-match-list-sublevels t)
                 (org-agenda-todo-ignore-scheduled 'future)))
         (tags-todo "-INBOX"
                    ((org-agenda-overriding-header "Next Actions")
                     (org-agenda-tags-todo-honor-ignore-options t)
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-skip-function
                      #'gsmlg-org-agenda-skip-nottodo-next)
                     (org-tags-match-list-sublevels t)
                     (org-agenda-sorting-strategy
                      '(todo-state-down effort-up category-keep))))
         (tags-todo ,active-project-match
                    ((org-agenda-overriding-header "Projects")
                     (org-tags-match-list-sublevels t)
                     (org-agenda-sorting-strategy '(category-keep))))
         (tags-todo "-INBOX/-NEXT"
                    ((org-agenda-overriding-header "Orphaned Tasks")
                     (org-agenda-tags-todo-honor-ignore-options t)
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-skip-function
                      #'gsmlg-org-agenda-skip-nottodo-todo)
                     (org-tags-match-list-sublevels t)
                     (org-agenda-sorting-strategy '(category-keep))))
         (tags-todo "/WAITING"
                    ((org-agenda-overriding-header "Waiting")
                     (org-agenda-tags-todo-honor-ignore-options t)
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-sorting-strategy '(category-keep))))
         (tags-todo "/DELEGATED"
                    ((org-agenda-overriding-header "Delegated")
                     (org-agenda-tags-todo-honor-ignore-options t)
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-sorting-strategy '(category-keep))))
         (tags-todo "-INBOX"
                    ((org-agenda-overriding-header "On Hold")
                     (org-agenda-skip-function
                      #'gsmlg-org-agenda-skip-nottodo-hold)
                     (org-tags-match-list-sublevels nil)
                     (org-agenda-sorting-strategy '(category-keep))))))))))

(defun gsmlg-org-configure-workflow ()
  "Configure Org task states, refiling, logging, and clocking."
  (require 'org-archive)
  (require 'org-clock)
  (require 'org-duration)
  (require 'org-protocol)
  (require 'org-refile)
  (let ((clock-directory
         (file-name-as-directory
          (expand-file-name "org/" gsmlg-state-directory))))
    (make-directory clock-directory t)
    (setopt
     org-clock-persist-file
     (expand-file-name "clock-save.el" clock-directory)
     org-id-locations-file
     (expand-file-name "id-locations" clock-directory)
     org-persist-directory
     (gsmlg-ensure-directory (gsmlg-cache-file "org-persist/"))))
  (setopt
   org-log-done 'time
   org-edit-timestamp-down-means-later t
   org-archive-mark-done nil
   org-hide-emphasis-markers t
   org-fold-catch-invisible-edits 'show
   org-fast-tag-selection-single-key 'expert
   org-tags-column 80
   org-refile-use-cache nil
   org-refile-targets
   '((nil :maxlevel . 5)
     (org-agenda-files :maxlevel . 5))
   org-refile-use-outline-path t
   org-outline-path-complete-in-steps nil
   org-refile-allow-creating-parent-nodes 'confirm
   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
     (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
     (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|"
               "CANCELLED(c@/!)" "MEETING"))
   org-todo-repeat-to-state "NEXT"
   org-todo-keyword-faces
   '(("PROJECT" :inherit font-lock-string-face)
     ("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold)
     ("PHONE" :foreground "forest green" :weight bold))
   org-clock-persist t
   org-clock-in-resume t
   org-clock-into-drawer t
   org-log-into-drawer t
   org-clock-out-remove-zero-time-clocks t
   org-duration-format '((special . h:mm))
   org-archive-location "%s_archive::* Archive")
  (setq-default org-agenda-clockreport-parameter-plist
                '(:link t :maxlevel 3))
  (unless (and noninteractive (not (daemonp)))
    (org-clock-persistence-insinuate))
  (with-eval-after-load 'ox
    (setopt org-export-coding-system 'utf-8))
  (with-eval-after-load 'ox-html
    (setopt org-html-validation-link nil)))

(defun gsmlg-org-available-babel-languages ()
  "Return enabled Babel languages with available Emacs libraries."
  (let (available)
    (dolist (entry gsmlg-org-babel-languages (nreverse available))
      (when (and (cdr entry)
                 (locate-library
                  (format "ob-%s" (symbol-name (car entry)))))
        (push entry available)))))

(defun gsmlg-org-configure-babel ()
  "Load the available subset of `gsmlg-org-babel-languages'."
  (require 'ob-core)
  (org-babel-do-load-languages
   'org-babel-load-languages
   (gsmlg-org-available-babel-languages)))

(use-package org
  :ensure nil
  :demand t
  :init
  (gsmlg-org-apply-path-settings)
  :config
  (gsmlg-org-configure-workflow)
  (gsmlg-org-configure-agenda)
  (gsmlg-org-configure-keys-and-speed-commands)
  (gsmlg-org-configure-babel))

;; org-pomodoro and org-modern are declared in gsmlg-app-packages for Elpaca
;; queueing; configure them when this application module loads.
(use-package org-pomodoro
  :ensure nil
  :commands org-pomodoro
  :custom
  (org-pomodoro-keep-killed-pomodoro-time t))

(use-package org-modern
  :ensure nil
  :hook (org-mode . org-modern-mode))

(provide 'gsmlg-org)
;;; gsmlg-org.el ends here
