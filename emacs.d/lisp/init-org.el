(use-package org-plus-contrib
  :ensure t
  :no-require t)

(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   :map org-mode-map
   ("C-M-<up>" . org-up-element)
   ("C-M-<down>" . org-down-element)
   :map org-agenda-mode-map
   ("W" . bh/widen))
  :config
  (progn
    ;; Various preferences
    (setq org-log-done t
	  org-directory "~/Documents/org/"
	  org-agenda-files "~/Documents/org/.agenda_files"
	  org-edit-timestamp-down-means-later t
	  org-archive-mark-done nil
	  org-hide-emphasis-markers t
	  org-catch-invisible-edits 'show
	  org-export-coding-system 'utf-8
	  org-fast-tag-selection-single-key 'expert
	  org-html-validation-link nil
	  org-export-kill-product-buffer-when-displayed t
	  org-tags-column 80)

    ;; docs [[https://orgmode.org/manual/Template-elements.html][Capture Templates]]
    (setq org-capture-templates
	  `(("t" "todo" entry (file "todo.org")  ; "" => `org-default-notes-file'
	     "* NEXT %?\n%U\n" :clock-resume t)
	    ("n" "note" entry (file "note.org")
	     "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
	    ("b" "bookmark" entry (file "bookmark.org")
	     "* %? \n%U\n" :clock-resume t)
	    ))

    
    ;; Refiling

    (setq org-refile-use-cache nil)

    ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
    (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

    (after-load 'org-agenda
      (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

    ;; Targets start with the file name - allows creating level 1 tasks
    ;;(setq org-refile-use-outline-path (quote file))
    (setq org-refile-use-outline-path t)
    (setq org-outline-path-complete-in-steps nil)

    ;; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    
    ;; To-do settings

    (setq org-todo-keywords
	  (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
		  (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
		  (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)" "MEETING")))
	  org-todo-repeat-to-state "NEXT")

    (setq org-todo-keyword-faces
	  (quote (;;("NEXT" :inherit warning)
		  ("PROJECT" :inherit font-lock-string-face)
		  ("TODO" :foreground "red" :weight bold)
		  ("NEXT" :foreground "blue" :weight bold)
		  ("DONE" :foreground "forest green" :weight bold)
		  ("WAITING" :foreground "orange" :weight bold)
		  ("HOLD" :foreground "magenta" :weight bold)
		  ("CANCELLED" :foreground "forest green" :weight bold)
		  ("MEETING" :foreground "forest green" :weight bold)
		  ("PHONE" :foreground "forest green" :weight bold))))

    
    ;; Agenda views

    (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


    (let ((active-project-match "-INBOX/PROJECT"))

      (setq org-stuck-projects
	    `(,active-project-match ("NEXT")))

      (setq org-agenda-compact-blocks t
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
			     '(lambda ()
				(or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
				    (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
			    (org-tags-match-list-sublevels t)
			    (org-agenda-sorting-strategy
			     '(todo-state-down effort-up category-keep))))
		(tags-todo ,active-project-match
			   ((org-agenda-overriding-header "Projects")
			    (org-tags-match-list-sublevels t)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-INBOX/-NEXT"
			   ((org-agenda-overriding-header "Orphaned Tasks")
			    (org-agenda-tags-todo-honor-ignore-options t)
			    (org-agenda-todo-ignore-scheduled 'future)
			    (org-agenda-skip-function
			     '(lambda ()
				(or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
				    (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
			    (org-tags-match-list-sublevels t)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "/WAITING"
			   ((org-agenda-overriding-header "Waiting")
			    (org-agenda-tags-todo-honor-ignore-options t)
			    (org-agenda-todo-ignore-scheduled 'future)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "/DELEGATED"
			   ((org-agenda-overriding-header "Delegated")
			    (org-agenda-tags-todo-honor-ignore-options t)
			    (org-agenda-todo-ignore-scheduled 'future)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		(tags-todo "-INBOX"
			   ((org-agenda-overriding-header "On Hold")
			    (org-agenda-skip-function
			     '(lambda ()
				(or (org-agenda-skip-subtree-if 'todo '("WAITING"))
				    (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
			    (org-tags-match-list-sublevels nil)
			    (org-agenda-sorting-strategy
			     '(category-keep))))
		;; (tags-todo "-NEXT"
		;;            ((org-agenda-overriding-header "All other TODOs")
		;;             (org-match-list-sublevels t)))
		)))))


    (add-hook 'org-agenda-mode-hook 'hl-line-mode)

    
    ;; Org clock

    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (after-load 'org
      (org-clock-persistence-insinuate))
    (setq org-clock-persist t)
    (setq org-clock-in-resume t)

    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Save state changes in the LOGBOOK drawer
    (setq org-log-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; Show clock sums as hours and minutes, not "n days" etc.
    (setq org-time-clocksum-format
	  '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    
    ;; Show the clocked-in task - if any - in the header line
    (defun sanityinc/show-org-clock-in-header-line ()
      (setq-default header-line-format '((" " org-mode-line-string " "))))

    (defun sanityinc/hide-org-clock-from-header-line ()
      (setq-default header-line-format nil))

    (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
    (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
    (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

    (after-load 'org-clock
      (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
      (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

    
    ;; org mobile
    (setq org-mobile-directory "/Volumes/org.gsmlg.org/"
          org-mobile-inbox-for-pull (expand-file-name "from-mobile.org" org-directory))

    
    ;; Archiving

    (setq org-archive-mark-done nil)
    (setq org-archive-location "%s_archive::* Archive")

    
    ;; org-protocol
    (require 'org-protocol)

    
    ;; Speed Commands
    ;; Speed commands allow access to frequently used commands when on the beginning of a headline - similar to one-key agenda commands.
    (setq org-use-speed-commands t)
    (setq org-speed-commands-user (quote (("h" . gsmlg/org-hide-other)
					  ("k" . org-kill-note-or-show-branches)
					  ("q" . org-agenda)
					  ("s" . org-save-all-org-buffers)
					  ("w" . org-refile)
					  ("z" . org-add-note)
                                          ("J" . org-clock-goto)
                                          ("P" . org-pomodoro)
					  ("W" . bh/widen))))
    
    (defun gsmlg/org-hide-other ()
      "Org hide other."
      (interactive)
      (save-excursion
	(org-back-to-heading 'invisible-ok)
	(hide-other)
	(org-cycle)
	(org-cycle)
	(org-cycle)))

    (defun bh/widen ()
      (interactive)
      (if (equal major-mode 'org-agenda-mode)
	  (progn
	    (org-agenda-remove-restriction-lock)
	    (when org-agenda-sticky
	      (org-agenda-redo)))
	(widen)))

    ;; call org capture from `Afred.app'
    (defun make-orgcapture-frame ()
      "Create a new frame and run org-capture."
      (interactive)
      (make-frame '((name . "remember") (width . 80) (height . 16)
		    (top . 400) (left . 300)
		    (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
		    ))
      (select-frame-by-name "remember")
      (org-capture))

    (when *is-a-mac*
      (define-key org-mode-map (kbd "M-h") nil)
      (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))

    (org-babel-do-load-languages
     'org-babel-load-languages
     `(
       ;; (R . t)
       (ditaa . t)
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
       (,(if (locate-library "ob-sh") 'sh 'shell) . t)
       (sql . nil)
       (sqlite . t)))
    ))

(use-package org-pomodoro
  :ensure t
  :bind (:map org-agenda-mode-map
	      ("P" . org-pomodoro))
  :init
  (setq org-pomodoro-keep-killed-pomodoro-time t))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(provide 'init-org)
