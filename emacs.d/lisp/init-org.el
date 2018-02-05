(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (progn
    ;; Various preferences
    (setq org-log-done t
	  org-directory "~/Documents/org"
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

    ;; docs [[https://orgmode.org/manual/Template-elements.html]]
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

    
    ;; Archiving

    (setq org-archive-mark-done nil)
    (setq org-archive-location "%s_archive::* Archive")

    
    ;; org-protocol
    (require 'org-protocol)

    
    ;; Speed Commands
    ;; Speed commands allow access to frequently used commands when on the beginning of a headline - similar to one-key agenda commands.
    (setq org-use-speed-commands t)
    (setq org-speed-commands-user (quote (("0" . ignore)
					  ("1" . ignore)
					  ("2" . ignore)
					  ("3" . ignore)
					  ("4" . ignore)
					  ("5" . ignore)
					  ("6" . ignore)
					  ("7" . ignore)
					  ("8" . ignore)
					  ("9" . ignore)

					  ("a" . ignore)
					  ("d" . ignore)
					  ("h" . gsmlg/org-hide-other)
					  ("i" progn
					   (forward-char 1)
					   (call-interactively 'org-insert-heading-respect-content))
					  ("k" . org-kill-note-or-show-branches)
					  ("l" . ignore)
					  ("m" . ignore)
					  ("q" . gsmlg/show-org-agenda)
					  ("r" . ignore)
					  ("s" . org-save-all-org-buffers)
					  ("w" . org-refile)
					  ("x" . ignore)
					  ("y" . ignore)
					  ("z" . org-add-note)

					  ("A" . ignore)
					  ("B" . ignore)
					  ("E" . ignore)
					  ("F" . bh/restrict-to-file-or-follow)
					  ("G" . ignore)
					  ("H" . ignore)
					  ("J" . org-clock-goto)
					  ("K" . ignore)
					  ("L" . ignore)
					  ("M" . ignore)
					  ("N" . bh/narrow-to-org-subtree)
					  ("P" . bh/narrow-to-org-project)
					  ("Q" . ignore)
					  ("R" . ignore)
					  ("S" . ignore)
					  ("T" . bh/org-todo)
					  ("U" . bh/narrow-up-one-org-level)
					  ("V" . ignore)
					  ("W" . bh/widen)
					  ("X" . ignore)
					  ("Y" . ignore)
					  ("Z" . ignore))))

    (defun gsmlg/org-hide-other ()
      "Org hide other."
      (interactive)
      (save-excursion
	(org-back-to-heading 'invisible-ok)
	(hide-other)
	(org-cycle)
	(org-cycle)
	(org-cycle)))

    (defun gsmlg/show-org-agenda ()
      "Switch to org agenda buffer."
      (interactive)
      ;; (if org-agenda-sticky
      ;;     (switch-to-buffer "*Org Agenda( )*")
      ;;   (switch-to-buffer "*Org Agenda*"))
      (org-agenda)
      (delete-other-windows))


    (defun bh/org-todo (arg)
      (interactive "p")
      (if (equal arg 4)
	  (save-restriction
	    (bh/narrow-to-org-subtree)
	    (org-show-todo-tree nil))
	(bh/narrow-to-org-subtree)
	(org-show-todo-tree nil)))

    (global-set-key (kbd "<S-f5>") 'bh/widen)

    (defun bh/widen ()
      (interactive)
      (if (equal major-mode 'org-agenda-mode)
	  (progn
	    (org-agenda-remove-restriction-lock)
	    (when org-agenda-sticky
	      (org-agenda-redo)))
	(widen)))

    (add-hook 'org-agenda-mode-hook
	      '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
	      'append)

    (defun bh/restrict-to-file-or-follow (arg)
      "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
      (interactive "p")
      (if (equal arg 4)
	  (org-agenda-follow-mode)
	(widen)
	(bh/set-agenda-restriction-lock 4)
	(org-agenda-redo)
	(beginning-of-buffer)))

    (add-hook 'org-agenda-mode-hook
	      '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
	      'append)

    (defun bh/narrow-to-org-subtree ()
      (widen)
      (org-narrow-to-subtree)
      (save-restriction
	(org-agenda-set-restriction-lock)))

    (defun bh/narrow-to-subtree ()
      (interactive)
      (if (equal major-mode 'org-agenda-mode)
	  (progn
	    (org-with-point-at (org-get-at-bol 'org-hd-marker)
	      (bh/narrow-to-org-subtree))
	    (when org-agenda-sticky
	      (org-agenda-redo)))
	(bh/narrow-to-org-subtree)))

    (add-hook 'org-agenda-mode-hook
	      '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
	      'append)

    (defun bh/narrow-up-one-org-level ()
      (widen)
      (save-excursion
	(outline-up-heading 1 'invisible-ok)
	(bh/narrow-to-org-subtree)))

    (defun bh/get-pom-from-agenda-restriction-or-point ()
      (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
	  (org-get-at-bol 'org-hd-marker)
	  (and (equal major-mode 'org-mode) (point))
	  org-clock-marker))

    (defun bh/narrow-up-one-level ()
      (interactive)
      (if (equal major-mode 'org-agenda-mode)
	  (progn
	    (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
	      (bh/narrow-up-one-org-level))
	    (org-agenda-redo))
	(bh/narrow-up-one-org-level)))

    (add-hook 'org-agenda-mode-hook
	      '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
	      'append)

    (defun bh/narrow-to-org-project ()
      (widen)
      (save-excursion
	(bh/find-project-task)
	(bh/narrow-to-org-subtree)))

    (defun bh/narrow-to-project ()
      (interactive)
      (if (equal major-mode 'org-agenda-mode)
	  (progn
	    (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
	      (bh/narrow-to-org-project)
	      (save-excursion
		(bh/find-project-task)
		(org-agenda-set-restriction-lock)))
	    (org-agenda-redo)
	    (beginning-of-buffer))
	(bh/narrow-to-org-project)
	(save-restriction
	  (org-agenda-set-restriction-lock))))

    (add-hook 'org-agenda-mode-hook
	      '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
	      'append)

    (defvar bh/project-list nil)

    (defun bh/view-next-project ()
      (interactive)
      (let (num-project-left current-project)
	(unless (marker-position org-agenda-restrict-begin)
	  (goto-char (point-min))
                                        ; Clear all of the existing markers on the list
	  (while bh/project-list
	    (set-marker (pop bh/project-list) nil))
	  (re-search-forward "Tasks to Refile")
	  (forward-visible-line 1))

                                        ; Build a new project marker list
	(unless bh/project-list
	  (while (< (point) (point-max))
	    (while (and (< (point) (point-max))
			(or (not (org-get-at-bol 'org-hd-marker))
			    (org-with-point-at (org-get-at-bol 'org-hd-marker)
			      (or (not (bh/is-project-p))
				  (bh/is-project-subtree-p)))))
	      (forward-visible-line 1))
	    (when (< (point) (point-max))
	      (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
	    (forward-visible-line 1)))

                                        ; Pop off the first marker on the list and display
	(setq current-project (pop bh/project-list))
	(when current-project
	  (org-with-point-at current-project
	    (setq bh/hide-scheduled-and-waiting-next-tasks nil)
	    (bh/narrow-to-project))
                                        ; Remove the marker
	  (setq current-project nil)
	  (org-agenda-redo)
	  (beginning-of-buffer)
	  (setq num-projects-left (length bh/project-list))
	  (if (> num-projects-left 0)
	      (message "%s projects left to view" num-projects-left)
	    (beginning-of-buffer)
	    (setq bh/hide-scheduled-and-waiting-next-tasks t)
	    (error "All projects viewed.")))))

    (add-hook 'org-agenda-mode-hook
	      '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
	      'append)

    (add-hook 'org-agenda-mode-hook
	      '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
	      'append)

    (defun bh/set-agenda-restriction-lock (arg)
      "Set restriction lock to current task subtree or file if prefix is specified"
      (interactive "p")
      (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
	     (tags (org-with-point-at pom (org-get-tags-at))))
	(let ((restriction-type (if (equal arg 4) 'file 'subtree)))
	  (save-restriction
	    (cond
	     ((and (equal major-mode 'org-agenda-mode) pom)
	      (org-with-point-at pom
		(org-agenda-set-restriction-lock restriction-type))
	      (org-agenda-redo))
	     ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
	      (org-agenda-set-restriction-lock 'file))
	     (pom
	      (org-with-point-at pom
		(org-agenda-set-restriction-lock restriction-type))))))))

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

    (after-load 'org
      (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
      (when *is-a-mac*
	(define-key org-mode-map (kbd "M-h") nil)
	(define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

    (after-load 'org
      (org-babel-do-load-languages
       'org-babel-load-languages
       `((R . t)
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
         (sqlite . t))))

    ))

(use-package org-pomodoro
  :ensure t
  :config
  (progn
    (setq org-pomodoro-keep-killed-pomodoro-time t)
    (after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))
    ))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init-org)