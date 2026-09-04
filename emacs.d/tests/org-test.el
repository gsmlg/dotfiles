;;; org-test.el --- Tests for GSMLG Org configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused behavior tests for the Org workflow.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'use-package)

(declare-function gsmlg-org-available-babel-languages "gsmlg-org" ())
(declare-function gsmlg-org-configure-macos-keys "gsmlg-org" ())
(declare-function org-agenda "org-agenda" (&optional arg keys restriction))
(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function org-clock-goto "org-clock" (&optional select))
(declare-function org-clock-menu "org-clock" ())
(declare-function org-down-element "org" ())
(declare-function org-fold-show-entry "org-fold" ())
(declare-function org-mac-grab-link "org-mac-link" ())
(declare-function org-modern-mode "org-modern" (&optional arg))
(declare-function org-pomodoro "org-pomodoro" ())
(declare-function org-store-link "ol" (&optional arg interactive))
(declare-function org-up-element "org" ())

(unless (require 'gsmlg-paths nil t)
  (defvar gsmlg-config-directory
    (file-name-as-directory
     (expand-file-name ".." (file-name-directory (or load-file-name
                                                     buffer-file-name)))))
  (defvar gsmlg-data-directory
    (file-name-as-directory (make-temp-file "gsmlg-org-data-" t)))
  (provide 'gsmlg-paths))

(defvar gsmlg-state-directory gsmlg-data-directory)

(eval-when-compile
  (cl-letf (((symbol-function #'use-package-ensure-elpa) #'ignore))
    (require 'gsmlg-org)
    (require 'gsmlg-elfeed)))

(cl-letf (((symbol-function #'use-package-ensure-elpa) #'ignore))
  (require 'gsmlg-org))

(ert-deftest gsmlg-org-capture-targets-follow-org-directory ()
  "Capture targets should follow `gsmlg-org-directory'."
  (let ((directory (file-name-as-directory
                    (make-temp-file "gsmlg-org-files-" t))))
    (unwind-protect
        (progn
          (setopt gsmlg-org-directory directory)
          (dolist (entry '(("t" . "todo.org")
                           ("n" . "note.org")
                           ("b" . "bookmark.org")))
            (let* ((template (assoc (car entry) org-capture-templates))
                   (target (nth 3 template)))
              (should (equal target
                             `(file ,(expand-file-name (cdr entry)
                                                      directory)))))))
      (delete-directory directory t))))

(ert-deftest gsmlg-org-agenda-preserves-gtd-workflow ()
  "The custom agenda should retain every legacy GTD section."
  (let* ((command (assoc "g" org-agenda-custom-commands))
         (serialized (prin1-to-string command)))
    (should command)
    (dolist (heading '("Inbox"
                       "Stuck Projects"
                       "Next Actions"
                       "Projects"
                       "Orphaned Tasks"
                       "Waiting"
                       "Delegated"
                       "On Hold"))
      (should (string-match-p (regexp-quote heading) serialized)))
    (should (string-match-p "nottodo" serialized))
    (should-not (string-match-p "nottododo" serialized))))

(ert-deftest gsmlg-org-gtd-agenda-can-be-constructed ()
  "The GTD custom agenda should render without configuration errors."
  (let ((org-file (make-temp-file "gsmlg-org-agenda-" nil ".org"))
        (agenda-buffer-name "*GSMLG Org Agenda Test*"))
    (unwind-protect
        (progn
          (with-temp-file org-file
            (insert "* TODO Standalone task\n"
                    "* PROJECT Project :PROJECT:\n"
                    "** NEXT Project action\n"
                    "* WAITING External action\n"))
          (let ((org-agenda-files (list org-file))
                (org-agenda-buffer-name agenda-buffer-name)
                (org-agenda-sticky nil))
            ;; Bypass bridge advice so this smoke test can use a local
            ;; org-file without interactive configure or feed ownership.
            (cl-letf (((symbol-function #'org-modern-mode) #'ignore)
                      ((symbol-function #'gsmlg-org-note-org--around-agenda)
                       (lambda (orig &rest args)
                         (apply orig args))))
              (save-window-excursion
                (org-agenda nil "g")))
            (should (buffer-live-p (get-buffer agenda-buffer-name)))))
      (when (get-buffer agenda-buffer-name)
        (kill-buffer agenda-buffer-name))
      (delete-file org-file))))

(ert-deftest gsmlg-org-task-and-clock-workflow-is-preserved ()
  "Task states, refiling, logging, and clocking should retain their behavior."
  (should
   (equal org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
            (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
            (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|"
                      "CANCELLED(c@/!)" "MEETING"))))
  (should (equal org-todo-repeat-to-state "NEXT"))
  (should (eq org-log-done 'time))
  (should (equal org-refile-targets
                 '((nil :maxlevel . 5)
                   (org-agenda-files :maxlevel . 5))))
  (should org-refile-use-outline-path)
  (should-not org-outline-path-complete-in-steps)
  (should (eq org-refile-allow-creating-parent-nodes 'confirm))
  (should org-clock-persist)
  (should org-clock-in-resume)
  (should org-clock-into-drawer)
  (should org-log-into-drawer)
  (should org-clock-out-remove-zero-time-clocks)
  (should (equal org-duration-format '((special . h:mm))))
  (should (file-in-directory-p org-clock-persist-file
                               gsmlg-state-directory))
  (should (equal org-archive-location "%s_archive::* Archive")))

(ert-deftest gsmlg-org-host-paths-are-configurable-and-optional ()
  "Host-specific agenda and PlantUML paths should be safe."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "gsmlg-org-paths-" t)))
         (agenda-source (expand-file-name ".agenda_files" directory))
         (missing-jar (expand-file-name "missing-plantuml.jar" directory)))
    (unwind-protect
        (progn
          (should-not (boundp 'gsmlg-org-mobile-directory))
          (setopt gsmlg-org-directory directory
                  gsmlg-org-agenda-files agenda-source
                  gsmlg-org-plantuml-jar-path missing-jar)
          ;; Phase 1: when the Org Note bridge is active, agenda files
          ;; are feed-only; otherwise they follow the host path setting.
          (if (bound-and-true-p gsmlg-org-note-org--activated)
              (progn
                (should (fboundp #'gsmlg-org-note-org-agenda-files))
                (should (equal org-agenda-files
                               (gsmlg-org-note-org-agenda-files)))
                (should-not (equal org-agenda-files agenda-source)))
            (should (equal org-agenda-files agenda-source)))
          (should-not org-plantuml-jar-path)
          (should (eq org-plantuml-exec-mode 'plantuml)))
      (delete-directory directory t))))

(ert-deftest gsmlg-org-capture-frame-marks-and-cleans-up ()
  "Dedicated capture frames are tagged and removed after finalize."
  (let (parameters deleted)
    (cl-letf (((symbol-function #'make-frame)
               (lambda (&optional params)
                 (setq parameters params)
                 (selected-frame)))
              ((symbol-function #'select-frame-set-input-focus) #'ignore)
              ((symbol-function #'org-capture) #'ignore)
              ((symbol-function #'frame-list)
               (lambda () (list (selected-frame))))
              ((symbol-function #'frame-live-p) (lambda (_frame) t))
              ((symbol-function #'frame-parameter)
               (lambda (_frame param)
                 (and (eq param 'gsmlg-org-capture)
                      (alist-get 'gsmlg-org-capture parameters))))
              ((symbol-function #'delete-frame)
               (lambda (&optional frame _force)
                 (setq deleted (or frame (selected-frame))))))
      (gsmlg-org-capture-frame)
      (should (eq (alist-get 'gsmlg-org-capture parameters) t))
      (should-not (alist-get 'top parameters))
      (should-not (alist-get 'left parameters))
      (should (memq #'gsmlg-org--delete-capture-frame
                    org-capture-after-finalize-hook))
      (gsmlg-org--delete-capture-frame)
      (should deleted))))

(ert-deftest gsmlg-org-keys-speed-commands-and-aliases-are-preserved ()
  "Org keys, speed commands, and external compatibility names should survive."
  (should (eq (key-binding (kbd "C-c l")) #'org-store-link))
  (should (eq (key-binding (kbd "C-c a")) #'org-agenda))
  (should (eq (key-binding (kbd "C-c c")) #'org-capture))
  (should (eq (lookup-key org-mode-map (kbd "C-M-<up>"))
              #'org-up-element))
  (should (eq (lookup-key org-mode-map (kbd "C-M-<down>"))
              #'org-down-element))
  (should
   (equal org-speed-commands
          '(("h" . gsmlg-org-hide-other)
            ("k" . org-kill-note-or-show-branches)
            ("q" . org-agenda)
            ("s" . org-save-all-org-buffers)
            ("w" . org-refile)
            ("z" . org-add-note)
            ("J" . org-clock-goto)
            ("P" . org-pomodoro)
            ("W" . gsmlg-org-widen))))
  (should (eq (lookup-key org-agenda-mode-map (kbd "P")) #'org-pomodoro))
  (should (eq (lookup-key org-clock-mode-line-map
                          [header-line mouse-2])
              #'org-clock-goto))
  (should (eq (lookup-key org-clock-mode-line-map
                          [header-line mouse-1])
              #'org-clock-menu))
  (should (memq #'org-fold-show-entry org-agenda-after-show-hook))
  (should (featurep 'org-protocol))
  (should (eq (indirect-function 'gsmlg/org-hide-other)
              (indirect-function 'gsmlg-org-hide-other)))
  (should (eq (indirect-function 'bh/widen)
              (indirect-function 'gsmlg-org-widen)))
  (should (eq (indirect-function 'make-orgcapture-frame)
              (indirect-function 'gsmlg-org-capture-frame))))

(ert-deftest gsmlg-org-macos-keys-preserve-fall-through ()
  "Platform Org keys should retain fall-through and optional link capture."
  (let ((old-meta-h (lookup-key org-mode-map (kbd "M-h")))
        (old-capture (lookup-key org-mode-map (kbd "C-c g")))
        (system-type 'darwin))
    (unwind-protect
        (cl-letf (((symbol-function #'org-mac-grab-link) #'ignore))
          (gsmlg-org-configure-macos-keys)
          (should-not (lookup-key org-mode-map (kbd "M-h")))
          (should (eq (lookup-key org-mode-map (kbd "C-c g"))
                      #'org-mac-grab-link)))
      (if old-meta-h
          (keymap-set org-mode-map "M-h" old-meta-h)
        (keymap-unset org-mode-map "M-h"))
      (if old-capture
          (keymap-set org-mode-map "C-c g" old-capture)
        (keymap-unset org-mode-map "C-c g")))))

(ert-deftest gsmlg-org-babel-skips-unavailable-and-disabled-languages ()
  "Babel should load only enabled languages whose libraries are available."
  (let ((gsmlg-org-babel-languages
         '((emacs-lisp . t)
           (gnuplot . t)
           (haskell . nil))))
    (cl-letf (((symbol-function #'locate-library)
               (lambda (library &rest _)
                 (and (equal library "ob-emacs-lisp")
                      "/mock/ob-emacs-lisp.el"))))
      (should
       (equal (gsmlg-org-available-babel-languages)
              '((emacs-lisp . t)))))))

(ert-deftest gsmlg-elfeed-uses-config-feed-and-xdg-data ()
  "Elfeed should read the tracked feed file and keep its database in data."
  (cl-letf (((symbol-function #'use-package-ensure-elpa) #'ignore))
    (require 'gsmlg-elfeed))
  (should
   (equal gsmlg-elfeed-feed-file
          (expand-file-name "elfeed.org" gsmlg-config-directory)))
  (should
   (equal elfeed-db-directory
          (file-name-as-directory
           (expand-file-name "elfeed/" gsmlg-data-directory))))
  (should (file-directory-p elfeed-db-directory)))

(provide 'org-test)
;;; org-test.el ends here
