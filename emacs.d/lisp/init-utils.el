;;----------------------------------------------------------------------------
;; Custom setting
;;----------------------------------------------------------------------------
(defcustom gsmlg/cache-directory (expand-file-name ".cache/" user-emacs-directory)
  "Defind the default cache directory")

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(setq max-specpdl-size 32000)


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;; define some function

;; set indent level for all mode
(defcustom gsmlg/preferred-indent-level 4 "Preferred indent level for all modes")

(defun gsmlg/set-indent (&optional width)
  "set the indent of each language mode,
now in js js2 coffeescript sgml(html,xml) sh(shell) c ruby css
should be set as same width"
  (interactive)
  (let ((indent-width (or width gsmlg/preferred-indent-level)))
    (setq js2-basic-offset indent-width
          js-switch-indent-offset indent-width
          js-indent-level indent-width
          coffee-tab-width indent-width
          sgml-basic-offset indent-width
          sh-basic-offset indent-width
          c-basic-offset indent-width
          ruby-indent-level indent-width
          css-indent-offset indent-width
          yaml-indent-offset indent-width
          )))

;; remap Command key binding when use macOS keyboard
(defun gsmlg/mac-osx-remap-command ()
  (interactive)
  (progn
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none)
    (setq-default default-input-method "MacOSX")
    ;; Make mouse wheel / trackpad scrolling less jerky
    (setq mouse-wheel-scroll-amount '(1
                                      ((shift) . 5)
                                      ((control))))
    (dolist (multiple '("" "double-" "triple-"))
      (dolist (direction '("right" "left"))
        (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
    (global-set-key (kbd "M-`") 'ns-next-frame)
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
    (global-set-key (kbd "M-˙") 'ns-do-hide-others)
    (after-load 'nxml-mode
      (define-key nxml-mode-map (kbd "M-h") nil))
    (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
    ))

;; remap Command key binding when use normal keyboard in macOS
(defun gsmlg/mac-osx-unremap-command ()
  (interactive)
  (progn
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq-default default-input-method "MacOSX")
    ;; Make mouse wheel / trackpad scrolling less jerky
    (setq mouse-wheel-scroll-amount '(1
                                      ((shift) . 5)
                                      ((control))))
    (dolist (multiple '("" "double-" "triple-"))
      (dolist (direction '("right" "left"))
        (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
    (global-set-key (kbd "s-`") 'ns-next-frame)
    (global-set-key (kbd "s-h") 'ns-do-hide-emacs)
    (global-set-key (kbd "s-˙") 'ns-do-hide-others)
    (after-load 'nxml-mode
      (define-key nxml-mode-map (kbd "s-h") nil))
    (global-set-key (kbd "s-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
    ))


(provide 'init-utils)
