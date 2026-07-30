;;; gsmlg-editing.el --- Core editing compatibility commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Small editing commands retained from the legacy configuration.  Public
;; commands use the `gsmlg-' prefix; slash-style aliases keep saved keyboard
;; macros and external local configuration working.

;;; Code:

(require 'gsmlg-core)

(declare-function ffap-file-at-point "ffap" ())
(declare-function compile "compile" (command &optional comint))
(declare-function ielm "ielm" ())
(declare-function ns-do-hide-emacs "ns-win" ())
(declare-function ns-do-hide-others "ns-win" ())
(declare-function ns-next-frame "ns-win" ())
(declare-function paredit-mode "paredit" (&optional argument))
(declare-function pp-eval-last-sexp "pp" (arg))
(declare-function vlf "vlf" (file &optional batch-size))
(defvar nxml-mode-map)
(defvar mc/list-file
  (gsmlg-ensure-parent-directory
   (gsmlg-state-file "multiple-cursors/lists.el")))

(defvaralias 'gsmlg/preferred-indent-level 'gsmlg-preferred-indent-width)
(defvaralias 'gsmlg/repl-original-buffer 'gsmlg-repl-origin-buffer)
(defvaralias 'gsmlg/repl-switch-function 'gsmlg-repl-switch-function)

(defgroup gsmlg-editing nil
  "GSMLG editing behavior."
  :group 'editing)

(defcustom gsmlg-repl-switch-function #'switch-to-buffer-other-window
  "Function used to switch between an Emacs Lisp buffer and IELM."
  :type 'function
  :group 'gsmlg-editing)

(defcustom gsmlg-indent-offset-variables
  '(c-basic-offset
    css-indent-offset
    js-indent-level
    js-switch-indent-offset
    json-ts-mode-indent-offset
    nxml-attribute-indent
    nxml-child-indent
    python-indent-offset
    ruby-indent-level
    rust-ts-mode-indent-offset
    sgml-basic-offset
    sh-basic-offset
    standard-indent
    typescript-ts-mode-indent-offset
    web-mode-code-indent-offset
    web-mode-css-indent-offset
    web-mode-markup-indent-offset
    yaml-indent-offset)
  "Mode-specific offset variables changed by `gsmlg-set-indent'.

Only variables already bound by the current major mode are changed."
  :type '(repeat symbol)
  :group 'gsmlg-editing)

(defcustom gsmlg-paredit-minibuffer-commands
  '(eval-expression
    pp-eval-expression
    ibuffer-do-eval
    ibuffer-do-view-and-eval)
  "Commands for which Paredit is enabled in the minibuffer."
  :type '(repeat function)
  :group 'gsmlg-editing)

(defvar-local gsmlg-repl-origin-buffer nil
  "Buffer from which the current REPL buffer was entered.")

(defun gsmlg-enable-paredit-in-minibuffer-maybe ()
  "Enable Paredit for configured Lisp-reading minibuffer commands."
  (when (memq this-command gsmlg-paredit-minibuffer-commands)
    (paredit-mode 1)))

(defun gsmlg-set-indent (&optional width)
  "Set current buffer indentation offsets to WIDTH.

Use `gsmlg-preferred-indent-width' when WIDTH is nil.  Mode-specific
variables are made buffer-local before they are changed."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (let ((indent-width (or width gsmlg-preferred-indent-width)))
    (unless (and (integerp indent-width) (> indent-width 0))
      (user-error "Indent width must be a positive integer"))
    (setq-local tab-width indent-width)
    (dolist (variable gsmlg-indent-offset-variables)
      (when (boundp variable)
        (set (make-local-variable variable) indent-width)))))

(defun gsmlg-newline-at-end-of-line ()
  "Move to the end of the current line, insert a newline, and indent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun gsmlg-kill-back-to-indentation ()
  "Kill text from point back to the first non-whitespace character."
  (interactive)
  (let ((origin (point)))
    (back-to-indentation)
    (kill-region (point) origin)))

(defun gsmlg-backward-up-sexp (arg)
  "Move backward out of ARG enclosing lists, treating a string as one level."
  (interactive "p")
  (let ((parse-state (syntax-ppss)))
    (if (nth 3 parse-state)
        (progn
          (goto-char (nth 8 parse-state))
          (gsmlg-backward-up-sexp (1- arg)))
      (backward-up-list arg))))

(defun gsmlg-open-line-with-reindent (count)
  "Insert COUNT lines after point and indent both sides of the opening.

Preserve an active fill prefix and left margin when the new line would
otherwise be blank."
  (interactive "*p")
  (let* ((copy-fill-prefix (and fill-prefix (bolp)))
         (copy-left-margin (and (bolp) (> (current-left-margin) 0)))
         (origin (point-marker))
         (remaining count)
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline count)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char origin)
    (while (> remaining 0)
      (when (bolp)
        (when copy-left-margin
          (indent-to (current-left-margin)))
        (when copy-fill-prefix
          (insert-and-inherit fill-prefix)))
      (forward-line 1)
      (setq remaining (1- remaining)))
    (goto-char origin)
    (set-marker origin nil)
    (end-of-line)
    (indent-according-to-mode)))

(defun gsmlg-maybe-suspend-frame ()
  "Suspend the current frame except in a graphical macOS session."
  (interactive)
  (unless (and (eq system-type 'darwin) (display-graphic-p))
    (suspend-frame)))

(defun gsmlg-eval-last-sexp-or-region (prefix)
  "Evaluate the active region, or the preceding sexp with PREFIX."
  (interactive "P")
  (if (use-region-p)
      (eval-region (region-beginning) (region-end))
    (pp-eval-last-sexp prefix)))

(defun gsmlg-byte-compile-file-batch (filename)
  "Byte-compile FILENAME in a clean Emacs subprocess."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((command
         (mapconcat
          #'shell-quote-argument
          (list (expand-file-name invocation-name invocation-directory)
                "-Q" "--batch" "--funcall" "batch-byte-compile"
                (expand-file-name filename))
          " ")))
    (compile command)))

(defun gsmlg-cl-libify-next ()
  "Replace the next legacy cl form name with its cl-lib equivalent."
  (interactive)
  (let ((case-fold-search nil)
        (pattern
         (concat
          "("
          (regexp-opt
           '("loop" "incf" "plusp" "first" "decf" "minusp" "assert"
             "case" "destructuring-bind" "second" "third" "defun*"
             "defmacro*" "return-from" "labels" "cadar" "fourth"
             "cadadr")
           t)
          "\\_>")))
    (unless (re-search-forward pattern nil t)
      (user-error "No legacy cl form remains after point"))
    (let ((form (match-string-no-properties 1)))
      (if (member form '("defun*" "defmacro*"))
          (replace-match
           (concat "cl-" (string-remove-suffix "*" form)) t t nil 1)
        (goto-char (match-beginning 1))
        (insert "cl-")))))

(defun gsmlg-switch-to-ielm ()
  "Switch to IELM and remember the current buffer as its origin."
  (interactive)
  (let ((origin (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall gsmlg-repl-switch-function (get-buffer "*ielm*"))
      (ielm))
    (setq gsmlg-repl-origin-buffer origin)))

(defun gsmlg-repl-switch-back ()
  "Return from IELM to the live buffer stored as its origin."
  (interactive)
  (unless (buffer-live-p gsmlg-repl-origin-buffer)
    (user-error "No live originating buffer"))
  (funcall gsmlg-repl-switch-function gsmlg-repl-origin-buffer))

(defun gsmlg-vlf-find-file-at-point ()
  "Open the existing file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (and file (file-exists-p file))
      (user-error "File does not exist: %s" (or file "")))
    (vlf file)))

(defun gsmlg--bind-horizontal-wheel-events ()
  "Ignore horizontal wheel events produced by macOS pointing devices."
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("left" "right"))
      (keymap-global-set
       (format "<%swheel-%s>" multiple direction)
       #'ignore))))

(defun gsmlg--macos-unset-mode-hide-key (key)
  "Allow the global macOS hide binding for KEY in XML buffers."
  (with-eval-after-load 'nxml-mode
    (keymap-unset nxml-mode-map key t)))

(defvar mac-command-modifier)
(defvar mac-option-modifier)

(defun gsmlg-mac-osx-remap-command ()
  "Use Command as Meta and install the Apple-keyboard macOS bindings."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "This key remapping is only available on macOS"))
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none
        mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (setq-default default-input-method "MacOSX")
  (gsmlg--bind-horizontal-wheel-events)
  (keymap-global-set "M-`" #'ns-next-frame)
  (keymap-global-set "M-h" #'ns-do-hide-emacs)
  (keymap-global-set "M-˙" #'ns-do-hide-others)
  (keymap-global-set "M-ˍ" #'ns-do-hide-others)
  (gsmlg--macos-unset-mode-hide-key "M-h"))

(defun gsmlg-mac-osx-unremap-command ()
  "Use Command as Super and install the PC-keyboard macOS bindings."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "This key remapping is only available on macOS"))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (setq-default default-input-method "MacOSX")
  (gsmlg--bind-horizontal-wheel-events)
  (keymap-global-set "s-`" #'ns-next-frame)
  (keymap-global-set "s-h" #'ns-do-hide-emacs)
  (keymap-global-set "s-˙" #'ns-do-hide-others)
  (keymap-global-set "s-ˍ" #'ns-do-hide-others)
  (gsmlg--macos-unset-mode-hide-key "s-h"))

;; Package declarations run only after the Elpaca use-package integration is
;; active.  Requiring this module directly for lint or unit tests therefore
;; cannot fall back to package.el or attempt network access.
(when (bound-and-true-p elpaca-use-package-mode)
  (use-package expand-region
    :no-require t
    :commands er/expand-region)

  (use-package multiple-cursors
    :no-require t
    :defer t)

  (use-package move-dup
    :no-require t
    :commands (move-dup-duplicate-down
               move-dup-duplicate-up
               move-dup-move-lines-down
               move-dup-move-lines-up))

  (use-package paredit
    :no-require t
    :commands paredit-mode
    :hook ((emacs-lisp-mode . paredit-mode)
           (lisp-interaction-mode . paredit-mode)
           (lisp-mode . paredit-mode)
           (minibuffer-setup . gsmlg-enable-paredit-in-minibuffer-maybe)
           (scheme-mode . paredit-mode)))

  (use-package paredit-everywhere
    :no-require t
    :hook ((css-mode . paredit-everywhere-mode)
           (prog-mode . paredit-everywhere-mode)))

  (use-package symbol-overlay
    :no-require t
    :hook ((css-mode . symbol-overlay-mode)
           (html-mode . symbol-overlay-mode)
           (prog-mode . symbol-overlay-mode)))

  (use-package rainbow-delimiters
    :no-require t
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package page-break-lines
    :no-require t
    :hook (after-init . global-page-break-lines-mode))

  (use-package vlf
    :no-require t
    :commands vlf)

  (use-package vundo
    :no-require t
    :commands vundo))

(defalias 'gsmlg/newline-at-end-of-line #'gsmlg-newline-at-end-of-line)
(defalias 'gsmlg/open-line-with-reindent #'gsmlg-open-line-with-reindent)
(defalias 'gsmlg/eval-last-sexp-or-region #'gsmlg-eval-last-sexp-or-region)
(defalias 'gsmlg/switch-to-ielm #'gsmlg-switch-to-ielm)
(defalias 'gsmlg/repl-switch-back #'gsmlg-repl-switch-back)
(defalias 'gsmlg/maybe-suspend-frame #'gsmlg-maybe-suspend-frame)
(defalias 'gsmlg/mac-osx-remap-command #'gsmlg-mac-osx-remap-command)
(defalias 'gsmlg/mac-osx-unremap-command #'gsmlg-mac-osx-unremap-command)
(defalias 'gsmlg/set-indent #'gsmlg-set-indent)
(defalias 'gsmlg/byte-compile-file-batch #'gsmlg-byte-compile-file-batch)
(defalias 'gsmlg/cl-libify-next #'gsmlg-cl-libify-next)
(defalias 'ffap-vlf #'gsmlg-vlf-find-file-at-point)
(defalias 'kill-back-to-indentation #'gsmlg-kill-back-to-indentation)
(defalias 'backward-up-sexp #'gsmlg-backward-up-sexp)

(provide 'gsmlg-editing)
;;; gsmlg-editing.el ends here
