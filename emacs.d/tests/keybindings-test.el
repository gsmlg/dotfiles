;;; keybindings-test.el --- Keybinding compatibility tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify the public keybinding contract and stateful search wrappers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-keybindings)

(declare-function corfu-next "corfu" ())
(declare-function corfu-previous "corfu" ())
(declare-function paredit-mode "paredit" (&optional argument))
(declare-function paredit-newline "paredit" ())
(declare-function magit-display-buffer-fullframe-status-v1
                  "magit-mode" (buffer))
(declare-function transient-get-suffix "transient" (prefix loc))
(declare-function vertico-next "vertico" ())
(declare-function vertico-previous "vertico" ())
(defvar corfu-map)
(defvar magit-diff-refine-hunk)
(defvar magit-display-buffer-function)
(defvar vertico-map)

(defun gsmlg-test-keybindings--map (map-symbol)
  "Return the keymap stored in MAP-SYMBOL."
  (and (boundp map-symbol)
       (symbol-value map-symbol)))

(defun gsmlg-test-keybindings--assert-contract-entry (entry)
  "Assert that keybinding contract ENTRY is installed."
  (let ((feature (plist-get entry :feature))
        (optional (plist-get entry :optional))
        (platform (plist-get entry :platform))
        (profile (plist-get entry :profile))
        (map-symbol (plist-get entry :map))
        (key (plist-get entry :key))
        (expected (plist-get entry :command)))
    (when (and platform (not (eq system-type platform)))
      (ert-skip (format "Binding only applies on %s" platform)))
    (when feature
      (if optional
          (unless (require feature nil t)
            (ert-skip (format "Optional feature %s is unavailable" feature)))
        (should (require feature nil t))))
    (unwind-protect
        (progn
          (when (eq profile 'macos-pc)
            (gsmlg-mac-osx-unremap-command))
          (let ((map (gsmlg-test-keybindings--map map-symbol)))
            (should (keymapp map))
            (should (eq (lookup-key map (key-parse key)) expected))))
      (when (eq profile 'macos-pc)
        (gsmlg-mac-osx-remap-command)))))

(let ((index 0))
  (dolist (entry gsmlg-keybinding-contract)
    (setq index (1+ index))
    (let ((test-name (intern (format "gsmlg-keybinding-contract-%03d" index))))
      (eval
       `(ert-deftest ,test-name ()
          (gsmlg-test-keybindings--assert-contract-entry ',entry))))))

(ert-deftest gsmlg-keybinding-prefixes-are-installed ()
  "The three compatibility prefix maps are reachable globally."
  (should (eq (lookup-key global-map (key-parse "C-c m"))
              gsmlg-multiple-cursors-prefix-map))
  (should (eq (lookup-key global-map (key-parse "C-x p"))
              gsmlg-project-prefix-map))
  (should-not (lookup-key global-map (key-parse "C-c p")))
  (should (eq (lookup-key global-map (key-parse "C-, r"))
              gsmlg-refactor-prefix-map)))

(ert-deftest gsmlg-project-prefix-commands-are-interactive ()
  "Every retained project-prefix target is an interactive command."
  (should (featurep 'gsmlg-project))
  (dolist (key '("p" "f" "b" "d" "D" "k" "c" "e" "s" "S" "!" "&"
                 "?" "g" "I" "o" "q" "r" "v"))
    (should (commandp (lookup-key gsmlg-project-prefix-map
                                  (key-parse key))))))

(ert-deftest gsmlg-refactor-prefix-commands-are-interactive ()
  "Every language-refactor target is an interactive command."
  (should (featurep 'gsmlg-eglot))
  (dolist (key '("r" "a" "f" "o"))
    (should (commandp (lookup-key gsmlg-refactor-prefix-map
                                  (key-parse key))))))

(ert-deftest gsmlg-diff-hl-transient-retains-all-heads ()
  "The diff-hl Transient maps every legacy menu key to its new command."
  (should (featurep 'gsmlg-vcs))
  (should (require 'transient nil t))
  (dolist (binding
           '(("j" . diff-hl-next-hunk)
             ("k" . diff-hl-previous-hunk)
             ("h" . gsmlg-diff-hl-first-hunk)
             ("l" . gsmlg-diff-hl-last-hunk)
             ("s" . diff-hl-stage-current-hunk)
             ("r" . diff-hl-revert-hunk)
             ("p" . diff-hl-show-hunk)
             ("R" . diff-hl-set-reference-rev-in-project)
             ("q" . gsmlg-diff-hl-transient-quit)
             ("Q" . gsmlg-diff-hl-disable)))
    (should
     (equal
      (transient-get-suffix 'gsmlg-diff-hl-transient (car binding))
      (transient-get-suffix 'gsmlg-diff-hl-transient (cdr binding))))))

(ert-deftest gsmlg-diff-hl-first-hunk-includes-point-min ()
  "The first-hunk wrapper must not skip a hunk beginning at `point-min'."
  (with-temp-buffer
    (insert "first hunk\nunchanged\nsecond hunk\n")
    (let ((first (make-overlay (point-min) (+ (point-min) 5)))
          (second (make-overlay (- (point-max) 7) (point-max)))
          (diff-hl-next-previous-hunk-auto-recenter nil))
      (overlay-put first 'diff-hl-hunk t)
      (overlay-put second 'diff-hl-hunk t)
      (goto-char (point-max))
      (gsmlg-diff-hl-first-hunk)
      (should (= (point) (overlay-start first))))))

(ert-deftest gsmlg-keybinding-help-remains-available-on-f1 ()
  "The deletion remap must leave the function-key Help prefix available."
  (should (eq (key-binding (key-parse "C-h")) #'delete-backward-char))
  (should (keymapp (lookup-key global-map (key-parse "<f1>"))))
  (should (eq (key-binding (key-parse "<f1> K")) #'find-function-on-key)))

(ert-deftest gsmlg-macos-modifier-profiles-support-ns-and-mac-builds ()
  "Both macOS builds receive the requested Command and Option mappings."
  (let ((system-type 'darwin)
        (mac-command-modifier 'super)
        (mac-option-modifier 'meta)
        (ns-command-modifier 'super)
        (ns-option-modifier 'meta))
    (gsmlg-mac-osx-remap-command)
    (should (eq mac-command-modifier 'meta))
    (should (eq mac-option-modifier 'none))
    (should (eq ns-command-modifier 'meta))
    (should (eq ns-option-modifier 'none))
    (gsmlg-mac-osx-unremap-command)
    (should (eq mac-command-modifier 'super))
    (should (eq mac-option-modifier 'meta))
    (should (eq ns-command-modifier 'super))
    (should (eq ns-option-modifier 'meta))))

(ert-deftest gsmlg-vertico-preserves-kill-ring-navigation-intent ()
  "The replacement chooser retains quit and item navigation keys."
  (let ((vertico-map (make-sparse-keymap)))
    (gsmlg--configure-vertico-keys)
    (should (eq (lookup-key vertico-map (key-parse "C-g"))
                #'abort-recursive-edit))
    (should (eq (lookup-key vertico-map (key-parse "M-n"))
                #'vertico-next))
    (should (eq (lookup-key vertico-map (key-parse "M-p"))
                #'vertico-previous))
    (should (eq (lookup-key vertico-map (key-parse "C-l"))
                #'vertico-directory-up))
    (should (eq (lookup-key vertico-map (key-parse "C-j"))
                #'vertico-directory-enter))))

(ert-deftest gsmlg-corfu-preserves-popup-navigation-intent ()
  "The replacement completion popup retains the legacy navigation keys."
  (let ((corfu-map (make-sparse-keymap)))
    (gsmlg--configure-corfu-keys)
    (should (eq (lookup-key corfu-map (key-parse "M-/"))
                #'corfu-next))
    (should (eq (lookup-key corfu-map (key-parse "C-n"))
                #'corfu-next))
    (should (eq (lookup-key corfu-map (key-parse "C-p"))
                #'corfu-previous))))

(ert-deftest gsmlg-consult-line-remembers-public-minibuffer-history ()
  "Line search records its origin and accepted query without private APIs."
  (let ((gsmlg-consult-line-history nil)
        (gsmlg-consult-line-origin-marker nil))
    (cl-letf (((symbol-function 'consult-line)
               (lambda (&optional initial _start)
                 (with-temp-buffer
                   (run-hooks 'minibuffer-setup-hook)
                   (insert (or initial "needle"))
                   (run-hooks 'minibuffer-exit-hook)))))
      (with-temp-buffer
        (insert "origin")
        (goto-char 4)
        (gsmlg-consult-line)
        (should (equal (car gsmlg-consult-line-history) "needle"))
        (should (eq (marker-buffer gsmlg-consult-line-origin-marker)
                    (current-buffer)))
        (should (= (marker-position gsmlg-consult-line-origin-marker) 4))))))

(ert-deftest gsmlg-consult-line-resume-restores-origin-and-query ()
  "Resume returns to the previous origin and seeds the previous query."
  (let ((origin-buffer (generate-new-buffer " *gsmlg-line-origin*"))
        (other-buffer (generate-new-buffer " *gsmlg-line-other*"))
        (gsmlg-consult-line-history '("needle"))
        gsmlg-consult-line-origin-marker
        seen-initial)
    (unwind-protect
        (progn
          (with-current-buffer origin-buffer
            (insert "abcdef")
            (setq gsmlg-consult-line-origin-marker (copy-marker 4)))
          (switch-to-buffer other-buffer)
          (cl-letf (((symbol-function 'consult-line)
                     (lambda (&optional initial _start)
                       (setq seen-initial initial))))
            (gsmlg-consult-line-resume))
          (should (eq (current-buffer) origin-buffer))
          (should (= (point) 4))
          (should (equal seen-initial "needle")))
      (when (buffer-live-p origin-buffer)
        (kill-buffer origin-buffer))
      (when (buffer-live-p other-buffer)
        (kill-buffer other-buffer)))))

(ert-deftest gsmlg-consult-line-from-isearch-seeds-active-query ()
  "The Isearch bridge exits Isearch and passes its active text to Consult."
  (let ((isearch-string "active query")
        exited
        seen-initial)
    (cl-letf (((symbol-function 'isearch-exit)
               (lambda () (setq exited t)))
              ((symbol-function 'consult-line)
               (lambda (&optional initial _start)
                 (setq seen-initial initial))))
      (gsmlg-consult-line-from-isearch))
    (should exited)
    (should (equal seen-initial "active query"))))

(ert-deftest gsmlg-editing-compatibility-aliases-remain-callable ()
  "Saved macros can still call the old slash-style editing command names."
  (dolist (pair '((gsmlg/newline-at-end-of-line . gsmlg-newline-at-end-of-line)
                  (gsmlg/open-line-with-reindent . gsmlg-open-line-with-reindent)
                  (gsmlg/eval-last-sexp-or-region . gsmlg-eval-last-sexp-or-region)
                  (gsmlg/switch-to-ielm . gsmlg-switch-to-ielm)
                  (gsmlg/repl-switch-back . gsmlg-repl-switch-back)
                  (gsmlg/maybe-suspend-frame . gsmlg-maybe-suspend-frame)
                  (gsmlg/set-indent . gsmlg-set-indent)
                  (gsmlg/mac-osx-remap-command . gsmlg-mac-osx-remap-command)
                  (gsmlg/mac-osx-unremap-command . gsmlg-mac-osx-unremap-command)
                  (gsmlg/byte-compile-file-batch . gsmlg-byte-compile-file-batch)
                  (gsmlg/cl-libify-next . gsmlg-cl-libify-next)
                  (ffap-vlf . gsmlg-vlf-find-file-at-point)))
    (should (fboundp (car pair)))
    (should (fboundp (cdr pair)))
    (should (eq (indirect-function (car pair))
                (indirect-function (cdr pair))))))

(ert-deftest gsmlg-cl-libify-next-preserves-the-legacy-command-intent ()
  "The compatibility command should modernize the next legacy cl form."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(loop for item in items collect item)\n(defun* example () t)")
    (goto-char (point-min))
    (gsmlg-cl-libify-next)
    (should (looking-at "loop"))
    (should (equal (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))
                   "(cl-loop for item in items collect item)"))
    (forward-line 1)
    (gsmlg-cl-libify-next)
    (should (equal (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))
                   "(cl-defun example () t)"))))

(ert-deftest gsmlg-magit-retains-refined-full-frame-status ()
  "The migrated Magit workflow should retain refined, full-frame status."
  (should (require 'magit nil t))
  (should magit-diff-refine-hunk)
  (should (eq magit-display-buffer-function
              #'magit-display-buffer-fullframe-status-v1)))

(ert-deftest gsmlg-editing-uses-built-in-electric-editing ()
  "The configuration should enable built-in electric editing modes."
  (should electric-pair-mode)
  (should electric-indent-mode))

(ert-deftest gsmlg-editing-compatibility-variable-aliases-remain ()
  "External local files can still use the old slash-style variable names."
  (should (eq (indirect-variable 'gsmlg/preferred-indent-level)
              'gsmlg-preferred-indent-width))
  (should (eq (indirect-variable 'gsmlg/repl-original-buffer)
              'gsmlg-repl-origin-buffer))
  (should (eq (indirect-variable 'gsmlg/repl-switch-function)
              'gsmlg-repl-switch-function)))

(ert-deftest gsmlg-vlf-find-file-at-point-validates-and-opens-file ()
  "The VLF compatibility command forwards an existing file at point."
  (let ((file (make-temp-file "gsmlg-vlf-"))
        opened)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'ffap-file-at-point)
                     (lambda () file))
                    ((symbol-function 'vlf)
                     (lambda (selected &optional _batch-size)
                       (setq opened selected))))
            (gsmlg-vlf-find-file-at-point))
          (should (equal opened file)))
      (delete-file file))))

(ert-deftest gsmlg-kill-back-to-indentation-preserves-editing-intent ()
  "The indentation deletion wrapper kills from point back to indentation."
  (with-temp-buffer
    (insert "  alpha")
    (goto-char (point-max))
    (gsmlg-kill-back-to-indentation)
    (should (equal (buffer-string) "  "))))

(ert-deftest gsmlg-set-indent-is-buffer-local ()
  "The indent compatibility command changes only the current buffer."
  (let ((default-tab-width (default-value 'tab-width)))
    (with-temp-buffer
      (setq-local css-indent-offset 8)
      (gsmlg-set-indent 4)
      (should (= tab-width 4))
      (should (= css-indent-offset 4)))
    (should (= (default-value 'tab-width) default-tab-width))))

(ert-deftest gsmlg-paredit-newline-preserves-structural-editing ()
  "Paredit retains its structural newline outside REPLs and minibuffers."
  (should (require 'paredit nil t))
  (with-temp-buffer
    (emacs-lisp-mode)
    (paredit-mode 1)
    (should (eq (key-binding (key-parse "RET")) #'paredit-newline))))

(ert-deftest gsmlg-paredit-newline-does-not-override-repls ()
  "Paredit leaves its return key unchanged in an Emacs Lisp REPL."
  (let ((major-mode 'inferior-emacs-lisp-mode)
        (minor-mode-overriding-map-alist nil))
    (gsmlg-paredit-newline-maybe)
    (should-not (assq 'paredit-mode minor-mode-overriding-map-alist))))

(ert-deftest gsmlg-paredit-minibuffer-enablement-is-command-scoped ()
  "Paredit is enabled only for configured Lisp-reading minibuffer commands."
  (let ((this-command 'pp-eval-expression)
        enabled)
    (cl-letf (((symbol-function 'paredit-mode)
               (lambda (&optional argument)
                 (setq enabled argument))))
      (gsmlg-enable-paredit-in-minibuffer-maybe))
    (should (= enabled 1)))
  (let ((this-command 'execute-extended-command)
        enabled)
    (cl-letf (((symbol-function 'paredit-mode)
               (lambda (&optional argument)
                 (setq enabled argument))))
      (gsmlg-enable-paredit-in-minibuffer-maybe))
    (should-not enabled)))

(ert-deftest gsmlg-backward-up-sexp-is-quote-aware ()
  "The upward-sexp wrapper treats the containing string as one level."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(message \"inside\")")
    (search-backward "inside")
    (gsmlg-backward-up-sexp 1)
    (should (eq (char-after) ?\"))))

(provide 'keybindings-test)
;;; keybindings-test.el ends here
