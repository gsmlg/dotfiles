;;; gsmlg-keybindings.el --- Legacy-compatible keybindings -*- lexical-binding: t; -*-

;;; Commentary:

;; This module owns the machine-readable legacy keybinding contract and the
;; small compatibility wrappers needed by the modern completion stack.

;;; Code:

(require 'gsmlg-editing)
(require 'isearch)
(require 'subr-x)

(declare-function consult-buffer "consult" ())
(declare-function consult-imenu "consult" ())
(declare-function consult-line "consult" (&optional initial start))
(declare-function consult-line-multi "consult" ())
(declare-function consult-recent-file "consult" ())
(declare-function consult-yank-pop "consult" ())
(declare-function corfu-next "corfu" ())
(declare-function corfu-previous "corfu" ())
(declare-function eglot-code-actions "eglot" ())
(declare-function eglot-rename "eglot" ())
(declare-function embark-act "embark" ())
(declare-function embark-dwim "embark" ())
(declare-function er/expand-region "expand-region" (arg))
(declare-function ert-results-rerun-all-tests "ert" ())
(declare-function git-messenger:popup-message "git-messenger" ())
(declare-function gsmlg-diff-hl-transient "gsmlg-vcs" ())
(declare-function gsmlg-eglot-organize-imports "gsmlg-eglot" ())
(declare-function gsmlg-format-buffer "gsmlg-format" ())
(declare-function gsmlg-project-search "gsmlg-project" ())
(declare-function macrostep-expand "macrostep" ())
(declare-function magit-section-up "magit-section" ())
(declare-function magit-status "magit" (&optional directory cache))
(declare-function mc/edit-beginnings-of-lines "mc-edit-lines" ())
(declare-function mc/edit-ends-of-lines "mc-edit-lines" ())
(declare-function mc/edit-lines "mc-edit-lines" (&optional arg))
(declare-function mc/mark-all-like-this "mc-mark-more" ())
(declare-function mc/mark-next-like-this "mc-mark-more" (arg))
(declare-function mc/mark-previous-like-this "mc-mark-more" (arg))
(declare-function move-dup-duplicate-down "move-dup" (&optional arg))
(declare-function move-dup-duplicate-up "move-dup" (&optional arg))
(declare-function move-dup-move-lines-down "move-dup" (&optional arg))
(declare-function move-dup-move-lines-up "move-dup" (&optional arg))
(declare-function org-clock-goto "org-clock" (&optional select))
(declare-function org-clock-menu "org-clock" ())
(declare-function org-down-element "org" ())
(declare-function org-mac-grab-link "org-mac-link" ())
(declare-function org-pomodoro "org-pomodoro" ())
(declare-function org-store-link "org" (&optional arg interactive))
(declare-function org-up-element "org" ())
(declare-function paredit-newline "paredit" ())
(declare-function set-rectangular-region-anchor "rectangular-region-mode" ())
(declare-function symbol-overlay-jump-next "symbol-overlay" ())
(declare-function symbol-overlay-jump-prev "symbol-overlay" ())
(declare-function vertico-next "vertico" ())
(declare-function vertico-previous "vertico" ())
(declare-function vertico-directory-enter "vertico-directory" (&optional arg))
(declare-function vertico-directory-up "vertico-directory" (&optional n))
(declare-function vundo "vundo" ())
(declare-function which-key-add-keymap-based-replacements "which-key" (keymap &rest replacements))

(defvar corfu-map)
(defvar ert-results-mode-map)
(defvar ielm-map)
(defvar magit-mode-map)
(defvar magit-status-mode-map)
(defvar nxml-mode-map)
(defvar org-agenda-mode-map)
(defvar org-clock-mode-line-map)
(defvar org-mode-map)
(defvar paredit-everywhere-mode-map)
(defvar paredit-mode-map)
(defvar symbol-overlay-mode-map)
(defvar vertico-map)

(defconst gsmlg-keybinding-contract
  '((:map global-map :key "C-c j" :command join-line :status exact)
    (:map global-map :key "C-h" :command delete-backward-char :status exact)
    (:map global-map :key "RET" :command newline-and-indent :status exact)
    (:map global-map :key "S-<return>" :command gsmlg-newline-at-end-of-line :status exact)
    (:map global-map :key "M-Z" :command zap-up-to-char :status exact)
    (:map global-map :key "M-Y" :command consult-yank-pop :status semantic-replacement)
    (:map vertico-map :key "C-g" :command abort-recursive-edit :status semantic-replacement
          :feature vertico)
    (:map vertico-map :key "M-n" :command vertico-next :status semantic-replacement
          :feature vertico)
    (:map vertico-map :key "M-p" :command vertico-previous :status semantic-replacement
          :feature vertico)
    (:map vertico-map :key "C-l" :command vertico-directory-up :status semantic-replacement
          :feature vertico-directory)
    (:map vertico-map :key "C-j" :command vertico-directory-enter :status semantic-replacement
          :feature vertico-directory)
    (:map global-map :key "C-=" :command er/expand-region :status exact)
    (:map global-map :key "C-." :command set-mark-command :status exact)
    (:map global-map :key "C-x C-." :command pop-global-mark :status exact)
    (:map global-map :key "C-<" :command mc/mark-previous-like-this :status exact)
    (:map global-map :key "C->" :command mc/mark-next-like-this :status exact)
    (:map global-map :key "C-+" :command mc/mark-next-like-this :status exact)
    (:map global-map :key "C-c C-<" :command mc/mark-all-like-this :status exact)
    (:map gsmlg-multiple-cursors-prefix-map :key "r" :command set-rectangular-region-anchor :status exact)
    (:map gsmlg-multiple-cursors-prefix-map :key "c" :command mc/edit-lines :status exact)
    (:map gsmlg-multiple-cursors-prefix-map :key "e" :command mc/edit-ends-of-lines :status exact)
    (:map gsmlg-multiple-cursors-prefix-map :key "a" :command mc/edit-beginnings-of-lines :status exact)
    (:map global-map :key "M-<left>" :command nil :status exact)
    (:map global-map :key "M-<right>" :command nil :status exact)
    (:map global-map :key "C-M-<backspace>" :command gsmlg-kill-back-to-indentation :status exact)
    (:map global-map :key "M-<up>" :command move-dup-move-lines-up :status semantic-replacement)
    (:map global-map :key "M-<down>" :command move-dup-move-lines-down :status semantic-replacement)
    (:map global-map :key "M-S-<up>" :command move-dup-move-lines-up :status semantic-replacement)
    (:map global-map :key "M-S-<down>" :command move-dup-move-lines-down :status semantic-replacement)
    (:map global-map :key "C-S-<up>" :command move-dup-move-lines-up :status semantic-replacement)
    (:map global-map :key "C-S-<down>" :command move-dup-move-lines-down :status semantic-replacement)
    (:map global-map :key "C-c d" :command move-dup-duplicate-down :status semantic-replacement)
    (:map global-map :key "C-c u" :command move-dup-duplicate-up :status semantic-replacement)
    (:map global-map :key "<remap> <backward-up-list>" :command gsmlg-backward-up-sexp :status exact)
    (:map global-map :key "C-o" :command gsmlg-open-line-with-reindent :status exact)
    (:map global-map :key "C-z" :command gsmlg-maybe-suspend-frame :status exact)
    (:map global-map :key "C-x C-b" :command ibuffer :status exact)
    (:map global-map :key "C-x u" :command vundo :status semantic-replacement)
    (:map global-map :key "M-`" :command ns-next-frame :status exact :platform darwin)
    (:map global-map :key "M-h" :command ns-do-hide-emacs :status exact :platform darwin)
    (:map global-map :key "M-˙" :command ns-do-hide-others :status exact :platform darwin)
    (:map global-map :key "M-ˍ" :command ns-do-hide-others :status exact :platform darwin)
    (:map global-map :key "<wheel-left>" :command ignore :status exact :platform darwin)
    (:map global-map :key "<wheel-right>" :command ignore :status exact :platform darwin)
    (:map global-map :key "<double-wheel-left>" :command ignore :status exact :platform darwin)
    (:map global-map :key "<double-wheel-right>" :command ignore :status exact :platform darwin)
    (:map global-map :key "<triple-wheel-left>" :command ignore :status exact :platform darwin)
    (:map global-map :key "<triple-wheel-right>" :command ignore :status exact :platform darwin)
    (:map nxml-mode-map :key "M-h" :command nil :status exact :platform darwin :feature nxml-mode)
    (:map global-map :key "s-`" :command ns-next-frame :status exact
          :platform darwin :profile macos-pc)
    (:map global-map :key "s-h" :command ns-do-hide-emacs :status exact
          :platform darwin :profile macos-pc)
    (:map global-map :key "s-˙" :command ns-do-hide-others :status exact
          :platform darwin :profile macos-pc)
    (:map global-map :key "s-ˍ" :command ns-do-hide-others :status exact
          :platform darwin :profile macos-pc)
    (:map nxml-mode-map :key "s-h" :command nil :status exact
          :platform darwin :profile macos-pc :feature nxml-mode)

    (:map global-map :key "M-x" :command execute-extended-command :status semantic-replacement)
    (:map global-map :key "C-x C-m" :command execute-extended-command :status semantic-replacement)
    (:map global-map :key "C-x C-f" :command find-file :status semantic-replacement)
    (:map global-map :key "C-c r" :command consult-recent-file :status semantic-replacement)
    (:map global-map :key "C-c i" :command consult-imenu :status semantic-replacement)
    (:map global-map :key "C-x b" :command consult-buffer :status semantic-replacement)
    (:map global-map :key "M-i" :command gsmlg-consult-line :status semantic-replacement)
    (:map global-map :key "M-I" :command gsmlg-consult-line-resume :status semantic-replacement)
    (:map global-map :key "C-c M-i" :command consult-line-multi :status semantic-replacement)
    (:map global-map :key "C-x M-i" :command consult-line-multi :status semantic-replacement)
    (:map isearch-mode-map :key "M-i" :command gsmlg-consult-line-from-isearch :status semantic-replacement
          :feature isearch)
    (:map global-map :key "C-;" :command embark-act :status intentional-deviation)
    (:map global-map :key "C-c C-;" :command embark-dwim :status intentional-deviation)

    (:map global-map :key "C-M-/" :command completion-at-point :status semantic-replacement)
    (:map global-map :key "M-/" :command completion-at-point :status semantic-replacement)
    (:map corfu-map :key "M-/" :command corfu-next :status semantic-replacement :feature corfu)
    (:map corfu-map :key "C-n" :command corfu-next :status semantic-replacement :feature corfu)
    (:map corfu-map :key "C-p" :command corfu-previous :status semantic-replacement :feature corfu)

    (:map gsmlg-project-prefix-map :key "p" :command project-switch-project :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "f" :command project-find-file :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "b" :command project-switch-to-buffer :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "d" :command project-dired :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "D" :command project-dired :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "k" :command project-kill-buffers :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "c" :command project-compile :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "e" :command project-eshell :status intentional-deviation)
    (:map gsmlg-project-prefix-map :key "s" :command gsmlg-project-search :status intentional-deviation)
    (:map gsmlg-project-prefix-map :key "S" :command project-eshell :status intentional-deviation)
    (:map gsmlg-project-prefix-map :key "!" :command project-shell-command :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "&" :command project-async-shell-command :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "?" :command xref-find-references :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "g" :command gsmlg-project-search :status intentional-deviation)
    (:map gsmlg-project-prefix-map :key "I" :command project-list-buffers :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "o" :command gsmlg-project-search :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "q" :command project-switch-project :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "r" :command project-query-replace-regexp :status semantic-replacement)
    (:map gsmlg-project-prefix-map :key "v" :command project-vc-dir :status semantic-replacement)

    (:map global-map :key "M-<f12>" :command magit-status :status exact)
    (:map global-map :key "C-x g" :command magit-status :status exact)
    (:map global-map :key "C-x M-g" :command magit-status :status exact)
    (:map magit-status-mode-map :key "C-M-<up>" :command magit-section-up :status exact :feature magit)
    (:map magit-mode-map :key "M-h" :command nil :status exact :platform darwin :feature magit)
    (:map vc-prefix-map :key "f" :command vc-git-grep :status exact)
    (:map vc-prefix-map :key "p" :command git-messenger:popup-message :status exact)
    (:map global-map :key "M-g M-g" :command gsmlg-diff-hl-transient :status semantic-replacement)

    (:map global-map :key "C-c l" :command org-store-link :status exact)
    (:map global-map :key "C-c a" :command org-agenda :status exact)
    (:map global-map :key "C-c c" :command org-capture :status exact)
    (:map org-mode-map :key "C-M-<up>" :command org-up-element :status exact :feature org)
    (:map org-mode-map :key "C-M-<down>" :command org-down-element :status exact :feature org)
    (:map org-mode-map :key "M-h" :command nil :status exact :platform darwin :feature org)
    (:map org-mode-map :key "C-c g" :command org-mac-grab-link :status exact
          :platform darwin :feature org-mac-link :optional t)
    (:map org-agenda-mode-map :key "P" :command org-pomodoro :status exact :feature org-agenda)
    (:map org-clock-mode-line-map :key "<header-line> <mouse-2>" :command org-clock-goto :status exact
          :feature org-clock)
    (:map org-clock-mode-line-map :key "<header-line> <mouse-1>" :command org-clock-menu :status exact
          :feature org-clock)

    (:map global-map :key "<remap> <eval-expression>" :command pp-eval-expression :status exact)
    (:map emacs-lisp-mode-map :key "C-x C-e" :command gsmlg-eval-last-sexp-or-region :status exact
          :feature elisp-mode)
    (:map emacs-lisp-mode-map :key "C-c C-z" :command gsmlg-switch-to-ielm :status exact
          :feature elisp-mode)
    (:map ielm-map :key "C-c C-z" :command gsmlg-repl-switch-back :status exact :feature ielm)
    (:map emacs-lisp-mode-map :key "C-c e" :command macrostep-expand :status exact
          :feature elisp-mode)
    (:map global-map :key "<f1> K" :command find-function-on-key :status exact)
    (:map ert-results-mode-map :key "g" :command ert-results-rerun-all-tests :status exact :feature ert)

    (:map symbol-overlay-mode-map :key "M-n" :command symbol-overlay-jump-next :status exact
          :feature symbol-overlay)
    (:map symbol-overlay-mode-map :key "M-p" :command symbol-overlay-jump-prev :status exact
          :feature symbol-overlay)
    (:map paredit-mode-map :key "C-<left>" :command nil :status exact :feature paredit)
    (:map paredit-mode-map :key "C-<right>" :command nil :status exact :feature paredit)
    (:map paredit-mode-map :key "C-M-<left>" :command nil :status exact :feature paredit)
    (:map paredit-mode-map :key "C-M-<right>" :command nil :status exact :feature paredit)
    (:map paredit-mode-map :key "M-s" :command nil :status exact :feature paredit)
    (:map paredit-mode-map :key "M-?" :command nil :status exact :feature paredit)
    (:map paredit-everywhere-mode-map :key "M-s" :command nil :status exact
          :feature paredit-everywhere)
    (:map global-map :key "M-." :command xref-find-definitions :status exact)
    (:map global-map :key "M-?" :command xref-find-references :status exact)

    (:map gsmlg-refactor-prefix-map :key "r" :command eglot-rename :status semantic-replacement)
    (:map gsmlg-refactor-prefix-map :key "a" :command eglot-code-actions :status semantic-replacement)
    (:map gsmlg-refactor-prefix-map :key "f" :command gsmlg-format-buffer :status semantic-replacement)
    (:map gsmlg-refactor-prefix-map :key "o" :command gsmlg-eglot-organize-imports
          :status semantic-replacement))
  "Machine-readable compatibility contract for migrated keybindings.

Each entry contains a keymap symbol, a key accepted by `key-parse', the
expected command, and its migration status.  Deferred package maps also name
the feature that defines the map.  Optional platform profiles name the setup
needed before asserting an alternate keyboard profile.")

(defvar-keymap gsmlg-multiple-cursors-prefix-map
  :doc "Legacy multiple-cursor editing commands."
  :name "Multiple Cursors"
  "r" #'set-rectangular-region-anchor
  "c" #'mc/edit-lines
  "e" #'mc/edit-ends-of-lines
  "a" #'mc/edit-beginnings-of-lines)

(defvar-keymap gsmlg-project-prefix-map
  :doc "Project commands backed by built-in project.el."
  :name "Project"
  "p" #'project-switch-project
  "f" #'project-find-file
  "b" #'project-switch-to-buffer
  "d" #'project-dired
  "D" #'project-dired
  "k" #'project-kill-buffers
  "c" #'project-compile
  "e" #'project-eshell
  "s" `("project search" . gsmlg-project-search)
  "S" #'project-eshell
  "!" #'project-shell-command
  "&" #'project-async-shell-command
  "?" #'xref-find-references
  "g" #'gsmlg-project-search
  "I" #'project-list-buffers
  "o" #'gsmlg-project-search
  "q" #'project-switch-project
  "r" #'project-query-replace-regexp
  "v" #'project-vc-dir)

(defvar-keymap gsmlg-refactor-prefix-map
  :doc "Language-aware refactoring commands."
  :name "Refactor"
  "r" #'eglot-rename
  "a" #'eglot-code-actions
  "f" #'gsmlg-format-buffer
  "o" #'gsmlg-eglot-organize-imports)

(defvar-keymap gsmlg-code-prefix-map
  :doc "Top-level language-aware command map."
  :name "Code"
  "r" gsmlg-refactor-prefix-map)

(defvar gsmlg-consult-line-history nil
  "Accepted queries from `gsmlg-consult-line', newest first.")

(defvar gsmlg-consult-line-origin-marker nil
  "Marker recording the origin of the most recent Consult line search.")

(defun gsmlg--capture-consult-line-query ()
  "Record the current minibuffer contents in public line-search history."
  (let ((query (minibuffer-contents-no-properties)))
    (unless (string-empty-p query)
      (add-to-history 'gsmlg-consult-line-history query))))

(defun gsmlg--install-consult-line-capture ()
  "Install a buffer-local hook that records the accepted line query."
  (add-hook 'minibuffer-exit-hook #'gsmlg--capture-consult-line-query nil t))

(defun gsmlg-consult-line (&optional initial)
  "Search the current buffer with Consult, optionally seeded by INITIAL.

Record the origin marker and accepted query using public minibuffer hooks and
history APIs."
  (interactive)
  (unless (fboundp 'consult-line)
    (user-error "Consult line search is unavailable"))
  (when (markerp gsmlg-consult-line-origin-marker)
    (set-marker gsmlg-consult-line-origin-marker nil))
  (setq gsmlg-consult-line-origin-marker (point-marker))
  (let ((minibuffer-setup-hook
         (cons #'gsmlg--install-consult-line-capture minibuffer-setup-hook)))
    (funcall-interactively #'consult-line initial)))

(defun gsmlg-consult-line-resume ()
  "Return to the last line-search origin and reuse its accepted query."
  (interactive)
  (unless (and (markerp gsmlg-consult-line-origin-marker)
               (marker-buffer gsmlg-consult-line-origin-marker))
    (user-error "No live Consult line-search origin"))
  (let ((origin gsmlg-consult-line-origin-marker)
        (query (car gsmlg-consult-line-history)))
    (pop-to-buffer (marker-buffer origin))
    (goto-char origin)
    (gsmlg-consult-line query)))

(defun gsmlg-consult-line-from-isearch ()
  "Exit Isearch and seed Consult line search with the active query."
  (interactive)
  (let ((query isearch-string))
    (isearch-exit)
    (gsmlg-consult-line query)))

(defun gsmlg-paredit-newline-maybe ()
  "Bind RET to structural newline where the legacy behavior applied."
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
              (minibufferp))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map paredit-mode-map)
      (keymap-set map "RET" #'paredit-newline)
      (setq-local
       minor-mode-overriding-map-alist
       (cons (cons 'paredit-mode map)
             (assq-delete-all
              'paredit-mode
              (copy-tree minor-mode-overriding-map-alist)))))))

(defun gsmlg--configure-vertico-keys ()
  "Preserve legacy minibuffer quit and navigation keys."
  (keymap-set vertico-map "C-g" #'abort-recursive-edit)
  (keymap-set vertico-map "M-n" #'vertico-next)
  (keymap-set vertico-map "M-p" #'vertico-previous)
  (keymap-set vertico-map "C-l" #'vertico-directory-up)
  (keymap-set vertico-map "C-j" #'vertico-directory-enter))

(defun gsmlg--configure-corfu-keys ()
  "Install legacy popup navigation keys in `corfu-map'."
  (keymap-set corfu-map "M-/" #'corfu-next)
  (keymap-set corfu-map "C-n" #'corfu-next)
  (keymap-set corfu-map "C-p" #'corfu-previous))

(defun gsmlg--configure-magit-keys ()
  "Install Magit bindings and macOS fall-through behavior."
  (keymap-set magit-status-mode-map "C-M-<up>" #'magit-section-up)
  (when (eq system-type 'darwin)
    (keymap-unset magit-mode-map "M-h" t)))

(defun gsmlg--configure-org-keys ()
  "Install Org structural keys and macOS fall-through behavior."
  (keymap-set org-mode-map "C-M-<up>" #'org-up-element)
  (keymap-set org-mode-map "C-M-<down>" #'org-down-element)
  (when (eq system-type 'darwin)
    (keymap-unset org-mode-map "M-h" t)
    (when (fboundp 'org-mac-grab-link)
      (keymap-set org-mode-map "C-c g" #'org-mac-grab-link))))

(defun gsmlg--configure-org-mac-link-key ()
  "Bind the optional macOS Org link grabber after it becomes available."
  (when (and (eq system-type 'darwin)
             (boundp 'org-mode-map)
             (fboundp 'org-mac-grab-link))
    (keymap-set org-mode-map "C-c g" #'org-mac-grab-link)))

(defun gsmlg--configure-org-agenda-keys ()
  "Install the legacy Pomodoro key in Org agenda buffers."
  (keymap-set org-agenda-mode-map "P" #'org-pomodoro))

(defun gsmlg--configure-org-clock-keys ()
  "Install mouse bindings for the Org clock header-line indicator."
  (keymap-set org-clock-mode-line-map
              "<header-line> <mouse-2>" #'org-clock-goto)
  (keymap-set org-clock-mode-line-map
              "<header-line> <mouse-1>" #'org-clock-menu))

(defun gsmlg--configure-elisp-keys ()
  "Install Emacs Lisp evaluation, IELM, and macro expansion bindings."
  (keymap-set emacs-lisp-mode-map "C-x C-e" #'gsmlg-eval-last-sexp-or-region)
  (keymap-set emacs-lisp-mode-map "C-c C-z" #'gsmlg-switch-to-ielm)
  (keymap-set emacs-lisp-mode-map "C-c e" #'macrostep-expand))

(defun gsmlg--configure-ielm-keys ()
  "Install the IELM return-to-origin binding."
  (keymap-set ielm-map "C-c C-z" #'gsmlg-repl-switch-back))

(defun gsmlg--configure-ert-keys ()
  "Install the legacy ERT rerun binding."
  (keymap-set ert-results-mode-map "g" #'ert-results-rerun-all-tests))

(defun gsmlg--configure-symbol-overlay-keys ()
  "Install occurrence navigation in `symbol-overlay-mode-map'."
  (keymap-set symbol-overlay-mode-map "M-n" #'symbol-overlay-jump-next)
  (keymap-set symbol-overlay-mode-map "M-p" #'symbol-overlay-jump-prev))

(defun gsmlg--configure-paredit-keys ()
  "Restore global fall-through and structural newline in Paredit."
  (dolist (key '("C-<left>" "C-<right>" "C-M-<left>"
                 "C-M-<right>" "M-s" "M-?"))
    (keymap-unset paredit-mode-map key t))
  (add-hook 'paredit-mode-hook #'gsmlg-paredit-newline-maybe))

(defun gsmlg--configure-paredit-everywhere-keys ()
  "Let the global search prefix win in Paredit Everywhere."
  (keymap-unset paredit-everywhere-mode-map "M-s" t))

(defun gsmlg--configure-which-key-labels ()
  "Describe custom prefix maps to built-in Which Key."
  (which-key-add-keymap-based-replacements
    gsmlg-project-prefix-map
    "p" "switch project" "f" "find file" "b" "switch buffer"
    "d" "dired" "k" "kill buffers" "c" "compile" "e" "eshell"
    "s" "project search" "S" "eshell" "?" "find references" "g" "ripgrep"
    "I" "list buffers" "o" "search project" "q" "switch project"
    "r" "replace regexp" "v" "VC directory")
  (which-key-add-keymap-based-replacements
    gsmlg-refactor-prefix-map
    "r" "rename" "a" "code action" "f" "format" "o" "organize imports"))

(keymap-global-set "C-c j" #'join-line)
(keymap-global-set "C-h" #'delete-backward-char)
(keymap-global-set "RET" #'newline-and-indent)
(keymap-global-set "S-<return>" #'gsmlg-newline-at-end-of-line)
(keymap-global-set "M-Z" #'zap-up-to-char)
(keymap-global-set "M-Y" #'consult-yank-pop)
(keymap-global-set "C-=" #'er/expand-region)
(keymap-global-set "C-." #'set-mark-command)
(keymap-global-set "C-x C-." #'pop-global-mark)
(keymap-global-set "C-<" #'mc/mark-previous-like-this)
(keymap-global-set "C->" #'mc/mark-next-like-this)
(keymap-global-set "C-+" #'mc/mark-next-like-this)
(keymap-global-set "C-c C-<" #'mc/mark-all-like-this)
(keymap-global-set "C-c m" gsmlg-multiple-cursors-prefix-map)
(keymap-global-unset "M-<left>")
(keymap-global-unset "M-<right>")
(keymap-global-set "C-M-<backspace>" #'gsmlg-kill-back-to-indentation)
(keymap-global-set "M-<up>" #'move-dup-move-lines-up)
(keymap-global-set "M-<down>" #'move-dup-move-lines-down)
(keymap-global-set "M-S-<up>" #'move-dup-move-lines-up)
(keymap-global-set "M-S-<down>" #'move-dup-move-lines-down)
(keymap-global-set "C-S-<up>" #'move-dup-move-lines-up)
(keymap-global-set "C-S-<down>" #'move-dup-move-lines-down)
(keymap-global-set "C-c d" #'move-dup-duplicate-down)
(keymap-global-set "C-c u" #'move-dup-duplicate-up)
(keymap-global-set "<remap> <backward-up-list>" #'gsmlg-backward-up-sexp)
(keymap-global-set "C-o" #'gsmlg-open-line-with-reindent)
(keymap-global-set "C-z" #'gsmlg-maybe-suspend-frame)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-x u" #'vundo)

(keymap-global-set "M-x" #'execute-extended-command)
(keymap-global-set "C-x C-m" #'execute-extended-command)
(keymap-global-set "C-x C-f" #'find-file)
(keymap-global-set "C-c r" #'consult-recent-file)
(keymap-global-set "C-c i" #'consult-imenu)
(keymap-global-set "C-x b" #'consult-buffer)
(keymap-global-set "M-i" #'gsmlg-consult-line)
(keymap-global-set "M-I" #'gsmlg-consult-line-resume)
(keymap-global-set "C-c M-i" #'consult-line-multi)
(keymap-global-set "C-x M-i" #'consult-line-multi)
(keymap-set isearch-mode-map "M-i" #'gsmlg-consult-line-from-isearch)
(keymap-global-set "C-;" #'embark-act)
(keymap-global-set "C-c C-;" #'embark-dwim)

(keymap-global-set "C-M-/" #'completion-at-point)
(keymap-global-set "M-/" #'completion-at-point)

(keymap-global-unset "C-c p")
(keymap-global-set "C-x p" gsmlg-project-prefix-map)
(keymap-global-set "C-," gsmlg-code-prefix-map)

(keymap-global-set "M-<f12>" #'magit-status)
(keymap-global-set "C-x g" #'magit-status)
(keymap-global-set "C-x M-g" #'magit-status)
(keymap-set vc-prefix-map "f" #'vc-git-grep)
(keymap-set vc-prefix-map "p" #'git-messenger:popup-message)
(keymap-global-set "M-g M-g" #'gsmlg-diff-hl-transient)

(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)

(keymap-global-set "<remap> <eval-expression>" #'pp-eval-expression)
(keymap-global-set "<f1> K" #'find-function-on-key)

(with-eval-after-load 'corfu
  (gsmlg--configure-corfu-keys))
(with-eval-after-load 'vertico
  (gsmlg--configure-vertico-keys))
(with-eval-after-load 'magit
  (gsmlg--configure-magit-keys))
(with-eval-after-load 'org
  (gsmlg--configure-org-keys))
(with-eval-after-load 'org-agenda
  (gsmlg--configure-org-agenda-keys))
(with-eval-after-load 'org-clock
  (gsmlg--configure-org-clock-keys))
(with-eval-after-load 'org-mac-link
  (gsmlg--configure-org-mac-link-key))
(with-eval-after-load 'elisp-mode
  (gsmlg--configure-elisp-keys))
(with-eval-after-load 'ielm
  (gsmlg--configure-ielm-keys))
(with-eval-after-load 'ert
  (gsmlg--configure-ert-keys))
(with-eval-after-load 'symbol-overlay
  (gsmlg--configure-symbol-overlay-keys))
(with-eval-after-load 'paredit
  (gsmlg--configure-paredit-keys))
(with-eval-after-load 'paredit-everywhere
  (gsmlg--configure-paredit-everywhere-keys))
(with-eval-after-load 'which-key
  (gsmlg--configure-which-key-labels))

(when (eq system-type 'darwin)
  (gsmlg-mac-osx-remap-command))

(provide 'gsmlg-keybindings)
;;; gsmlg-keybindings.el ends here
