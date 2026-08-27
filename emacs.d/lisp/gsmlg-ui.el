;;; gsmlg-ui.el --- Theme, frames, and native mode line -*- lexical-binding: t; -*-

;;; Commentary:
;; A font-optional UI using the Duskmoon Moonlight theme, Nerd Font glyphs
;; when available, and a mood-line-inspired native mode line (not the
;; mood-line package).  File paths stay in the header breadcrumb.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-bootstrap)
(require 'gsmlg-compat)
(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function set-fontset-font
                  "fontset" (name target font-spec &optional frame add))
(declare-function mc/num-cursors "multiple-cursors")
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-running-backends "flymake")
(declare-function flymake-reporting-backends "flymake")
(defvar duskmoon-header-line-style)
(defvar duskmoon-mode-line-style)
(defvar org-mode-line-string)
(defvar multiple-cursors-mode)
(defvar flymake-mode)
(defvar vc-display-status)

(defcustom gsmlg-default-font-family "Source Code Pro"
  "Preferred default font family, used only when it is installed."
  :type 'string
  :group 'gsmlg)

(defcustom gsmlg-default-font-height 160
  "Preferred default face height in tenths of a point."
  :type 'integer
  :group 'gsmlg)

(defcustom gsmlg-cjk-font-family "Hiragino Sans GB"
  "Preferred CJK font family, used only when it is installed."
  :type 'string
  :group 'gsmlg)

(defcustom gsmlg-nerd-font-family "JetBrainsMono Nerd Font Mono"
  "Preferred Nerd Font family for UI glyphs."
  :type 'string
  :group 'gsmlg)

(defvar gsmlg-ui-nerd-font-available nil
  "Non-nil when the configured Nerd Font is available on the selected frame.

Prefer `gsmlg-ui-nerd-font-available-p' for frame-aware rendering.")

(defconst gsmlg-ui-nerd-font-parameter 'gsmlg-nerd-font-available
  "Frame parameter storing whether Nerd Font glyphs can render on a frame.")

(defvar-local gsmlg-ui--breadcrumb-cache nil
  "Buffer-local cached file breadcrumb for the header line.")

(defvar-local gsmlg-ui--breadcrumb-cache-key nil
  "Cache key for `gsmlg-ui--breadcrumb-cache'.")

(defvar-local gsmlg-ui--vc-cache nil
  "Buffer-local cached VC mode-line segment.")

(defvar-local gsmlg-ui--vc-cache-key nil
  "Cache key for `gsmlg-ui--vc-cache'.")

(defconst gsmlg-ui-glyphs-ascii
  '((:checker-info . ?i)
    (:checker-issues . ?+)
    (:checker-good . ?-)
    (:checker-checking . ?~)
    (:checker-errored . ?x)
    (:checker-interrupted . ?=)
    (:vc-added . ?+)
    (:vc-needs-merge . ?>)
    (:vc-needs-update . ?v)
    (:vc-conflict . ?x)
    (:vc-good . ?-)
    (:buffer-narrowed . ?v)
    (:buffer-modified . ?*)
    (:buffer-read-only . ?#)
    (:frame-client . ?@)
    (:count-separator . ?*))
  "ASCII glyph map for mode-line segments.")

(defconst gsmlg-ui-glyphs-nerd
  '((:checker-info . ?↳)
    (:checker-issues . ?→)
    (:checker-good . ?✓)
    (:checker-checking . ?⟳)
    (:checker-errored . ?✖)
    (:checker-interrupted . ?⏸)
    (:vc-added . ?+)
    (:vc-needs-merge . ?⟷)
    (:vc-needs-update . ?↓)
    (:vc-conflict . ?✖)
    (:vc-good . ?✓)
    (:buffer-narrowed . ?◢)
    (:buffer-modified . ?●)
    (:buffer-read-only . ?■)
    (:frame-client . ?@)
    (:count-separator . ?×))
  "Nerd Font-friendly glyph map for mode-line segments.")

(defcustom gsmlg-ui-glyph-alist nil
  "Alist mapping mode-line glyph names to characters.
When nil, use `gsmlg-ui-glyphs-nerd' if a Nerd Font is available,
otherwise `gsmlg-ui-glyphs-ascii'.  Missing keys fall back to ASCII."
  :type '(choice (const :tag "Auto" nil)
                 (alist :key-type symbol :value-type character))
  :group 'gsmlg)

(defface gsmlg-ui-unimportant
  '((t (:inherit shadow :weight normal)))
  "Face for less important mode-line elements."
  :group 'gsmlg)

(defface gsmlg-ui-status-info
  '((t (:inherit font-lock-keyword-face :weight normal)))
  "Face for informational mode-line status."
  :group 'gsmlg)

(defface gsmlg-ui-status-success
  '((t (:inherit success :weight normal)))
  "Face for success mode-line status."
  :group 'gsmlg)

(defface gsmlg-ui-status-warning
  '((t (:inherit warning :weight normal)))
  "Face for warning mode-line status."
  :group 'gsmlg)

(defface gsmlg-ui-status-error
  '((t (:inherit error :weight normal)))
  "Face for error mode-line status."
  :group 'gsmlg)

(defface gsmlg-ui-status-neutral
  '((t (:inherit gsmlg-ui-unimportant)))
  "Face for neutral or inactive mode-line status."
  :group 'gsmlg)

(defface gsmlg-ui-buffer-status-modified
  '((t (:inherit error :weight normal)))
  "Face for a modified buffer status glyph."
  :group 'gsmlg)

(defface gsmlg-ui-buffer-status-read-only
  '((t (:inherit shadow :weight normal)))
  "Face for a read-only buffer status glyph."
  :group 'gsmlg)

(defface gsmlg-ui-buffer-status-narrowed
  '((t (:inherit font-lock-doc-face :weight normal)))
  "Face for a narrowed buffer status glyph."
  :group 'gsmlg)

(defface gsmlg-ui-major-mode
  '((t (:inherit bold)))
  "Face for the major-mode name in the mode line."
  :group 'gsmlg)

(defun gsmlg-ui-nerd-font-available-p (&optional frame)
  "Return non-nil when FRAME can render configured Nerd Font glyphs."
  (let ((frame (or frame (selected-frame))))
    (and (display-graphic-p frame)
         (frame-parameter frame gsmlg-ui-nerd-font-parameter))))

(defun gsmlg-ui-apply-fonts (&optional frame)
  "Apply optional configured fonts to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (let ((available
           (and (display-graphic-p)
                (find-font (font-spec :family gsmlg-nerd-font-family)))))
      (set-frame-parameter nil gsmlg-ui-nerd-font-parameter (and available t))
      (setq gsmlg-ui-nerd-font-available (and available t))
      (when (display-graphic-p)
        (when (find-font (font-spec :family gsmlg-default-font-family))
          (set-face-attribute
           'default nil
           :family gsmlg-default-font-family
           :height gsmlg-default-font-height))
        (when (find-font (font-spec :family gsmlg-cjk-font-family))
          (dolist (charset '(kana han cjk-misc bopomofo))
            (set-fontset-font t charset
                              (font-spec :family gsmlg-cjk-font-family))))))))

(defun gsmlg-ui-enable-theme ()
  "Enable Duskmoon Moonlight without prompting."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'duskmoon-moonlight t))

(defun gsmlg-ui-icon (glyph)
  "Return Nerd Font GLYPH, or an empty string when unavailable."
  (if (gsmlg-ui-nerd-font-available-p)
      (propertize glyph 'face `(:family ,gsmlg-nerd-font-family))
    ""))

(defun gsmlg-ui-glyph (name)
  "Return the mode-line glyph string for semantic NAME.
Look up `gsmlg-ui-glyph-alist' (or the auto nerd/ascii default), then
fall back to `gsmlg-ui-glyphs-ascii'."
  (let* ((alist (or gsmlg-ui-glyph-alist
                    (if (gsmlg-ui-nerd-font-available-p)
                        gsmlg-ui-glyphs-nerd
                      gsmlg-ui-glyphs-ascii)))
         (char (or (alist-get name alist)
                   (alist-get name gsmlg-ui-glyphs-ascii))))
    (char-to-string char)))

(defun gsmlg-ui-open-header-directory (directory)
  "Open header breadcrumb DIRECTORY."
  (dired directory))

(defun gsmlg-ui--header-path-button (label directory)
  "Return a header-line button named LABEL that opens DIRECTORY."
  (make-text-button
   label nil
   'action #'gsmlg-ui-open-header-directory
   'button-data directory
   'follow-link t
   'gsmlg-directory directory
   'help-echo (format "Open %s" (abbreviate-file-name directory))))

(defun gsmlg-ui--breadcrumb-cache-key ()
  "Return the cache key for the current buffer breadcrumb."
  (list buffer-file-name default-directory))

(defun gsmlg-ui--compute-file-breadcrumb ()
  "Return project context and a clickable breadcrumb for the current file."
  (let* ((project (project-current nil (file-name-directory
                                        buffer-file-name)))
         (root (if project
                 (project-root project)
               (concat (file-remote-p buffer-file-name) "/")))
         (relative (file-relative-name buffer-file-name root))
         (parts (split-string relative "/" t))
         (directory root)
         (breadcrumb
          (if project
              (list
               "["
               (gsmlg-ui-icon " ")
               (gsmlg-ui--header-path-button
                (file-name-nondirectory (directory-file-name root))
                root)
               "] - [")
            (list
             "["
             (gsmlg-ui-icon " ")
             (gsmlg-ui--header-path-button
              (abbreviate-file-name root) root)
             " / "))))
    (while (cdr parts)
      (setq directory (expand-file-name (file-name-as-directory (car parts))
                                        directory)
            breadcrumb
            (append breadcrumb
                    (list (gsmlg-ui--header-path-button
                           (car parts) directory)
                          " / "))
            parts (cdr parts)))
    (append breadcrumb
            (list (gsmlg-ui-icon " ") (car parts) "]"))))

(defun gsmlg-ui-file-breadcrumb ()
  "Return project context and a clickable breadcrumb for the current file."
  (when buffer-file-name
    (let ((key (gsmlg-ui--breadcrumb-cache-key)))
      (unless (equal key gsmlg-ui--breadcrumb-cache-key)
        (setq gsmlg-ui--breadcrumb-cache-key key
              gsmlg-ui--breadcrumb-cache (gsmlg-ui--compute-file-breadcrumb)))
      gsmlg-ui--breadcrumb-cache)))

(defun gsmlg-ui--buffer-identification-string ()
  "Return the display string for a non-file buffer."
  (unless buffer-file-name
    (let ((from-mode-line
           (ignore-errors
             (let ((ident (format-mode-line mode-line-buffer-identification)))
               (unless (string-blank-p ident)
                 (string-trim ident))))))
      (or from-mode-line
          (unless (string-blank-p (buffer-name))
            (buffer-name))))))

(defun gsmlg-ui-buffer-identity ()
  "Return header-line identification for non-file buffers."
  (let ((ident (gsmlg-ui--buffer-identification-string)))
    (when ident
      (list (propertize ident 'face 'gsmlg-ui-major-mode)))))

(defun gsmlg-ui-header-line ()
  "Return the file breadcrumb and active Org clock for the header line."
  (let* ((breadcrumb (or (gsmlg-ui-file-breadcrumb)
                         (gsmlg-ui-buffer-identity)))
         (clock (and (boundp 'org-mode-line-string)
                     org-mode-line-string))
         (clock-text (when clock (format "%s" clock))))
    (when (or breadcrumb (not (string-blank-p clock-text)))
      (append '(" ") breadcrumb
              (when (not (string-blank-p clock-text))
                `("    " ,clock-text " "))))))

(defun gsmlg-ui-segment-buffer-identity ()
  "Return buffer identification for non-file buffers in the mode line."
  (let ((ident (gsmlg-ui--buffer-identification-string)))
    (when ident
      (propertize ident 'face 'gsmlg-ui-major-mode))))

(defun gsmlg-ui-segment-buffer-status ()
  "Return a glyph for modified, read-only, or narrowed file buffers."
  (if (buffer-file-name (buffer-base-buffer))
      (cond
       ((and (buffer-narrowed-p) (buffer-modified-p))
        (propertize (gsmlg-ui-glyph :buffer-narrowed)
                    'face 'gsmlg-ui-buffer-status-modified))
       ((and (buffer-narrowed-p) buffer-read-only)
        (propertize (gsmlg-ui-glyph :buffer-narrowed)
                    'face 'gsmlg-ui-buffer-status-read-only))
       ((buffer-narrowed-p)
        (propertize (gsmlg-ui-glyph :buffer-narrowed)
                    'face 'gsmlg-ui-buffer-status-narrowed))
       ((buffer-modified-p)
        (propertize (gsmlg-ui-glyph :buffer-modified)
                    'face 'gsmlg-ui-buffer-status-modified))
       (buffer-read-only
        (propertize (gsmlg-ui-glyph :buffer-read-only)
                    'face 'gsmlg-ui-buffer-status-read-only)))
    (when (buffer-narrowed-p)
      (propertize (gsmlg-ui-glyph :buffer-narrowed)
                  'face 'gsmlg-ui-buffer-status-narrowed))))

(defun gsmlg-ui-segment-multiple-cursors ()
  "Return the active multiple-cursors count, when enabled."
  (when (bound-and-true-p multiple-cursors-mode)
    (format #("MC%s%d"
              2 5 (face gsmlg-ui-status-info))
            (gsmlg-ui-glyph :count-separator)
            (mc/num-cursors))))

(defun gsmlg-ui-segment-cursor-position ()
  "Return the cursor line and column."
  (format "%d:%d" (line-number-at-pos) (current-column)))

(defun gsmlg-ui-segment-scroll ()
  "Return the relative scroll position."
  (let ((position (format-mode-line "%o")))
    (unless (string-blank-p position)
      (propertize position 'face 'gsmlg-ui-unimportant))))

(defun gsmlg-ui--vc-revision (vc-mode-str backend)
  "Return the revision or branch name for BACKEND from VC-MODE-STR."
  (or (unless vc-display-status
        (symbol-name backend))
      (pcase backend
        ('Git (substring-no-properties vc-mode-str 5))
        ('Hg (substring-no-properties vc-mode-str 4)))
      (ignore-errors
        (substring (vc-working-revision buffer-file-name backend) 0 7))
      "???"))

(defun gsmlg-ui--vc-cache-key ()
  "Return the cache key for the current buffer VC segment."
  (list buffer-file-name
        (and vc-mode (substring-no-properties vc-mode))))

(defun gsmlg-ui--invalidate-vc-cache ()
  "Drop the cached VC segment for the current buffer."
  (setq gsmlg-ui--vc-cache nil
        gsmlg-ui--vc-cache-key nil))

(defun gsmlg-ui--install-buffer-line-hooks ()
  "Install buffer-local hooks that refresh cached mode-line segments."
  (add-hook 'after-save-hook #'gsmlg-ui--invalidate-vc-cache nil t))

(defun gsmlg-ui--compute-vc-segment ()
  "Return branch/revision text colored by live `vc-state'."
  (let* ((backend (vc-backend buffer-file-name))
         (state (and backend (vc-state buffer-file-name)))
         (rev (and backend (gsmlg-ui--vc-revision vc-mode backend)))
         (branch-icon (gsmlg-ui-icon " "))
         (glyph-and-face
          (cond
           ((memq state '(edited added))
            (cons (gsmlg-ui-glyph :vc-added) 'gsmlg-ui-status-info))
           ((eq state 'needs-merge)
            (cons (gsmlg-ui-glyph :vc-needs-merge)
                  'gsmlg-ui-status-warning))
           ((eq state 'needs-update)
            (cons (gsmlg-ui-glyph :vc-needs-update)
                  'gsmlg-ui-status-warning))
           ((memq state '(removed conflict unregistered))
            (cons (gsmlg-ui-glyph :vc-conflict) 'gsmlg-ui-status-error))
           (t
            (cons (gsmlg-ui-glyph :vc-good) 'gsmlg-ui-status-neutral)))))
    (when rev
      (concat branch-icon
              (propertize (format "%s %s" (car glyph-and-face) rev)
                          'face (cdr glyph-and-face))))))

(defun gsmlg-ui-segment-vc ()
  "Return branch/revision text colored by live `vc-state'."
  (when (and vc-mode buffer-file-name)
    (if (file-remote-p buffer-file-name)
        (gsmlg-ui--compute-vc-segment)
      (let ((key (gsmlg-ui--vc-cache-key)))
        (unless (equal key gsmlg-ui--vc-cache-key)
          (setq gsmlg-ui--vc-cache-key key
                gsmlg-ui--vc-cache (gsmlg-ui--compute-vc-segment)))
        gsmlg-ui--vc-cache))))

(defun gsmlg-ui--major-mode-name ()
  "Return a plain major-mode display name."
  (let ((formatted (format-mode-line mode-name)))
    (cond
     ((and formatted (not (string-blank-p formatted)))
      (substring-no-properties formatted))
     ((stringp mode-name)
      mode-name)
     ((and (consp mode-name) (stringp (car mode-name)))
      (car mode-name))
     (t
      (symbol-name major-mode)))))

(defun gsmlg-ui-segment-major-mode ()
  "Return the major mode name only."
  (propertize (gsmlg-ui--major-mode-name) 'face 'gsmlg-ui-major-mode))

(defun gsmlg-ui-segment-misc-info ()
  "Return trimmed `mode-line-misc-info', when non-empty."
  (let ((misc-info (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p misc-info)
      (propertize (string-trim misc-info)
                  'face 'gsmlg-ui-unimportant))))

(defun gsmlg-ui--flymake-counts ()
  "Return plist (:error N :warning N :note N) from one diagnostics scan."
  (let ((counts (list :error 0 :warning 0 :note 0)))
    (when (and (fboundp #'flymake-diagnostics)
               (fboundp #'flymake-diagnostic-type))
      (dolist (diag (flymake-diagnostics))
        (let* ((diag-type (flymake-diagnostic-type diag))
               (severity (gsmlg-compat-flymake-diagnostic-severity diag-type)))
          (cond
           ((eq severity (warning-numeric-level :error))
            (plist-put counts :error (1+ (plist-get counts :error))))
           ((eq severity (warning-numeric-level :warning))
            (plist-put counts :warning (1+ (plist-get counts :warning))))
           ((eq severity (warning-numeric-level :debug))
            (plist-put counts :note (1+ (plist-get counts :note))))
           ((memq diag-type '(flymake-error :error error))
            (plist-put counts :error (1+ (plist-get counts :error))))
           ((memq diag-type '(flymake-warning :warning warning))
            (plist-put counts :warning (1+ (plist-get counts :warning))))
           ((memq diag-type '(flymake-note :note :debug note))
            (plist-put counts :note (1+ (plist-get counts :note))))))))
    counts))

(defun gsmlg-ui--format-flymake (status error warning note)
  "Format Flymake STATUS with ERROR, WARNING, and NOTE counts."
  (pcase status
    ('running
     (format #("%s Checking"
               0 11 (face gsmlg-ui-status-neutral))
             (gsmlg-ui-glyph :checker-checking)))
    ('errored
     (format #("%s Error"
               0 2 (face gsmlg-ui-status-error))
             (gsmlg-ui-glyph :checker-errored)))
    ('interrupted
     (format #("%s Paused"
               0 9 (face gsmlg-ui-status-neutral))
             (gsmlg-ui-glyph :checker-interrupted)))
    ('finished
     (cond
      ((> error 0)
       (let ((issues (+ error warning)))
         (format #("%s %s Issue%s"
                   0 2 (face gsmlg-ui-status-error))
                 (gsmlg-ui-glyph :checker-issues)
                 issues
                 (if (> issues 1) "s" ""))))
      ((> warning 0)
       (format #("%s %s Issue%s"
                 0 2 (face gsmlg-ui-status-warning))
               (gsmlg-ui-glyph :checker-issues)
               warning
               (if (> warning 1) "s" "")))
      ((> note 0)
       (format #("%s %s Note%s"
                 0 2 (face gsmlg-ui-status-info))
               (gsmlg-ui-glyph :checker-info)
               note
               (if (> note 1) "s" "")))
      (t
       (format #("%s No Issues"
                 0 12 (face gsmlg-ui-status-neutral))
               (gsmlg-ui-glyph :checker-good)))))))

(defun gsmlg-ui-segment-flymake ()
  "Return a live Flymake summary when `flymake-mode' is enabled."
  (when (bound-and-true-p flymake-mode)
    (let* ((checking
            (and (fboundp #'flymake-running-backends)
                 (fboundp #'flymake-reporting-backends)
                 (seq-difference (flymake-running-backends)
                                 (flymake-reporting-backends))))
           (status (if checking 'running 'finished))
           (counts (gsmlg-ui--flymake-counts))
           (error (plist-get counts :error))
           (warning (plist-get counts :warning))
           (note (plist-get counts :note)))
      (gsmlg-ui--format-flymake status error warning note))))

(defun gsmlg-ui-segment-process ()
  "Return trimmed `mode-line-process', when non-empty."
  (let ((process-info (format-mode-line mode-line-process)))
    (unless (string-blank-p process-info)
      (string-trim process-info))))

(defvar gsmlg-ui--escape-buffer nil
  "Scratch buffer used by `gsmlg-ui--escape-mode-line'.")

(defun gsmlg-ui--escape-mode-line (&rest strings)
  "Escape mode-line %-constructs in STRINGS by doubling each `%'."
  (with-current-buffer
      (setq gsmlg-ui--escape-buffer
            (or (and (buffer-live-p gsmlg-ui--escape-buffer)
                     gsmlg-ui--escape-buffer)
                (get-buffer-create " *gsmlg-ui-mode-line*")))
    (erase-buffer)
    (apply #'insert strings)
    (goto-char (point-max))
    (while (search-backward "%" nil t)
      (goto-char (match-beginning 0))
      (insert-char ?% 1 t)
      (goto-char (1- (point))))
    (buffer-string)))

(defun gsmlg-ui--join-segments (segments)
  "Join non-nil SEGMENTS with single spaces."
  (string-join (delq nil segments) " "))

(defun gsmlg-ui-mode-line ()
  "Build a left/right-aligned mood-line-style mode line string."
  (let* ((left (gsmlg-ui--join-segments
                (list (gsmlg-ui-segment-buffer-identity)
                      (gsmlg-ui-segment-buffer-status)
                      (gsmlg-ui-segment-multiple-cursors)
                      (gsmlg-ui-segment-cursor-position)
                      (gsmlg-ui-segment-scroll))))
         (right (gsmlg-ui--join-segments
                 (list (gsmlg-ui-segment-vc)
                       (gsmlg-ui-segment-major-mode)
                       (gsmlg-ui-segment-misc-info)
                       (gsmlg-ui-segment-flymake)
                       (gsmlg-ui-segment-process))))
         (spacer
          (propertize
           " "
           'display
           `((space :align-to (- right
                                 (- 0 right-margin)
                                 ,(string-width right)))))))
    (gsmlg-ui--escape-mode-line " " left " " spacer right " ")))

(setq-default mode-line-format
              '("%e" (:eval (gsmlg-ui-mode-line))))

(setq-default header-line-format
              '((:eval (gsmlg-ui-header-line))))

(when (fboundp #'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(add-hook 'after-make-frame-functions #'gsmlg-ui-apply-fonts)
(add-hook 'emacs-startup-hook #'gsmlg-ui-apply-fonts 80)
(add-hook 'find-file-hook #'gsmlg-ui--install-buffer-line-hooks)

(use-package emacs-duskmoon-theme
  :ensure
  (:host github
   :repo "duskmoon-dev/emacs-duskmoon-theme"
   :files ("*.el"))
  :demand t
  :init
  (setq duskmoon-mode-line-style 'vivid
        duskmoon-header-line-style 'accent)
  :config
  (gsmlg-ui-enable-theme))

(provide 'gsmlg-ui)
;;; gsmlg-ui.el ends here
