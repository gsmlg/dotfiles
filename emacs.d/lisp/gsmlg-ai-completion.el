;;; gsmlg-ai-completion.el --- Inline AI completion policy -*- lexical-binding: t; -*-

;;; Commentary:
;; Minuet integration with GSMLG eligibility, CAPF priority, and diagnostics.
;; Automatic completion is never enabled merely by loading this module.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gsmlg-ai)

(defvar minuet-provider)
(defvar minuet-n-completions)
(defvar minuet-context-window)
(defvar minuet-request-timeout)
(defvar minuet-auto-suggestion-debounce-delay)
(defvar minuet-auto-suggestion-throttle-delay)
(defvar minuet-auto-suggestion-block-predicates)
(defvar minuet-active-mode-map)
(declare-function minuet-show-suggestion "minuet" ())
(declare-function minuet-auto-suggestion-mode "minuet" (&optional arg))
(declare-function minuet-accept-suggestion "minuet" ())
(declare-function minuet-accept-suggestion-line "minuet" (&optional n))
(declare-function minuet-accept-suggestion-word "minuet" (&optional n))
(declare-function minuet-next-suggestion "minuet" ())
(declare-function minuet-previous-suggestion "minuet" ())
(declare-function minuet-dismiss-suggestion "minuet" ())

(defgroup gsmlg-ai-completion nil
  "Inline AI completion policy for Minuet."
  :group 'gsmlg-ai
  :prefix "gsmlg-ai-completion-")

(defcustom gsmlg-ai-completion-provider nil
  "Preferred Minuet provider symbol, or nil to leave Minuet's value unchanged.
Examples: openai-fim-compatible, openai-compatible.  Never a secret."
  :type '(choice (const :tag "Use Minuet default" nil) symbol)
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-auto-enable nil
  "When non-nil, enabling the global mode may activate eligible buffers.
Still requires an explicit global/local mode command; never runs at startup."
  :type 'boolean
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-allowed-mode-predicates
  '(gsmlg-ai-completion--prog-mode-p)
  "Predicates that must pass for automatic completion eligibility."
  :type '(repeat function)
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-block-predicates
  '(gsmlg-ai-completion--block-minibuffer-p
    gsmlg-ai-completion--block-read-only-p
    gsmlg-ai-completion--block-sensitive-p
    gsmlg-ai-completion--block-oversized-p
    gsmlg-ai-completion--block-remote-p
    gsmlg-ai-completion--block-completion-in-region-p
    gsmlg-ai-completion--block-region-p
    gsmlg-ai-completion--block-multiple-cursors-p)
  "Predicates that block automatic or policy-sensitive completion."
  :type '(repeat function)
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-sensitive-file-patterns
  gsmlg-ai-sensitive-file-patterns
  "Sensitive path patterns that block completion."
  :type '(repeat regexp)
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-max-buffer-bytes 524288
  "Maximum buffer size for completion requests."
  :type 'integer
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-prefix-chars 8000
  "Maximum characters of prefix context before point."
  :type 'integer
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-suffix-chars 8000
  "Maximum characters of suffix context after point."
  :type 'integer
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-context-window 512
  "Minuet context window size to apply when configuring the provider."
  :type 'integer
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-debounce 0.5
  "Idle debounce delay for automatic suggestions."
  :type 'number
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-throttle 1.0
  "Minimum delay between automatic completion requests."
  :type 'number
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-timeout 3.0
  "Request timeout forwarded to Minuet."
  :type 'number
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-candidate-count 1
  "Number of completion candidates requested from Minuet."
  :type 'integer
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-allow-remote nil
  "When non-nil, allow completion in remote/TRAMP buffers."
  :type 'boolean
  :group 'gsmlg-ai-completion)

(defcustom gsmlg-ai-completion-widen t
  "When non-nil, capture completion context from the widened buffer."
  :type 'boolean
  :group 'gsmlg-ai-completion)

(defvar gsmlg-ai-completion--ensure-minuet-function nil
  "Optional override used by offline tests instead of loading Minuet.")

(defvar gsmlg-ai-completion--show-function nil
  "Optional override for manual suggestion requests in tests.")

(defvar-local gsmlg-ai-completion--state 'disabled
  "Buffer-local completion state.")

(defvar-local gsmlg-ai-completion--last-blocker nil
  "Last eligibility blocker message.")

(defvar gsmlg-ai-completion--configured nil
  "Non-nil after Minuet public options have been applied once.")

(defun gsmlg-ai-completion--prog-mode-p ()
  "Return non-nil when the current buffer derives from `prog-mode'."
  (derived-mode-p 'prog-mode))

(defun gsmlg-ai-completion--block-minibuffer-p ()
  "Block completion in the minibuffer."
  (when (minibufferp)
    "minibuffer"))

(defun gsmlg-ai-completion--block-read-only-p ()
  "Block completion in read-only buffers."
  (when buffer-read-only
    "read-only"))

(defun gsmlg-ai-completion--block-sensitive-p ()
  "Block completion for sensitive file names."
  (let ((path (or buffer-file-name (buffer-name))))
    (when (cl-some (lambda (pattern)
                     (or (string-match-p pattern path)
                         (string-match-p pattern
                                         (file-name-nondirectory path))))
                   gsmlg-ai-completion-sensitive-file-patterns)
      "sensitive-file")))

(defun gsmlg-ai-completion--block-oversized-p ()
  "Block completion when the buffer exceeds the configured size."
  (save-restriction
    (when gsmlg-ai-completion-widen
      (widen))
    (when (> (string-bytes
              (buffer-substring-no-properties (point-min) (point-max)))
             gsmlg-ai-completion-max-buffer-bytes)
      "oversized-buffer")))

(defun gsmlg-ai-completion--block-remote-p ()
  "Block remote buffers unless explicitly allowed."
  (when (and (not gsmlg-ai-completion-allow-remote)
             (or (and buffer-file-name (file-remote-p buffer-file-name))
                 (and default-directory (file-remote-p default-directory))))
    "remote-buffer"))

(defun gsmlg-ai-completion--block-completion-in-region-p ()
  "Block when deterministic CAPF/Corfu completion is active."
  (when completion-in-region-mode
    "completion-in-region"))

(defun gsmlg-ai-completion--block-region-p ()
  "Block while an active region is being manipulated."
  (when (use-region-p)
    "active-region"))

(defun gsmlg-ai-completion--block-multiple-cursors-p ()
  "Block when multiple-cursors mode is active."
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    "multiple-cursors"))

(defun gsmlg-ai-completion-blocker (&optional manual)
  "Return the first eligibility blocker, or nil when eligible.
When MANUAL is non-nil, skip automatic-only mode predicates."
  (or
   (cl-loop for predicate in gsmlg-ai-completion-block-predicates
            for reason = (funcall predicate)
            when reason return reason)
   (unless manual
     (unless (cl-some #'funcall gsmlg-ai-completion-allowed-mode-predicates)
       "mode-not-allowed"))))

(defun gsmlg-ai-completion--ensure-minuet ()
  "Load Minuet or run the offline test override."
  (if gsmlg-ai-completion--ensure-minuet-function
      (funcall gsmlg-ai-completion--ensure-minuet-function)
    (require 'minuet)
    (when (fboundp #'gsmlg-bootstrap-wait)
      (gsmlg-bootstrap-wait))
    (gsmlg-ai-completion--configure-minuet)))

(defun gsmlg-ai-completion--configure-minuet ()
  "Apply GSMLG policy to public Minuet options once."
  (unless gsmlg-ai-completion--configured
    (when gsmlg-ai-completion-provider
      (setq minuet-provider gsmlg-ai-completion-provider))
    (setopt minuet-n-completions gsmlg-ai-completion-candidate-count
            minuet-context-window gsmlg-ai-completion-context-window
            minuet-request-timeout gsmlg-ai-completion-timeout
            minuet-auto-suggestion-debounce-delay
            gsmlg-ai-completion-debounce
            minuet-auto-suggestion-throttle-delay
            gsmlg-ai-completion-throttle)
    (setq minuet-auto-suggestion-block-predicates
          (append (list #'gsmlg-ai-completion--minuet-block-p)
                  (bound-and-true-p
                   minuet-auto-suggestion-block-predicates)))
    (gsmlg-ai-completion--install-active-map)
    (add-hook 'completion-in-region-mode-hook
              #'gsmlg-ai-completion--on-completion-in-region)
    (setq gsmlg-ai-completion--configured t)))

(defun gsmlg-ai-completion--minuet-block-p ()
  "Minuet block predicate honoring GSMLG eligibility."
  (let ((reason (gsmlg-ai-completion-blocker)))
    (setq gsmlg-ai-completion--last-blocker reason)
    (when reason
      (setq gsmlg-ai-completion--state 'ineligible))
    reason))

(defun gsmlg-ai-completion--install-active-map ()
  "Install suggestion-local acceptance keys on Minuet's active map."
  (when (boundp 'minuet-active-mode-map)
    (keymap-set minuet-active-mode-map "TAB" #'minuet-accept-suggestion)
    (keymap-set minuet-active-mode-map "<tab>" #'minuet-accept-suggestion)
    (keymap-set minuet-active-mode-map "M-RET" #'minuet-accept-suggestion-line)
    (keymap-set minuet-active-mode-map "M-f" #'minuet-accept-suggestion-word)
    (keymap-set minuet-active-mode-map "M-n" #'minuet-next-suggestion)
    (keymap-set minuet-active-mode-map "M-p" #'minuet-previous-suggestion)
    (keymap-set minuet-active-mode-map "C-g" #'gsmlg-ai-completion-dismiss)))

(defun gsmlg-ai-completion--on-completion-in-region ()
  "Dismiss AI suggestions when CAPF/Corfu becomes active."
  (when completion-in-region-mode
    (gsmlg-ai-completion-dismiss t)))

(defun gsmlg-ai-completion-dismiss (&optional silent)
  "Dismiss the active suggestion and cancel in-flight requests.
When SILENT is non-nil, suppress the status message."
  (interactive)
  (when (fboundp #'minuet-dismiss-suggestion)
    (ignore-errors (minuet-dismiss-suggestion)))
  (setq gsmlg-ai-completion--state 'idle)
  (unless silent
    (message "AI suggestion dismissed")))

(defun gsmlg-ai-completion--bounded-context ()
  "Return a cons of (PREFIX . SUFFIX) from the current buffer."
  (save-restriction
    (when gsmlg-ai-completion-widen
      (widen))
    (cons
     (buffer-substring-no-properties
      (max (point-min) (- (point) gsmlg-ai-completion-prefix-chars))
      (point))
     (buffer-substring-no-properties
      (point)
      (min (point-max) (+ (point) gsmlg-ai-completion-suffix-chars))))))

;;;###autoload
(defun gsmlg-ai-completion-show ()
  "Manually request one inline suggestion at point."
  (interactive)
  (when-let* ((reason (gsmlg-ai-completion-blocker t)))
    (setq gsmlg-ai-completion--last-blocker reason
          gsmlg-ai-completion--state 'ineligible)
    (user-error "Inline completion blocked: %s" reason))
  (gsmlg-ai-completion--ensure-minuet)
  (setq gsmlg-ai-completion--state 'waiting)
  (if gsmlg-ai-completion--show-function
      (funcall gsmlg-ai-completion--show-function
               (gsmlg-ai-completion--bounded-context))
    (call-interactively #'minuet-show-suggestion)
    (setq gsmlg-ai-completion--state 'showing)))

;;;###autoload
(define-minor-mode gsmlg-ai-completion-mode
  "Buffer-local automatic AI inline completion."
  :lighter " AI⌘"
  (if gsmlg-ai-completion-mode
      (progn
        (gsmlg-ai-completion--ensure-minuet)
        (when-let* ((reason (gsmlg-ai-completion-blocker)))
          (setq gsmlg-ai-completion-mode nil
                gsmlg-ai-completion--last-blocker reason
                gsmlg-ai-completion--state 'ineligible)
          (user-error "Cannot enable AI completion: %s" reason))
        (when (fboundp #'minuet-auto-suggestion-mode)
          (minuet-auto-suggestion-mode 1))
        (setq gsmlg-ai-completion--state 'idle)
        (add-hook 'kill-buffer-hook #'gsmlg-ai-completion--cleanup nil t)
        (add-hook 'change-major-mode-hook #'gsmlg-ai-completion--cleanup nil t))
    (when (fboundp #'minuet-auto-suggestion-mode)
      (minuet-auto-suggestion-mode -1))
    (gsmlg-ai-completion-dismiss t)
    (setq gsmlg-ai-completion--state 'disabled)
    (remove-hook 'kill-buffer-hook #'gsmlg-ai-completion--cleanup t)
    (remove-hook 'change-major-mode-hook #'gsmlg-ai-completion--cleanup t)))

(defun gsmlg-ai-completion--cleanup ()
  "Clean buffer-local completion state."
  (gsmlg-ai-completion-dismiss t)
  (when gsmlg-ai-completion-mode
    (setq gsmlg-ai-completion-mode nil)))

(defvar gsmlg-ai-global-completion-mode nil
  "Non-nil when global AI inline completion is enabled.")

(defun gsmlg-ai-completion--maybe-enable ()
  "Enable local completion in eligible buffers for the global mode."
  (when (and gsmlg-ai-global-completion-mode
             (not (gsmlg-ai-completion-blocker)))
    (gsmlg-ai-completion-mode 1)))

;;;###autoload
(define-globalized-minor-mode gsmlg-ai-global-completion-mode
  gsmlg-ai-completion-mode
  gsmlg-ai-completion--maybe-enable
  :group 'gsmlg-ai-completion)

;;;###autoload
(defun gsmlg-ai-completion-diagnose ()
  "Display provider readiness, eligibility, and effective limits."
  (interactive)
  (let* ((blocker (gsmlg-ai-completion-blocker))
         (minuet-loaded (featurep 'minuet))
         (provider
          (cond
           (gsmlg-ai-completion-provider
            (format "%s" gsmlg-ai-completion-provider))
           ((and minuet-loaded (boundp 'minuet-provider))
            (format "%s" minuet-provider))
           (t "unconfigured")))
         (context (ignore-errors (gsmlg-ai-completion--bounded-context))))
    (with-current-buffer (get-buffer-create "*GSMLG AI Completion*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (format
          "Provider: %s
Minuet loaded: %s
Local mode: %s
Global mode: %s
State: %s
Blocker: %s
Allow remote: %s
Widen: %s
Debounce: %s
Throttle: %s
Timeout: %s
Candidates: %s
Max buffer bytes: %s
Prefix/suffix chars: %s/%s
Context window: %s
Prefix bytes (now): %s
Suffix bytes (now): %s
"
          provider
          minuet-loaded
          (if (bound-and-true-p gsmlg-ai-completion-mode) "on" "off")
          (if (bound-and-true-p gsmlg-ai-global-completion-mode) "on" "off")
          gsmlg-ai-completion--state
          (or blocker "none")
          gsmlg-ai-completion-allow-remote
          gsmlg-ai-completion-widen
          gsmlg-ai-completion-debounce
          gsmlg-ai-completion-throttle
          gsmlg-ai-completion-timeout
          gsmlg-ai-completion-candidate-count
          gsmlg-ai-completion-max-buffer-bytes
          gsmlg-ai-completion-prefix-chars
          gsmlg-ai-completion-suffix-chars
          gsmlg-ai-completion-context-window
          (if context (string-bytes (car context)) 0)
          (if context (string-bytes (cdr context)) 0))))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer "*GSMLG AI Completion*")))

(provide 'gsmlg-ai-completion)
;;; gsmlg-ai-completion.el ends here
