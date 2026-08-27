;;; ui-test.el --- Mode line and header UI assertions -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify buffer identity, frame-local glyphs, and cached mode-line segments.

;;; Code:

(require 'test-helper)
(require 'gsmlg-ui)

(declare-function set-face-attribute "faces" (face frame &rest args))
(declare-function set-fontset-font "fontset" (font-object charset font-spec &optional frame add))

(ert-deftest gsmlg-ui-non-file-buffer-shows-identity-in-header ()
  "Non-file buffers should expose buffer identification in the header line."
  (with-temp-buffer
    (rename-buffer "*UI-Test-Help*" t)
    (setq mode-line-buffer-identification '("%b"))
    (let ((header (gsmlg-ui-header-line)))
      (should header)
      (should (member "*UI-Test-Help*"
                      (mapcar #'substring-no-properties header))))))

(ert-deftest gsmlg-ui-non-file-buffer-shows-identity-in-mode-line ()
  "Non-file buffers should expose buffer identification in the mode line."
  (with-temp-buffer
    (rename-buffer "*UI-Test-Dired*" t)
    (setq mode-line-buffer-identification '("%b"))
    (let ((segment (gsmlg-ui-segment-buffer-identity)))
      (should segment)
      (should (string-match-p "\\*UI-Test-Dired\\*" segment)))))

(ert-deftest gsmlg-ui-file-buffer-mode-line-omits-file-path ()
  "File buffers should keep paths in the header breadcrumb only."
  (let ((file (expand-file-name "example.el" gsmlg-test-xdg-root)))
    (write-region "x" nil file nil 'silent)
    (with-current-buffer (find-file-noselect file)
      (let ((line (gsmlg-ui-mode-line)))
        (should (gsmlg-ui-file-breadcrumb))
        (should (not (string-match-p (regexp-quote file) line)))))))

(ert-deftest gsmlg-ui-nerd-font-is-frame-local ()
  "Nerd Font capability follows the current frame display type."
  (cl-letf (((symbol-function #'display-graphic-p)
             (lambda (&optional _frame) t))
            ((symbol-function #'find-font) (lambda (_spec) t))
            ((symbol-function #'set-face-attribute) (lambda (&rest _) nil))
            ((symbol-function #'set-fontset-font) (lambda (&rest _) nil)))
    (gsmlg-ui-apply-fonts (selected-frame))
    (should (frame-parameter (selected-frame) gsmlg-ui-nerd-font-parameter)))
  (cl-letf (((symbol-function #'display-graphic-p)
             (lambda (&optional _frame) nil)))
    (should-not (gsmlg-ui-nerd-font-available-p (selected-frame)))))

(ert-deftest gsmlg-ui-breadcrumb-reuses-cache ()
  "File breadcrumbs should not recompute project context on every redisplay."
  (let ((project-calls 0)
        (file (expand-file-name "src/foo.el" gsmlg-test-xdg-root)))
    (make-directory (file-name-directory file) t)
    (write-region "" nil file nil 'silent)
    (with-current-buffer (find-file-noselect file)
      (setq gsmlg-ui--breadcrumb-cache nil
            gsmlg-ui--breadcrumb-cache-key nil)
      (cl-letf (((symbol-function #'project-current)
                 (lambda (&rest _)
                   (setq project-calls (1+ project-calls))
                   nil)))
        (gsmlg-ui-file-breadcrumb)
        (gsmlg-ui-file-breadcrumb)
        (should (= project-calls 1))))))

(ert-deftest gsmlg-ui-vc-segment-reuses-cache-for-local-files ()
  "Local VC segments should cache until the VC mode string changes."
  (let ((state-calls 0)
        (file (expand-file-name "tracked.el" gsmlg-test-xdg-root)))
    (write-region "" nil file nil 'silent)
    (with-current-buffer (find-file-noselect file)
      (setq vc-mode " Git:main")
      (cl-letf (((symbol-function #'vc-backend) (lambda (_file) 'Git))
                ((symbol-function #'vc-state)
                 (lambda (_file &rest _)
                   (setq state-calls (1+ state-calls))
                   'up-to-date))
                ((symbol-function #'gsmlg-ui--vc-revision)
                 (lambda (_mode _backend) "main")))
        (gsmlg-ui-segment-vc)
        (gsmlg-ui-segment-vc)
        (should (= state-calls 1))))))

(ert-deftest gsmlg-ui-flymake-counts-scan-diagnostics-once ()
  "Flymake summaries should count diagnostics in a single pass."
  (let ((calls 0))
    (cl-letf (((symbol-function #'flymake-diagnostics)
               (lambda (&optional _beg _end)
                 (setq calls (1+ calls))
                 '()))
              ((symbol-function #'flymake-running-backends) (lambda () nil))
              ((symbol-function #'flymake-reporting-backends) (lambda () nil)))
      (let ((flymake-mode t))
        (gsmlg-ui-segment-flymake)
        (should (= calls 1))))))

(provide 'ui-test)
;;; ui-test.el ends here
