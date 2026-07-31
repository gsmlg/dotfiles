;;; ui-test.el --- Tests for GSMLG UI configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify the native mode line and file breadcrumb header.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'flymake)
(require 'gsmlg-ui)

(defvar org-mode-line-string)
(defvar vc-mode)

(ert-deftest gsmlg-ui-configures-duskmoon-bar-styles ()
  "Duskmoon should own the configured mode and header line styles."
  (should (eq duskmoon-mode-line-style 'vivid))
  (should (eq duskmoon-header-line-style 'accent)))

(ert-deftest gsmlg-ui-header-shows-project-relative-file-breadcrumb ()
  "The header should show clickable project-relative parent directories."
  (let* ((root (file-name-as-directory
                (make-temp-file "gsmlg-ui-project-" t)))
         (file (expand-file-name "lib/example.el" root))
         (project `(transient . ,root)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file)
          (cl-letf (((symbol-function #'project-current)
                     (lambda (&rest _) project)))
            (let* ((breadcrumb (gsmlg-ui-file-breadcrumb))
                   (buttons
                    (cl-remove-if-not
                     (lambda (item)
                       (and (stringp item)
                            (get-text-property
                             0 'gsmlg-directory item)))
                     breadcrumb))
                   (root-button (nth 0 buttons))
                   (directory-button (nth 1 buttons)))
              (should (equal (mapconcat #'identity breadcrumb "")
                             (format "[%s] - [lib / example.el]"
                                     (file-name-nondirectory
                                      (directory-file-name root)))))
              (should (equal (get-text-property
                              0 'gsmlg-directory root-button)
                             root))
              (should (equal
                       (get-text-property
                        0 'gsmlg-directory directory-button)
                       (expand-file-name "lib/" root))))))
      (delete-directory root t))))

(ert-deftest gsmlg-ui-header-retains-breadcrumb-with-org-clock ()
  "An active Org clock should appear after, not replace, the breadcrumb."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.el")
    (let ((org-mode-line-string "Clocked task"))
      (cl-letf (((symbol-function #'project-current) #'ignore))
        (let ((header (gsmlg-ui-header-line)))
          (should (string-match-p "example\\.el"
                                  (mapconcat #'identity header "")))
          (should (member org-mode-line-string header)))))))

(ert-deftest gsmlg-ui-header-and-mode-line-use-nerd-font-glyphs ()
  "Nerd Font glyphs should decorate header and VC context."
  (let ((gsmlg-ui-nerd-font-available t)
        (vc-mode " Git:main"))
    (should (string-match-p ""
                            (mapconcat
                             #'identity
                             (cl-letf (((symbol-function #'project-current)
                                        (lambda (&rest _)
                                          '(transient . "/tmp/"))))
                               (with-temp-buffer
                                 (setq buffer-file-name "/tmp/example.el")
                                 (gsmlg-ui-file-breadcrumb)))
                             "")))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/example.el"
            vc-mode " Git:main")
      (should (string-match-p ""
                              (cl-letf (((symbol-function #'vc-backend)
                                         (lambda (&rest _) 'Git))
                                        ((symbol-function #'vc-state)
                                         (lambda (&rest _) 'up-to-date)))
                                (gsmlg-ui-segment-vc))))
      (insert "abc\ndef")
      (goto-char (point-min))
      (should (string-match-p "[0-9]+:[0-9]+"
                              (gsmlg-ui-segment-cursor-position))))))

(ert-deftest gsmlg-ui-mode-line-omits-buffer-identification ()
  "The mode line should not repeat the filename shown in the header."
  (should-not (memq 'mode-line-buffer-identification
                    (default-value 'mode-line-format)))
  (should (equal (default-value 'mode-line-format)
                 '("%e" (:eval (gsmlg-ui-mode-line))))))

(ert-deftest gsmlg-ui-mode-line-aligns-left-and-right ()
  "The assembled mode line should right-align via :align-to."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let* ((line (gsmlg-ui-mode-line))
           (display-pos
            (cl-position-if
             (lambda (i)
               (let ((disp (get-text-property i 'display line)))
                 (and (consp disp)
                      (eq (car-safe (car disp)) 'space))))
             (number-sequence 0 (1- (length line)))))
           (spec (and display-pos
                      (car (get-text-property display-pos
                                              'display line)))))
      (should (stringp line))
      (should display-pos)
      (should (eq (car spec) 'space))
      (should (eq (cadr spec) :align-to)))))

(ert-deftest gsmlg-ui-segment-vc-uses-info-face-when-edited ()
  "An edited VC state should use the info status face."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.el"
          vc-mode " Git:main")
    (cl-letf (((symbol-function #'vc-backend)
               (lambda (&rest _) 'Git))
              ((symbol-function #'vc-state)
               (lambda (&rest _) 'edited)))
      (let ((segment (gsmlg-ui-segment-vc)))
        (should (string-match-p "main" segment))
        (should (eq (get-text-property
                     (1- (length segment)) 'face segment)
                    'gsmlg-ui-status-info))))))

(ert-deftest gsmlg-ui-segment-flymake-summarizes-errors ()
  "Flymake errors should produce an issue summary in the segment."
  (with-temp-buffer
    (flymake-mode 1)
    (cl-letf (((symbol-function #'flymake-running-backends)
               (lambda () nil))
              ((symbol-function #'flymake-reporting-backends)
               (lambda () nil))
              ((symbol-function #'flymake-diagnostics)
               (lambda (&rest _)
                 (list (flymake-make-diagnostic
                        (current-buffer) 1 2 :error "boom")))))
      (let ((segment (gsmlg-ui-segment-flymake)))
        (should (string-match-p "Issue" segment))
        (should (eq (get-text-property 0 'face segment)
                    'gsmlg-ui-status-error))))))

(ert-deftest gsmlg-ui-glyph-falls-back-to-ascii-without-nerd ()
  "Without Nerd Font, buffer status glyphs should stay readable ASCII."
  (let ((gsmlg-ui-nerd-font-available nil)
        (gsmlg-ui-glyph-alist nil))
    (should (equal (gsmlg-ui-glyph :buffer-modified) "*"))
    (should (equal (gsmlg-ui-glyph :buffer-read-only) "#"))
    (should (equal (gsmlg-ui-glyph :vc-good) "-"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/example.el")
      (set-buffer-modified-p t)
      (let ((status (gsmlg-ui-segment-buffer-status)))
        (should (equal status "*"))
        (should (eq (get-text-property 0 'face status)
                    'gsmlg-ui-buffer-status-modified))))))

(provide 'ui-test)
;;; ui-test.el ends here
