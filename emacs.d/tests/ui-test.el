;;; ui-test.el --- Tests for GSMLG UI configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify the native mode line and file breadcrumb header.

;;; Code:

(require 'cl-lib)
(require 'ert)
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
  "Nerd Font glyphs should decorate header, position, and VC context."
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
    (should (string-match-p "" (gsmlg-ui-mode-line-position-icon)))
    (should (string-match-p
             ""
             (mapconcat #'identity (gsmlg-ui-mode-line-vc) "")))))

(ert-deftest gsmlg-ui-mode-line-omits-buffer-identification ()
  "The mode line should not repeat the filename shown in the header."
  (should-not (memq 'mode-line-buffer-identification
                    (default-value 'mode-line-format))))

(provide 'ui-test)
;;; ui-test.el ends here
