;;; emacs-agent-policy-test.el --- Policy tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-agent-policy)

(ert-deftest emacs-agent-policy-resolves-contained-path ()
  (let ((root (make-temp-file "agent-policy-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" root))
          (should
           (equal (emacs-agent-policy-resolve root "src/new.el" t)
                  (expand-file-name
                   "src/new.el"
                   (file-name-as-directory (file-truename root))))))
      (delete-directory root t))))

(ert-deftest emacs-agent-policy-rejects-parent-and-git ()
  (let ((root (make-temp-file "agent-policy-" t)))
    (unwind-protect
        (progn
          (should-error (emacs-agent-policy-resolve root "../secret" t)
                        :type 'emacs-agent-error)
          (make-directory (expand-file-name ".git" root))
          (should-error (emacs-agent-policy-resolve root ".git/config" t)
                        :type 'emacs-agent-error))
      (delete-directory root t))))

(ert-deftest emacs-agent-policy-rejects-escaping-symlink ()
  (let ((root (make-temp-file "agent-policy-root-" t))
        (outside (make-temp-file "agent-policy-outside-" t)))
    (unwind-protect
        (progn
          (make-symbolic-link outside (expand-file-name "escape" root))
          (should-error
           (emacs-agent-policy-resolve root "escape/new.el" t)
           :type 'emacs-agent-error))
      (delete-directory root t)
      (delete-directory outside t))))

(ert-deftest emacs-agent-policy-rejects-binary-and-secret ()
  (let ((root (make-temp-file "agent-policy-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "binary" root)
            (set-buffer-multibyte nil)
            (insert "a" (string 0) "b"))
          (should-error
           (emacs-agent-policy-assert-document root "binary")
           :type 'emacs-agent-error)
          (should-error
           (emacs-agent-policy-assert-document root ".env" t)
           :type 'emacs-agent-error))
      (delete-directory root t))))

(provide 'emacs-agent-policy-test)
;;; emacs-agent-policy-test.el ends here
