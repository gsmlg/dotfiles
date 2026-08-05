;;; ai-completion-test.el --- Offline AI completion policy tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Eligibility, CAPF priority, and stubbed manual completion.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gsmlg-ai-completion)

(ert-deftest gsmlg-ai-completion-blocks-read-only-and-remote ()
  "Sensitive policy blockers fire before requests."
  (with-temp-buffer
    (setq buffer-read-only t)
    (should (equal (gsmlg-ai-completion-blocker t) "read-only")))
  (with-temp-buffer
    (setq default-directory "/ssh:example:/tmp/")
    (let ((gsmlg-ai-completion-allow-remote nil))
      (should (equal (gsmlg-ai-completion-blocker t) "remote-buffer")))))

(ert-deftest gsmlg-ai-completion-capf-priority-blocker ()
  "Block AI automatic completion while `completion-in-region' is active."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((completion-in-region-mode t))
      (should (equal (gsmlg-ai-completion-blocker)
                     "completion-in-region")))))

(ert-deftest gsmlg-ai-completion-manual-uses-bounded-context ()
  "Manual completion sends only bounded prefix/suffix via the stub."
  (let (seen)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert (make-string 100 ?a))
      (insert "POINT")
      (insert (make-string 100 ?z))
      (goto-char (+ (point-min) 100))
      (let ((gsmlg-ai-completion--ensure-minuet-function #'ignore)
            (gsmlg-ai-completion--show-function
             (lambda (context) (setq seen context)))
            (gsmlg-ai-completion-prefix-chars 10)
            (gsmlg-ai-completion-suffix-chars 10))
        (gsmlg-ai-completion-show)
        (should seen)
        (should (<= (length (car seen)) 10))
        (should (<= (length (cdr seen)) 10))))))

(ert-deftest gsmlg-ai-completion-not-loaded-as-capf ()
  "Never register AI completion on `completion-at-point-functions'."
  (require 'gsmlg-ai-completion)
  (should-not
   (cl-find-if (lambda (fn)
                 (and (symbolp fn)
                      (string-prefix-p "gsmlg-ai-completion" (symbol-name fn))))
               completion-at-point-functions)))

(ert-deftest gsmlg-ai-completion-mode-stays-off-by-default ()
  "Loading the module does not enable automatic completion."
  (should-not (bound-and-true-p gsmlg-ai-global-completion-mode))
  (with-temp-buffer
    (should-not (bound-and-true-p gsmlg-ai-completion-mode))))

(provide 'ai-completion-test)
;;; ai-completion-test.el ends here
