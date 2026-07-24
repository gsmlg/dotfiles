;;; init-benchmarking.el --- Configuration for init-benchmarking -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration module init-benchmarking.

;;; Code:

(declare-function tablist-minor-mode "tablist")

(defun gsmlg/time-subtract-millis (b a)
  "Calculate time difference between B and A in milliseconds."
  (* 1000.0 (float-time (time-subtract b a))))


(defvar gsmlg/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun gsmlg/build-require-times (orig-fn feature &optional filename noerror)
  "Note in `gsmlg/require-times' the time taken by ORIG-FN to require FEATURE."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (funcall orig-fn feature filename noerror)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (gsmlg/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'gsmlg/require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around #'gsmlg/build-require-times)


(define-derived-mode gsmlg/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 gsmlg/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 gsmlg/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'gsmlg/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun gsmlg/require-times-sort-by-start-time-pred (entry1 entry2)
  "Sort predicate for ENTRY1 and ENTRY2 by start time."
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun gsmlg/require-times-sort-by-load-time-pred (entry1 entry2)
  "Sort predicate for ENTRY1 and ENTRY2 by load time."
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun gsmlg/require-times-tabulated-list-entries ()
  "Generate entries for `gsmlg/require-times-mode'."
  (cl-loop for (feature start-time millis) in gsmlg/require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (gsmlg/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun gsmlg/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (gsmlg/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))


(defun gsmlg/show-init-time ()
  "Report total init completion time."
  (message "init completed in %.2fms"
           (gsmlg/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'gsmlg/show-init-time)

(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
