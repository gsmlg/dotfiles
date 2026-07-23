(defun gsmlginc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar gsmlginc/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun gsmlginc/build-require-times (orig-fn feature &optional filename noerror)
  "Note in `gsmlginc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (funcall orig-fn feature filename noerror)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (gsmlginc/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'gsmlginc/require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around #'gsmlginc/build-require-times)


(define-derived-mode gsmlginc/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 gsmlginc/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 gsmlginc/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'gsmlginc/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun gsmlginc/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun gsmlginc/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun gsmlginc/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in gsmlginc/require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (gsmlginc/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun gsmlginc/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (gsmlginc/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))




(defun gsmlginc/show-init-time ()
  (message "init completed in %.2fms"
           (gsmlginc/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'gsmlginc/show-init-time)


(provide 'init-benchmarking)
