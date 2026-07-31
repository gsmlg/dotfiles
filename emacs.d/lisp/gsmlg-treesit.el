;;; gsmlg-treesit.el --- Tree-sitter readiness and mode selection -*- lexical-binding: t; -*-

;;; Commentary:
;; Grammar readiness helpers, tree-sitter-or-fallback mode selection, reports,
;; and explicit grammar installation.  Grammars are never downloaded at startup.

;;; Code:

(require 'gsmlg-language-registry)
(require 'treesit)

(defconst gsmlg-treesit-languages
  (gsmlg-language-registry-treesit-languages)
  "Tree-sitter grammars used by this configuration when available.")

(defun gsmlg-treesit-ready-p (language)
  "Return non-nil when the grammar for LANGUAGE is installed and usable."
  (and (treesit-available-p)
       (condition-case nil
           (treesit-ready-p language t)
         (error nil))))

(defun gsmlg-treesit-or-fallback (language tree-mode fallback-mode)
  "Activate TREE-MODE for LANGUAGE when ready, otherwise FALLBACK-MODE."
  (funcall
   (if (and (fboundp tree-mode)
            (gsmlg-treesit-ready-p language))
       tree-mode
     fallback-mode)))

(defun gsmlg-auto-mode-prepend (entry)
  "Move auto-mode ENTRY to the front of `auto-mode-alist'."
  (setq auto-mode-alist (cons entry (delete entry auto-mode-alist))))

;;;###autoload
(defun gsmlg-treesit-report ()
  "Display availability of every tree-sitter grammar used here."
  (interactive)
  (with-help-window "*GSMLG Tree-sitter Report*"
    (princ "GSMLG tree-sitter grammar report\n\n")
    (dolist (language gsmlg-treesit-languages)
      (princ (format "%-12s %s\n"
                     language
                     (if (gsmlg-treesit-ready-p language)
                         "ready"
                       "missing"))))))

;;;###autoload
(defun gsmlg-treesit-install-language-grammar (language)
  "Explicitly install the tree-sitter grammar for LANGUAGE."
  (interactive
   (list
    (intern
     (completing-read
      "Install grammar: "
      (mapcar #'symbol-name gsmlg-treesit-languages)
      nil t))))
  (treesit-install-language-grammar language))

(provide 'gsmlg-treesit)
;;; gsmlg-treesit.el ends here
