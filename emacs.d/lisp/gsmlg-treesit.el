;;; gsmlg-treesit.el --- Tree-sitter readiness and mode selection -*- lexical-binding: t; -*-

;;; Commentary:
;; Grammar readiness helpers, tree-sitter-or-fallback mode selection, reports,
;; and explicit grammar installation.  Grammars are never downloaded at startup.

;;; Code:

(require 'gsmlg-paths)
(require 'gsmlg-language-registry)
(require 'treesit)

(defconst gsmlg-treesit-languages
  (gsmlg-language-registry-treesit-languages)
  "Tree-sitter grammars used by this configuration when available.")

(defconst gsmlg-treesit-default-sources
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
    (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (heex "https://github.com/phoenixframework/tree-sitter-heex")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                "master" "src")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
         "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                "master" "typescript/src")
    (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
  "Default `treesit-language-source-alist' recipes for GSMLG languages.")

(defun gsmlg-treesit-grammar-directory ()
  "Return the XDG data directory used for compiled tree-sitter grammars."
  (gsmlg-ensure-directory (gsmlg-data-file "tree-sitter/")))

(defun gsmlg-treesit-ensure-sources ()
  "Install default grammar recipes without replacing existing entries."
  (dolist (recipe gsmlg-treesit-default-sources)
    (unless (assq (car recipe) treesit-language-source-alist)
      (push recipe treesit-language-source-alist))))

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
  (gsmlg-treesit-ensure-sources)
  (treesit-install-language-grammar language
                                    (gsmlg-treesit-grammar-directory)))

;;;###autoload
(defun gsmlg-treesit-install-all-language-grammars ()
  "Explicitly install every tree-sitter grammar declared here.

Languages already ready are skipped.  Failures are reported and do not
abort the remaining installations.  Default recipes from
`gsmlg-treesit-default-sources' fill gaps in
`treesit-language-source-alist' without replacing existing entries.
Compiled grammars are installed under the XDG data directory.  Never
runs during startup.

Return a plist with `:succeeded', `:skipped', and `:failed' language
lists."
  (interactive)
  (gsmlg-treesit-ensure-sources)
  (let ((out-dir (gsmlg-treesit-grammar-directory))
        succeeded skipped failed errors)
    (dolist (language gsmlg-treesit-languages)
      (cond
       ((gsmlg-treesit-ready-p language)
        (push language skipped))
       (t
        (condition-case err
            (progn
              (treesit-install-language-grammar language out-dir)
              (push language succeeded))
          (error
           (let ((detail (error-message-string err)))
             (push language failed)
             (push (cons language detail) errors)
             (message "GSMLG treesit: failed to install %s: %s"
                      language detail)))))))
    (setq succeeded (nreverse succeeded)
          skipped (nreverse skipped)
          failed (nreverse failed)
          errors (nreverse errors))
    (message "GSMLG treesit: installed %d, skipped %d, failed %d"
             (length succeeded) (length skipped) (length failed))
    (when (and errors (called-interactively-p 'interactive))
      (with-help-window "*GSMLG Tree-sitter Install*"
        (princ "Failed grammar installations:\n\n")
        (dolist (entry errors)
          (princ (format "%-12s %s\n" (car entry) (cdr entry))))))
    (list :succeeded succeeded :skipped skipped :failed failed)))

(gsmlg-treesit-ensure-sources)
(add-to-list 'treesit-extra-load-path (gsmlg-treesit-grammar-directory))

(provide 'gsmlg-treesit)
;;; gsmlg-treesit.el ends here
