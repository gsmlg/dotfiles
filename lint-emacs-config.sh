#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EMACS_DIR="$SCRIPT_DIR/emacs.d"

echo "=== Linting Emacs Configuration Files ==="

${EMACS:=emacs} -nw --batch \
  --eval "(progn
            (setq user-emacs-directory \"$EMACS_DIR/\")
            (add-to-list 'load-path \"$EMACS_DIR/lisp\")
            (setq byte-compile-error-on-warn nil)
            (load-file \"$EMACS_DIR/init.el\"))" \
  --eval "(progn
            (require 'checkdoc)
            (let ((files (directory-files-recursively \"$EMACS_DIR/lisp\" \"\\\\.el$\")))
              (push \"$EMACS_DIR/init.el\" files)
              (dolist (file files)
                (message \"\n--- Linting %s ---\" (file-relative-name file \"$SCRIPT_DIR\"))
                (byte-compile-file file)
                (let ((checkdoc-diagnostic-buffer \"*checkdoc-diagnostics*\"))
                  (when (get-buffer checkdoc-diagnostic-buffer)
                    (kill-buffer checkdoc-diagnostic-buffer))
                  (checkdoc-file file)
                  (when (get-buffer checkdoc-diagnostic-buffer)
                    (with-current-buffer checkdoc-diagnostic-buffer
                      (message \"%s\" (buffer-string))))))))"

# Clean up temporary byte-compiled .elc files created during lint check
rm -f "$EMACS_DIR"/*.elc "$EMACS_DIR"/lisp/*.elc

echo ""
echo "=== Lint Check Completed ==="
