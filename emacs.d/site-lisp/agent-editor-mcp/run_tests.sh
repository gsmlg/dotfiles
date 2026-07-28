#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

${EMACS:=emacs} -Q --batch \
  -L "$SCRIPT_DIR" \
  -L "$SCRIPT_DIR/test" \
  --eval "(progn
            (require 'ert)
            (dolist (file (directory-files \"$SCRIPT_DIR/test\" t \"-test\\\\.el$\"))
              (load file nil nil t))
            (ert-run-tests-batch-and-exit))"
