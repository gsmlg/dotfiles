#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

owns_test_root=0
if [[ -n "${GSMLG_EMACS_TEST_ROOT:-}" ]]; then
  test_root="$GSMLG_EMACS_TEST_ROOT/agent-editor-mcp"
  mkdir -p "$test_root"
else
  test_root="$(mktemp -d "/tmp/gsmlg-emacs-agent-editor.XXXXXX")"
  owns_test_root=1
fi

cleanup() {
  if [[ "$owns_test_root" -eq 1 ]] &&
     [[ "$test_root" == /tmp/gsmlg-emacs-agent-editor.* ]]; then
    rm -rf "$test_root"
  fi
}
trap cleanup EXIT

test_home="$test_root/home"
xdg_config_home="$test_root/config"
xdg_data_home="${GSMLG_EMACS_TEST_DATA_HOME:-$test_root/data}"
xdg_cache_home="$test_root/cache"
xdg_state_home="$test_root/state"
xdg_runtime_dir="$test_root/runtime"
mkdir -p "$test_home" "$xdg_config_home" "$xdg_data_home" \
  "$xdg_cache_home" "$xdg_state_home" "$xdg_runtime_dir"
chmod 700 "$xdg_runtime_dir"

env \
  "HOME=$test_home" \
  "XDG_CONFIG_HOME=$xdg_config_home" \
  "XDG_DATA_HOME=$xdg_data_home" \
  "XDG_CACHE_HOME=$xdg_cache_home" \
  "XDG_STATE_HOME=$xdg_state_home" \
  "XDG_RUNTIME_DIR=$xdg_runtime_dir" \
  "GSMLG_EMACS_LOCAL=" \
  "GSMLG_EMACS_TESTING=1" \
  "EMACS_AGENT_AUTOSTART=" \
  "${EMACS:-emacs}" -Q --batch \
  -L "$SCRIPT_DIR" \
  -L "$SCRIPT_DIR/test" \
  --eval "(progn
            (require 'ert)
            (dolist (file (directory-files \"$SCRIPT_DIR/test\" t \"-test\\\\.el$\"))
              (load file nil nil t))
            (ert-run-tests-batch-and-exit))"
