#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"
emacs="${EMACS:-emacs}"
owns_test_root=0

if [[ -n "${GSMLG_EMACS_TEST_ROOT:-}" ]]; then
  test_root="$GSMLG_EMACS_TEST_ROOT/module-load"
  mkdir -p "$test_root"
else
  test_root="$(mktemp -d "/tmp/gsmlg-emacs-modules.XXXXXX")"
  owns_test_root=1
fi

cleanup() {
  if [[ "$owns_test_root" -eq 1 ]] &&
     [[ "$test_root" == /tmp/gsmlg-emacs-modules.* ]]; then
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

mkdir -p \
  "$test_home" \
  "$xdg_config_home" \
  "$xdg_data_home" \
  "$xdg_cache_home" \
  "$xdg_state_home" \
  "$xdg_runtime_dir"
chmod 700 "$xdg_runtime_dir"

features=(
  gsmlg-paths
  gsmlg-bootstrap
  gsmlg-core
  gsmlg-ui
  gsmlg-completion
  gsmlg-editing
  gsmlg-keybindings
  gsmlg-project
  gsmlg-vcs
  gsmlg-eglot
  gsmlg-tramp
  gsmlg-session
  gsmlg-org
  gsmlg-elfeed
  gsmlg-agent
  gsmlg-lang-elisp
  gsmlg-lang-beam
  gsmlg-lang-web
  gsmlg-lang-systems
  gsmlg-lang-scripting
  gsmlg-lang-infra
)

for feature in "${features[@]}"; do
  printf 'Requiring %s in isolation\n' "$feature"
  env \
    "HOME=$test_home" \
    "XDG_CONFIG_HOME=$xdg_config_home" \
    "XDG_DATA_HOME=$xdg_data_home" \
    "XDG_CACHE_HOME=$xdg_cache_home" \
    "XDG_STATE_HOME=$xdg_state_home" \
    "XDG_RUNTIME_DIR=$xdg_runtime_dir" \
    "GSMLG_EMACS_OFFLINE=1" \
    "GSMLG_EMACS_LOCAL=" \
    "EMACS_AGENT_AUTOSTART=" \
    "$emacs" -Q --batch \
    --eval "(setq user-emacs-directory
                  (file-name-as-directory
                   (expand-file-name \"$repo_root/emacs.d\")))" \
    --load "$repo_root/emacs.d/early-init.el" \
    -L "$repo_root/emacs.d/lisp" \
    -L "$repo_root/emacs.d/lisp/lang" \
    -L "$repo_root/emacs.d/site-lisp/agent-editor-mcp" \
    --eval "
(progn
  (require '$feature)
  (when (fboundp 'gsmlg-bootstrap-wait)
    (gsmlg-bootstrap-wait))
  (unless (featurep '$feature)
    (error \"Feature did not provide itself: $feature\")))"
done

printf 'Independent module loading passed for %d features.\n' \
  "${#features[@]}"
