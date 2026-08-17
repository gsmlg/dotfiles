#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
emacs="${EMACS:-emacs}"
owns_test_root=0

if [[ -n "${GSMLG_EMACS_TEST_ROOT:-}" ]]; then
  test_root="$GSMLG_EMACS_TEST_ROOT"
  mkdir -p "$test_root"
else
  test_root="$(mktemp -d "/tmp/gsmlg-emacs-suite.XXXXXX")"
  owns_test_root=1
fi

cleanup() {
  if [[ "$owns_test_root" -eq 1 ]] &&
     [[ "$test_root" == /tmp/gsmlg-emacs-suite.* ]]; then
    rm -rf "$test_root"
  fi
}
trap cleanup EXIT

export GSMLG_EMACS_TEST_ROOT="$test_root"
export GSMLG_EMACS_TEST_DATA_HOME="${GSMLG_EMACS_TEST_DATA_HOME:-$test_root/data}"

test_home="$test_root/home"
xdg_config_home="$test_root/config"
xdg_data_home="$GSMLG_EMACS_TEST_DATA_HOME"
xdg_cache_home="$test_root/cache"
xdg_state_home="$test_root/state"
xdg_runtime_dir="$test_root/runtime"
config_directory="$xdg_config_home/emacs"
status_before="$test_root/emacs-status-before"
status_after="$test_root/emacs-status-after"
digest_before="$test_root/checkout-digest-before"
digest_after="$test_root/checkout-digest-after"

checkout_digest() {
  git -C "$repo_root" ls-files --cached --others --exclude-standard -z |
    while IFS= read -r -d '' path; do
      printf 'path:%q\n' "$path"
      if [[ -L "$repo_root/$path" ]]; then
        printf 'link:%q\n' "$(readlink "$repo_root/$path")"
      elif [[ -f "$repo_root/$path" ]]; then
        shasum -a 256 "$repo_root/$path" | awk '{print "file:" $1}'
      else
        printf 'missing\n'
      fi
    done |
    shasum -a 256 | awk '{print $1}'
}

mkdir -p "$xdg_runtime_dir"
chmod 700 "$xdg_runtime_dir"
git -C "$repo_root" status --short --untracked-files=all --ignored \
  >"$status_before"
checkout_digest >"$digest_before"

printf '\n==> Installer migration tests\n'
bash "$repo_root/emacs.d/tests/install-test.sh"

printf '\n==> Fresh and warm/offline startup tests\n'
EMACS="$emacs" "$repo_root/test-emacs-startup.sh"

printf '\n==> Independent module loading and cycle detection\n'
EMACS="$emacs" "$repo_root/emacs.d/tests/module-load-test.sh"

printf '\n==> ERT configuration suite\n'
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
                 (expand-file-name \"$config_directory\")))" \
  --load "$config_directory/early-init.el" \
  --load "$config_directory/init.el" \
  -L "$repo_root/emacs.d/tests" \
  --load "$repo_root/emacs.d/tests/test-helper.el" \
  --eval "
(progn
  (dolist (file
           (directory-files \"$repo_root/emacs.d/tests\"
                            t \"-test\\\\.el\\\\'\"))
    (load file nil 'nomessage))
  (run-hooks 'after-init-hook 'emacs-startup-hook))" \
  --funcall ert-run-tests-batch-and-exit

printf '\n==> First-party byte compilation, checkdoc, and dependency scans\n'
GSMLG_EMACS_LINT_SKIP_STARTUP=1 \
  EMACS="$emacs" \
  "$repo_root/lint-emacs-config.sh"

printf '\n==> Agent Editor MCP package suite\n'
EMACS="$emacs" \
  "$repo_root/emacs.d/site-lisp/agent-editor-mcp/run_tests.sh"

printf '\n==> Org Note package suite\n'
EMACS="$emacs" \
  "$repo_root/emacs.d/site-lisp/org-note/run_tests.sh"

git -C "$repo_root" status --short --untracked-files=all --ignored \
  >"$status_after"
checkout_digest >"$digest_after"
if ! diff -u "$status_before" "$status_after"; then
  printf '\nTests changed the repository status.\n' >&2
  exit 1
fi
if ! diff -u "$digest_before" "$digest_after"; then
  printf '\nTests changed tracked or untracked repository contents.\n' >&2
  exit 1
fi

printf '\nAll Emacs configuration tests passed.\n'
