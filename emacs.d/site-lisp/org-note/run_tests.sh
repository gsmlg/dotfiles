#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
: "${EMACS:=emacs}"

owns_test_root=0
if [[ -n "${GSMLG_EMACS_TEST_ROOT:-}" ]]; then
  test_root="$GSMLG_EMACS_TEST_ROOT/org-note"
  mkdir -p "$test_root"
else
  test_root="$(mktemp -d "/tmp/gsmlg-emacs-org-note.XXXXXX")"
  owns_test_root=1
fi

cleanup() {
  if [[ "$owns_test_root" -eq 1 ]] &&
     [[ "$test_root" == /tmp/gsmlg-emacs-org-note.* ]]; then
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

test_args=()
while IFS= read -r test_file; do
  test_args+=(-l "$test_file")
done < <(find "$script_dir/test" -type f -name '*-test.el' -print | LC_ALL=C sort)

env \
  "HOME=$test_home" \
  "XDG_CONFIG_HOME=$xdg_config_home" \
  "XDG_DATA_HOME=$xdg_data_home" \
  "XDG_CACHE_HOME=$xdg_cache_home" \
  "XDG_STATE_HOME=$xdg_state_home" \
  "XDG_RUNTIME_DIR=$xdg_runtime_dir" \
  "GSMLG_EMACS_LOCAL=" \
  "GSMLG_EMACS_TESTING=1" \
  "$EMACS" -Q --batch -L "$script_dir" -L "$script_dir/test" \
  -l ert "${test_args[@]}" -f ert-run-tests-batch-and-exit
