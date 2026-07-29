#!/usr/bin/env bash
set -u
set -o pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd -P)"
emacs_source="$(cd "$repo_root/emacs.d" && pwd -P)"
test_root="$(mktemp -d "${TMPDIR:-/tmp}/gsmlg-emacs-install.XXXXXX")"
tests_run=0
tests_failed=0

shopt -s nullglob

cleanup() {
  if [[ "$test_root" == "${TMPDIR:-/tmp}"/gsmlg-emacs-install.* ]]; then
    rm -rf "$test_root"
  fi
}
trap cleanup EXIT

fail() {
  printf '    %s\n' "$*" >&2
  return 1
}

assert_eq() {
  local expected="$1"
  local actual="$2"
  local description="$3"

  [[ "$actual" == "$expected" ]] ||
    fail "$description: expected '$expected', got '$actual'"
}

assert_file_contents() {
  local path="$1"
  local expected="$2"

  [[ -f "$path" ]] || {
    fail "missing file: $path"
    return 1
  }
  assert_eq "$expected" "$(cat "$path")" "contents of $path"
}

assert_emacs_link() {
  local config_home="$1"
  local link="$config_home/emacs"

  [[ -L "$link" ]] || {
    fail "expected symlink: $link"
    return 1
  }
  assert_eq "$emacs_source" "$(readlink "$link")" "Emacs link target" ||
    return 1
  [[ -f "$link/early-init.el" ]] || {
    fail "installed link lacks early-init.el"
    return 1
  }
  [[ -f "$link/init.el" ]] || {
    fail "installed link lacks init.el"
    return 1
  }
}

make_home() {
  mktemp -d "$test_root/home.XXXXXX"
}

run_install() {
  local home="$1"
  local config_home="${2-}"
  local log="$home/install.log"

  if [[ -n "$config_home" ]]; then
    HOME="$home" \
      XDG_CONFIG_HOME="$config_home" \
      DOTFILES_DIR="$repo_root" \
      bash "$repo_root/install.sh" >"$log" 2>&1
  else
    (
      unset XDG_CONFIG_HOME
      HOME="$home" \
        DOTFILES_DIR="$repo_root" \
        bash "$repo_root/install.sh"
    ) >"$log" 2>&1
  fi
}

test_fresh_install() {
  local home
  home="$(make_home)"
  printf 'keep me\n' >"$home/unrelated"

  run_install "$home" || return 1

  assert_emacs_link "$home/.config" || return 1
  [[ ! -e "$home/.emacs" && ! -L "$home/.emacs" ]] ||
    fail "fresh install unexpectedly created ~/.emacs"
  [[ ! -e "$home/.emacs.el" && ! -L "$home/.emacs.el" ]] ||
    fail "fresh install unexpectedly created ~/.emacs.el"
  [[ ! -e "$home/.emacs.d" && ! -L "$home/.emacs.d" ]] ||
    fail "fresh install unexpectedly created ~/.emacs.d"
  assert_file_contents "$home/unrelated" "keep me"
}

test_repeated_install_is_idempotent() {
  local home
  local backups
  home="$(make_home)"

  run_install "$home" || return 1
  run_install "$home" || return 1

  assert_emacs_link "$home/.config" || return 1
  backups=("$home/.config"/emacs.backup.*)
  assert_eq "0" "${#backups[@]}" "backup count after repeated install"
  grep -Fq "Already linked" "$home/install.log" ||
    fail "repeated install did not report the existing correct link"
}

test_migrates_old_emacs_symlink() {
  local home
  local backups
  home="$(make_home)"
  ln -s "$emacs_source/init.el" "$home/.emacs"

  run_install "$home" || return 1

  [[ ! -e "$home/.emacs" && ! -L "$home/.emacs" ]] ||
    fail "old repository ~/.emacs symlink was not removed"
  backups=("$home"/.emacs.backup.*)
  assert_eq "0" "${#backups[@]}" "old repository symlink backup count"
  assert_emacs_link "$home/.config"
}

test_backs_up_real_emacs_file() {
  local home
  local backups
  home="$(make_home)"
  printf 'user emacs config\n' >"$home/.emacs"

  run_install "$home" || return 1

  [[ ! -e "$home/.emacs" ]] || fail "real ~/.emacs was not migrated"
  backups=("$home"/.emacs.backup.*)
  assert_eq "1" "${#backups[@]}" "real ~/.emacs backup count" || return 1
  assert_file_contents "${backups[0]}" "user emacs config" || return 1
  grep -Fq "${backups[0]}" "$home/install.log" ||
    fail "installer did not print the ~/.emacs backup path"
}

test_backs_up_real_emacs_directory() {
  local home
  local backups
  home="$(make_home)"
  mkdir -p "$home/.emacs.d"
  printf 'user directory\n' >"$home/.emacs.d/marker"

  run_install "$home" || return 1

  [[ ! -e "$home/.emacs.d" ]] || fail "real ~/.emacs.d was not migrated"
  backups=("$home"/.emacs.d.backup.*)
  assert_eq "1" "${#backups[@]}" "real ~/.emacs.d backup count" || return 1
  assert_file_contents "${backups[0]}/marker" "user directory"
}

test_backs_up_real_emacs_el_file() {
  local home
  local backups
  home="$(make_home)"
  printf 'user emacs el config\n' >"$home/.emacs.el"

  run_install "$home" || return 1

  [[ ! -e "$home/.emacs.el" ]] || fail "real ~/.emacs.el was not migrated"
  backups=("$home"/.emacs.el.backup.*)
  assert_eq "1" "${#backups[@]}" "real ~/.emacs.el backup count" || return 1
  assert_file_contents "${backups[0]}" "user emacs el config"
}

test_honors_xdg_config_home() {
  local home
  local config_home
  home="$(make_home)"
  config_home="$home/custom-config"

  run_install "$home" "$config_home" || return 1

  assert_emacs_link "$config_home" || return 1
  [[ ! -e "$home/.config/emacs" && ! -L "$home/.config/emacs" ]] ||
    fail "installer ignored XDG_CONFIG_HOME"
}

test_backs_up_conflicting_xdg_directory() {
  local home
  local config_home
  local backups
  home="$(make_home)"
  config_home="$home/custom-config"
  mkdir -p "$config_home/emacs"
  printf 'existing XDG config\n' >"$config_home/emacs/marker"

  run_install "$home" "$config_home" || return 1

  assert_emacs_link "$config_home" || return 1
  backups=("$config_home"/emacs.backup.*)
  assert_eq "1" "${#backups[@]}" "XDG directory backup count" || return 1
  assert_file_contents "${backups[0]}/marker" "existing XDG config"
}

test_backs_up_broken_xdg_symlink() {
  local home
  local config_home
  local backups
  home="$(make_home)"
  config_home="$home/custom-config"
  mkdir -p "$config_home"
  ln -s "missing-emacs-config" "$config_home/emacs"

  run_install "$home" "$config_home" || return 1

  assert_emacs_link "$config_home" || return 1
  backups=("$config_home"/emacs.backup.*)
  assert_eq "1" "${#backups[@]}" "broken XDG symlink backup count" || return 1
  [[ -L "${backups[0]}" ]] || fail "broken symlink backup is not a symlink"
  assert_eq "missing-emacs-config" "$(readlink "${backups[0]}")" \
    "broken symlink backup target"
}

run_test() {
  local name="$1"
  local function_name="$2"
  local status
  tests_run=$((tests_run + 1))
  printf 'TEST %s\n' "$name"
  (
    set -e
    "$function_name"
  )
  status=$?
  if [[ "$status" -eq 0 ]]; then
    printf '  PASS\n'
  else
    tests_failed=$((tests_failed + 1))
    printf '  FAIL\n'
  fi
}

[[ -f "$emacs_source/early-init.el" ]] ||
  fail "source configuration lacks early-init.el" || exit 1
[[ -f "$emacs_source/init.el" ]] ||
  fail "source configuration lacks init.el" || exit 1

run_test "fresh install" test_fresh_install
run_test "repeated install" test_repeated_install_is_idempotent
run_test "old ~/.emacs symlink migration" test_migrates_old_emacs_symlink
run_test "real ~/.emacs backup" test_backs_up_real_emacs_file
run_test "real ~/.emacs.d backup" test_backs_up_real_emacs_directory
run_test "real ~/.emacs.el backup" test_backs_up_real_emacs_el_file
run_test "XDG_CONFIG_HOME" test_honors_xdg_config_home
run_test "conflicting XDG directory backup" test_backs_up_conflicting_xdg_directory
run_test "broken XDG symlink backup" test_backs_up_broken_xdg_symlink

printf '\nRESULT: %d tests, %d failures\n' "$tests_run" "$tests_failed"
[[ "$tests_failed" -eq 0 ]]
