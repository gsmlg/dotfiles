#!/usr/bin/env bash
set -euo pipefail
shopt -s nullglob

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
emacs="${EMACS:-emacs}"
owns_test_root=0

if [[ -n "${GSMLG_EMACS_TEST_ROOT:-}" ]]; then
  test_root="$GSMLG_EMACS_TEST_ROOT"
  mkdir -p "$test_root"
else
  test_root="$(mktemp -d "/tmp/gsmlg-emacs-lint.XXXXXX")"
  owns_test_root=1
fi

xdg_data_home="${GSMLG_EMACS_TEST_DATA_HOME:-${XDG_DATA_HOME:-$HOME/.local/share}}"

cleanup() {
  if [[ "$owns_test_root" -eq 1 ]] &&
     [[ "$test_root" == /tmp/gsmlg-emacs-lint.* ]]; then
    rm -rf "$test_root"
  fi
}
trap cleanup EXIT

if [[ "${GSMLG_EMACS_LINT_SKIP_STARTUP:-0}" != "1" ]]; then
  GSMLG_EMACS_TEST_ROOT="$test_root" \
    GSMLG_EMACS_TEST_DATA_HOME="$xdg_data_home" \
    GSMLG_EMACS_STARTUP_MODE=reuse \
    EMACS="$emacs" \
    "$repo_root/test-emacs-startup.sh"
fi

test_home="$test_root/home"
xdg_config_home="$test_root/config"
xdg_cache_home="$test_root/cache"
xdg_state_home="$test_root/state"
xdg_runtime_dir="$test_root/runtime"
compile_output="$test_root/byte-compiled"
config_directory="$xdg_config_home/emacs"

mkdir -p "$compile_output" "$xdg_runtime_dir"
chmod 700 "$xdg_runtime_dir"

first_party_files=(
  "$repo_root/emacs.d/early-init.el"
  "$repo_root/emacs.d/init.el"
)
for file in "$repo_root"/emacs.d/lisp/gsmlg-*.el; do
  first_party_files+=("$file")
done
for file in "$repo_root"/emacs.d/lisp/lang/gsmlg-*.el; do
  first_party_files+=("$file")
done
for file in "$repo_root"/emacs.d/tests/*.el; do
  first_party_files+=("$file")
done

printf '==> Checking lexical-binding headers\n'
for file in "${first_party_files[@]}"; do
  if ! head -n 1 "$file" | grep -Fq 'lexical-binding: t'; then
    printf 'Missing lexical-binding header: %s\n' \
      "${file#"$repo_root"/}" >&2
    exit 1
  fi
done

old_modules=("$repo_root"/emacs.d/lisp/init-*.el)
if [[ "${#old_modules[@]}" -ne 0 ]]; then
  printf 'Legacy init-* modules remain active:\n' >&2
  printf '  %s\n' "${old_modules[@]#"$repo_root"/}" >&2
  exit 1
fi

assert_no_repository_runtime_state() {
  local candidates=(
    "$repo_root/emacs.d/.cache"
    "$repo_root/emacs.d/.cask"
    "$repo_root"/emacs.d/.emacs.desktop*
    "$repo_root/emacs.d/.mc-lists.el"
    "$repo_root/emacs.d/.session"
    "$repo_root/emacs.d/agent-editor"
    "$repo_root/emacs.d/auto-save-list"
    "$repo_root/emacs.d/backups"
    "$repo_root/emacs.d/bookmarks"
    "$repo_root/emacs.d/custom.el"
    "$repo_root/emacs.d/custom.el~"
    "$repo_root/emacs.d/desktop"
    "$repo_root/emacs.d/elfeed"
    "$repo_root/emacs.d/eln-cache"
    "$repo_root/emacs.d/elpa"
    "$repo_root"/emacs.d/elpa-*
    "$repo_root/emacs.d/elpaca"
    "$repo_root/emacs.d/eshell"
    "$repo_root/emacs.d/helm-adaptive-history"
    "$repo_root/emacs.d/history"
    "$repo_root/emacs.d/native-lisp"
    "$repo_root/emacs.d/network-security.data"
    "$repo_root/emacs.d/org-clock-save.el"
    "$repo_root/emacs.d/org-id-locations"
    "$repo_root/emacs.d/places"
    "$repo_root/emacs.d/projectile-bookmarks.eld"
    "$repo_root/emacs.d/projectile.cache"
    "$repo_root/emacs.d/projects"
    "$repo_root/emacs.d/recentf"
    "$repo_root/emacs.d/savehist"
    "$repo_root/emacs.d/server"
    "$repo_root/emacs.d/tramp"
    "$repo_root/emacs.d/transient"
    "$repo_root/emacs.d/url"
    "$repo_root/emacs.d/var"
  )
  local found=()
  local path

  for path in "${candidates[@]}"; do
    if [[ -e "$path" || -L "$path" ]]; then
      found+=("$path")
    fi
  done
  if [[ "${#found[@]}" -ne 0 ]]; then
    printf 'Mutable Emacs state exists inside the configuration:\n' >&2
    printf '  %s\n' "${found[@]#"$repo_root"/}" >&2
    return 1
  fi
}

assert_no_repository_runtime_state

runtime_code=()
while IFS= read -r file; do
  runtime_code+=("$file")
done < <(
  find "$repo_root/emacs.d" -type f \
    \( -name '*.elc' -o -name '*.eln' \) -print | sort
)
if [[ "${#runtime_code[@]}" -ne 0 ]]; then
  printf 'Generated Emacs code exists inside the configuration:\n' >&2
  printf '  %s\n' "${runtime_code[@]#"$repo_root"/}" >&2
  exit 1
fi

test_environment=(
  "HOME=$test_home"
  "XDG_CONFIG_HOME=$xdg_config_home"
  "XDG_DATA_HOME=$xdg_data_home"
  "XDG_CACHE_HOME=$xdg_cache_home"
  "XDG_STATE_HOME=$xdg_state_home"
  "XDG_RUNTIME_DIR=$xdg_runtime_dir"
  "GSMLG_EMACS_OFFLINE=1"
  "GSMLG_EMACS_LOCAL="
  "EMACS_AGENT_AUTOSTART="
  "EMACS_AGENT_WORKSPACE="
  "GSMLG_CONFIG_ROOT=$repo_root/emacs.d"
  "GSMLG_COMPILE_OUTPUT=$compile_output"
)

printf '==> Byte-compiling first-party Emacs Lisp (warnings are errors)\n'
env "${test_environment[@]}" "$emacs" -Q --batch \
  --eval "(setq user-emacs-directory
                (file-name-as-directory
                 (expand-file-name \"$config_directory\")))" \
  --load "$config_directory/early-init.el" \
  --load "$config_directory/init.el" \
  --eval '
(progn
  (require (quote bytecomp))
  (let* ((root (file-name-as-directory (getenv "GSMLG_CONFIG_ROOT")))
         (output (file-name-as-directory (getenv "GSMLG_COMPILE_OUTPUT")))
         (files
          (append
           (list (expand-file-name "early-init.el" root)
                 (expand-file-name "init.el" root))
           (directory-files (expand-file-name "lisp" root)
                            t "\\`gsmlg-.*\\.el$")
           (directory-files (expand-file-name "lisp/lang" root)
                            t "\\`gsmlg-.*\\.el$")
           (directory-files (expand-file-name "tests" root)
                            t "\\.el$")))
         (byte-compile-error-on-warn t)
         (byte-compile-dest-file-function
          (lambda (file)
            (expand-file-name
             (concat (file-name-nondirectory file) "c")
             output))))
    (add-to-list (quote load-path) (expand-file-name "tests" root))
    (dolist (file files)
      (message "Byte-compiling %s" (file-relative-name file root))
      (unless (byte-compile-file file)
        (error "Byte compilation did not produce output for %s" file)))))'

printf '==> Running checkdoc on first-party Emacs Lisp\n'
env "${test_environment[@]}" "$emacs" -Q --batch \
  --eval '
(progn
  (require (quote cl-lib))
  (require (quote checkdoc))
  (let* ((root (file-name-as-directory (getenv "GSMLG_CONFIG_ROOT")))
         (files
          (append
           (list (expand-file-name "early-init.el" root)
                 (expand-file-name "init.el" root))
           (directory-files (expand-file-name "lisp" root)
                            t "\\`gsmlg-.*\\.el$")
           (directory-files (expand-file-name "lisp/lang" root)
                            t "\\`gsmlg-.*\\.el$")
           (directory-files (expand-file-name "tests" root)
                            t "\\.el$")))
         failures)
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (let (errors)
          (cl-letf (((symbol-function (quote checkdoc-error))
                     (lambda (point message)
                       (push
                        (format "%s:%d: %s"
                                (file-relative-name file root)
                                (line-number-at-pos (or point (point-min)))
                                message)
                        errors))))
            (checkdoc-current-buffer t))
          (setq failures (nconc (nreverse errors) failures)))))
    (when failures
      (dolist (failure (nreverse failures))
        (message "%s" failure))
      (error "checkdoc found %d first-party issue(s)" (length failures)))))'

printf '==> Scanning active Emacs Lisp for removed dependencies\n'
legacy_pattern='(^|[^[:alnum:]_])(helm-lsp|lsp-mode|lsp-ui|js2-refactor|js2-mode|rjsx-mode|git-gutter|undo-tree|all-the-icons|projectile|flycheck|company|helm|hydra|spaceline|alchemist|tern)([^[:alnum:]_]|$)'
legacy_files=()
while IFS= read -r file; do
  legacy_files+=("$file")
done < <(
  find "$repo_root/emacs.d" -type f -name '*.el' \
    ! -path '*/tests/*' \
    ! -path '*/test/*' -print | sort
)
if command -v rg >/dev/null 2>&1; then
  legacy_matches() {
    rg -n -i "$legacy_pattern" "$@"
  }
else
  legacy_matches() {
    grep -Eni "$legacy_pattern" "$@"
  }
fi
if legacy_matches "${legacy_files[@]}"; then
  printf 'Removed dependencies are still referenced by active Emacs Lisp.\n' >&2
  exit 1
fi

package_pattern='package-(initialize|refresh-contents|install)|package-vc-install'
if command -v rg >/dev/null 2>&1; then
  package_bootstrap_matches() {
    rg -n "$package_pattern" "$@"
  }
else
  package_bootstrap_matches() {
    grep -ERn "$package_pattern" "$@"
  }
fi
if package_bootstrap_matches \
  "$repo_root/emacs.d/early-init.el" \
  "$repo_root/emacs.d/init.el" \
  "$repo_root/emacs.d/lisp"; then
  printf 'A package.el bootstrap call remains in active configuration.\n' >&2
  exit 1
fi

runtime_code=()
while IFS= read -r file; do
  runtime_code+=("$file")
done < <(
  find "$repo_root/emacs.d" -type f \
    \( -name '*.elc' -o -name '*.eln' \) -print | sort
)
if [[ "${#runtime_code[@]}" -ne 0 ]]; then
  printf 'Linting wrote generated code into the repository:\n' >&2
  printf '  %s\n' "${runtime_code[@]#"$repo_root"/}" >&2
  exit 1
fi

assert_no_repository_runtime_state

printf 'Byte compilation, checkdoc, and dependency scans passed.\n'
