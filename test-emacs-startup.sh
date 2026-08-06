#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
emacs="${EMACS:-emacs}"
emacsclient="${EMACSCLIENT:-emacsclient}"
if [[ -z "${EMACSCLIENT:-}" && "$emacs" == */* ]] &&
   [[ -x "$(dirname "$emacs")/emacsclient" ]]; then
  emacsclient="$(dirname "$emacs")/emacsclient"
fi
startup_mode="${GSMLG_EMACS_STARTUP_MODE:-reuse}"
owns_test_root=0
daemon_name=""
daemon_socket=""
daemon_pid=""
daemon_started=0

if [[ -n "${GSMLG_EMACS_TEST_ROOT:-}" ]]; then
  test_root="$GSMLG_EMACS_TEST_ROOT"
  mkdir -p "$test_root"
else
  test_root="$(mktemp -d "/tmp/gsmlg-emacs-startup.XXXXXX")"
  owns_test_root=1
fi

stop_test_daemon() {
  if [[ "$daemon_started" -eq 1 ]]; then
    env "${test_environment[@]}" \
      "$emacsclient" --socket-name "${daemon_socket:-$daemon_name}" \
      --eval '(kill-emacs 0)' >/dev/null 2>&1 || true
  fi
}

cleanup() {
  stop_test_daemon
  if [[ "$owns_test_root" -eq 1 ]] &&
     [[ "$test_root" == /tmp/gsmlg-emacs-startup.* ]]; then
    rm -rf "$test_root"
  fi
}
trap cleanup EXIT

case "$startup_mode" in
  fresh | reuse)
    ;;
  *)
    printf 'Unknown GSMLG_EMACS_STARTUP_MODE: %s (expected fresh or reuse)\n' \
      "$startup_mode" >&2
    exit 2
    ;;
esac

command -v "$emacs" >/dev/null 2>&1 ||
  {
    printf 'Emacs executable not found: %s\n' "$emacs" >&2
    exit 127
  }
command -v "$emacsclient" >/dev/null 2>&1 ||
  {
    printf 'emacsclient executable not found: %s\n' "$emacsclient" >&2
    exit 127
  }

test_home="$test_root/home"
xdg_config_home="$test_root/config"
if [[ -n "${GSMLG_EMACS_TEST_DATA_HOME:-}" ]]; then
  xdg_data_home="$GSMLG_EMACS_TEST_DATA_HOME"
elif [[ "$startup_mode" == "reuse" ]]; then
  xdg_data_home="${XDG_DATA_HOME:-$HOME/.local/share}"
else
  xdg_data_home="$test_root/data"
fi
xdg_cache_home="$test_root/cache"
xdg_state_home="$test_root/state"
xdg_runtime_dir="$test_root/runtime"
install_log="$test_root/install.log"

mkdir -p \
  "$test_home" \
  "$xdg_config_home" \
  "$xdg_data_home" \
  "$xdg_cache_home" \
  "$xdg_state_home" \
  "$xdg_runtime_dir"
chmod 700 "$xdg_runtime_dir"

if [[ "$startup_mode" == "fresh" ]]; then
  case "$xdg_data_home" in
    "$test_root" | "$test_root"/*)
      rm -rf "$xdg_data_home/emacs/elpaca"
      ;;
    *)
      printf 'Refusing to clear fresh-test data outside %s: %s\n' \
        "$test_root" "$xdg_data_home" >&2
      exit 2
      ;;
  esac
fi

# Isolate Agent Editor MCP from any host listener on the default port 9876.
pick_free_loopback_port() {
  python3 - <<'PY'
import socket

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
    sock.bind(("127.0.0.1", 0))
    print(sock.getsockname()[1])
PY
}

agent_port="${EMACS_AGENT_PORT:-$(pick_free_loopback_port)}"
if [[ ! "$agent_port" =~ ^[0-9]+$ ]] ||
   ((agent_port < 1 || agent_port > 65535)); then
  printf 'Unable to allocate a loopback Agent Editor MCP port\n' >&2
  exit 2
fi

test_environment=(
  "HOME=$test_home"
  "XDG_CONFIG_HOME=$xdg_config_home"
  "XDG_DATA_HOME=$xdg_data_home"
  "XDG_CACHE_HOME=$xdg_cache_home"
  "XDG_STATE_HOME=$xdg_state_home"
  "XDG_RUNTIME_DIR=$xdg_runtime_dir"
  "GSMLG_EMACS_LOCAL="
  "EMACS_AGENT_AUTOSTART="
  "EMACS_AGENT_PORT=$agent_port"
)

printf '==> Installing the Emacs configuration into an isolated HOME\n'
if ! env "${test_environment[@]}" \
  DOTFILES_DIR="$repo_root" \
  bash "$repo_root/install.sh" >"$install_log" 2>&1; then
  cat "$install_log" >&2
  exit 1
fi

config_directory="$xdg_config_home/emacs"
early_init="$config_directory/early-init.el"
init_file="$config_directory/init.el"

[[ -L "$config_directory" ]] ||
  {
    printf 'Installer did not create %s as a symlink\n' "$config_directory" >&2
    exit 1
  }
[[ -f "$early_init" && -f "$init_file" ]] ||
  {
    printf 'Installed configuration lacks early-init.el or init.el\n' >&2
    exit 1
  }

startup_assertions='
(progn
  (run-hooks (quote after-init-hook) (quote emacs-startup-hook))
  (unless (version<= "30.2" emacs-version)
    (error "GNU Emacs 30.2 or newer is required, found %s" emacs-version))
  (unless (and (featurep (quote early-init)) (featurep (quote init)))
    (error "early-init.el and init.el did not both load"))
  (when (eq system-type (quote darwin))
    (unless (getenv "MACOSX_DEPLOYMENT_TARGET")
      (error "Native compilation lacks the macOS deployment target guard")))
  (when (boundp (quote native-comp-eln-load-path))
    (let ((expected-cache
           (file-truename
            (expand-file-name "eln-cache/" gsmlg-cache-directory)))
          (configuration-root
           (file-truename
            (file-name-as-directory gsmlg-config-directory))))
      (unless
          (equal
           (file-truename (car native-comp-eln-load-path))
           expected-cache)
        (error "Native compilation cache is not redirected to XDG cache"))
      (dolist (path native-comp-eln-load-path)
        (when
            (file-in-directory-p
             (file-truename (expand-file-name path))
             configuration-root)
          (error
           "Native compilation path remains inside the configuration: %s"
           path)))))
  (unless (and (boundp (quote elpaca-lock-file))
               (file-readable-p elpaca-lock-file))
    (error "Elpaca lock file is not active and readable"))
  (unless (and (boundp (quote elpaca-menu-functions))
               (eq (car elpaca-menu-functions)
                   (quote elpaca-menu-lock-file)))
    (error "The Elpaca lock file is not the first recipe source"))
  (let ((entries
         (with-temp-buffer
           (insert-file-contents elpaca-lock-file)
           (read (current-buffer)))))
    (unless entries
      (error "The committed Elpaca lock file is empty"))
    (dolist (entry entries)
      (let* ((id (car entry))
             (properties (cdr entry))
             (recipe (plist-get properties :recipe))
             (revision (plist-get recipe :ref))
             (package (elpaca-get id)))
        (unless (and (stringp revision)
                     (string-match-p "\\`[[:xdigit:]]\\{40\\}$" revision))
          (error "Lock entry %S does not contain an exact revision" id))
        (unless package
          (error "Locked package %S was not realized" id))
        (unless (eq (elpaca<-status package) (quote finished))
          (error "Locked package %S finished with status %S"
                 id (elpaca<-status package)))
        (let ((source (elpaca<-source-dir package))
              (build (elpaca<-build-dir package)))
          (unless (and (file-directory-p source)
                       (file-directory-p build))
            (error "Locked package %S lacks its source or build directory" id))
          (let ((actual (gsmlg-bootstrap-source-revision package)))
            (unless (or
                     (equal
                      (getenv "GSMLG_EMACS_TEST_ALLOW_UNLOCKED") "1")
                     (equal actual revision))
              (error "Locked package %S is at %s, expected %s"
                     id actual revision)))))))
  (let* ((repository
          (expand-file-name "elpaca/repos/elpaca/" gsmlg-data-directory))
         (default-directory repository)
         (revision
          (string-trim
           (with-output-to-string
             (with-current-buffer standard-output
               (unless (zerop (process-file "git" nil t nil
                                            "rev-parse" "HEAD"))
                 (error "Unable to inspect the bootstrapped Elpaca revision")))))))
    (unless (equal revision gsmlg-elpaca-revision)
      (error "Elpaca revision %s does not match pin %s"
             revision gsmlg-elpaca-revision)))
  (unless (memq (quote duskmoon-moonlight) custom-enabled-themes)
    (error "Duskmoon Moonlight is not enabled"))
  (when (and (featurep (quote server)) (server-running-p))
    (error "Batch startup opened an Emacs server"))
  (when (and (fboundp (quote emacs-agent-editor-running-p))
             (emacs-agent-editor-running-p))
    (error "Batch startup opened an Agent Editor MCP listener"))
  (princ "GSMLG startup assertions passed\n"))'

run_emacs_startup() {
  env "${test_environment[@]}" "$@" "$emacs" -Q --batch \
    --eval "(setq user-emacs-directory
                  (file-name-as-directory
                   (expand-file-name \"$config_directory\")))" \
    --load "$early_init" \
    --load "$init_file" \
    --eval "$startup_assertions"
}

printf '==> Starting GNU Emacs (%s package data)\n' "$startup_mode"
run_emacs_startup

offline_bin="$test_root/offline-bin"
mkdir -p "$offline_bin"
real_git="$(command -v git)"

printf '%s\n' \
  '#!/usr/bin/env bash' \
  'set -euo pipefail' \
  'for argument in "$@"; do' \
  '  case "$argument" in' \
  '    clone|fetch|pull|ls-remote)' \
  '      printf "Offline startup attempted git %s\n" "$argument" >&2' \
  '      exit 97' \
  '      ;;' \
  '  esac' \
  'done' \
  'exec "$GSMLG_REAL_GIT" "$@"' >"$offline_bin/git"

for program in curl wget; do
  printf '%s\n' \
    '#!/usr/bin/env bash' \
    'printf "Offline startup attempted a network downloader\n" >&2' \
    'exit 97' >"$offline_bin/$program"
done
chmod +x "$offline_bin/git" "$offline_bin/curl" "$offline_bin/wget"

network_guard='
(progn
  (require (quote url))
  (require (quote network-stream))
  (require (quote package))
  (require (quote package-vc))
  (defun gsmlg-test-network-forbidden (&rest _arguments)
    (error "Warm startup attempted an Emacs network operation"))
  (defun gsmlg-test-package-operation-forbidden (&rest _arguments)
    (error "Warm startup attempted a package.el operation"))
  (defun gsmlg-test-nonlocal-network-forbidden (&rest arguments)
    (unless (eq (plist-get arguments :family) (quote local))
      (error "Warm startup attempted a nonlocal Emacs network operation")))
  (dolist (function
           (quote (url-retrieve
                   url-retrieve-synchronously
                   url-copy-file
                   open-network-stream)))
    (advice-add function :override (function gsmlg-test-network-forbidden)))
  (dolist (function
           (quote (package-refresh-contents
                   package-install
                   package-install-file
                   package-vc-install)))
    (advice-add function
                :override
                (function gsmlg-test-package-operation-forbidden)))
  (advice-add (quote make-network-process)
              :before
              (function gsmlg-test-nonlocal-network-forbidden)))'

printf '==> Repeating startup with package network access blocked\n'
env "${test_environment[@]}" \
  "PATH=$offline_bin:$PATH" \
  "GSMLG_REAL_GIT=$real_git" \
  "GSMLG_EMACS_OFFLINE=1" \
  "$emacs" -Q --batch \
  --eval "(setq user-emacs-directory
                (file-name-as-directory
                 (expand-file-name \"$config_directory\")))" \
  --load "$early_init" \
  --eval "$network_guard" \
  --load "$init_file" \
  --eval "$startup_assertions"

daemon_name="gsmlg-xdg-test-$$"
# Match Emacs/emacsclient default: $XDG_RUNTIME_DIR/emacs/<name>
daemon_socket="$xdg_runtime_dir/emacs/$daemon_name"
daemon_log="$test_root/daemon.log"
daemon_assertions="
(progn
  (unless (and (featurep 'early-init) (featurep 'init))
    (error \"The named daemon did not naturally load early-init.el and init.el\"))
  (unless (equal (file-truename user-init-file)
                 (file-truename \"$init_file\"))
    (error \"Named daemon loaded unexpected init file: %S\" user-init-file))
  (unless (and (fboundp 'emacs-agent-editor-running-p)
               (emacs-agent-editor-running-p))
    (error \"Named daemon did not start Agent Editor MCP with its server\"))
  t)"

printf '==> Verifying natural XDG discovery in a named daemon\n'
daemon_started=1
if ! env "${test_environment[@]}" \
  "PATH=$offline_bin:$PATH" \
  "GSMLG_REAL_GIT=$real_git" \
  "GSMLG_EMACS_OFFLINE=1" \
  "$emacs" --daemon="$daemon_name" >"$daemon_log" 2>&1; then
  cat "$daemon_log" >&2
  exit 1
fi

env "${test_environment[@]}" \
  "$emacsclient" --socket-name "$daemon_socket" \
  --eval "$daemon_assertions" >/dev/null
daemon_pid="$(
  env "${test_environment[@]}" \
    "$emacsclient" --socket-name "$daemon_socket" \
    --eval '(emacs-pid)' | tr -d '[:space:]'
)"
stop_test_daemon

for _attempt in {1..50}; do
  if ! env "${test_environment[@]}" \
    "$emacsclient" --socket-name "$daemon_socket" \
    --eval t >/dev/null 2>&1; then
    break
  fi
  sleep 0.1
done
if env "${test_environment[@]}" \
  "$emacsclient" --socket-name "$daemon_socket" \
  --eval t >/dev/null 2>&1; then
  printf 'Named test daemon %s did not terminate\n' "$daemon_name" >&2
  exit 1
fi
for _attempt in {1..50}; do
  if [[ ! "$daemon_pid" =~ ^[0-9]+$ ]] ||
     ! kill -0 "$daemon_pid" 2>/dev/null; then
    break
  fi
  sleep 0.1
done
if [[ "$daemon_pid" =~ ^[0-9]+$ ]] && kill -0 "$daemon_pid" 2>/dev/null; then
  printf 'Named test daemon PID %s is still running\n' "$daemon_pid" >&2
  exit 1
fi
if [[ -e "$daemon_socket" || -L "$daemon_socket" ]]; then
  printf 'Named test daemon left its socket behind: %s\n' \
    "$daemon_socket" >&2
  exit 1
fi
daemon_started=0

printf 'Fresh/warm startup validation passed.\n'
