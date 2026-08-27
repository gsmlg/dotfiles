#!/usr/bin/env bash
# Install or refresh the GSMLG Emacs user service for the current OS.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
server_name="${GSMLG_EMACS_SERVER_NAME:-main}"

install_macos() {
  local template="$repo_root/emacs.d/services/com.gsmlg.emacs.plist.in"
  local target="$HOME/Library/LaunchAgents/com.gsmlg.emacs.plist"
  local state_dir="${XDG_STATE_HOME:-$HOME/.local/state}/emacs"
  mkdir -p "$(dirname "$target")" "$state_dir"
  sed "s|REPLACE_WITH_HOME|$HOME|g" "$template" >"$target"
  launchctl bootout "gui/$(id -u)/com.gsmlg.emacs" 2>/dev/null || true
  launchctl bootstrap "gui/$(id -u)" "$target"
  launchctl enable "gui/$(id -u)/com.gsmlg.emacs"
  launchctl kickstart -k "gui/$(id -u)/com.gsmlg.emacs"
  printf 'Installed launchd agent com.gsmlg.emacs (server %s)\n' "$server_name"
}

install_systemd() {
  local unit_src="$repo_root/emacs.d/services/gsmlg-emacs.service"
  local unit_dir="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"
  mkdir -p "$unit_dir"
  cp "$unit_src" "$unit_dir/gsmlg-emacs.service"
  systemctl --user daemon-reload
  systemctl --user enable --now gsmlg-emacs.service
  printf 'Installed systemd user unit gsmlg-emacs.service (server %s)\n' \
    "$server_name"
}

case "$(uname -s)" in
  Darwin) install_macos ;;
  Linux) install_systemd ;;
  *)
    printf 'Unsupported OS for automatic Emacs service install: %s\n' \
      "$(uname -s)" >&2
    exit 1
    ;;
esac
