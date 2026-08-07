#!/usr/bin/env bash
set -e

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"

echo "=== Installing Dotfiles Configuration ==="

# Helper function to check if symlink points to target
is_symlink_to() {
  local link="$1"
  local target="$2"
  [ -L "$link" ] && [ "$(readlink "$link" 2>/dev/null)" = "$target" ]
}

# Helper function to check if files are identical
is_file_identical() {
  local src="$1"
  local dest="$2"
  [ -f "$dest" ] && cmp -s "$src" "$dest"
}

# Resolve an existing file or directory without requiring GNU readlink.
canonical_existing_path() {
  local path="$1"
  local directory
  local basename

  if [ -d "$path" ]; then
    (cd "$path" && pwd -P)
  elif [ -e "$path" ]; then
    directory="$(cd "$(dirname "$path")" && pwd -P)"
    basename="$(basename "$path")"
    printf '%s/%s\n' "$directory" "$basename"
  else
    return 1
  fi
}

# Resolve a symlink target relative to the directory containing the link.
resolved_symlink_target() {
  local link="$1"
  local raw_target
  local candidate
  local directory

  [ -L "$link" ] || return 1
  raw_target="$(readlink "$link")"
  if [[ "$raw_target" = /* ]]; then
    candidate="$raw_target"
  else
    directory="$(cd "$(dirname "$link")" && pwd -P)"
    candidate="$directory/$raw_target"
  fi

  if canonical_existing_path "$candidate" 2>/dev/null; then
    return 0
  fi

  directory="$(dirname "$candidate")"
  if [ -d "$directory" ]; then
    directory="$(cd "$directory" && pwd -P)"
    printf '%s/%s\n' "$directory" "$(basename "$candidate")"
  else
    printf '%s\n' "$candidate"
  fi
}

symlink_points_to_path() {
  local link="$1"
  local target="$2"
  local resolved_link
  local resolved_target

  [ -L "$link" ] || return 1
  resolved_link="$(resolved_symlink_target "$link")"
  resolved_target="$(canonical_existing_path "$target")"
  [ "$resolved_link" = "$resolved_target" ]
}

next_backup_path() {
  local path="$1"
  local timestamp
  local candidate
  local suffix=0

  timestamp="$(date '+%Y%m%d-%H%M%S')"
  candidate="${path}.backup.${timestamp}"
  while [ -e "$candidate" ] || [ -L "$candidate" ]; do
    suffix=$((suffix + 1))
    candidate="${path}.backup.${timestamp}.${suffix}"
  done
  printf '%s\n' "$candidate"
}

backup_emacs_path() {
  local path="$1"
  local backup

  backup="$(next_backup_path "$path")"
  mv "$path" "$backup"
  echo "  ✓ Moved existing $path to $backup"
}

# Step 1: Emacs configuration
echo "[1/5] Installing Emacs configuration..."
emacs_source="$(canonical_existing_path "$DOTFILES_DIR/emacs.d")"
emacs_config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
emacs_link="$emacs_config_home/emacs"

if [ ! -f "$emacs_source/early-init.el" ] || [ ! -f "$emacs_source/init.el" ]; then
  echo "  ✗ Emacs source must contain early-init.el and init.el: $emacs_source" >&2
  exit 1
fi

for legacy_path in "$HOME/.emacs" "$HOME/.emacs.el" "$HOME/.emacs.d"; do
  if [ ! -e "$legacy_path" ] && [ ! -L "$legacy_path" ]; then
    continue
  fi

  if symlink_points_to_path "$legacy_path" "$emacs_source" ||
     symlink_points_to_path "$legacy_path" "$emacs_source/init.el"; then
    rm "$legacy_path"
    echo "  ✓ Removed legacy repository symlink $legacy_path"
  else
    backup_emacs_path "$legacy_path"
  fi
done

mkdir -p "$emacs_config_home"
if symlink_points_to_path "$emacs_link" "$emacs_source"; then
  echo "  - Already linked $emacs_link -> $emacs_source (skipping)"
else
  if [ -e "$emacs_link" ] || [ -L "$emacs_link" ]; then
    backup_emacs_path "$emacs_link"
  fi
  ln -s "$emacs_source" "$emacs_link"
  echo "  ✓ Symlinked $emacs_link -> $emacs_source"
fi

if [ ! -f "$emacs_link/early-init.el" ] || [ ! -f "$emacs_link/init.el" ]; then
  echo "  ✗ Installed Emacs configuration is incomplete: $emacs_link" >&2
  exit 1
fi

# Step 2: Email configuration
echo "[2/5] Installing Email configurations (mbsync & msmtp)..."
if is_symlink_to "$HOME/.mbsyncrc" "$DOTFILES_DIR/mbsyncrc" && \
   is_symlink_to "$HOME/.msmtprc" "$DOTFILES_DIR/msmtprc"; then
  echo "  - Already linked ~/.mbsyncrc and ~/.msmtprc (skipping)"
else
  ln -sf "$DOTFILES_DIR/mbsyncrc" "$HOME/.mbsyncrc"
  ln -sf "$DOTFILES_DIR/msmtprc" "$HOME/.msmtprc"
  echo "  ✓ Symlinked ~/.mbsyncrc and ~/.msmtprc"
fi

# Step 3: Vim configuration
echo "[3/5] Installing Vim configuration..."
if is_file_identical "$DOTFILES_DIR/vimrc/vimrcs/basic.vim" "$HOME/.vimrc"; then
  echo "  - Already installed ~/.vimrc (skipping)"
else
  cat "$DOTFILES_DIR/vimrc/vimrcs/basic.vim" > "$HOME/.vimrc"
  echo "  ✓ Generated ~/.vimrc"
fi

# Step 4: Oh-My-Zsh configuration
# Skip linking when ~/.zshrc already exists (e.g. managed by Nix/home-manager).
echo "[4/5] Installing Oh-My-Zsh configuration..."
mkdir -p "$DOTFILES_DIR/oh-my-zsh/cache"
if is_symlink_to "$HOME/.zshrc" "$DOTFILES_DIR/oh-my-zsh/zshrc"; then
  echo "  - Already linked ~/.zshrc (skipping)"
elif [ -e "$HOME/.zshrc" ] || [ -L "$HOME/.zshrc" ]; then
  echo "  - ~/.zshrc already exists (skipping; not managed by this installer)"
else
  ln -s "$DOTFILES_DIR/oh-my-zsh/zshrc" "$HOME/.zshrc"
  echo "  ✓ Symlinked ~/.zshrc and ensured cache directory"
fi

# Step 5: Git global configuration
echo "[5/5] Installing Git global configuration..."
if is_file_identical "$DOTFILES_DIR/gitconfig" "$HOME/.gitconfig" && \
   is_file_identical "$DOTFILES_DIR/gitignore_global" "$HOME/.gitignore_global"; then
  echo "  - Already copied ~/.gitconfig and ~/.gitignore_global (skipping)"
else
  cp "$DOTFILES_DIR/gitconfig" "$HOME/.gitconfig"
  cp "$DOTFILES_DIR/gitignore_global" "$HOME/.gitignore_global"
  echo "  ✓ Copied ~/.gitconfig and ~/.gitignore_global"
fi

echo ""
echo "=== Dotfiles Installation Complete ==="
