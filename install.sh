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

# Step 1: Emacs configuration
echo "[1/5] Installing Emacs configuration..."
if is_symlink_to "$HOME/.emacs" "$DOTFILES_DIR/emacs.d/init.el"; then
  echo "  - Already linked ~/.emacs -> $DOTFILES_DIR/emacs.d/init.el (skipping)"
else
  ln -sf "$DOTFILES_DIR/emacs.d/init.el" "$HOME/.emacs"
  echo "  ✓ Symlinked ~/.emacs -> $DOTFILES_DIR/emacs.d/init.el"
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
echo "[4/5] Installing Oh-My-Zsh configuration..."
mkdir -p "$DOTFILES_DIR/oh-my-zsh/cache"
if is_symlink_to "$HOME/.zshrc" "$DOTFILES_DIR/oh-my-zsh/zshrc"; then
  echo "  - Already linked ~/.zshrc (skipping)"
else
  ln -sf "$DOTFILES_DIR/oh-my-zsh/zshrc" "$HOME/.zshrc"
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
