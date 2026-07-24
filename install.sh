#!/usr/bin/env bash
set -e

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"

echo "=== Installing Dotfiles Configuration ==="

# Step 1: Emacs configuration
echo "[1/5] Installing Emacs configuration..."
ln -sf "$DOTFILES_DIR/emacs.d/init.el" "$HOME/.emacs"
echo "  ✓ Symlinked ~/.emacs -> $DOTFILES_DIR/emacs.d/init.el"

# Step 2: Email configuration
echo "[2/5] Installing Email configurations (mbsync & msmtp)..."
ln -sf "$DOTFILES_DIR/mbsyncrc" "$HOME/.mbsyncrc"
ln -sf "$DOTFILES_DIR/msmtprc" "$HOME/.msmtprc"
echo "  ✓ Symlinked ~/.mbsyncrc and ~/.msmtprc"

# Step 3: Vim configuration
echo "[3/5] Installing Vim configuration..."
cat "$DOTFILES_DIR/vimrc/vimrcs/basic.vim" > "$HOME/.vimrc"
echo "  ✓ Generated ~/.vimrc"

# Step 4: Oh-My-Zsh configuration
echo "[4/5] Installing Oh-My-Zsh configuration..."
ln -sf "$DOTFILES_DIR/oh-my-zsh/zshrc" "$HOME/.zshrc"
mkdir -p "$DOTFILES_DIR/oh-my-zsh/cache"
echo "  ✓ Symlinked ~/.zshrc and created cache directory"

# Step 5: Git global configuration
echo "[5/5] Installing Git global configuration..."
cp "$DOTFILES_DIR/gitconfig" "$HOME/.gitconfig"
cp "$DOTFILES_DIR/gitignore_global" "$HOME/.gitignore_global"
echo "  ✓ Copied ~/.gitconfig and ~/.gitignore_global"

echo ""
echo "=== Dotfiles Installation Complete ==="
