# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository containing configuration for Emacs, Vim, Zsh (oh-my-zsh), and Git. The repository uses symbolic linking to install configurations into the home directory.

## Installation Commands

```bash
# Install all configurations (creates symlinks to home directory)
./install.sh

# Update oh-my-zsh to latest version (preserves custom zshrc)
./update_ohmyzsh.sh

# Install gsmlg-cli (platform-specific binary installer)
./install_cli.sh

# Test Emacs configuration loads correctly
./test-emacs-startup.sh
```

## Architecture

### Emacs Configuration Structure

The Emacs setup uses a modular architecture with `emacs.d/init.el` as the entry point:

- **init.el** loads modules in specific order via `require` statements
- **lisp/** directory contains modular configuration files named `init-*.el`
- Load order is critical - utilities and package management load first, then UI/settings, then language modes
- Language support modules: `init-go.el`, `init-ruby.el`, `init-rust.el`, `init-elixir.el`, `init-erlang.el`, `init-javascript.el`, `init-web.el`, etc.
- Tool integration: `init-lsp.el` (LSP support), `init-git.el` (Magit), `init-company.el` (completion), `init-helm.el` (navigation), `init-flycheck.el` (syntax checking)
- Applications: `init-org.el`, `init-elfeed.el` (RSS reader)
- **custom.el** stores Emacs customize interface settings (loaded last)
- Platform detection: `*is-a-mac*`, `*is-a-win*`, `*is-a-lin*` constants used for OS-specific configuration

### Oh-My-Zsh Configuration

- Custom installation path: `~/.dotfiles/oh-my-zsh` (not default `~/.oh-my-zsh`)
- `zshrc` file defines theme ("jonathan") and plugins
- Active plugins: git, docker, docker-compose, kubectl, golang, rust, npm, yarn, mix, flutter, node
- Auto-updates disabled (`DISABLE_AUTO_UPDATE="true"`)
- Integrations: iTerm2 shell integration, dart-cli completion, terraform completion, mc completion

### Vim Configuration

- Multiple vimrc files in `vimrc/vimrcs/`: `basic.vim`, `extended.vim`, `filetypes.vim`, `plugins_config.vim`
- Install script concatenates `basic.vim` to `~/.vimrc`

### Git Configuration

- `gitconfig` - global Git settings (copied to `~/.gitconfig`)
- `gitignore_global` - global ignore patterns (copied to `~/.gitignore_global`)

## Code Style

- **Shell scripts**: Use `#!/bin/bash` or `#!/bin/sh -e`, 2-space indentation
- **Emacs Lisp**: Follow Emacs conventions, modular `init-*.el` structure in `lisp/` directory
- **File naming**: kebab-case for Emacs Lisp files, snake_case for shell scripts
- **Error handling**: Use `set -e` in shell scripts for fail-fast behavior

## Modifying Configurations

When adding new Emacs functionality:
1. Create new `init-*.el` file in `emacs.d/lisp/` if needed
2. Add `(require 'init-<module>)` to `init.el` in appropriate order
3. Test with `./test-emacs-startup.sh` to ensure no errors
4. Core utilities and package management must load before feature modules
5. Language modes should load after LSP/completion setup

When modifying zsh configuration:
- Edit `oh-my-zsh/zshrc` directly (not `~/.zshrc` which is a symlink)
- Plugin order matters for some functionality
- Run `./update_ohmyzsh.sh` to update oh-my-zsh while preserving custom config

## Testing

- `./test-emacs-startup.sh` - Validates Emacs configuration loads without errors in batch mode
- Uses `--batch` mode with `debug-on-error` enabled to catch startup issues
