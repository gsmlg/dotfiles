# AGENTS.md

## Build/Test Commands
- Test emacs startup: `./test-emacs-startup.sh`
- No lint/format commands found - use standard conventions
- No package.json or build system detected

## Code Style Guidelines
- **Shell scripts**: Use `#!/bin/bash` or `#!/bin/sh -e`, 2-space indentation
- **EditorConfig**: LF line endings, UTF-8, final newline, 2-space indent
- **Emacs Lisp**: Follow Emacs conventions, use `init-*.el` modular structure
- **Zsh**: Follow oh-my-zsh patterns, use `.zsh` extension for plugins
- **Naming**: Use kebab-case for files (`init-company.el`), snake_case for scripts
- **Error handling**: Use `set -e` in shell scripts, `debug-on-error` in Emacs
- **Imports**: No explicit imports in shell/dotfiles, load order matters in init.el
- **Documentation**: Include comments for complex configurations