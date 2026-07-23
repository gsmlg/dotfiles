# Project Context & Agent Directives

## 1. Overview & Architecture
- **Repository Purpose**: Dotfiles repository managing system environment configurations for Emacs, Zsh (Oh-My-Zsh), Vim, Git, and mail utilities (`mbsync`, `msmtp`).
- **Execution & Deployment Model**: Configuration files are linked or copied into the user's home directory (`~`) via `./install.sh` shell script symlinks.
- **Emacs Architecture**:
  - Modular Emacs Lisp setup using `emacs.d/init.el` as the main loader.
  - Sub-modules reside in `emacs.d/lisp/` named as `init-*.el`.
  - Sequential load ordering: core utilities/package management -> UI/navigation -> language modes/LSP -> `custom.el` (loaded last).
  - Cross-platform OS detection via `*is-a-mac*`, `*is-a-win*`, `*is-a-lin*` constants.
- **Shell & Tooling Architecture**:
  - Custom Oh-My-Zsh installation rooted in `oh-my-zsh/` with `oh-my-zsh/zshrc` as primary config source.
  - Multi-file Vim configuration under `vimrc/vimrcs/` (`basic.vim`, `extended.vim`, `filetypes.vim`, `plugins_config.vim`).

## 2. Environment & Tooling
- **Runtimes & Interpreters**: POSIX `sh`/`bash`, Emacs Lisp (`emacs`), Zsh, Vim script.
- **Key Scripts & Workflows**:
  - `./install.sh`: Deploys symlinks (`.emacs`, `.zshrc`, `.mbsyncrc`, `.msmtprc`) and copies git configurations into `$HOME`.
  - `./update_ohmyzsh.sh`: Updates Oh-My-Zsh installation while preserving local `zshrc`.
  - `./install_cli.sh`: Downloads platform-specific `gsmlg-cli` binary.
  - `./test-emacs-startup.sh`: Executes batch validation (`emacs -nw --batch`) to verify Emacs configuration loads cleanly.
- **Package Management**: Emacs `package.el` with ELPA/MELPA repositories (`emacs.d/elpa-*`).

## 3. Engineering Conventions & Code Rules
- **Shell Scripts**:
  - Use `#!/bin/bash` or `#!/bin/sh -e` with fail-fast options (`set -e`).
  - Indentation: 2 spaces.
  - Naming: `snake_case` for shell scripts (`update_ohmyzsh.sh`, `test-emacs-startup.sh`).
- **Emacs Lisp**:
  - Follow standard Emacs Lisp conventions.
  - File Naming: `kebab-case` with `init-` prefix for modules (`init-company.el`, `init-lsp.el`).
  - Maintain strict module separation in `emacs.d/lisp/` and explicit `require` calls in `init.el`.
- **General Formatting**:
  - LF line endings, UTF-8 encoding, final newline, 2-space indentation (enforced via EditorConfig).

## 4. Operational Instructions for AI Agents
- **Target Repository Files**: Modify files in `~/.dotfiles/` directly (e.g., `oh-my-zsh/zshrc`, `emacs.d/lisp/init-*.el`). Do not attempt to modify target symlinks in `$HOME`.
- **Validation Requirement**: Always execute `./test-emacs-startup.sh` after modifying any Emacs Lisp configuration to verify clean startup without runtime exceptions.
- **Code Changes**: Make surgical, minimal edits. Match local conventions exactly. Preserve comment headers and load order sequences.
- **Dependency & Secrets Hygiene**: Do not check in hardcoded private keys or passwords; reference environment variables or external path configurations where necessary.