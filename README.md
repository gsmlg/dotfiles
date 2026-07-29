# dotfiles

Personal dotfiles for Emacs, Vim, Zsh, Git, mbsync, and msmtp.

## Install

```sh
./install.sh
```

The installer links the Emacs configuration to
`${XDG_CONFIG_HOME:-$HOME/.config}/emacs`, so GNU Emacs 30.2 or newer discovers
`early-init.el` and `init.el` normally. Existing Emacs files and directories
are moved to timestamped backups instead of being overwritten. The remaining
dotfile installation behavior is unchanged.

The first Emacs startup bootstraps the pinned Elpaca package graph and needs
Git plus network access. Subsequent startups use the installed builds and
committed lock file without refreshing package sources.

See [`emacs.d/README.md`](emacs.d/README.md) for prerequisites, local
configuration, tests, package maintenance, and recovery.

## Repository structure

- `emacs.d/` — Emacs 30.2+ configuration, tests, documentation, snippets, and
  the bundled Agent Editor MCP package
- `vimrc/` — basic and extended Vim configuration
- `oh-my-zsh/` — Oh My Zsh configuration and themes
- `gitconfig` and `gitignore_global` — Git defaults
- `mbsyncrc` and `msmtprc` — mail transport configuration

## Test Emacs

```sh
./run-emacs-tests.sh
```

The standalone startup, installer, lint, and Agent Editor commands are
documented in
[`emacs.d/README.md`](emacs.d/README.md).
