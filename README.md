# dotfiles

Personal dotfiles collection for Emacs, Vim, Zsh, and Git configuration.

## Quick Start

```bash
# Install all configurations
./install.sh

# Test Emacs configuration
./test-emacs-startup.sh
```

## Structure

- **emacs.d/** - Emacs configuration with modular init files
- **vimrc/** - Vim configuration with basic and extended setups
- **oh-my-zsh/** - Zsh configuration and themes
- **gitconfig** - Global Git settings
- **mbsyncrc/msmtprc** - Email configuration

## Components

### Emacs
- Modular configuration in `emacs.d/init-*.el`
- Package management via ELPA
- Language-specific configurations (Elixir, Go, Ruby, etc.)

### Vim
- Basic and extended vimrc configurations
- Plugin management via pathogen
- Popular plugins included (NERDTree, lightline, etc.)

### Zsh
- Oh My Zsh framework
- Custom themes and plugins
- Shell configuration and aliases

## Testing

Run `./test-emacs-startup.sh` to verify Emacs loads correctly.

