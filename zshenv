# Portable user environment retained after nix-darwin retirement.
# Machine-local paths and credentials remain in ~/.zshenv.local.

export EDITOR="${EDITOR:-nvim}"
export GEM_HOME="${GEM_HOME:-$HOME/.local/share/gem}"
export GOPRIVATE="${GOPRIVATE:-gitlab.zdns.cn,git.gsmlg.net,gitlab.gsmlg.net,github.com/gsmlg-dev/}"
export HEX_HOME="${HEX_HOME:-$HOME/.local/share/hex}"
export HOMEBREW_BREW_GIT_REMOTE="${HOMEBREW_BREW_GIT_REMOTE:-https://git.gsmlg.net/mirrors/brew.git}"
export HOMEBREW_CORE_GIT_REMOTE="${HOMEBREW_CORE_GIT_REMOTE:-https://git.gsmlg.net/mirrors/homebrew-core.git}"
export MIX_HOME="${MIX_HOME:-$HOME/.local/share/mix}"
export NPM_CONFIG_PREFIX="${NPM_CONFIG_PREFIX:-$HOME/.local/share/npm}"
export no_proxy="${no_proxy:-localhost,127.0.0.1,::1,.local,.cn,.gsmlg.com,.gsmlg.app,.gsmlg.net,.gsmlg.org,.gsmlg.dev,10.0.0.0/8,172.16.0.0/12,192.168.0.0/16}"

[[ -r "$HOME/.zshenv.local" ]] && source "$HOME/.zshenv.local"

# Keep the first occurrence of each path entry. ~/.zshenv.local places
# user-managed tools before MacPorts, then legacy managers and macOS tools.
typeset -U path PATH
