#!/bin/bash

# install emacs
ln -f -s ~/.dotfiles/emacs.d/init.el ~/.emacs

# install email config
ln -f -s ~/.dotfiles/emacs.d/mbsyncrc ~/.mbsyncrc
ln -f -s ~/.dotfiles/emacs.d/msmtprc ~/.msmtprc

# install vim
cat ~/.dotfiles/vimrc/vimrcs/basic.vim > ~/.vimrc
# or
#git clone --depth=1 https://github.com/amix/vimrc.git ~/.vim_runtime
#sh ~/.vim_runtime/install_basic_vimrc.sh

# install oh-my-zsh
#sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
ln -f -s ~/.dotfiles/oh-my-zsh/zshrc ~/.zshrc

# install git global config
cp gitconfig ~/.gitconfig
cp gitignore_global ~/.gitignore_global


