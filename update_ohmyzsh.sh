#!/bin/bash

curl -sL https://github.com/ohmyzsh/ohmyzsh/archive/refs/heads/master.zip -o /tmp/omz.zip

unzip -d /tmp/ /tmp/omz.zip

test -d /tmp/ohmyzsh-master/ || mkdir /tmp/ohmyzsh-master/

cp ~/.dotfiles/oh-my-zsh/zshrc /tmp/zshrc

rsync -a --delete /tmp/ohmyzsh-master/ oh-my-zsh/

cp /tmp/zshrc ~/.dotfiles/oh-my-zsh/zshrc

