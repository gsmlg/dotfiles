#!/bin/bash

curl -sL https://github.com/ohmyzsh/ohmyzsh/archive/refs/heads/master.zip -o /tmp/omz.zip

unzip -d /tmp/ /tmp/omz.zip

rsync -a --delete /tmp/ohmyzsh-master/ oh-my-zsh/

