#!/usr/bin/env zsh

# make bin directory in home if it doesn't already exist
mkdir -p ~/bin

# setup fresh
#git clone https://github.com/freshshell/fresh.git ~/.fresh/source/freshshell/fresh
#ln -s ~/.fresh/build/.freshrc ~/.freshrc
#~/.fresh/source/freshshell/fresh/bin/fresh

# run dot file manager
~/.dotfiles/bin/dfm install

# setup vundle
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

# setup prezto
#git clone --recursive https://github.com/sorin-ionescu/prezto.git ~/.zprezto
#setopt EXTENDED_GLOB
#for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
#ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
#done

# setup powerline
pip install git+git://github.com/Lokaltog/powerline.git
pip install git+git://github.com/kovidgoyal/powerline-daemon.git
