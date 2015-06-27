#!/usr/bin/env zsh

#References:
#https://github.com/darol100/lazydubuntu/blob/master/lazydubuntu.sh


# remember dir for using other scripts
DIR="${BASH_SOURCE%/*}"
if [[ ! -d "$DIR" ]]; then DIR="$PWD"; fi

# make bin directory in home if it doesn't already exist
[ ! -d $HOME/bin ] && mkdir -p $HOME/bin


# setup fresh
#git clone https://github.com/freshshell/fresh.git ~/.fresh/source/freshshell/fresh
#ln -s ~/.fresh/build/.freshrc ~/.freshrc
#~/.fresh/source/freshshell/fresh/bin/fresh

if command -v fresh >/dev/null 2>&1; then
    echo fresh installed...OK
    #echo updating fresh ...
    #fresh update
else
  echo installing fresh...
  FRESH_LOCAL_SOURCE=~rosenstrauch/dotfiles bash <(curl -sL get.freshshell.com)
  echo fresh installed...OK
fi

# install betty if its not already installed
if [ ! -d /opt/betty ]
then
    echo initialising betty...
    cp -rf ~/.fresh/source/pickhardt/betty /opt/betty/
else
    echo betty is initialized...OK
fi

# setup vundle (now via pacman or vimrc)
# git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

# use evil pyenv installer see https://github.com/yyuu/pyenv
#curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash

#install cask

#curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

#go get github.com/monochromegane/vagrant-global-status/...
#go get github.com/peco/peco/cmd/peco



# setup prezto - this is done by fresh
#git clone --recursive https://github.com/sorin-ionescu/prezto.git ~/.zprezto
#setopt EXTENDED_GLOB
#for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
#ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
#done


install_bashit()
{
  echo -n "Install bashit?"
  read BASH
  if [[ $BASH =~ ^[Yy]$ ]]
  then
    if [ ! -d $HOME/.bash_it ]
    then
    echo "INSTALLING BASHIT"
    git clone https://github.com/revans/bash-it.git "$HOME/.bash_it"
    sh "$HOME/.bash_it/install.sh"
    else
      "$HOME/.bash_it/upgrade_bashit"
    fi
  fi
}

install_atom()
{
  echo -n "Install atom plugins?"
  read ATOM

  if [[ $ATOM =~ ^[Yy]$ ]]
  then
  echo "atom plugins"
  . "$DIR/atomplugins.sh"
  fi
}

# import terminal profiles

install_terminal_profiles()
{
  echo -n "Install terminal profiles?"
  read terminal

  if [[ $terminal =~ ^[Yy]$ ]]
  then
  echo "terminal profiles imported"
  gconftool-2 --load ~/.dotfiles/config/gnome-terminal/gnome-terminal-conf.xml
  fi
}


# install xiki
install_xiki()
{
  echo -n "Install xiki?"
  read XIKI

  if [[ $XIKI =~ ^[Yy]$ ]]
  then
  echo "XIKI"
  cd ~; curl -LO https://github.com/trogdoro/xiki/archive/master.tar.gz ; tar xzf master.tar.gz; cd xiki-master/bin; ./clearxsh; ./xsh
  fi
}
# install npm
install_npm()
{
  echo -n "Install NPM apps?"
  read NPM
  if [[ $NPM =~ ^[Yy]$ ]]
  then
  echo "NPM"
  . "$DIR/setup-npm.zsh"
  fi
}
install_atom
install_terminal_profiles
install_xiki
install_npm
install_bashit

# make sure zsh is the default shell
if [[ $SHELL == $(which zsh) ]]
  then echo "shell is $SHELL...OK"
else
  # switch to zsh
  chsh -s $(which zsh)
fi
