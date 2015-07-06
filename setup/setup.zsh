#!/usr/bin/env bash

# setup script meant to be run once on clean arch install (worry about other distros later).
#
#References:

#https://github.com/darol100/lazydubuntu/blob/master/lazydubuntu.sh


# remember dir for using other scripts
DIR="${BASH_SOURCE%/*}"
if [[ ! -d "$DIR" ]]; then DIR="$PWD"; fi

# make bin directory in users home if it doesn't already exist for user commands
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
  rmdir $HOME/Desktop
  FRESH_LOCAL_SOURCE=rosenstrauch/dotfiles bash <(curl -sL https://raw.githubusercontent.com/freshshell/fresh/master/install.sh)
  echo fresh installed...OK
  mv ~/.fresh/build.new ~/.fresh/build
  ~/bin/fresh update
fi



# # install betty if its not already installed
# if [ ! -d /opt/betty ]
# then
#     echo initialising betty...
#     cp -rf ~/.fresh/source/pickhardt/betty /opt/betty/
# else
#     echo betty is initialized...OK
# fi

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
    . "$HOME/.bash_it/install.sh"
    else
      #"$HOME/.bash_it/upgrade_bashit"
      echo "bashit seems installed"
    fi
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

# make zsh the default shell
set_zsh_default () {
  # make sure zsh is the default shell
if [[ $SHELL == $(which zsh) ]]
  then echo "shell is $SHELL...OK"
else
  # switch to zsh
  chsh -s $(which zsh)
fi
}

install_terminal_profiles
install_bashit
install_xiki
set_zsh_default
