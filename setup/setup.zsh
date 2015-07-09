#!/usr/bin/env bash

# setup script meant to be run once on clean arch install (worry about other distros later).
#
#References:

#https://github.com/darol100/lazydubuntu/blob/master/lazydubuntu.sh


# remember dir for using other scripts
DIR="${BASH_SOURCE%/*}"
if [[ ! -d "$DIR" ]]; then DIR="$PWD"; fi

# make directories in users home if they don't already exist
[ ! -d $HOME/bin ] && mkdir -p $HOME/bin
[ ! -d $HOME/.config/terminator ] && mkdir -p $HOME/.config/terminator
[ ! -d $HOME/.tmuxinator ] && mkdir -p $HOME/.tmuxinator
[ ! -d $HOME/.cheat ] && mkdir -p $HOME/.cheat
[ ! -d $HOME/scripts ] && mkdir -p $HOME/scripts
[ ! -d $HOME/.emacs.d ] && mkdir -p $HOME/.emacs.d

#must move old bashrc
case $OSTYPE in
  darwin*)
    CONFIG_FILE=.bash_profile
    ;;
  *)
    CONFIG_FILE=.bashrc
    ;;
esac
make_backup () {
  CONFIG_FILE=$1
  BACKUP_FILE=$CONFIG_FILE.bak

  if [ -e "$HOME/$BACKUP_FILE" ]; then
      echo "Backup file already exists. Make sure to backup your $CONFIG_FILE before running this installation." >&2
      while true
      do
          read -e -n 1 -r -p "Would you like to overwrite the existing backup? This will delete your existing backup file ($HOME/$BACKUP_FILE) [y/N] " RESP
          case $RESP in
          [yY])
              break
              ;;
          [nN]|"")
              echo -e "\033[91mInstallation aborted. Please come back soon!\033[m"
              exit 1
              ;;
          *)
              echo -e "\033[91mPlease choose y or n.\033[m"
              ;;
          esac
      done
  fi

  test -w "$HOME/$CONFIG_FILE" &&
    mv "$HOME/$CONFIG_FILE" "$HOME/$CONFIG_FILE.bak" &&
    echo "Your original $CONFIG_FILE has been backed up to $CONFIG_FILE.bak"

}


# setup fresh
#git clone https://github.com/freshshell/fresh.git ~/.fresh/source/freshshell/fresh
#ln -s ~/.fresh/build/.freshrc ~/.freshrc
#~/.fresh/source/freshshell/fresh/bin/fresh






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

setup_gsettings () {
  # Font settings
   echo 'Setting font preferences...'
   gsettings set org.gnome.desktop.interface text-scaling-factor '1.1'
   gsettings set org.gnome.desktop.interface document-font-name 'Cantarell 11'
   gsettings set org.gnome.desktop.interface font-name 'Cantarell 11'
   gsettings set org.gnome.nautilus.desktop font 'Cantarell 11'
   gsettings set org.gnome.desktop.wm.preferences titlebar-font 'Cantarell Bold 11'
   gsettings set org.gnome.settings-daemon.plugins.xsettings antialiasing 'rgba'
   gsettings set org.gnome.settings-daemon.plugins.xsettings hinting 'slight'
   echo 'Done. '
   # Nautilus Preferences
   echo 'Setting Nautilus preferences...'
   gsettings set org.gnome.nautilus.preferences sort-directories-first true
   # Gedit Preferences
   echo 'Setting Gedit preferences...'
   gsettings set org.gnome.gedit.preferences.editor display-line-numbers true
   gsettings set org.gnome.gedit.preferences.editor create-backup-copy false
   gsettings set org.gnome.gedit.preferences.editor auto-save true
   gsettings set org.gnome.gedit.preferences.editor insert-spaces true
   gsettings set org.gnome.gedit.preferences.editor tabs-size 4
}

install_bashit
#install_xiki
set_zsh_default

if command -v fresh >/dev/null 2>&1; then
    echo fresh installed...OK
    cd ~/.dotfiles && git pull
    echo updating fresh ...
    fresh update
else
  echo "installing fresh..."
  make_backup .bashrc
  make_backup .zprofile
  make_backup .zshrc
  make_backup .zlogin
  make_backup Desktop
  FRESH_LOCAL_SOURCE=rosenstrauch/dotfiles bash <(curl -sL https://raw.githubusercontent.com/freshshell/fresh/master/install.sh)
  echo "fresh installed...OK"
  if [ ! -d $HOME/.fresh/build ]
    then mv ~/.fresh/build.new ~/.fresh/build
  fi
#  ~/bin/fresh update
fi

install_terminal_profiles
setup_gsettings

echo "best logout and log back in now"
