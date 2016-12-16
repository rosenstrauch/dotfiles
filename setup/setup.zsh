#!/usr/bin/env zsh

# setup script meant to be run once on clean arch install (worry about other distros later). this will configure your $HOME settings and applications.
# for now this script assumes you are using fresh and gets the dotfiles for rosenstrauch

#References:

#https://github.com/darol100/lazydubuntu/blob/master/lazydubuntu.sh


# remember dir this script is executed from for sourcing other scripts
DIR="${BASH_SOURCE%/*}"
if [[ ! -d "$DIR" ]]; then DIR="$PWD"; fi

# make directories in users home if they don't already exist
[ ! -d $HOME/bin ] && mkdir -p $HOME/bin
[ ! -d $HOME/.config/terminator ] && mkdir -p $HOME/.config/terminator
[ ! -d $HOME/.tmuxinator ] && mkdir -p $HOME/.tmuxinator
[ ! -d $HOME/.cheat ] && mkdir -p $HOME/.cheat
[ ! -d $HOME/scripts ] && mkdir -p $HOME/scripts
[ ! -d $HOME/.emacs.d ] && mkdir -p $HOME/.emacs.d
[ ! -d $HOME/.local/share/applications ] && mkdir -p $HOME/.local/share/applications

#must move old bashrc
# case $OSTYPE in
#   darwin*)
#     CONFIG_FILE=.bash_profile
#     ;;
#   *)
#     CONFIG_FILE=.bashrc
#     ;;
# esac

# function to backup existing files and folders
# in your $HOME before linking your fresh dotfiles

make_backup () {
  CONFIG_FILE=$1
  BACKUP_FILE=$CONFIG_FILE.bak

  if [ -e "$HOME/$BACKUP_FILE" ]; then
      echo "Backup file already exists. Make sure to backup your $CONFIG_FILE before running this installation." >&2
      while true
      do
        echo -n "Would you like to overwrite the existing backup??"
        read RESP
        #read -e -n 1 -r -p "Would you like to overwrite the existing backup? This will delete your existing backup file ($HOME/$BACKUP_FILE) [y/N] " RESP
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





# # install betty if its not already installed
# if [ ! -d /opt/betty ]
# then
#     echo initialising betty...
#     cp -rf ~/.fresh/source/pickhardt/betty /opt/betty/
# else
#     echo betty is initialized...OK
# fi

# TODO: Figure out where and when to best install these go commands
#go get github.com/monochromegane/vagrant-global-status/...
#go get github.com/peco/peco/cmd/peco






# function to import terminal profiles from your dotfiles

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

# function to install the bash-it framework
install_bashit()
{
  if [ ! -d $HOME/.bash_it ]; then
    echo -n "Install bashit?"
    read BASH
    if [[ $BASH =~ ^[Yy]$ ]]; then
      echo "INSTALLING BASHIT"
      git clone https://github.com/revans/bash-it.git "$HOME/.bash_it"
      . "$HOME/.bash_it/install.sh"
    fi
    else
      #"$HOME/.bash_it/upgrade_bashit"
      echo "bashit seems installed"
  fi
}

# function to install xsh
install_xiki()
{
if [ ! -f $HOME/.xsh ]; then

  echo -n "Install xiki?"
  read XIKI

  if [[ $XIKI =~ ^[Yy]$ ]]; then
  echo "XIKI"
  cd ; curl -LO https://github.com/trogdoro/xiki/archive/master.tar.gz ; tar xzf master.tar.gz; cd xiki-master/bin; ./clearxsh; ./xsh
  fi
fi
echo "Xiki installed ...OK"
}

# function make zsh the default shell
set_zsh_default () {
  # make sure zsh is the default shell
if [[ $SHELL == $(which zsh) ]]
  then echo "shell is $SHELL...OK"
else
  # switch to zsh
  chsh -s $(which zsh)
fi
}

# function to configure gnome and gnome apps
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
  echo 'gnome Desktop settings'
  gsettings set org.gnome.desktop.interface clock-show-date true
  gsettings set org.gnome.shell enabled-extensions 'elementary'
  gsettings set org.gnome.desktop.wm.preferences button-layout ':minimize,maximize,close'
  echo 'gnome terminal settings'
  gsettings set org.gnome.Terminal.Legacy.Settings dark-theme true
  gsettings set org.gnome.Terminal.Legacy.Settings new-terminal-mode 'tab'
  gsettings set org.gnome.Terminal.Legacy.Settings default-show-menubar false
  # gsettings set org.gnome.Terminal.Legacy.Keybindings prev-tab '<Alt>Left'
  # gsettings set org.gnome.Terminal.Legacy.Keybindings next-tab '<Alt>Right'
  # gsettings set org.gnome.desktop.background picture-uri 'file:///usr/share/backgrounds/gnome/Mirror.jpg'
  # gsettings set org.gnome.desktop.datetime automatic-timezone true
}

install_bashit
set_zsh_default

# setup your fresh dotfiles from github
if command -v fresh >/dev/null 2>&1; then
    echo fresh installed...OK
    cd ~/.dotfiles && git pull
    echo updating fresh ...
    fresh
else
  echo "installing fresh..."
  make_backup .bashrc
  make_backup .zprofile
  make_backup .zshrc
  make_backup .zlogin
  make_backup Desktop
  make_backup .config/mimeapps.list
  make_backup .rvm/gemsets/default.gems
  make_backup .rvm/gemsets/global.gems
  FRESH_LOCAL_SOURCE=rosenstrauch/dotfiles bash <(curl -sL https://raw.githubusercontent.com/freshshell/fresh/master/install.sh)
  echo "fresh installed...OK"
  if [ ! -d $HOME/.fresh/build ]
    then mv ~/.fresh/build.new ~/.fresh/build
  fi
#  ~/bin/fresh update
fi

# now that we have fresh dotfiles ask if we want to setup terminal profiles
install_terminal_profiles
# configure our gnome shell and gnome apps
setup_gsettings

echo "best logout and log back in now"
