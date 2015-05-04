#!/usr/bin/env zsh

# setup script kindly borrowed from dfm and adjusted for our purposes as alternative to rake to augment fresh

# make bin directory in home if it doesn't already exist
mkdir -p ~/bin

# setup fresh
#git clone https://github.com/freshshell/fresh.git ~/.fresh/source/freshshell/fresh
#ln -s ~/.fresh/build/.freshrc ~/.freshrc
#~/.fresh/source/freshshell/fresh/bin/fresh
FRESH_LOCAL_SOURCE=~/.dotfiles bash <(curl -sL get.freshshell.com)
# run dot file manager - replaced by fresh
#~/.dotfiles/bin/dfm install
cp -rf ~/.fresh/source/pickhardt/betty /root/betty/
# setup vundle
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

# use evil pyenv installer see https://github.com/yyuu/pyenv
curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash

#install cask

curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

# setup prezto - this is done by fresh
#git clone --recursive https://github.com/sorin-ionescu/prezto.git ~/.zprezto
#setopt EXTENDED_GLOB
#for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
#ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
#done

#go get github.com/monochromegane/vagrant-global-status/...
#go get github.com/peco/peco/cmd/peco

sudo pip install Pygments
#sudo easy_install bugwarrior
# setup powerline
sudo pip install git+git://github.com/Lokaltog/powerline.git
sudo pip install git+git://github.com/kovidgoyal/powerline-daemon.git
# setup cheat
sudo pip install git+git://github.com/chrisallenlane/cheat.git
# bitbucket-issues-cli needs https://bitbucket.org/jsmits/bitbucket-issues-cli/pull-request/2/patched-to-include-git-repositories-fixes/diff
sudo pip install -e hg+https://bitbucket.org/jsmits/bitbucket-issues-cli#egg=bbi

#sudo pip install gcalcli

# taskwarrior addons
sudo pip install git+git://github.com/burnison/tasksync.git
# taskwarrior hooks
mkdir -p ~/.task/hooks
sudo pip install taskwarrior-time-tracking-hook
ln -s `which taskwarrior_time_tracking_hook` ~/.task/hooks/on-modify.timetracking


# Install atom plugins
#apm install --packages-file ~/.dotfiles/config/atom/atom-packages.json

# import terminal profiles
gconftool-2 --load ~/.dotfiles/gnome-terminal/gnome-terminal-conf.xml

# switch to zsh
chsh -s "which zsh"

# install xiki
cd ~/src/xiki
gem install bundler
bundle
ruby etc/command/copy_xiki_command_to.rb /usr/local/bin/xiki
cd

# setup npm
#./setup-npm.zsh
