#!/usr/bin/env zsh

# setup script kindly borrowed from dfm and adjusted for our purposes as alternative to rake to augment fresh

# make bin directory in home if it doesn't already exist
mkdir -p ~/bin

# setup fresh
#git clone https://github.com/freshshell/fresh.git ~/.fresh/source/freshshell/fresh
#ln -s ~/.fresh/build/.freshrc ~/.freshrc
#~/.fresh/source/freshshell/fresh/bin/fresh

# run dot file manager - replaced by fresh
#~/.dotfiles/bin/dfm install

# setup vundle
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

# setup prezto - this is done by fresh
#git clone --recursive https://github.com/sorin-ionescu/prezto.git ~/.zprezto
#setopt EXTENDED_GLOB
#for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
#ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
#done

pip install Pygments

# setup powerline
pip install git+git://github.com/Lokaltog/powerline.git
pip install git+git://github.com/kovidgoyal/powerline-daemon.git
# setup cheat
pip install git+git://github.com/chrisallenlane/cheat.git

# Install atom plugins
apm install --packages-file ~/.dotfiles/config/atom/atom-packages.json

# import terminal profiles
gconftool-2 --load ~/.fresh/build/gnome-terminal-conf.xml

# switch to zsh
chsh -s 'which zsh'

# install xiki
cd ~/.src/xiki
gem install bundler
bundle
ruby etc/command/copy_xiki_command_to.rb /usr/local/bin/xiki
cd

# setup npm
./setup-npm.zsh
