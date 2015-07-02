
#sudo easy_install bugwarrior

# setup powerline
sudo pip install git+git://github.com/Lokaltog/powerline.git
sudo pip install git+git://github.com/kovidgoyal/powerline-daemon.git
# setup cheat
sudo pip install git+git://github.com/chrisallenlane/cheat.git
# pygments for cheat
sudo pip install Pygments
# bitbucket-issues-cli needs https://bitbucket.org/jsmits/bitbucket-issues-cli/pull-request/2/patched-to-include-git-repositories-fixes/diff
sudo pip install -e hg+https://bitbucket.org/jsmits/bitbucket-issues-cli#egg=bbi

#sudo pip install gcalcli

# taskwarrior addons
sudo pip install git+git://github.com/burnison/tasksync.git
# taskwarrior hooks
mkdir -p ~/.task/hooks
sudo pip install taskwarrior-time-tracking-hook
ln -s `which taskwarrior_time_tracking_hook` ~/.task/hooks/on-modify.timetracking
