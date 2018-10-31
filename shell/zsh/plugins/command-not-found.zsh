# command-not-found

# works if you installed https://wiki.archlinux.org/index.php/Pkgfile
# taken from archwiki [fn:35:https://wiki.archlinux.org/index.php/zsh#The_.22command_not_found.22_hook]



if [ -r /usr/share/doc/pkgfile/command-not-found.zsh ]; then
    source /usr/share/doc/pkgfile/command-not-found.zsh
fi
