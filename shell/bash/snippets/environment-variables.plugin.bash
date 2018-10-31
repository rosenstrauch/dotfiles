# Environment variables


# better yaourt colors
 export YAOURT_COLORS="nb=1:pkg=1:ver=1;32:lver=1;45:installed=1;42:grp=1;34:od=1;41;5:votes=1;44:dsc=0:other=1;35"
 export HISTSIZE=10000
 export HISTFILESIZE=${HISTSIZE}

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
 export HISTCONTROL=ignoreboth
 export JAVA_FONTS=/usr/share/fonts/TTF
 export TERM=xterm-256color
 export TERMINAL=/usr/bin/urxvt
 if [ -n "$DISPLAY" ]; then
     export BROWSER=/usr/bin/xdg-open
 else
     export BROWSER=/usr/bin/emacsclient --eval "(browse-url (replace-regexp-in-string \"'\" \"\" \"%u\"))"
 fi
