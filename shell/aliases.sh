# aliases


# default aliases provided by fasd. kept here for reference, override by redefining
alias a='fasd -a'        # any
alias s='fasd -si'       # show / search / select
alias d='fasd -d'        # directory
alias f='fasd -f'        # file
alias sd='fasd -sid'     # interactive directory selection
alias sf='fasd -sif'     # interactive file selection
alias z='fasd_cd -d'     # cd, same functionality as j in autojump
alias zz='fasd_cd -d -i' # cd with interactive selection

alias j='fasd_cd -d -i' # cd with interactive selection
# Customized commands
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias more=less
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias xo='xdg-open &>/dev/null'
# Custom aliases
alias fixit='sudo rm -f /var/lib/pacman/db.lck && sudo pacman-mirrors -g && sudo pacman -Syyuu  && sudo pacman -Suu'

alias v='f -e vim' # quick opening files with vim
alias m='f -e mplayer' # quick opening files with mplayer
alias o='a -e xdg-open' # quick opening files with xdg-open
alias e='f -e emacs -nw' # quick opening files with emacs
alias Se='f -e sudo emacs'
alias ez='j -e "emacsclient -n -c"'
alias gaacp='git add --all && git commit -a && git push'

alias 1='pushd +1'
alias 2='pushd +2'
alias 3='pushd +3'
alias drupalcs="phpcs --standard=Drupal --extensions='php,module,inc,install,test,profile,theme,js,css,info,txt'"
alias fuck='eval $(thefuck $(fc -ln -1 | tail -n 1)); fc -R'
alias sorry='sudo $(history -p !-1)'

alias docker-composer='nocorrect docker-compose'
alias fin stio='fin stop'

alias git-team="git log --format='%aN' | sort -u"
