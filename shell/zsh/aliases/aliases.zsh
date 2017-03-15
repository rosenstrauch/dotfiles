# aliases sourced from zshrc
alias git="git-achievements"

#alias fu="python2 ~/.fresh/build/src/fu/fu" --> moved to prezto
alias git-team="git log --format='%aN' | sort -u"
alias ...='nocorrect cd ../../'
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'
alias zcp='zmv -C'
alias zln='zmv -L'
alias vg='vagrant'
#alias bty='~/betty/main.rb'
alias drupalcs="phpcs --standard=Drupal --extensions='php,module,inc,install,test,profile,theme,js,css,info,txt'"
# XXX: https://github.com/nvbn/thefuck
alias fuck='eval $(thefuck $(fc -ln -1 | tail -n 1)); fc -R'
alias sorry='sudo $(history -p !-1)'
#alias please='sudo $(fc -nl -1)'
#alias please='eval "sudo $(history -p !!)"'
alias cdl='cdls'
alias chrome='chromium'
alias gfl='git-flow'
alias docker-composer='docker-compose'
