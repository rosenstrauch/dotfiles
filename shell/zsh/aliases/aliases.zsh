# aliases sourced from zshrc
alias git="git-achievements"
alias fu="python ~/.fresh/build/src/fu/fu"
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
alias drush5='~/drush5/drush'
alias drush6='~/drush6/vendor/bin/drush'
alias drush7='~/drush7/vendor/bin/drush'
alias drush8='~/drush8/vendor/bin/drush'
alias cdl='cdls'
