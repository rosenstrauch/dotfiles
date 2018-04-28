# Disabled as test if it interferes with tramp
# Load RVM into a shell session *as a function*
# [[ -s "${HOME}/.rvm/scripts/rvm" ]] && . "${HOME}/.rvm/scripts/rvm"
# export PATH="$PATH:$HOME/bin:$HOME/.rvm/bin" # Add RVM to PATH for scripting
case "$TERM" in
   "dumb")
       export PS1="> "
       ;;
   xterm*|rxvt*|eterm*|screen*)
       tty -s && export PS1="some crazy prompt stuff"
       ;;
esac
