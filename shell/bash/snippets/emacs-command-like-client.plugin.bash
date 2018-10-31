# make emacs command behave like emacsclient

#       add a function to bashrc

#       #+NAME: shellfunc-emacs

function emacs {
  if [[ $# -eq 0 ]]; then
    /usr/bin/emacs # "emacs" is function, will cause recursion
    return
  fi
  args=($*)
  for ((i=0; i <= ${#args}; i++)); do
    local a=${args[i]}
    # NOTE: -c for creating new frame
    if [[ ${a:0:1} == '-' && ${a} != '-c' ]]; then
      /usr/bin/emacs ${args[*]}
      return
    fi
  done
  setsid emacsclient -n -a /usr/bin/emacs ${args[*]}
}


# emacs
alias e='emacsclient -nc -F "((fullscreen . maximized))"'
alias et='emacsclient -t'
export SUDO_EDITOR="emacsclient -nc"
export ALTERNATE_EDITOR=""
export EDITOR="emacs"
