local _sublime_darwin_subl3=/Applications/Sublime\ Text\ 3.app/Contents/SharedSupport/bin/subl

if  [[ $('uname') == 'Darwin' ]]; then
  # Check if Sublime is installed in user's home application directory
  if [[ -a $HOME/${_sublime_darwin_subl3} ]]; then
    alias st3='$HOME/${_sublime_darwin_subl3}'
  else
    alias st3='${_sublime_darwin_subl3}'
  fi
fi
alias stt3='st3 .'
