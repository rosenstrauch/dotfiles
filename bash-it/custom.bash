%Override mcd function
function mcd() {
  mkdir -p "$1" && cd "$1";
}
source ~/.liquidprompt/liquidprompt
source ~/.config/liquidpromptrc
source ~/.fresh/build/shell.sh # source fresh rc for dotfile management
