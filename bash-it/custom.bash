
source ~/.liquidprompt/liquidprompt
source ~/.config/liquidpromptrc
source ~/.fresh/build/shell.sh # source fresh rc for dotfile management
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm
export PATH="$(yarn global bin):$PATH"
