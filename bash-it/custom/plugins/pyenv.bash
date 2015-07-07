
# load pyenv in bash if we have it installed
if [ -d "$HOME/.pyenv/bin" ] then
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
fi
