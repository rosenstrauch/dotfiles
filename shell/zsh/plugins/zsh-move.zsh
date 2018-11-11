# zsh move
# if you happen to find yourself needing to bulk rename outside of emacs

# Load the great zmv function for bulk renaming
autoload -U zmv
alias zcp='zmv -C'
alias zln='zmv -L'
