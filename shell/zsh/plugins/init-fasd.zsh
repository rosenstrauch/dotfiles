# fasd in zsh

eval "$(fasd --init auto)"
#
# Aliases
#

# Changes the current working directory interactively.
alias j='fasd_cd -i'
alias o='a -e xdg-open' # quick opening files with xdg-open
alias c='fasd_cd -d'
# function fasd_cd is defined in posix-alias
