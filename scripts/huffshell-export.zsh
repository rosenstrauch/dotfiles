#!/usr/bin/env zsh
[[ -t 1 ]] && echo "Outputting control characters"
[[ -t 1 ]] || echo "Not outputting control characters" 

# load rvm ruby
source /home/discipolo/.rvm/environments/ruby-1.9.3-p551

huffshell | less -R > ~/.dotfiles/manual/huffshell/huffshell-aliases.md