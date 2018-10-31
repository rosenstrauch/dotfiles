# run help

# taken from archwiki [fn:34:https://wiki.archlinux.org/index.php/zsh#Help_command]
# #+BEGIN_QUOTE
# run-help will invoke man for external commands. Default keyboard shortcut is Alt+h or Esc+h.
# #+END_QUOTE
# #+BEGIN_EXAMPLE
# run-help git commit
# #+END_EXAMPLE

autoload -Uz run-help
unalias run-help
alias help=run-help


autoload -Uz run-help-git
autoload -Uz run-help-sudo
