# Features:
#
# Finds and sources drush.complete.sh from your drush directory,
# enabling autocompletion for drush commands.
#
# Creates aliases to common drush commands that work in a global context:
#
# dr - drush
# ddd - drush drupal-directory
# dl - drush pm-download
# ev - drush php-eval
# sa - drush site-alias
# sa - drush site-alias --local (show local site aliases)
# st - drush core-status
# use - drush site-set
#
# Aliases for drush commands that work on the current drupal site:
#
# cc - drush cache-clear
# cca - drush cache-clear all
# dis - drush pm-disable
# en - drush pm-enable
# i - drush pm-info
# pml - drush pm-list
# rf - drush pm-refresh
# unin - drush pm-uninstall
# up - drush pm-update
# upc - drush pm-updatecode
# updb - drush updatedb
# q - drush sql-query
#

#
# Drush site alias expansion is also done for the cpd command:
#
# cpd -R @site1:%files @site2:%files
#
# Note that the 'cpd' alias only works for local sites. Use
# `drush rsync` or gitd` to move files between remote sites.
#
# Aliases are also possible for the following standard
# commands. Uncomment their definitions below as desired.
#
# cd - cddl [*]
# ls - lsd
# cp - cpd
# ssh - dssh
# git - gitd
#
# These standard commands behave exactly the same as they always
# do, unless a drush site specification such as @dev or @live:%files
# is used in one of the arguments.

# Aliases for common drush commands that work in a global context.
alias dr='drush'
alias ddd='drush drupal-directory'
alias dl='drush pm-download'
alias ev='drush php-eval'
alias sa='drush site-alias'
alias lsa='drush site-alias --local'
alias st='drush core-status'
alias use='drush site-set'

# Aliases for drush commands that work on the current drupal site
alias cc='drush cache-clear'
alias cca='drush cache-clear all'
alias dis='drush pm-disable'
alias en='drush pm-enable'
alias pmi='drush pm-info'
alias pml='drush pm-list'
alias rf='drush pm-refresh'
alias unin='drush pm-uninstall'
alias up='drush pm-update'
alias upc='drush pm-updatecode'
alias updb='drush updatedb'
alias q='drush sql-query'

# Overrides for standard shell commands. Uncomment to enable. Alias
# cd='cdd' if you want to be able to use cd @remote to ssh to a
# remote site.

# alias cd='cddl'
# alias ls='lsd'
# alias cp='cpd'
# alias ssh='dssh'
# alias git='gitd'

