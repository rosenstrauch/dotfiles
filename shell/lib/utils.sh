#!/bin/bash

# References:
# http://natelandau.com/bash-scripting-utilities/

#
# Set Colors
#

bold=$(tput bold)
underline=$(tput sgr 0 1)
reset=$(tput sgr0)

purple=$(tput setaf 171)
red=$(tput setaf 1)
green=$(tput setaf 76)
tan=$(tput setaf 3)
blue=$(tput setaf 38)

#
# Headers and  Logging
#

e_header() { printf "\n${bold}${purple}==========  %s  ==========${reset}\n" "$@"
}
e_arrow() { printf "➜ $@\n"
}
e_success() { printf "${green}✔ %s${reset}\n" "$@"
}
e_error() { printf "${red}✖ %s${reset}\n" "$@"
}
e_warning() { printf "${tan}➜ %s${reset}\n" "$@"
}
e_underline() { printf "${underline}${bold}%s${reset}\n" "$@"
}
e_bold() { printf "${bold}%s${reset}\n" "$@"
}
e_note() { printf "${underline}${bold}${blue}Note:${reset}  ${blue}%s${reset}\n" "$@"
}

#
# USAGE FOR SEEKING CONFIRMATION
# seek_confirmation "Ask a question"
# Credt: https://github.com/kevva/dotfiles
#
# if is_confirmed; then
#   some action
# else
#   some other action
# fi
#

seek_confirmation() {
  printf "\n${bold}$@${reset}"
  read -p " (y/n) " -n 1
  printf "\n"
}

# underlined
seek_confirmation_head() {
  printf "\n${underline}${bold}$@${reset}"
  read -p "${underline}${bold} (y/n)${reset} " -n 1
  printf "\n"
}

# Test whether the result of an 'ask' is a confirmation
is_confirmed() {
if [[ "$REPLY" =~ ^[Yy]$ ]]; then
  return 0
fi
return 1
}

#
# Test whether a command exists
# $1 = cmd to test
# Usage:
# if type_exists 'git'; then
#   some action
# else
#   some other action
# fi
#

type_exists() {
if [ $(type -P $1) ]; then
  return 0
fi
return 1
}

#
# Test which OS the user runs
# $1 = OS to test
# Usage: if is_os 'darwin'; then
#

is_os() {
if [[ "${OSTYPE}" == $1* ]]; then
  return 0
fi
return 1
}

#
# Pushover Notifications
# Usage: pushover "Title Goes Here" "Message Goes Here"
# Credit: http://ryonsherman.blogspot.com/2012/10/shell-script-to-send-pushover.html
#

pushover () {
    PUSHOVERURL="https://api.pushover.net/1/messages.json"
    API_KEY="your-api-here"
    USER_KEY="your-user-key-here"
    DEVICE=""

    TITLE="${1}"
    MESSAGE="${2}"

    curl \
    -F "token=${API_KEY}" \
    -F "user=${USER_KEY}" \
    -F "device=${DEVICE}" \
    -F "title=${TITLE}" \
    -F "message=${MESSAGE}" \
    "${PUSHOVERURL}" > /dev/null 2>&1
}

#
# Given a list of desired items and installed items, return a list
# of uninstalled items. Arrays in bash are insane (not in a good way).
# Credit: https://github.com/cowboy/dotfiles
#
#
function to_install() {
  local debug desired installed i desired_s installed_s remain
  if [[ "$1" == 1 ]]; then debug=1; shift; fi
    # Convert args to arrays, handling both space- and newline-separated lists.
    read -ra desired < <(echo "$1" | tr '\n' ' ')
    read -ra installed < <(echo "$2" | tr '\n' ' ')
    # Sort desired and installed arrays.
    unset i; while read -r; do desired_s[i++]=$REPLY; done < <(
      printf "%s\n" "${desired[@]}" | sort
    )
    unset i; while read -r; do installed_s[i++]=$REPLY; done < <(
      printf "%s\n" "${installed[@]}" | sort
    )
    # Get the difference. comm is awesome.
    unset i; while read -r; do remain[i++]=$REPLY; done < <(
      comm -13 <(printf "%s\n" "${installed_s[@]}") <(printf "%s\n" "${desired_s[@]}")
  )
  [[ "$debug" ]] && for v in desired desired_s installed installed_s remain; do
    echo "$v ($(eval echo "\${#$v[*]}")) $(eval echo "\${$v[*]}")"
  done
  echo "${remain[@]}"
}
readconf () {
  FILE=$1
  while read -r line; do
    # skip comments
     [[ $line = \#* ]] && continue
     # skip empty lines
        -z "${line}"  && continue
       eval ${line}
     printf '%s\n' "$line"
  done < "$FILE"
}

# return 1 if global command line program installed, else 0
# example
# echo "node: $(program_is_installed node)"
function program_is_installed {
  # set to 1 initially
  local return_=1
  # set to 0 if not found
  type $1 >/dev/null 2>&1 || { local return_=0; }
  # return value
  echo "$return_"
}

# return 1 if local npm package is installed at ./node_modules, else 0
# example
# echo "gruntacular : $(npm_package_is_installed gruntacular)"
function npm_package_is_installed {
  # set to 1 initially
  local return_=1
  # set to 0 if not found
  ls node_modules | grep -w $1 >/dev/null 2>&1 || { local return_=0; }
  # return value
  echo "$return_"
}

# display a message in red with a cross by it
# example
# echo echo_fail "No"
function echo_fail {
  # echo first argument in red
  printf "\e[31m✘ ${1}"
  # reset colours back to normal
  tput sgr0                               # Reset attributes.
}

# display a message in green with a tick by it
# example
# echo echo_fail "Yes"
function echo_pass {
  # echo first argument in green
  printf "\e[32m✔ ${1}"
  # reset colours back to normal
tput sgr0                               # Reset attributes.
}

# echo pass or fail
# example
# echo echo_if 1 "Passed"
# echo echo_if 0 "Failed"
function echo_if {
  if [ $1 == 1 ]; then
    echo_pass $2
  else
    echo_fail $2
  fi
}
