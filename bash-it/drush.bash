# -*- mode: shell-script; mode: flyspell-prog; ispell-local-dictionary: "american" -*-
#
# Example bash aliases to improve your drush experience with bash.
# Copy this file to your home directory, rename and customize it to
# suit, and source it from your ~/.bash_profile file.
#
# Example - rename this to .drush_bashrc, and in your
# ~/.bash_profile add:
#
#
# if [ -f ~/.drush_bashrc ] ; then
# . ~/.drush_bashrc
# fi


# Provides several common shell commands to work better with drush:
#
# ddd @dev - print the path to the root directory of @dev
# cdd @dev - change the current working directory to @dev
# lsd @dev - ls root folder of @dev
# lsd %files - ls "files" directory of current site
# lsd @dev:%devel - ls devel module directory in @dev
# @dev st - drush @dev core-status
# dssh @live - ssh to the remote server @live points at
# gitd @live pull - run `git pull` on the drupal root of @live
# Find the drush executable and test it.
d=$(which drush)
# If no program is found try an alias.
if [ -z "$d" ]; then
  d=$(alias drush | cut -f 2 -d '=' | sed "s/'//g")
fi
# Test that drush is an executable.
[ -x "$d" ] || exit 0

# If the file found is a symlink, resolve to the actual file.
if [ -h "$d" ] ; then
  # Change `readlink` to `readlink -f` if your drush is a symlink to a symlink. -f is unavailable on OSX's readlink.
  d=$(readlink $d)
fi

# Get the directory that drush is stored in.
d="${d%/*}"
# If we have found drush.complete.sh, then source it.
if [ -f "$d/drush.complete.sh" ] ; then
  . "$d/drush.complete.sh"
fi

# We extend the cd command to allow convenient
# shorthand notations, such as:
# cd @site1
# cd %modules
# cd %devel
# cd @site2:%files
# You must use 'cddl' instead of 'cd' if you are not using
# the optional 'cd' alias from above.
# This is the "local-only" version of the function;
# see the cdd function, below, for an expanded implementation
# that will ssh to the remote server when a remote site
# specification is used.
function cddl() {
  s="$1"
  if [ -z "$s" ]
  then
    builtin cd
  elif [ "${s:0:1}" == "@" ] || [ "${s:0:1}" == "%" ]
  then
    d="$(drush drupal-directory $1 --local 2>/dev/null)"
    if [ $? == 0 ]
    then
      echo "cd $d";
      builtin cd "$d";
    else
      echo "Cannot cd to remote site $s"
    fi
  else
    builtin cd "$s";
  fi
}

# Works just like the `cd` shell alias above, with one additional
# feature: `cdd @remote-site` works like `ssh @remote-site`,
# whereas cd above will fail unless the site alias is local. If
# you prefer the `ssh` behavior, you can rename this shell alias
# to `cd`.
function cdd() {
  s="$1"
  if [ -z "$s" ]
  then
    builtin cd
  elif [ "${s:0:1}" == "@" ] || [ "${s:0:1}" == "%" ]
  then
    d="$(drush drupal-directory $s 2>/dev/null)"
    rh="$(drush sa ${s%%:*} --fields=remote-host --format=list)"
    if [ -z "$rh" ]
    then
      echo "cd $d"
      builtin cd "$d"
    else
      if [ -n "$d" ]
      then
        c="cd \"$d\" \; bash"
        drush -s ${s%%:*} ssh --tty
        drush ${s%%:*} ssh --tty
      else
        drush ssh ${s%%:*}
      fi
    fi
  else
    builtin cd "$s"
  fi
}

# Allow `git @site gitcommand` as a shortcut for `cd @site; git gitcommand`.
# Also works on remote sites, though.
function gitd() {
  s="$1"
  if [ -n "$s" ] && [ ${s:0:1} == "@" ] || [ ${s:0:1} == "%" ]
  then
    d="$(drush drupal-directory $s 2>/dev/null)"
    rh="$(drush sa ${s%%:*} --fields=remote-host --format=list)"
    if [ -n "$rh" ]
    then
      drush ${s%%:*} ssh "cd '$d' ; git ${@:2}"
    else
      echo cd "$d" \; git "${@:2}"
      (
        cd "$d"
        "git" "${@:2}"
      )
    fi
  else
    "git" "$@"
  fi
}

# Get a directory listing on @site or @site:%files, etc, for local or remote sites.
function lsd() {
  p=()
  r=
  for a in "$@" ; do
    if [ ${a:0:1} == "@" ] || [ ${a:0:1} == "%" ]
    then
      p[${#p[@]}]="$(drush drupal-directory $a 2>/dev/null)"
      if [ ${a:0:1} == "@" ]
      then
        rh="$(drush sa ${a%:*} --fields=remote-host --format=list)"
        if [ -n "$rh" ]
        then
          r=${a%:*}
        fi
      fi
    elif [ -n "$a" ]
    then
      p[${#p[@]}]="$a"
    fi
  done
  if [ -n "$r" ]
  then
    drush $r ssh 'ls "${p[@]}"'
  else
    "ls" "${p[@]}"
  fi
}

# Copy files from or to @site or @site:%files, etc; local sites only.
function cpd() {
  p=()
  for a in "$@" ; do
    if [ ${a:0:1} == "@" ] || [ ${a:0:1} == "%" ]
    then
      p[${#p[@]}]="$(drush drupal-directory $a --local 2>/dev/null)"
    elif [ -n "$a" ]
    then
      p[${#p[@]}]="$a"
    fi
  done
  "cp" "${p[@]}"
}

# This alias allows `dssh @site` to work like `drush @site ssh`.
# Ssh commands, such as `dssh @site ls /tmp`, are also supported.
function dssh() {
  d="$1"
  if [ ${d:0:1} == "@" ]
  then
    drush "$d" ssh "${@:2}"
  else
    "ssh" "$@"
  fi
}

# Drush checks the current PHP version to ensure compatibility, and fails with
# an error if less than the supported minimum (currently 5.3.3). If you would
# like to try to run Drush on a lower version of PHP, you can un-comment the
# line below to skip this check. Note, however, that this is un-supported.

# DRUSH_NO_MIN_PHP=TRUE
