#!/usr/bin/env zsh
for f in about author example group param version
do
  eval "$f() { :; }"
done
unset f
cite () {
	about 'creates one or more meta keywords for use in your functions'
	param 'one or more keywords'
	example '$ cite url username'
	example '$ url http://somewhere.com'
	example '$ username alice'
	group 'composure'
	if [ -z "$1" ]
	then
		printf '%s\n' 'missing parameter(s)'
		reference cite
		return
	fi
	typeset keyword
	for keyword in "$@"
	do
		eval "$keyword() { :; }"
	done
}
orgpub ()
{
  author 'banjee'
  about 'run org pubkject export'
  param 'path to el file'
  example 'orgpub  ~/.dotfiles/publish-dotfiles.el'
  group 'publish'
  emacs --batch --no-init-file --load $1
}

main() {
  
  echo "publishing dotfiles"
  orgpub ~/.dotfiles/publish-dotfiles.el
  exit 0
}

main $*
