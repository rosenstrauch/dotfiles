# bash directory stack history
# remember directory history in bash
# replaced custom function with external plugin again [fn:31:Bash-it/bash-it plugins/available/dirs.plugin.bash]

# function cd() {
#   if [ "$#" = "0" ]
#   then
#   pushd ${HOME} > /dev/null
#   elif [ -f "${1}" ]
#   then
#     ${EDITOR} ${1}
#   else
#   pushd "$1" > /dev/null
#   fi
# }

# function bd(){
#   if [ "$#" = "0" ]
#   then
#     popd > /dev/null
#   else
#     for i in $(seq ${1})
#     do
#       popd > /dev/null
#     done
#   fi
# }

# history -n ${HISTFILE}

# if test -r ~/.bash_dirs_${HOSTNAME}; then
# for d in `uniq ~/.bash_dirs_${HOSTNAME}` ; do
# if test $d != $HOME; then
# pushd -n "$d" > /dev/null
# fi
# done
# fi
