# Messages
# warn if we are missing global systemwide vars
#       #+NAME: tangle-bashrc-check-friendly-vars

if [[ -z ${MYDATADIR} ]];then
  echo 'MYDATADIR is not defined. contact your system administrator'
fi
if [[ -z ${MYEXPORTEDDATA} ]];then
  echo 'MYEXPORTEDDATA is not defined. contact your system administrator'
fi
