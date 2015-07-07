#
# # Use the system config if any
# if [ -f /etc/bashrc ]; then
#         . /etc/bashrc   # --> Read /etc/bashrc, if present.
# fi
#
# # Use bash completion, if installed
# if [ -f /etc/bash_completion ]; then
#     . /etc/bash_completion
# fi
#

for custom in $BASH_IT/custom/plugins/*.bash; do
	. $custom
done
