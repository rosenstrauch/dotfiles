# load fasd in bash


#       as an executable but cache it [fn:3]
# try using the bashit plugin instead
#       #+NAME: load-fasd-in-bash

# fasd_cache="$HOME/.fasd-init-bash"
# if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
#   fasd --init posix-alias posix-hook bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
# fi
# source "$fasd_cache"
# unset fasd_cache
