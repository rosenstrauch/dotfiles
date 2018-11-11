# suggestions
# this requires a package from the repos [fn:36:https://www.archlinux.org/packages/community/any/zsh-autosuggestions/]


if [ -r ~/.fresh/build/vendor/zsh-autosuggestions.zsh ]; then
    source ~/.fresh/build/vendor/zsh-autosuggestions.zsh
fi

if [ -r /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi
