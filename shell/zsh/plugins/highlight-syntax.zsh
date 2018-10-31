# syntax-highlight

# works if you installed https://www.archlinux.org/packages/?name=zsh-syntax-highlighting
# via pacman or fresh


if [ -r ~/.fresh/build/vendor/zsh-syntax-highlighting.zsh ]; then
    source ~/.fresh/build/vendor/zsh-syntax-highlighting.zsh
fi

if [ -r /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
