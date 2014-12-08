#!/usr/bin/env zsh


setopt EXTENDED_GLOB
# for file in "${ZDOTDIR:-$HOME}"/.dotfiles/setup/*.zsh; do
# shocco -t "${file:t}" ${file} > "${ZDOTDIR:-$HOME}/.dotfiles/docs/setup/${file:h:t}.html"
# done
for file in "${ZDOTDIR:-$HOME}"/.dotfiles/setup/*.zsh; do
  docco -L "${ZDOTDIR:-$HOME}"/.dotfiles/config/docco/languages.json ${file} -o "${ZDOTDIR:-$HOME}/.dotfiles/docs/${file:h:t}"
done


for file in "${ZDOTDIR:-$HOME}"/.fresh/source/sorin-ionescu/prezto/modules/**/*.md; do
  shocco -t "${file:t}" ${file} > "${ZDOTDIR:-$HOME}/.dotfiles/docs/prezto/${file:h:t}.html"
done


for file in "${ZDOTDIR:-$HOME}"/.dotfiles/zsh/*; do
  shocco -t "${file:t}" ${file} > "${ZDOTDIR:-$HOME}/.dotfiles/docs/zsh/${file:t}.html"
done


