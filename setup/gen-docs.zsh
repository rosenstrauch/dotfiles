#!/usr/bin/env zsh
# TODO: find a way to generate docs for everything in dotfiles repo

setopt EXTENDED_GLOB
# for file in "${ZDOTDIR:-$HOME}"/.dotfiles/setup/*.zsh; do
# shocco -t "${file:t}" ${file} > "${ZDOTDIR:-$HOME}/.dotfiles/docs/setup/${file:h:t}.html"
# done

# for file in "${ZDOTDIR:-$HOME}"/.dotfiles/setup/*.zsh; do
#   docco -L "${ZDOTDIR:-$HOME}"/.dotfiles/config/docco/languages.json ${file} -o "${ZDOTDIR:-$HOME}/.dotfiles/docs/${file:h:t}"
# done
#
#
# for file in "${ZDOTDIR:-$HOME}"/.fresh/source/sorin-ionescu/prezto/modules/**/*.md; do
#   shocco -t "${file:t}" ${file} > "${ZDOTDIR:-$HOME}/.dotfiles/docs/prezto/${file:h:t}.html"
# done
#
#
# for file in "${ZDOTDIR:-$HOME}"/.dotfiles/zsh/*; do
#   shocco -t "${file:t}" ${file} > "${ZDOTDIR:-$HOME}/.dotfiles/docs/zsh/${file:t}.html"
# done
#
# find any readme files from fresh sources and convert to HTML

for file in "${ZDOTDIR:-$HOME}"/.fresh/source/**/**/*(#ia2)readme.*; do
  mkdir -p ${ZDOTDIR:-$HOME}/.dotfiles/docs/${file:h:t}
  pandoc ${file} -f markdown -t html -s -o "${ZDOTDIR:-$HOME}/.dotfiles/docs/${file:h:t}/${file:h:t}.html"
done

# System Hardware
inxi -Fxz > "${ZDOTDIR:-$HOME}/.dotfiles/docs/system.inxi"


# Sources and their readmes

# commands and cheatsheets

# configurations and settings

# gems and npms
