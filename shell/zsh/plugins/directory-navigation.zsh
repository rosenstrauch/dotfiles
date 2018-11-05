# File manager key binds
# navigating up and back is as easy as =M-<left>= and =M-<up>== [fn:32:https://wiki.archlinux.org/index.php/zsh#File_manager_key_binds]

cdUndoKey() {
  popd
  zle       reset-prompt
  echo
  ls
  zle       reset-prompt
}

cdParentKey() {
  pushd ..
  zle      reset-prompt
  echo
  ls
  zle       reset-prompt
}

zle -N                 cdParentKey
zle -N                 cdUndoKey
bindkey '^[[1;5A'      cdParentKey
bindkey '^[[1;5D'      cdUndoKey
