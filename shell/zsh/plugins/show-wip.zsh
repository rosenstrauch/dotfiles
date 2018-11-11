# git-show-wip


function git_show_wip {
  if [ ! $git_show_wip ]; then
    git_show_wip=1
    # Look for a proper PROJECT_ROOT path
    PROJECT_ROOT=$(pwd)
    while [[ "$PROJECT_ROOT" != "/" && ! -e "$PROJECT_ROOT/.git" ]]; do
      PROJECT_ROOT=${PROJECT_ROOT:A:h}
    done

    if [[ "$PROJECT_ROOT" == "/" ]]; then
      PROJECT_ROOT="."
    fi

    # Check for if we are in a git repo
    if [[ -d "$PROJECT_ROOT/.git" ]]; then
      REPO_ORIGIN=$(git config --get remote.origin.url)
    else
      REPO_ORIGIN=""
    fi

    if [[ "$REPO_ORIGIN" != "" ]]; then

      # show current wip only if we just entered a project
      if [[ "$CD_WORKING_DIR" != "$PROJECT_ROOT" ]]; then

        # Runs Show wip command for current git repo
        show-wip -f "$PROJECT_ROOT" && export CD_WORKING_DIR="$PROJECT_ROOT"

      fi
    elif [[ -n $CD_WORKING_DIR && -n $WORKING_DIR ]]; then
      # We've just left the repo
      unset CD_WORKING_DIR
    fi
    unset PROJECT_ROOT
    unset git_show_wip
  fi
}

if [[ ! $DISABLE_SHOWWIP_CD -eq 1 ]]; then
  # Append git_show_wip to the chpwd_functions array, so it will be called on cd
  # http://zsh.sourceforge.net/Doc/Release/Functions.html
  if ! (( $chpwd_functions[(I)git_show_wip] )); then
    chpwd_functions+=(git_show_wip)
  fi
fi
