# Requirements

- tmux
- ack
- ctags
- git
- ruby
- rake
- vim-gnome

# Components
http://dotfiles.github.io/
https://github.com/revans/bash-it
https://github.com/carlhuda/janus
https://github.com/robbyrussell/oh-my-zsh


# Installation using fresh

    FRESH_LOCAL_SOURCE=rosenstrauch/dotfiles bash <(curl -sL get.freshshell.com)

##install or update bashit, janus and oh-my-zsh

    cd ~/ && rake

> My dotfiles are managed by fresh.

# Usage
bash-it and janus are configured for login shells

## Janus
https://github.com/carlhuda/janus#customization

## Bash-it
some example bash-it commands to finde your way around
    $ bash-it show plugins
    $ bash-it show aliases
    $ bash-it help plugin ruby
    $ bash-it show alias
    $ bash-it help aliases git

### bashit customizations
> https://github.com/revans/bash-it#your-custom-scripts-aliases-and-functions
- overridden mcd function

## CLI Binaries/scripts:
### https://github.com/sivel/speedtest-cli
    speedtest
### Github    
usage: gh [<command>] [<number>]

The following commands are performed on the current project.

Available commands:

    -i --issues [<number>] Open issues, optionally pass a number to open a specific issue
    -p --pulls [<number>]  Open pull requests, optionally pass a number to open a specific pull request
    -w --wiki              Open wiki

## Shell enhancements:
###Drush
- [drush.ini](https://github.com/drush-ops/drush/blob/master/examples/example.drush.ini)
- [drushrc.php](https://github.com/drush-ops/drush/blob/master/examples/example.drushrc.php)

#### [Drush bashrc aliases & extensions](https://github.com/drush-ops/drush/blob/master/examples/example.bashrc)
these are added into [bash-it custom files](https://github.com/revans/bash-it#your-custom-scripts-aliases-and-functions)


### Bash
> @see http://wiki.ubuntuusers.de/Bash 
> @see https://github.com/jasoncodes/dotfiles/blob/master/config/inputrc

- By default up/down are bound to previous-history and next-history respectively. The following does the same but gives the extra functionality where if you type any text (or more accurately, if there is any text between the start of the line and the cursor), the subset of the history starting with that text is searched (like 4dos for e.g.). Note to get rid of a line just Ctrl-C
- mappings for Ctrl-left-arrow and Ctrl-right-arrow for word moving
- allow the use of the Delete/Insert keys
- ignore iTerms focus notifications https://github.com/sjl/vitality.vim/issues/2
- clear the screen with ctrl-l
- Includes system wide settings which are ignored by default if one has their own .inputrc

### liquidprompt
https://github.com/nojhan/liquidprompt
- just use it
