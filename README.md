# Requirements

- tmux
- ack
- ctags
- git
- ruby
- rake
- vim-gnome
- zsh
- silversearcher-ag (https://github.com/ggreer/the_silver_searcher)
- terminator

# Components

- http://dotfiles.github.io/
- https://github.com/revans/bash-it
- https://github.com/carlhuda/janus
- https://github.com/robbyrussell/oh-my-zsh
- https://github.com/robbyrussell/oh-my-zsh/wiki/Plugins

# Installation using fresh

    FRESH_LOCAL_SOURCE=rosenstrauch/dotfiles bash <(curl -sL get.freshshell.com)

## Initial setup
there are two possibilities for initial setup. this only has to be run once after initial fresh install (not on updates) the rakefile lets you select each option, the setup script assumes you want everything

### Via Rake
    cd ~/ && rake
    or
### Via setup.zsh
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

### highlights

for moving around the terminal
- https://github.com/clvv/fasd 
- https://github.com/rupa/z
- [liquidprompt](https://github.com/nojhan/liquidprompt)
- [Speedtest](https://github.com/sivel/speedtest-cli)
- github commands
    gh [<command>] [<number>]

    The following commands are performed on the current project.

    Available commands:

        -i --issues [<number>] Open issues, optionally pass a number to open a specific issue
        -p --pulls [<number>]  Open pull requests, optionally pass a number to open a specific pull request
        -w --wiki              Open wiki

### bashit customizations

> https://github.com/revans/bash-it#your-custom-scripts-aliases-and-functions

- overridden mcd function



## Shell enhancements:
###Drush
- [drush.ini](https://github.com/drush-ops/drush/blob/master/examples/example.drush.ini)
- [drushrc.php](https://github.com/drush-ops/drush/blob/master/examples/example.drushrc.php)

#### [Drush bashrc aliases & extensions](https://github.com/drush-ops/drush/blob/master/examples/example.bashrc)
these are added into [bash-it custom files](https://github.com/revans/bash-it#your-custom-scripts-aliases-and-functions)


### Bash
@see http://wiki.ubuntuusers.de/Bash
@see https://github.com/jasoncodes/dotfiles/blob/master/config/inputrc

- By default up/down are bound to previous-history and next-history respectively. The following does the same but gives the extra functionality where if you type any text (or more accurately, if there is any text between the start of the line and the cursor), the subset of the history starting with that text is searched (like 4dos for e.g.). Note to get rid of a line just Ctrl-C
- mappings for Ctrl-left-arrow and Ctrl-right-arrow for word moving
- allow the use of the Delete/Insert keys
- ignore iTerms focus notifications https://github.com/sjl/vitality.vim/issues/2
- clear the screen with ctrl-l
- Includes system wide settings which are ignored by default if one has their own .inputrc


# TODO:

##integrate more

- https://github.com/simonwhitaker/gitignore-boilerplates

##investigate alternatives
- https://github.com/webflo/drush_zsh_completion
- https://github.com/drush-ops/drush/blob/master/README.md#post-install
- https://github.com/skwp/dotfiles
- https://github.com/sorin-ionescu/prezto
- https://github.com/Lokaltog/powerline
# gibo
fresh simonwhitaker/gitignore-boilerplates gibo --bin
