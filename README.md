# Dotfiles Documentation # {#mainpage}

## Requirements ##

- tmux
- ack
- ctags
- git
- ruby (with rvm)
- rake
- zsh
- terminator
- python-pip

## Recomendations ##
- silversearcher-ag (https://github.com/ggreer/the_silver_searcher)
- vim-gnome

## Installation Via setup.zsh ##

> My dotfiles are managed by fresh.

## Components ##

- http://dotfiles.github.io/
- https://github.com/revans/bash-it
- https://github.com/sorin-ionescu/prezto


## Usage ##
bash-it configured for login shells

## Bash-it ##

some example bash-it commands to finde your way around

    $ bash-it show plugins
    $ bash-it show aliases
    $ bash-it help plugin ruby
    $ bash-it show alias
    $ bash-it help aliases git

### highlights

for moving around the terminal
- https://github.com/clvv/fasd

customized prompts
- [liquidprompt](https://github.com/nojhan/liquidprompt)


### bashit customizations

> https://github.com/revans/bash-it#your-custom-scripts-aliases-and-functions

- overridden mcd function



## Shell enhancements:

### Drush
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

 https://github.com/monochromegane/vagrant-global-status/releases/download/v0.0.3/vagrant-global-status_linux_amd64.tar.gz --filter="tar -ztvf" --bin
$ todo add "add https://github.com/simonwhitaker/gitignore-boilerplates +dotfiles @home
- https://github.com/erichs/composure
- https://github.com/webflo/drush_zsh_completion
- https://github.com/drush-ops/drush/blob/master/README.md#post-install
- https://github.com/skwp/dotfiles
- https://github.com/Lokaltog/powerline
gibo
fresh simonwhitaker/gitignore-boilerplates gibo --bin
http://yannesposito.com/Scratch/en/blog/Vim-as-IDE/
http://yannesposito.com/Scratch/en/blog/Learn-Vim-Progressively/
composure
shocco from repo
rvm
