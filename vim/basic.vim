
"" Basics
" Disable strange Vi defaults.
set nocompatible
" Turn on filetype plugins (:help filetype-plugin).
if has('autocmd')
filetype plugin indent on
endif
" Enable syntax highlighting.
if has('syntax')
syntax enable
endif
" Autoindent when starting new line, or using `o` or `O`.
set autoindent
" Allow backspace in insert mode.
set backspace=indent,eol,start
" Don't scan included files. The .tags file is more performant.
set complete-=i
" Use 'shiftwidth' when using `<Tab>` in front of a line.
" By default it's used only for shift commands (`<`, `>`).
set smarttab
" Disable octal format for number processing.
set nrformats-=octal
" Allow for mappings including `Esc`, while preserving
" zero timeout after pressing it manually.
set ttimeout
set ttimeoutlen=100
" Enable highlighted case-insensitive incremential search.
set incsearch
" Indent using two spaces.
set tabstop=2
set shiftwidth=2
set expandtab
" Use `Ctrl-L` to clear the highlighting of :set hlsearch.
nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
" Always show window statuses, even if there's only one.
set laststatus=2
" Show the line and column number of the cursor position.
set ruler
" Show the size of block one selected in visual mode.
set showcmd
" Autocomplete commands using nice menu in place of window status.
" Enable `Ctrl-N` and `Ctrl-P` to scroll through matches.
set wildmenu
" When 'wrap' is on, display last line even if it doesn't fit.
set display+=lastline
" Force utf-8 encoding in GVim
if &encoding ==# 'latin1' && has('gui_running')
set encoding=utf-8
endif
" Set default whitespace characters when using `:set list`
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
" Delete comment character when joining commented lines
if v:version > 703 || v:version == 703 && has("patch541")
set formatoptions+=j
endif
" Search upwards for tags file instead only locally
if has('path_extra')
setglobal tags-=./tags tags^=./tags;
endif
" Reload unchanged files automatically.
set autoread
" Support all kind of EOLs by default.
set fileformats+=mac
" Increase history size to 1000 items.
set history=1000
" Allow for up to 50 opened tabs on Vim start.
set tabpagemax=50
" Always save upper case variables to viminfo file.
set viminfo^=!
" Enable backup and undo files by default.
let s:dir = has('win32') ? '$APPDATA/Vim' : isdirectory($HOME.'/Library') ? '~/Library/Vim' : empty($XDG_DATA_HOME) ? '~/.local/share/vim' : '$XDG_DATA_HOME/vim'
let &backupdir = expand(s:dir) . '/backup//'
let &undodir = expand(s:dir) . '/undo//'
set undofile
" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^linux'
set t_Co=16
endif
" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
runtime! macros/matchit.vim
endif
" `Ctrl-U` in insert mode deletes a lot. Use `Ctrl-G` u to first break undo,
" so that you can undo `Ctrl-U` without undoing what you typed before it.
inoremap <C-U> <C-G>u<C-U>
" Avoid problems with fish shell
" ([issue](https://github.com/tpope/vim-sensible/issues/50)).
if &shell =~# 'fish$'
set shell=/bin/bash
endif

filetype plugin on
