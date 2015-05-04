" http://vim.wikia.com/wiki/Create_a_color_scheme_based_on_another
"these lines are suggested to be at the top of every colorscheme
hi clear
if exists("syntax_on")
  syntax reset
endif

"Load the 'base' colorscheme - the one you want to alter
runtime colors/ron.vim

"Override the name of the base colorscheme with the name of this custom one
let g:colors_name = "friendlyvim"

"Clear the colors for any items that you don't like
hi clear StatusLine
hi clear StatusLineNC

"Set up your new & improved colors
hi StatusLine guifg=black guibg=white
hi StatusLineNC guifg=LightCyan guibg=blue gui=bold
