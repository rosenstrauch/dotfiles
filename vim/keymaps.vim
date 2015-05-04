
" Enable saving by `Ctrl-s`
nnoremap <C-s> :w<CR>
inoremap <C-s> <ESC>:w<CR>


" By pressing Ctrl-N twice in normal mode, Vim toggles between showing and hiding line numbers.
" http://stackoverflow.com/a/14976528
" http://vim.wikia.com/wiki/Display_line_numbers#Creating_a_Mapping_to_Toggle_Line_Numbers

nmap <C-n><C-n> :set invnumber<CR>
