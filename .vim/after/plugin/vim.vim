autocmd BufNewFile,BufRead *.vim setlocal expandtab
autocmd BufNewFile,BufRead *.vim setlocal scrolloff=2
autocmd BufNewFile,BufRead *.vim setlocal shiftwidth=2
autocmd BufNewFile,BufRead *.vim setlocal tabstop=2
autocmd BufNewFile,BufRead *.vim setlocal foldmethod=marker

" Execute current line or current selection as Vim EX commands. {{{
"http://stackoverflow.com/questions/14385998/how-can-i-execute-the-current-line-as-vim-ex-commands
au filetype vim nmap <F5> :exe getline(".")<CR>
au filetype vim vmap <F5> :<C-w>exe join(getline("'<","'>"),'<Bar>')<CR>
"}}}
