"TODO: First install: pydoc: If you add .PY to your PATHEXT environment variable,
"you don't need the batch script. Just add C:\Python27\Lib to PATH, and you're all set.

au filetype py nnoremap <F5> :!python %<cr>
au filetype py setlocal omnifunc=RopeCompleteFunc
au filetype py setlocal completefunc=jedi#complete
"disable preview window
au filetype py setlocal completeopt-=preview
" convert tabs to spaces before writing file
if !&readonly && &modifiable
  autocmd! BufWritePre *.py setlocal expandtab | retab!
endif
nnoremap <buffer><F8> :call Flake8()<CR>

setlocal expandtab
setlocal scrolloff=2
setlocal shiftwidth=4
setlocal smartindent
setlocal tabstop=4
