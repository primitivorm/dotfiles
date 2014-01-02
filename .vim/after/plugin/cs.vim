""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"(my personal macros) mapping like refactor
"http://www.vim.org/scripts/script.php?script_id=2087
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Automatic re-tab
setlocal expandtab
" convert tabs to spaces before writing file
if !&readonly && &modifiable
  autocmd! BufWritePre *.cs retab!
endif

"Autoformat
autocmd BufRead,BufNewFile *.cs nnoremap <leader>f :silent Autoformat<cr>
autocmd BufRead,BufNewFile *.cs vmap <leader>f :silent Autoformat<cr>

"syntax completion
autocmd FileType cs exe('setlocal dict+='.$HOME.'/vimfiles/syntax/csharp.vim')

"syntastic format
autocmd FileType cs setlocal errorformat=\ %#%f(%l\\\,%c):\ error\ CS%n:\ %m
"autocmd FileType cs setlocal makeprg=msbuild\ \"%\"\ /nologo\ /v:q\ /property:GenerateFullPaths=true\ $*

"Omnisharp
autocmd FileType cs setlocal omnifunc=OmniSharp#Complete

"Create a Property based on a word with CamelCase format
au BufRead,BufNewFile *.cs nnoremap <A-r>cp Ipublic string <esc>w"zywA {<cr>}<esc>Oget { return <esc>"zpbi_<esc>l~b"xywea; }<esc>oset { <esc>"xpa = value; }<esc>kkOprivate string <esc>"xpa = string.Empty;<cr>/// <summary><cr> Obtiene o establece <esc>"zpo</summary><esc>4jV7k=
au BufRead,BufNewFile *.cs vnoremap <A-r>cp Ipublic string <esc>w"zywA {<cr>}<esc>Oget { return <esc>"zpbi_<esc>l~b"xywea; }<esc>oset { <esc>"xpa = value; }<esc>kkOprivate string <esc>"xpa = string.Empty;<cr>/// <summary><cr> Obtiene o establece <esc>"zpo</summary><esc>4jV7k=
"<A-r>s by Surround + first letter
"Surround visual selection with Namespace, #Region, Class, Struct, Interface, Try, Foreach, While, Using
au BufRead,BufNewFile *.cs vnoremap <A-r>sn <esc>gv"zdOnamespace namespace_name {<cr>}<esc>"zP?namespace_name<cr>viw
au BufRead,BufNewFile *.cs vnoremap <A-r>sc <esc>gv"zdOpublic class class_name {<cr>}<esc>"zP?class_name<cr>viw
au BufRead,BufNewFile *.cs vnoremap <A-r>ss <esc>gv"zdOpublic struct struct_name {<cr>}<esc>"zP?struct_name<cr>viw
au BufRead,BufNewFile *.cs vnoremap <A-r>si <esc>gv"zdOpublic interface interface_name {<cr>}<esc>"zP?interface_name<cr>viw
au BufRead,BufNewFile *.cs vnoremap <A-r>sr <esc>gv"zdO#region region_name<cr>#endregion<esc>"zP?region_name<cr>viw
au BufRead,BufNewFile *.cs vnoremap <A-r>st <esc>gv"zdOtry {<cr>} catch (Exception) {<cr>throw;<cr>} finally {<cr><cr>}<esc>?try {<cr>"zp/Exception<cr>viw
au BufRead,BufNewFile *.cs vnoremap <A-r>sf <esc>gv"zdOforeach (var item in collection) {<cr>}<esc>"zP?collection<cr>viw
au BufRead,BufNewFile *.cs vnoremap <A-r>sw <esc>gv"zdOwhile (true) {<cr>}<esc>"zP?true<cr>viw
au BufRead,BufNewFile *.cs vnoremap <A-r>su <esc>gv"zdOusing (resource) {<cr>}<esc>"zP?resource<cr>viw

"format statement like visual studio
function! FormatStatement()
  let line = getline('.')
  let pattern = '\v([\+|\-|\*|\/|\%|\=|!|<|>]+)'
  let line = substitute(line,pattern,' \1 ','g')
  let pattern = '\v([\{|\}|;|,])'
  let line = substitute(line,pattern,' \1 ','g')
  let pattern = '\v([A-z])\s+([+-]{2})'
  let line = substitute(line,pattern,'\1\2','g')
  let pattern = '\v([+-]{2})\s+([A-z])'
  let line = substitute(line,pattern,'\1\2','g')
  let line = substitute(line, '\v\s+([;,])', '\1',  'g')
  let line = substitute(line, '\s\+', ' ',  'g')
  let line = substitute(line, '\s\+$', '',  'g')
  call setline('.', line)
  exec 'normal! V=$l'
  startinsert
  return
endfunction

au BufRead,BufNewFile *.cs nnoremap <leader>fs :call FormatStatement()<cr>
au BufRead,BufNewFile *.cs inoremap <silent>; ;<esc>:call FormatStatement()<cr>

"for each line in buffer rewrap on write
"http://vim.wikia.com/wiki/Power_of_g
"<leader>ra :call argumentrewrap#RewrapArguments()<cr>
autocmd! BufWritePre *.cs :silent g/\v^(\s|\w)+\(.+,+.+\)/call argumentrewrap#RewrapArguments()
