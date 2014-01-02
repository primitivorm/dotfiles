" C:/cygwin/home/Proman02/vimfiles\sessions\default.vim: Vim session script.
" Created by session.vim 1.5 on 01 Abril 2013 at 11:10:19.
" Open this file in Vim and run :source % to restore your session.

set guioptions=egmrLtT
silent! set guifont=Ubuntu_Mono_for_powerline:h10:cANSI
if exists('g:syntax_on') != 1 | syntax on | endif
if exists('g:did_load_filetypes') != 1 | filetype on | endif
if exists('g:did_load_ftplugin') != 1 | filetype plugin on | endif
if exists('g:did_indent_on') != 1 | filetype indent on | endif
if &background != 'dark'
	set background=dark
endif
if !exists('g:colors_name') || g:colors_name != 'Monokai-Refined' | colorscheme Monokai-Refined | endif
call setqflist([])
let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd c:\Inetpub\wwwroot\AUTOMARSH_SitiosEsp\Framework35\MMC.AutoMarsh.Derivacion\MMC.AutoMarsh.Derivacion.Business
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +37 C:/Inetpub/wwwroot/AUTOMARSH_SitiosEsp/Framework35/MMC.AutoMarsh.Derivacion/MMC.AutoMarsh.Derivacion.sln
badd +1 c:/Inetpub/wwwroot/AUTOMARSH_SitiosEsp/Framework35/MMC.AutoMarsh.Derivacion/MMC.AutoMarsh.Derivacion.Business/SolicitudDerivacionHDI.cs
badd +1 c:/Inetpub/wwwroot/AUTOMARSH_SitiosEsp/Framework35/MMC.AutoMarsh.Derivacion/MMC.AutoMarsh.Derivacion.Business/StyleCop.Cache
args /Inetpub/wwwroot/AUTOMARSH_SitiosEsp/Framework35/MMC.AutoMarsh.Derivacion/MMC.AutoMarsh.Derivacion.sln
set lines=53 columns=159
edit c:/Inetpub/wwwroot/AUTOMARSH_SitiosEsp/Framework35/MMC.AutoMarsh.Derivacion/MMC.AutoMarsh.Derivacion.Business/StyleCop.Cache
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd _ | wincmd |
split
1wincmd k
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 45 + 79) / 159)
exe '2resize ' . ((&lines * 1 + 26) / 53)
exe 'vert 2resize ' . ((&columns * 113 + 79) / 159)
exe '3resize ' . ((&lines * 49 + 26) / 53)
exe 'vert 3resize ' . ((&columns * 113 + 79) / 159)
argglobal
enew
" file c:/Inetpub/wwwroot/AUTOMARSH_SitiosEsp/Framework35/MMC.AutoMarsh.Derivacion/NERD_tree_1
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=2
setlocal fml=1
setlocal fdn=10
setlocal nofen
wincmd w
argglobal
enew
file c:/Inetpub/wwwroot/AUTOMARSH_SitiosEsp/Framework35/MMC.AutoMarsh.Derivacion/MMC.AutoMarsh.Derivacion.Business/-MiniBufExplorer-
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=2
setlocal fml=1
setlocal fdn=10
setlocal nofen
wincmd w
argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=2
setlocal fml=1
setlocal fdn=10
setlocal nofen
let s:l = 35 - ((34 * winheight(0) + 24) / 49)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
35
normal! 0
wincmd w
3wincmd w
exe 'vert 1resize ' . ((&columns * 45 + 79) / 159)
exe '2resize ' . ((&lines * 1 + 26) / 53)
exe 'vert 2resize ' . ((&columns * 113 + 79) / 159)
exe '3resize ' . ((&lines * 49 + 26) / 53)
exe 'vert 3resize ' . ((&columns * 113 + 79) / 159)
tabnext 1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
tabnext 1
1wincmd w
let s:bufnr = bufnr("%")
NERDTree c:\Inetpub\wwwroot\AUTOMARSH_SitiosEsp\Framework35\MMC.AutoMarsh.Derivacion
execute "bwipeout" s:bufnr
1resize 51|vert 1resize 45|2resize 1|vert 2resize 113|3resize 49|vert 3resize 113|
tabnext 1
3wincmd w

" vim: ft=vim ro nowrap smc=128
