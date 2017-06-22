syntax on
syntax enable
filetype on
filetype plugin on
filetype plugin indent on
set ai
set autoread
set autowrite
set background=dark
"set background=light
set cursorline
set expandtab
set foldlevel=1
set foldmethod=indent
set foldnestmax=10
set hlsearch
set ignorecase
set incsearch
set nobackup
set nocp
set nofoldenable
set noswapfile
set nowritebackup
set nowrap
set nu
set rnu
set scrolloff=3
set shiftwidth=4
set smartcase
set smartindent
set smarttab
set tabstop=4
"set listchars=tab:\|-,trail:-,eol:¨
set listchars=tab:ª\-,trail:∑,eol:¨
set guioptions-=T
set mouse=a
" first full match
set visualbell " don't beep
set noerrorbells " don't beep
"disable blink
autocmd GUIEnter * set visualbell t_vb=
set vb t_vb= " Turn off visual bell, error flash
set noshowmode "show current mode
set showcmd " show (partial) command in the last line of the screen
" this also shows visual selection info
" set nomodeline " disable mode lines (security measure)
set wildmenu " make tab completion for files/buffers act like bash
set wildmode=list:longest,full " show a list when pressing tab and complete
set modeline
set cmdheight=2 " use a status bar that is 2 rows high
" allow backspacing over everything in insert mode
set backspace=indent,eol,start
set backspace=2
"set backspace=
set t_Co=256
call pathogen#infect()

"colo default
"colo xoria256
"colo proman
colo onedark
"colorscheme zenburn 

"if exists('+colorcolumn')
  "set colorcolumn=79
  ""let &colorcolumn="79,".join(range(100,999),",")
"endif

" Highlight all words when press <CR> {{{
let g:highlighting = 0
function! Highlighting()
  if g:highlighting == 1 && @/ =~ '^' . expand('<cword>') . '$'
    let g:highlighting = 0
    return ":silent nohlsearch\<CR>"
  endif
  let @/ = expand('<cword>')
  let g:highlighting = 1
  return ":silent set hlsearch\<CR>"
endfunction
"TODO: Uncoment this
nmap <silent> <expr> <CR> Highlighting()
"nmap <silent> <expr> <2-LeftMouse> Highlighting()
" }}}

" Vundle {{{
"https://github.com/gmarik/vundle
"rtp
set runtimepath+=~/.vim/
set runtimepath+=~/.vim/bin/
set runtimepath+=~/.vim/bundle/
set runtimepath+=~/.vim/bundle/vundle/
set runtimepath+=~/.vim/bundle/Vundle.vim/
set nocompatible " be iMproved
"https://github.com/gmarik/vundle/issues/211
call vundle#begin('~/.vim/bundle/')
"" let Vundle manage Vundle
"" required!
Plugin 'primitivorm/Align'
Plugin 'L9'
Plugin 'SirVer/ultisnips'
Plugin 'Valloric/YouCompleteMe'
Plugin 'ervandew/supertab'
Plugin 'godlygeek/tabular'
Plugin 'hexHighlight.vim'
Plugin 'honza/vim-snippets'
Plugin 'majutsushi/tagbar'
Plugin 'mileszs/ack.vim'
Plugin 'primitivorm/vim-autocomplpop'
Plugin 'primitivorm/QuickBuf'
Plugin 'primitivorm/vim-latino'
Plugin 'primitivorm/vim-proman-theme'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'shawncplus/Vim-toCterm'
Plugin 'sjl/gundo.vim'
Plugin 'skammer/vim-swaplines'
Plugin 'tpope/vim-fugitive'
Plugin 'Zenburn'
Plugin '29decibel/codeschool-vim-theme'
Plugin 'Shougo/vimproc.vim'
Plugin 'idanarye/vim-vebugger'
Plugin 'xoria256.vim'
Plugin 'Chiel92/vim-autoformat'
Plugin 'powerline/powerline'
Plugin 'tpope/vim-pathogen'
Plugin 'joshdick/onedark.vim'

" Editor layout {{{
"set lazyredraw " don't update the display while executing macros
" tell VIM to always put a status line in, even
set laststatus=2
if has("statusline")
  "for powerline
  set rtp+=/usr/local/lib/python2.7/dist-packages/powerline/bindings/vim/
  set guifont=PragmataPro\ 12
  "set guifont=Fira\ Mono\ for\ Powerline\ 11
  "set guifont=Fira\ Mono\ 11
  "set guifont=Input\ 10
  "set guifont=DejaVu\ Sans\ Mono\ 10
  "set guifont=Monospace\ 10
  "set guifont=Consola\ Mono\ 10
  "set guifont=Ubuntu\ Mono\ 11
  "set runtimepath+=~/vimfiles/cream/
  "source $HOME/.vim/cream/genutils.vim
  "source $HOME/.vim/cream/cream-lib.vim
  "source $HOME/.vim/cream/cream-lib-os.vim
  "source $HOME/.vim/cream/cream-statusline.vim

  "" for cream statusline
  "hi! User1  gui=NONE guifg=#999999 guibg=#073642 gui=bold
  "hi! User2  gui=NONE guifg=#93a1a1 guibg=#073642 gui=NONE
  "hi! User3  gui=NONE guifg=#bcc9db guibg=#073642 gui=bold
  "hi! User4  gui=NONE guifg=#d7d7af guibg=#073642 gui=bold
endif
"}}}

"Plugin 'primitivorm/vim-predictive'
call vundle#end()
"}}}

"NERDTree {{{
if !&diff
  au vimenter * NERDTree
  au vimenter * if !argc() | NERDTree | endif
endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
"custom settings
let g:NERDTreeBookmarksFile     = expand($HOME.'/_NERDTreeBookmarks')
let g:NERDTreeShowBookmarks     = 1
let g:NERDTreeWinSize           = 25
let g:NERDTreeChristmasTree     = 1
let g:NERDTreeCaseSensitiveSort = 0
let g:NERDTreeQuitOnOpen        = 1
let g:NERDTreeMouseMode         = 2
let NERDTreeShowHidden          = 1
"the working directory is always the one where the active buffer is located.
set autochdir
"I make sure the working directory is set correctly.
let g:NERDTreeChDirMode=2
"ignore some file types
let g:NERDTreeIgnore=[
    \'\.pyc$', '\.pyo$', '\.py\$class$', '\.obj$',
    \'\.o$', '\.so$', '\.egg$', '^\.git$', '^\.svn$',
    \'\.FxCop$','\.scc$','\.vssscc$','\.ini$', '\.pol$',
    \'\.user$', '\.cd$', '\.Cache$', '\.mdf$', '\.ldf$',
    \'\.tmp$', '^NTUSER.DAT*', '\.zip$', '\.pdb$', '\.dll$',
    \'tags', '\.suo$','\.vspscc$']

map <F2> :NERDTreeToggle<CR>
"}}}

" CtrlP {{{
"https://github.com/kien/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_cache_dir = $HOME.'/ctrlp_cache'
let g:ctrlp_max_height = 15
let g:ctrlp_clear_cache_on_exit = 1
let g:ctrlp_follow_symlinks = 1
let g:ctrlp_match_window_bottom = 0
let g:ctrlp_prompt_mappings='<S-F4>'
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe
let g:ctrlp_custom_ignore = {
    \ 'dir': '\.git$\|\.hg$\|\.svn$',
    \ 'file': '\.exe$\|\.so$\|\.dll$',
    \ 'link': 'some_bad_symbolic_links',
    \ }
"http://vim.wikia.com/wiki/Fix_errors_that_relate_to_reading_or_creating_files_in_the_temp_or_tmp_environment_on_an_MS_Windows_PC
if has('win32') || has('win64')
  let g:ctrlp_user_command = 'dir %s /-n /b /s /a-d' " Windows
else
  let g:ctrlp_user_command = 'find %s -type f' " MacOSX/Linux
endif

nmap <C-p> :CtrlP<CR>
imap <C-p> <esc>:CtrlP<CR>
nmap <C-b> :CtrlPBuffer<CR>
imap <C-b> <esc>:CtrlPBuffer<CR>
nnoremap <leader>t :CtrlPTag<cr>

"}}}

" AutoComplPop {{{
" Remove dictionary lookup from the Vim keyword completion.  It did always
" complete the first match for me.  If you edit files with tags you might
" want to add those.
let g:acp_completeOption = '.,w,b,u,i'

"predictive
let g:predictive#dict_path=expand($HOME.'/quick_references/predictive_dict.txt')
let g:predictive#file_types = ['*', 'text', 'vim', 'python', 'cs', 'sql', 'java', 'ruby', 'html', 'xml', 'javascript']
let g:predictive#keyword_patterns = '^[a-zA-ZÒ—·ÈÌÛ˙¡…Õ”⁄]+$'
let g:predictive#disable=0
"}}}

" SuperTab {{{
let g:SuperTabDefaultCompletionType='context'
let g:SuperTabContextDefaultCompletionType='<c-x><c-o>'
let g:SuperTabDefaultCompletionTypeDiscovery=["&completefunc:<c-x><c-o>","&omnifunc:<c-x><c-u>", "&omnifunc:<c-x><c-k>"]
let g:SuperTabClosePreviewOnPopupClose=1
let g:SuperTabNoCompleteAfter=['^', ',', '\s']
"let g:SuperTabLongestHighlight=1
let g:SuperTabLongestHighlight=0
"defaults
let g:SuperTabMappingForward='<tab>'
let g:SuperTabMappingBackward='<s-tab>'
"let g:SuperTabLongestEnhanced=1
let g:SuperTabLongestEnhanced=0
"}}}

" Ultisnip {{{
"https://github.com/vim-scripts/UltiSnips
"defaults
let g:UltiSnipsExpandTrigger         = '<tab>'
let g:UltiSnipsListSnippets          = '<c-space>'
let g:UltiSnipsJumpForwardTrigger    = '<tab>'
let g:UltiSnipsJumpBackwardTrigger   = '<s-tab>'
"}}}

" GUndo {{{
nmap <silent><S-U> :GundoToggle<CR>
" }}}

" Tagbar {{{
" http://www.vim.org/scripts/script.php?script_id=3465
" https://github.com/majutsushi/tagbar
"file to find tags
set tags=tags,./tags
"to specify one or more file extensions, which Vim will attempt to use when looking up a filename with the gf
"set suffixes+=.cs,.py,.rb,.js
"toggle fold = o/za
"open fold=+
"close fold=-
"openall=*
let g:tagbar_width     = 25  "default 40
let g:tagbar_compact   = 1   "default 0
let g:tagbar_foldlevel   = 2  "default 99
let g:tagbar_ctags_bin   = '/usr/bin/ctags'
let g:tagbar_autofocus   = 1   "default 0
let g:tagbar_expand    = 0
let g:tagbar_iconchars  = ['+', '-']
let g:tagbar_autoclose   = 0
let g:tagbar_singleclick = 1
"let g:tagbar_map_closeallfolds = ['_', 'zM',]

map <F3> :TagbarToggle<CR>
nmap <C-]> :tabnew %<CR>g<C-]>
vmap <C-]> <Esc>:tabnew %<CR>gvg<C-]>
""}}}

" Syntastic {{{
" https://github.com/scrooloose/syntastic
let g:syntastic_enable_balloons = 1
let g:syntastic_auto_loc_list=2
let g:syntastic_auto_jump=1
let g:syntastic_enable_signs=1
let g:syntastic_mode_map = { 'mode': 'active',
    \ 'active_filetypes': ['ruby', 'php', 'cs', 'python', 'lisp', 'json', 'js', 'html', 'xhtml', 'xml'],
    \ 'passive_filetypes': ['puppet'] }
let g:syntastic_enable_highlighting = 1
let g:syntastic_error_symbol='E'
let g:syntastic_style_error_symbol='S'
let g:syntastic_warning_symbol='W'
let g:syntastic_style_warning_symbol='S'
let g:syntastic_always_populate_loc_list=1
"configuring for cs files
"require mono mcs parser
"configuring for python files
let g:syntastic_python_checkers=['pylint']
if !&diff
  let g:syntastic_check_on_open=1
endif
"quickfix
set cscopequickfix=s-,c-,d-,i-,t-,e-,g-,f-
nmap <silent> <leader>sc :SyntasticCheck<cr>
"}}}

"keymaps {{{
let mapleader=','

"center search {{{
"http://vim.wikia.com/wiki/VimTip528
nmap n nzz
nmap N Nzz
nmap * *zz
nmap # #zz
nmap g* g*zz
nmap g# g#zz
nmap { {zz
nmap } }zz
nmap G Gzz
" }}}

"vundle
nmap <leader>bi :PluginInstall<cr>
"HELP
map <F1> <ESC>:exec "help ".expand("<cword>")<CR>
map <C-Left> <C-w>h " focus the window to the left
map <C-Down> <C-w>j " focus the window to the down
map <C-Up> <C-w>k " focus the window to the up
map <C-Right> <C-w>l " focus the window to the right
nmap <silent><C-tab> :tabnext<cr>
nmap <silent><C-t> :tabnew<cr>
nmap <silent><S-tab> :tabprev<cr>
"FIND
nmap <c-f> :Ack <C-r><C-w>
"CUT
vmap <leader>x "+x
"YANK
vmap <leader>y "+y
"PASTE
nmap <leader>p "+p
"CTRL-S is Save file
nmap <C-s> :update<cr>
"list
nmap <leader>l :set list!<CR>
"Type <leader>hl to toggle highlighting on/off, and show current value.
map <leader>h :set hlsearch! hlsearch?<CR>
"Omnicompletion
imap <c-space> <c-x><c-o>
"disable paste when MiddleMouse press
"http://vim.wikia.com/wiki/Mouse_wheel_for_scroll_only_-_disable_middle_button_paste
"nnoremap <MiddleMouse> <LeftMouse>
"CTRL-ENTER is insert line after
nmap <c-cr> o<esc>
"CTRL-ENTER is insert line before
nmap <c-s-cr> O<esc>
"SUDO to write
cmap w!! w !sudo tee % >/dev/null

" Edit the vimrc file
nmap <silent><leader>ed :tabnew $MYVIMRC<CR>
nmap <silent><F5> :source $MYVIMRC<CR>

" Fix: VIM UP and Down Keys Inserting A B C D 
imap <ESC>oA <ESC>ki
imap <ESC>oB <ESC>ji
imap <ESC>oC <ESC>li
imap <ESC>oD <ESC>hi

"CTRL-ENTER is insert line after
nmap <c-cr> o<esc>
"CTRL-ENTER is insert line before
nmap <c-s-cr> O<esc>

"It is much more efficient having Q save and quit the current buffer
nnoremap <silent> Q ZZ

"delete duplicate lines
"nmap <leader>d :v/./,/./-1join <cr>
nmap <leader>d :%s/\(\n\n\)\n\+/\1/<cr>

"insert autoclose for {
imap {<CR> {<CR>}<Esc>O

" Create window splits easier. The default {{{
" way is Ctrl-w,v and Ctrl-w,s. I remap
" this to vv and ss
nmap <silent>ss <C-w>s
nmap <silent>vv <C-w>v
"}}}

"{{{
"YouCompleteMe
"https://github.com/Valloric/YouCompleteMe
let g:ycm_confirm_extra_conf=0
let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
let g:ycm_key_list_select_completion = ['<Down>']
let g:ycm_key_list_previous_completion = ['<Up>']
"let g:ycm_autoclose_preview_window_after_completion=1
let g:ycm_autoclose_preview_window_after_completion=0
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]
let g:syntastic_always_populate_loc_list = 1
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_filetype_blacklist = {
\ 'tagbar' : 1,
\ 'qf' : 1,
\ 'notes' : 1,
\ 'markdown' : 1,
\ 'unite' : 1,
\ 'text' : 1,
\ 'vimwiki' : 1,
\ 'pandoc' : 1,
\ 'infolog' : 1,
\ 'mail' : 1,
\ 'java' : 1,
\ 'help' : 1,
\ 'vim' : 1,
\ 'sql' : 1
\}
nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
"}
"}}}

" vim-autoformat {{{
" https://github.com/Chiel92/vim-autoformat
let g:formatprg_cs = "astyle"
let g:formatprg_args_cs = "--mode=cs --style=ansi -pcHs4"
let g:formatprg_args_expr_cs = '"--mode=cs --style=ansi -pcHs".&shiftwidth'
let g:formatprg_c = "astyle"
let g:formatprg_args_c = "--style=kr -C -N -m0 -M40 -w -xw -Y -c -p -H -U -xe -k3 -s4"
let g:formatprg_args_expr_c = '"--style=kr -C -N -m0 -M40 -w -xw -Y -c -p -H -U -xe -k3 -s".&shiftwidth'
let g:formatprg_cpp = "astyle"
let g:formatprg_args_cpp =  "--style=kr -C -N -m0 -M40 -w -xw -Y -c -p -H -U -xe -k3 -s4"
let g:formatprg_args_expr_cpp = '"--style=kr -C -N -m0 -M40 -w -xw -Y -c -p -H -U -xe -k3 -s".&shiftwidth'
set equalprg=astyle
autocmd BufRead,BufNewFile *.c nnoremap <leader>f :silent Autoformat<cr>
autocmd BufRead,BufNewFile *.c vmap <leader>f :silent Autoformat<cr>
"}}}

" filetype {{{
autocmd filetype java nnoremap <F5> :wa <cr> <bar> :!javac % <cr>
autocmd filetype java nnoremap <C-F5> :!java %:r <cr>
" }}}

"abbreviations {{{
cabbr ack Ack
" }}}
