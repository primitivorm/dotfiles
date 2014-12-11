syntax on
syntax enable
filetype on
filetype plugin on
filetype plugin indent on
set nocp
set nobackup
set nowritebackup
set noswapfile
set autowrite
set autoread
set background=dark
set nu
set smartindent
set ai
set tabstop=4
set shiftwidth=4
set expandtab
set scrolloff=3
set foldmethod=indent
set foldnestmax=10
set foldlevel=1
set nofoldenable
set hlsearch
set ignorecase
set hlsearch
set ignorecase
set cursorline
"set listchars=tab:\|-,trail:-,eol:¬¨
set guifont=Monaco\ 10
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
set modeline
call pathogen#infect()

colorscheme proman 
"keymaps {{{
let mapleader=','
"HELP
map <F1> <ESC>:exec "help ".expand("<cword>")<CR>
map <C-Left> <C-w>h " focus the window to the left
map <C-Down> <C-w>j " focus the window to the down
map <C-Up> <C-w>k " focus the window to the up
map <C-Right> <C-w>l " focus the window to the right
nmap <silent><C-tab> :tabNext<cr>
nmap <silent><C-t> :tabnew<cr>
nmap <silent><S-tab> :tabprev<cr>
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
nmap <silent><leader>ld :source $MYVIMRC<CR>


"}}}

" Highlight all words when press <CR> {{{
let g:highlighting = 0
function! Highlighting()
  if g:highlighting == 1 && @/ =~ '^\\<'.expand('<cword>').'\\>$'
    let g:highlighting = 0
    return ":silent nohlsearch\<CR>"
  endif
  let @/ = '\<'.expand('<cword>').'\>'
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
Plugin 'primitivorm/ack.vim'
Plugin 'primitivorm/AutoComplPop'
Plugin 'primitivorm/gundo.vim'
Plugin 'primitivorm/nerdcommenter'
Plugin 'primitivorm/nerdtree'
Plugin 'primitivorm/supertab'
Plugin 'primitivorm/ultisnips'
Plugin 'tpope/vim-fugitive'
Plugin 'primitivorm/vim-predictive'
Plugin 'primitivorm/vim-proman-theme'
Plugin 'primitivorm/L9'
Plugin 'primitivorm/vim-swap-lines'

call vundle#end()
nmap <leader>pi :PluginInstall<cr>
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
"let g:acp_completeOption = '.,w,b,u,t,i'
let g:acp_completeOption = '.,w,b,u,i'
"length to trigger AutoComplPop
"let g:acp_behaviorFileLength = 3
"let g:acp_behaviorKeywordLength = 3
"let g:acp_behaviorXmlOmniLength = 3
"let g:acp_behaviorHtmlOmniLength = 3
"let g:acp_behaviorPythonOmniLength = 3
"let g:acp_behaviorCssOmniValueLength = 3
"let g:acp_behaviorRubyOmniSymbolLength = 3
"let g:acp_behaviorCssOmniPropertyLength = 3
"let g:acp_behaviorSnipmateLength=3
"let g:acp_behaviorRubyOmniMethodLength=3

"predictive
let g:predictive#dict_path=expand($HOME.'/quick_references/predictive_dict.txt')
let g:predictive#file_types = ['*', 'text', 'vim', 'python', 'cs', 'sql', 'java', 'ruby', 'html', 'xml', 'javascript']
let g:predictive#keyword_patterns = '^[a-zA-ZÒ—·ÈÌÛ˙¡…Õ”⁄]+$'
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
set runtimepath+=~/vimfiles/snippets/
let g:UltiSnipsDoHash                = 0
let g:UltiSnipsExpandTrigger         = '<tab>'
let g:UltiSnipsListSnippets          = '<c-space>'
let g:UltiSnipsJumpForwardTrigger    = '<tab>'
let g:UltiSnipsJumpBackwardTrigger   = '<s-tab>'
let g:UltiSnipsSnippetsDir           = "~/.vim/snippets/"
let g:UltiSnipsSnippetDirectories    = ["UltiSnips", "snippets"]
let g:UltiSnipsDontReverseSearchPath = "1"
"}}}

" GUndo {{{
nmap <silent><S-U> :GundoToggle<CR>
" }}}
