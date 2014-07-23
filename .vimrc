syntax on
syntax enable
filetype on
filetype plugin on
filetype plugin indent on
set nocompatible
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
set listchars=tab:\|-,trail:-,eol:¬
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
nmap <silent><C-tab> :tabnext<cr>
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
nnoremap <MiddleMouse> <LeftMouse>
"CTRL-ENTER is insert line after
nmap <c-cr> o<esc>
"CTRL-ENTER is insert line before
nmap <c-s-cr> O<esc>
"SUDO to write
cmap w!! w !sudo tee % >/dev/null
"}}}


" Vundle {{{
"https://github.com/gmarik/vundle
"rtp
set runtimepath+=~/.vim/
set runtimepath+=~/.vim/bin/
set runtimepath+=~/.vim/bundle/
set runtimepath+=~/.vim/bundle/vundle/
set nocompatible " be iMproved
"https://github.com/gmarik/vundle/issues/211
"call vundle#rc()
call vundle#rc('~/.vim/bundle/')
"" let Vundle manage Vundle
"" required!
Bundle 'gmarik/vundle'

nmap <leader>bi :BundleInstall<cr>
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
let NERDTreeShowHidden          = 0
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
let g:acp_completeOption = '.,w,b,u'
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
let g:acp_behaviorUserDefinedFunction = 'predictive#complete'
let g:acp_behaviorUserDefinedMeets = 'predictive#meetsForPredictive'
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

