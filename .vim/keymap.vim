"https://github.com/code2k/dot_vim/blob/master/keymap.vim
"http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)
"mapea leader a coma
let mapleader=","

" Opening files located in the same directory as the current file {{{
cmap %% <C-R>=expand('%:h').'/'<cr>
"open in the same window replace existent
map <leader>ew :e %%
"split horizontal
map <leader>es :sp %%
"split vertical
map <leader>ev :vsp %%
"open in a new tab
map <leader>et :tabe %%
" }}}

" Create window splits easier. The default {{{
" way is Ctrl-w,v and Ctrl-w,s. I remap
" this to vv and ss
nmap <silent>ss <C-w>s
nmap <silent>vv <C-w>v
" }}}

" Adjust viewports {{{
"Same size
nmap <Leader>= <C-w>=
"Maximize Up to Down
nmap <Leader>_ <C-w>_
"Maximize Left to Right
nmap <Leader><Bar> <C-w><Bar>
"}}}

" Window navigation {{{
map <C-w> <C-w>w " cycle between the open windows
map <C-Left> <C-w>h " focus the window to the left
map <C-Down> <C-w>j " focus the window to the down
map <C-Up> <C-w>k " focus the window to the up
map <C-Right> <C-w>l " focus the window to the right
"}}}

" Resize window {{{
"http://vim.usrsb.in/117060445
"can resize your windows using Alt plus an arrow key.
nmap <C-A-Left>  : vertical res +1<cr>
nmap <C-A-Right> : vertical res -1<cr>
nmap <C-A-Down>  : res +1<cr>
nmap <C-A-Up>    : res -1<cr>
" }}}

" Easy Vim Mapping like others editors {{{
"easy help current word under cursor
map <F1> <ESC>:exec "help ".expand("<cword>")<CR>

"Shift-Home, Shift-End
nnoremap <silent><S-Home> <Esc>v^
nnoremap <silent><S-End> <Esc>v$
inoremap <silent><S-Home> <Esc>v^
inoremap <silent><S-End> <Right><Esc>v$

"Ctrl-Shift-Home, Ctrl-Shift-End
nnoremap <silent><C-S-Home> <Esc>v^gg
nnoremap <silent><C-S-End> <Esc>v$G$
inoremap <silent><C-S-Home> <Esc>v^gg
inoremap <silent><C-S-End> <Right><Esc>v$G$

"Ctrl-Shift-Left, Ctrl-Shift-Right
nnoremap <silent><C-S-Left> <Esc>vb
nnoremap <silent><C-S-Right> <Esc>vw
inoremap <silent><C-S-Left> <Esc>vb
inoremap <silent><C-S-Right> <Right><Esc>vw

"Shift-Up, Shift-Down
nnoremap <silent><S-Up> <Esc>vk
nnoremap <silent><S-Down> <Esc>vj
vnoremap <silent><S-Up> k
vnoremap <silent><S-Down> j
inoremap <silent><S-Up> <Esc>vk
inoremap <silent><S-Down> <Right><Esc>vj

"Change to Normal mode
vnoremap <silent><Up> <Esc>k
vnoremap <silent><Down> <Esc>j

"Sift-left, Shift-Right
nnoremap <silent><S-Left> vh
nnoremap <silent><S-Right> vl
vnoremap <silent><S-Left> h
vnoremap <silent><S-Right> l
inoremap <silent><S-Left> <Esc>vh
inoremap <silent><S-Right> <Right><Esc>vl

"in Insert mode Left goto last initial Visual selection and Change to Insert mode
vnoremap <silent><Left> <Esc>`<i
"in Insert mode Right goto last final Visual selection and Change to Insert mode
vnoremap <silent><Right> <Esc>`>a

" Backspace in Visual mode deletes selection
vnoremap <silent><BS> di
" Backspace in Normal mode goto back word
nnoremap <silent><BS> b
" Ctrl-Backspace erase a word
nnoremap <silent><C-BS> diw
"Tab in Normal mode goto next word
nnoremap <silent><Tab> w
" Del switch to Insert mode
nnoremap <silent><C-Del> <Del>diW
" Del switch to Insert mode
vnoremap <silent><Del> <Del>i

" to the clipboard with <leader>x <leader>y and <leader>p
"CUT
nmap <leader>x "+x
vmap <leader>x "+x
"YANK
"Y copy current line
vmap <leader>y "+y
"PASTE
map <leader>p "+gp
"noremap <leader>P "+gP

"disable paste when MiddleMouse press
"http://vim.wikia.com/wiki/Mouse_wheel_for_scroll_only_-_disable_middle_button_paste
nnoremap <MiddleMouse> <LeftMouse>

"CTRL-S is Save file
nmap <C-s> :update<cr>
if has('gui_running')
  "CTRL-O is Open file
  nmap <C-o> :browse confirm tabnew<cr>
  "CTRL-H is Replace
  nmap <C-h> :promptrepl <c-r><c-w><cr>
else
  "CTRL-O is Open file
  nmap <C-o> :tabnew %%
  "CTRL-H is Replace
  nmap <C-h> :%s/<C-r><C-w>/
endif
"CTRL-F is Find command
nmap <C-F> /\v<C-r><C-w>
"CTRL-U is Change minus to MAYUS
vmap <C-u> U
"CTRL-L is Change MAYUS to minus
vmap <C-l> u

"CTRL-ENTER is insert line after
nmap <c-cr> o<esc>
"CTRL-ENTER is insert line before
nmap <c-s-cr> O<esc>

"Spelling Check
nmap <C-f7> :set spell!<cr>
"nmap <f7> ]s
"nmap <s-f7> [s
autocmd BufReadPre * if &diff|nmap <f7> ]c<cr>|nmap <s-f7> [c|else|nmap <f7> ]s|nmap <s-f7> [s|endif

"insert autoclose for {
imap {<CR> {<CR>}<Esc>O
"insert autoclose for %
inoremap % %%<Left>
"}}}

" Tab completion {{{
"for file completion
imap <C-f> <C-x><C-f>
"for line completion
imap <C-l> <C-x><C-l>
"for thesaurus file
imap <C-t> <C-x><C-t>
"for tag completion
"imap <C-]> <C-x><C-]>
"current file all words
imap <C-u> <C-x><C-u>
""macro completion
"imap <C-d> <C-x><C-d>
""for words in current file
"imap <C-i> <C-x><C-i>
"}}}

"Open file manager {{{
"http://vim.wikia.com/wiki/Open_Windows_Explorer_showing_directory_of_current_buffer
nmap <leader>e :!start explorer "%:p:h"<CR>
"}}}

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

" General {{{
"Go to last edit location with ,.
nmap <leader>. '.

" Type <leader>hl to toggle highlighting on/off, and show current value.
map <leader>h :set hlsearch! hlsearch?<CR>

" Make shift-insert work like in Xterm
"map <S-Insert> <MiddleMouse>
"map! <S-Insert> <MiddleMouse>

"list
nmap <leader>l :set list!<CR>

" Edit the vimrc file
nmap <silent><leader>ed :tabnew $MYVIMRC<CR>
nmap <silent><leader>ld :source $MYVIMRC<CR>

" Quickly get out of insert mode without your fingers having to leave the
" home row (either use 'jj' or 'jk')
imap jj <Esc>

" Quick alignment of text
nmap <leader>al :left<CR>
nmap <leader>ar :right<CR>
nmap <leader>ac :center<CR>

" Sudo to write
cmap w!! w !sudo tee % >/dev/null

" Folding
nmap <Space> za
vmap <Space> za

" Reselect text that was just pasted with ,p
nmap <leader>v V`]

"Omnicompletion
imap <c-space> <c-x><c-o>

" }}}
