"
" Filename: cream-lib
"
" Cream -- An easy-to-use configuration of the famous Vim text editor
" [ http://cream.sourceforge.net ] Copyright (C) 2001-2011 Steve Hall
"
" License
" GNU General Public License (GPL) {{{1
"
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 3 of the License, or
" (at your option) any later version. [
" http://www.gnu.org/licenses/gpl.html ]
"
" This program is distributed in the hope that it will be useful, but
" WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
" General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
" 02111-1307, USA.
"
" 1}}}
" General
" Path and Files {{{1

function! Cream_browse_pathfile()
" uses dialog to return a path/filename

	let mypathfile = browse(0, "Select file", getcwd(), '*.*')
	" remove filename
	let mypathfile = fnamemodify(mypathfile, ":p")
	" handle spaces
	let mypathfile = escape(mypathfile, ' \')

	return mypathfile

endfunction

function! Cream_browse_path(...)
" Return a directory (full path) via dialog (back/slashes and escaping
" not guaranteed to be correct for system call).
" o Returns "" on cancel or error.
" o Verifies return is a directory.
" o Does not verify return is writable.
" o Argument 1: optional start path
" o Argument 2: (requires arg 1) optional file filter

	" NOTE: THIS FUNCTION IS GOING TO BECOME OBSOLETE WITH
	" THE INTRODUCTION OF browsedir().

""*** DEBUG:
"let n = confirm(
"    \ "DEBUG:\n" .
"    \ "  a:0    = \"" . a:0 . "\"\n" .
"    \ "  a:1    = \"" . a:1 . "\"\n" .
"    \ "  a:2    = \"" . a:2 . "\"\n" .
"    \ "\n", "&Ok\n&Cancel", 1, "Info")
"    if n != 1
"    return
"endif
""***

	if a:0 > 0
		if a:0 == 1
			"call confirm(
			"    \ "Error: Incorrect number of arguments passed to Cream_browse_path().\n" .
			"    \ "\n" .
			"    \ "Quitting...\n" .
			"    \ "\n", "&Ok", 1, "Info")
			"return ""
			let initpath = a:1
			let filefilter = "*.*"
		else
			let initpath = a:1
			let filefilter = a:2
		endif
	else
		let initpath = getcwd()
		let filefilter = "*.*"
	endif

	" TODO: filters broken.
	"if exists("g:browsefilter")
	"    unlet g:browsefilter
	"endif
	"let g:browsefilter = "Directories\t*\*;*\*\nVim Files\t*.vim;*.vim\nAll Files\t*.*;*.*\n"
	"let g:browsefilter = "Directories\t*\\*;*\\*\nVim Files\t*.vim;*.vim\nAll Files\t*.*;*.*\n"
	"let g:browsefilter = "Directories (*.)\nVim Files (*.vim)\nAll Files (*.*)\n"
	"let g:browsefilter = "Bob Files\t*.bob\nVim Files\t*.vim\nAll Files\t*.*\n"
	"let g:browsefilter = "Directories\t*.\nVim Files\t*.vim\nAll Files\t*.*\n"


	let mypath = browse(0, Cream_str("0000004"), initpath, filefilter)
	" cancel or error, return empty
	if mypath == ""
		return ""
	endif

""*** DEBUG:
"if exists("g:cream_dev")
"    let n = confirm(
"        \ "DEBUG: in Cream_browse_path()\n" .
"        \ "  mypath    = \"" . mypath . "\"\n" .
"        \ "\n", "&Ok\n&Cancel", 1, "Info")
"        if n != 1
"        return
"    endif
"endif
""***

	" expand to full path, subtract filename
	let mypath = fnamemodify(mypath, ":p:h")

	" handle escaping and spaces
	if Cream_has("ms")
		let mypath = substitute(mypath, '/', '\', 'g')
	else
		let mypath = escape(mypath, ' \')
	endif
	" append final slash back
	let mypath = mypath . "/"
	" test and return
	if isdirectory(mypath) != 0
		return mypath
	else
		if exists("g:cream_dev")
			" failed
			call Cream_error_warning("Cream Devel: Cream_browse_path() failed to find object.")
		endif
		return ""
	endif

endfunction

function! Cream_get_dir_above(mypath)
" returns a directory one level above value or empty if doesn't exist

	" remove filename
	let mystr = fnamemodify(a:mypath, ":h")
	" remove lowest subdirectory
	let mystr = fnamemodify(mystr, ":h")
	if !Cream_has("ms")
		" handle spaces
		let mystr = escape(mystr, ' \')
	endif
	" append final slash
	let mystr = mystr . "/"
	" test and return
	if isdirectory(mystr) != 0
		return mystr
	else
		if exists("g:cream_dev")
			" failed
			call Cream_error_warning("Cream_get_dir_above() failed to get directory")
		endif
		return 0
	endif

endfunction

function! Cream_getfilelist(path)
" Return a file list based on path, including wildcards. The number of
" newlines equals the number of files. If no file matches {path}, ""
" is returned.
" Notes:
" o Separator between file names returned is a newline ("\n"). A
"   trailing newline is added if doesn't exist.
" o Wildcard "*" for filename won't return files beginning with dot.
"   (Must use ".*" to obtain.)
" o To optain directories, trail with "*/"
" o Regexp support is available, e.g., to restrict extensions
"   beginning with "b" and "r", pass "*.[^br]*".

	" get file list
	let myfiles = glob(a:path) . "\n"

	" change paths to forward slashes to avoid escaping
	if Cream_has("ms")
		return substitute(myfiles, '\\', '/', 'g')
	endif

	" (from explorer.vim)
	" Add the dot files now, making sure "." is not included!
	"let myfiles = substitute(glob(g:rmpath), "[^\n]*/./\\=\n", '' , '')

	" ensure a return at the end if not empty
	if myfiles != "" && myfiles !~ '\n$'
		let myfiles = myfiles . "\n"
	endif
	return myfiles

endfunction

function! Cream_path_addtrailingslash(path)
" Confirms {path} has a trailing slash appropriate for the system.

	let path = a:path
	if path !~ '/$' && path !~ '\$'
		if Cream_has("ms")
			let path = path . '\'
		else
			let path = path . '/'
		endif
	endif
	return path

endfunction

function! Cream_get_creamrc()
	if filereadable($CREAM . "creamrc") > 0
		return $CREAM . "creamrc"
	else
		call Cream_error_warning("Cream_get_creamrc() failed to find vimrc")
	endif
endfunction

function! Cream_cmd_on_files(path, cmd)
" do a particular command within the files in a series of paths
" * If file is not modified as a result of the operation, it is closed
"   without saving.

	"let myfiles = glob(a:path)
	"" change paths to forward slashes to avoid escaping
	"if Cream_has("ms")
	"    let myfiles = substitute(myfiles, '\\', '/', 'g')
	"endif
	"" ensure last has ending
	"if strlen(myfiles) > 0
	"    let myfiles = myfiles . "\n"
	"endif
	let myfiles = Cream_getfilelist(a:path)

	let i = 0
	let max = MvNumberOfElements(myfiles, "\n")
	while i < max
		" get file
		let myfile = MvElementAt(myfiles, "\n", i)
		" save options {{{2
		" turn off redraw
		let mylazyredraw = &lazyredraw
		set lazyredraw
		" turn off swap
		let myswapfile = &swapfile
		set noswapfile
		" turn off undo
		let myundolevels = &undolevels
		set undolevels=-1
		" ignore autocmds
		let myeventignore = &eventignore
		set eventignore=all
		" unload buffers
		let myhidden = &hidden
		set nohidden
		" 2}}}
		" progress indication
		let mycmdheight = &cmdheight
		set cmdheight=2
		echo " " . Cream_str("0000005") . ": " . i . " of " . max . " (" . ((i*100) / max) . "%)"
		let &cmdheight = mycmdheight
		" open file
		execute "silent! edit! " . myfile
		" do it!
		execute a:cmd
		if &modified == 1
			" save file
			execute "silent! write! " . myfile
		endif
		" close file (delete from buffer as is our preference ;)
		execute "silent! bwipeout!"
		" restore options {{{2
		let &lazyredraw = mylazyredraw
		let &swapfile = myswapfile
		let &undolevels = myundolevels
		let &eventignore = myeventignore
		let &hidden = myhidden
		" 2}}}
		let i = i + 1
	endwhile

endfunction

function! Cream_str_tofile(str, filename)
" Write any {str} to {filename}. Filename that does not exist will be
" created.

	" OPTION 1
	"""" save options
	"""" turn off redraw
	"""let lazyredraw = &lazyredraw
	"""set lazyredraw
	"""" turn off swap
	"""let swapfile = &swapfile
	"""set noswapfile
	"""" turn off undo
	"""let undolevels = &undolevels
	"""set undolevels=-1
	"""" ignore autocmds
	"""let eventignore = &eventignore
	"""set eventignore=all
	"""" unload buffers
	"""let hidden = &hidden
	"""set nohidden
	"""
	"""" open file
	"""execute "silent! edit! " . a:filename
	"""let bufnr = bufnr("%")
	"""" do it!
	"""let @x = a:str
	"""put x
	"""" save file
	"""execute "silent! write! " . a:filename
	"""" close file (delete from buffer as is our preference ;)
	"""execute "silent! bwipeout! " . bufnr
	"""
	"""" restore options
	"""let &lazyredraw = lazyredraw
	"""let &swapfile = swapfile
	"""let &undolevels = undolevels
	"""let &eventignore = eventignore
	"""let &hidden = hidden

	" OPTION 2
	let @x = a:str
	execute "redir > " . a:filename
	silent! echo @x
	redir END

	" verify
	if filewritable(a:filename) != 1
		call confirm(
			\ "Error in Cream_str_tofile(): filename passed:\n" .
			\ "\n" .
			\ "  a:filename  = \"" . a:filename . "\"\n" .
			\ "\n" .
			\ "not created.\n" .
			\ "\n", "&Ok\n&Cancel", 1, "Info")
		if n != 1
			return
		endif
	endif

endfunction

function! Cream_appendext(pathfile, ext)
" save a file with an appended extension (good for temp files)
" * return -1 if unable

	" append extension
	let myfilename =  a:pathfile . "." . a:ext

	" open file as new buffer, if doesn't exist
	if filereadable(myfilename) == "TRUE"
		execute "saveas " . myfilename
		return
	else
		call confirm("Can not continue, a file named \"" . myfilename . "\" already exists here.", "&" . Cream_str("0000001"), 1, "Warning")
		return -1
	endif

endfunction

function! Cream_buffers_pathfile()
" Initialize all buffers' b:cream_pathfilename var.

	" page through each buffer
	let i = 0
	while i <= bufnr('$')
		let i = i + 1
		if   Cream_buffer_isspecial(i) == 0
		\ && Cream_buffer_isnewunmod(i) == 0

			if bufexists(i)
				let bufname = Cream_path_fullsystem(fnamemodify(bufname(i), ':p'))
				call setbufvar(i, "cream_pathfilename", bufname)
			endif

		endif
	endwhile

endfunction

function! Cream_buffer_pathfile()
" Determine b:cream_pathfilename for the current buffer. (Used for
" display in statusline and window title.)
" o Set as text
" o Proper according to the platform.

	" verify
	if !exists("b:cream_nr")
		call Cream_buffer_nr()
	endif

	if !exists("b:cream_pathfilename") || b:cream_pathfilename == ""
		let b:cream_pathfilename = Cream_path_fullsystem(expand('%:p'))
		" is directory
		if     isdirectory(b:cream_pathfilename)
			let b:cream_pathfilename = ""
		" doesn't exist
		elseif bufexists(b:cream_nr) == 0
			 let b:cream_pathfilename = "[doesn't exist]"
		endif
	endif
	" keep testing if doesn't exist (in case SaveAs)
	if b:cream_pathfilename == "[doesn't exist]"
		if bufexists(b:cream_nr) != 0
			let b:cream_pathfilename = Cream_path_fullsystem(expand('%:p'))
		endif
	endif

endfunction

function! Cream_buffer_nr()
" Determine's the current buffer's number.

	if !exists("b:cream_nr")
		let b:cream_nr = bufnr("%")
	endif

endfunction

" Path and Files, User {{{1

function! Cream_load_user()
" loads cream-user if present
" Note: Cream_userdir() must be called prior to here to establish
" g:cream_user if available (currently handled in autocmds).

	" load system cream-user
	call Cream_source($CREAM . "cream-user.vim")

	" load $HOME's cream-user
	if exists("g:cream_user")
		call Cream_source(g:cream_user . "cream-user.vim")
	endif

endfunction


" Debug and Error Handling {{{1

"function! Cream_debug()
"" insert debug template
"    let mystr = ""
"    let mystr = mystr . "\"*** DEBUG:\n"
"    let mystr = mystr . "let n = confirm(\n"
"    let mystr = mystr . "\t\\ \"DEBUG:\\n\" .\n"
"    let mystr = mystr . "\t\\ \"  myvar    = \" . myvar . \"\\n\" .\n"
"    let mystr = mystr . "\t\\ \"\\n\", \"&Ok\\n&Cancel\", 1, \"Info\")\n"
"    let mystr = mystr . "if n != 1\n"
"    let mystr = mystr . "\treturn\n"
"    let mystr = mystr . "endif\n"
"    let mystr = mystr . "\"***\n"
"    let mystr = mystr . "\n"
"    let @x = mystr
"    normal "xp
"endfunction

function! Debug(...)
" Handle debug requests
" * accepts any number of quoted variables, and displays name and value

	let myarg = "DEBUG:\n\n"

	let i = 1
	while exists("a:" . i)
		execute "let myarg = myarg . \"  " . a:{i} . " = \" . " . a:{i} " . \"\n\""
		let i = i + 1
	endwhile

	if has("gui_runnning") && has("dialog_gui")
		call confirm(myarg, "&Ok", 1)
	else
		echo myarg
	endif

endfunction

function! Cream_error_notice(myerror)
	if has("dialog_gui")
		call confirm("Notice:\n" . a:myerror, "&Ok", 1, "Info")
	else
		echo "----------------------------------------------------------------------"
		echo "Notice:"
		echo a:myerror
		echo "----------------------------------------------------------------------"
	endif
endfunction

function! Cream_error_warning(myerror)
	if has("dialog_gui")
		call confirm("Warning:\n" . a:myerror, "&Ok", 1, "Error")
	else
		echo "----------------------------------------------------------------------"
		echo "Warning:"
		echo a:myerror
		echo "----------------------------------------------------------------------"
	endif
endfunction

function! Cream_error_critical(myerror)
	if has("dialog_gui")
		call confirm("Critical:\n" . a:myerror, "&Ok", 1, "Warning")
	else
		echo "----------------------------------------------------------------------"
		echo "Critical:"
		echo a:myerror
		echo "----------------------------------------------------------------------"
	endif
endfunction

" 1}}}
" Help {{{1

function! Cream_help(...)
" open a $CREAM docs-html/{file} in the default browser
" If {...} is empty, use keyboardshortcuts.html.

	let file = ""
	" try argument
	if a:0 > 0
		let file = Cream_path_fullsystem($CREAM . 'docs-html/' . a:1)
	endif

	if filereadable(file) != 1
		" if argument, warn invalid
		if a:0 > 0
			" no command found, exit
			call confirm(
				\ "Invalid document looked up, using default.\n" .
				\ "\n", "&Ok", 1, "Info")
		endif
		let file = Cream_path_fullsystem($CREAM . 'docs-html/keyboardshortcuts.html')
	endif

	return Cream_file_open_defaultapp(file)

endfunction

function! Cream_help_find(...)
" help control

	if a:0 > 0
		let myhelp = a:1
		" toggle open/closed (only close if on first page, help.txt)
		" try to find it
		let i = 1
		while i <= bufnr("$")
			" move to it
			if Cream_buffer_ishelp(i) == 1
			\&& fnamemodify(bufname(i), ":t") == "help.txt"
				" goto "help.txt"
				call MoveCursorToWindow(bufwinnr(i))
				bwipeout!
				return
			endif
			let i = i + 1
		endwhile
	else
		let mymsg =
			\ "Enter a command or word to find help on:\n" .
			\ "\n" .
			\ "Prepend i_ for Input mode commands (e.g.: i_CTRL-X)\n" .
			\ "Prepend c_ for command-line editing commands (e.g.: c_<Del>)\n" .
			\ "Prepend ' for an option name (e.g.: 'shiftwidth')\n"
		let myhelp = inputdialog(mymsg)
		" if empty, quit without confirmation
		if myhelp == ""
			return
		endif
	endif

	" window management
	" don't pop help from a special, unless it's help
	if Cream_buffer_isspecial() == 1
	\&& Cream_buffer_ishelp() == 0
		call Cream_TryAnotherWindow()
	endif

	let v:errmsg = ""
	silent! execute "help " . myhelp
	" indicate error in dialog
	if v:errmsg != ""
		call confirm(v:errmsg, "&Ok", 1, "Info")
		return
	endif

	" remember help height during the session
	let g:cream_help_size = winheight(0)

	" window management
	" remove all hidden help buffers (do before setup, since Vim's
	" :help command won't remove a previously open)
	call Cream_help_hidden_remove()
	" get help buffer number (current)
	let mybufnr = bufnr("%")
	call Cream_window_setup()
	" restore cursor to help buffer's (new) window
	call MoveCursorToWindow(bufwinnr(mybufnr))

endfunction

function! Cream_help_hidden_remove()
	" remove all hidden help buffers (do this after we open ours so we
	" know it's hidden ;)
	let mybufnr = bufnr("%")
	let i = 1
	while i <= bufnr('$')
		if Cream_buffer_ishelp(i) == 1
		\&& bufwinnr(i) == -1
			execute "bwipeout! " . i
		endif
		let i = i + 1
	endwhile

endfunction

function! Cream_help_listtopics()

	let mymsg =
		\ "Enter a command or word to list help topics:\n" .
		\ "(results will be listed below the statusbar)\n"
		\ "\n"
	let myhelp = inputdialog(mymsg)
	if myhelp != ""
		let v:errmsg = ""
		execute "normal :help " . myhelp . "\<C-d>"
		if v:errmsg != ""
			"+++ Cream: add dialog feedback
			if has("dialog_gui")
				call confirm(v:errmsg, "&Ok", 1)
			else
				echo v:errmsg
			endif
			"+++
		endif
	endif
	call Cream_help_find()

endfunction

function! Cream_help_tags()
" Re-define helptags of all .txt files in the current buffer's
" directory. (See :helptags.)

	"set filetype=help
	execute "helptags " . expand("%:p:h")
	"set filetype=txt
	filetype detect

endfunction

" About Splash/License {{{1

function! Cream_splash()
	if has("gui_running")
		let str =
			\ "\n" .
			\ "                 Cream (for Vim)                  \n" .
			\ "\n" .
			\ "     http://cream.sourceforge.net      \n" .
			\ "   " . g:cream_mail . "    \n" .
			\ "\n" .
			\ "\n" .
			\ "    Version:     " . g:cream_version_str . " \n" .
			\ "   " . g:cream_updated . "         \n" .
			\ "                      * * *              \n" .
			\ "\n" .
			\ "  Vim version:  " . Cream_version("string") . "     \n" .
			\ "\n"
		let n = confirm(
			\ str .
			\ "\n", "&Ok", 3, "Info")
	endif
endfunction

function! Cream_license()
" split open $CREAM/docs/COPYING.txt read-only
	call Cream_file_new()
	execute "read " . $CREAM . "docs/COPYING.txt"
	set nomodified
endfunction

" Features Cream_has(){{{1

function! Cream_has(property)
" return 1 if Cream has feature, 0 if not, -1 on error
" o See cases below for allowable arguments

	" a tags file exists
	if     a:property == "tagsfile"
		if filereadable(expand("%:p:h") . "/tags") == 1
			return 1
		endif

	" app tags (or variant) exists
	" Note: Gentoo Linux properly appends "ctags" with "exuberant-"
	elseif a:property == "tags"
		if  executable("ctags") == 1
		\|| executable("exuberant-ctags") == 1
		\|| executable("tags") == 1
			return 1
		endif

	" on a Microsoft system
	elseif a:property == "ms"
		" function defined in genutils.vim
		return OnMS()

	endif

	return 0

endfunction

" Vim file lists {{{1

function! Cream_vim_syntax_list()
" returns alphabetized list of syntax/*.vim, separated by "\n"
" TODO: Use this prototype to return any file list from path.

	let mypath = $VIMRUNTIME . '/syntax/'
	if v:version >= 602 && Cream_has("ms")
		" BUGFIX: glob needs an "OS-friendly" path
		let myrt = fnamemodify(mypath, ":8")
	endif
	let myfiles = glob(mypath . "*.vim")
	" self-determine preceding path (as determined by glob() above)
	let str = matchstr(myfiles, '.\{-1,}syntax.\ze..\{-1,}\.vim')
	let myfiles = substitute(myfiles, escape(str, '~'), "", "g")
	let myfiles = substitute(myfiles, '\.vim', "", "g")
	return myfiles

endfunction

" Get filetypes {{{1
function! Cream_get_filetypes()
" get a list of all potential Vim filetypes by parsing the output of
" "autocmd filetypedetect"
"
" Hack: We're opening a temp buffer and processing the redirected
"       output. Not sure to sort, substitute, and uniq a var.

	" save cmdheight
	let mych = &cmdheight
	set cmdheight=8
	set shortmess=stOTI

	" get list of filetype autocommands
	redir @x
	silent! autocmd filetypedetect
	redir END

	" restore cmdheight
	let &cmdheight = mych

	" open temp buffer
	silent! enew
	silent! put x

	" put all 'setf [:alnum:] at line beginnings
	silent! %substitute/^\s*.*\(setf\s\+[A-Za-z0-9]\+\)/\1\r/gei
	" remove all lines not beginning 'setf'
	silent! %substitute/^\(setf\)\@!.*$//gei
	" remove all initial 'setf '
	silent! %substitute/^setf\s\+//gei
	" remove all empty lines
	silent! %substitute/\n\n\+/\r/gei
	" sort file
	silent! call BISort2(0, line("$"))
	" unselect
	normal i
	" remove initial empty line
	silent! %substitute/^\n//gei
	" uniq
	silent! call Uniq(0, line("$"))

	" select all
	silent! call Cream_select_all("endvisual")
	" yank
	normal "xy

	" close buffer
	silent! bwipeout!

	return @x

endfunction

" Strings, dialogs, and i18n {{{1

function! Cream_confirm()
"

endfunction

function! Cream_echo()

endfunction

function! Cream_inputdialog(prompt, default)

endfunction

function! Cream_str(id)
" Returns string number {id}, utilizing a translation for the current
" language if it exists or, otherwise, the default.
"
" Example string function, found in lang/strings_us_en.utf-8.vim
"
"     function! Cream_str_en_us_0000001()
"         return "This is string 0000001"
"     endfunction

	if !exists("g:CREAM_I18N_COUNTRY")
		let g:CREAM_I18N_COUNTRY = "us"
	endif
	if !exists("g:CREAM_I18N_LANG")
		let g:CREAM_I18N_LANG = "en"
	endif

	let str = "*Cream_str_" . g:CREAM_I18N_COUNTRY . g:CREAM_I18N_LANG
	if exists(str)
		let functionname = "Cream_str_" . g:CREAM_I18N_COUNTRY . "_" . g:CREAM_I18N_LANG . "_" . a:id . "()"
	else
		let functionname = "Cream_str_en_us_" . a:id . "()"
	endif

	return functionname

endfunction


" Inputdialog() {{{1
function! Inputdialog(prompt, default)
" Wraps Vim's "inputdialog()" function to accommodate Vim versions
" prior to 6.2 which are unable to distinguish between a returned
" Cancel value or an empty string value. When Vim version < 6.2, the
" user is first prompted to distingish the two.
"
" Returns:
"
"   string      When return value is not empty
"   ""          When determins the user wants an empty value
"   "{cancel}"  When determins the user wants to cancel
"

	if version >= 602
		return inputdialog(a:prompt, a:default, "{cancel}")
	endif

	let myreturn = inputdialog(a:prompt, a:default)
	if myreturn == ""
		let n = confirm(
			\ "Leave Empty or Cancel?\n" .
			\ "\n" .
			\ "(Vim versions prior to 6.2 can't tell\n",
			\ "between an empty value and cancel.)\n",
			\ "&Leave\ Empty\n&Cancel", 2)
		if n != 1
			return "{cancel}"
		else
			return ""
		endif
	endif

	return myreturn

endfunction

" 1}}}
" File open, close
" Last buffer restore {{{1

function! Cream_start()
	" obsolete
endfunction

function! Cream_last_buffer_restore()
" Recall name of last current buffer and restore.

	" Note: Cream_exit ensures remembered file is not special

	" Refresh tabs after all buffer restoration is complete. Do this
	" prior to making the last buffer current so it doesn't disappear
	" down the road!
	call Cream_tabpages_refresh()

	" see if last buffer name was saved in Cream_exit() last session
	if exists("g:CREAM_LAST_BUFFER")
		" only open if exists (able to be written)
		if filewritable(g:CREAM_LAST_BUFFER) == 1
			" NOTE: Must use name, number changes on restart.
			if bufexists(g:CREAM_LAST_BUFFER)
				if exists("g:CREAM_TABPAGES") && g:CREAM_TABPAGES == 1
					call Cream_tab_goto(bufnr(g:CREAM_LAST_BUFFER))
				else
					execute "buffer! " . g:CREAM_LAST_BUFFER
				endif
			else
				" Don't warn if unable to find! (This could be
				" experienced from any number of means, such as with a
				" second instance.)
			endif
		endif
	endif

	" TODO: Technically, this is startup fix stuff, but this routine
	" would only occur at startup.
	"

endfunction

function! Cream_last_buffer_toggle()
" toggle restoration of last buffer behavior
" default (restore) is not existing (or 0), don't restore == 1

	if exists("g:CREAM_LAST_BUFFER_FORGET")
		unlet g:CREAM_LAST_BUFFER_FORGET
	else
		let g:CREAM_LAST_BUFFER_FORGET = 1
	endif

	call Cream_menu_settings_preferences()

endfunction

" Var Management {{{1

function! Cream_var_manage()
" variable management
" * handle obsolete memorized global variable names
" * correct variable types

	if !exists("g:CREAM_VARFIX_022")

		" v.12 and prior
		" --------------

		" last buffer
		if exists("g:LAST_BUFFER")
			let g:CREAM_LAST_BUFFER = g:LAST_BUFFER
			unlet g:LAST_BUFFER
		endif

		" ReplaceMultifile
		if exists("g:RMFIND")
			let g:CREAM_RMFIND = g:RMFIND
			unlet g:RMFIND
		endif
		if exists("g:RMREPL")
			let g:CREAM_RMREPL = g:RMREPL
			unlet g:RMREPL
		endif
		if exists("g:RMPATH")
			let g:CREAM_RMPATH = g:RMPATH
			unlet g:RMPATH
		endif

		" MRU (Most recent used file menu)
		if exists("g:MRU_MENU")
			let g:CREAM_MRU_MENU = g:MRU_MENU
			unlet g:MRU_MENU
		endif
		if exists("g:MRU_MAX")
			let g:CREAM_MRU_MAX = g:MRU_MAX
			unlet g:MRU_MAX
		endif
		if exists("g:MRU_BUF_COUNT")
			let g:CREAM_MRU_BUF_COUNT = g:MRU_BUF_COUNT
			unlet g:MRU_BUF_COUNT
		endif
		if exists("g:MRU_MENU_PRIO")
			let g:CREAM_MRU_MENU_PRIO = g:MRU_MENU_PRIO
			unlet g:MRU_MENU_PRIO
		endif
		if exists('g:CREAM_MRU_MENU_PRIO')
			unlet g:CREAM_MRU_MENU_PRIO
		endif

		" individual buffers, now obsolete (1 through arbitrary 30)
		if exists("g:CREAM_MRU_MAX")
			let i = 0
			while i < 30
				"call Debug("i")
				if exists("g:MRU_BUFFER" . i)
					execute "let g:CREAM_MRU_BUFFER" . i . " = g:MRU_BUFFER" . i
					execute "unlet g:MRU_BUFFER" . i
				endif
				let i = i + 1
			endwhile
		endif

		if exists('g:CREAM_MRU_MAX')
			unlet g:CREAM_MRU_MAX
		endif


		" v.13
		" ----
		" update language variable (only english existed)
		if exists("g:CREAM_SPELL_LANG")
			if g:CREAM_SPELL_LANG == "eng"
				let g:CREAM_SPELL_LANG = "eng,m;"
			endif
		endif


		" v.14
		" ----
		" version case change
		if exists('g:Cream_version')
			unlet g:Cream_version
		endif

		" show invisibles
		if exists('g:CREAM_LIST')
			unlet g:CREAM_LIST
		endif

		" migrate to distinguishing between platforms so multiple can be
		" supported [08 Sep 2002 v0.15]
		if exists("g:CREAM_COLS")
			let g:CREAM_COLS_{myos} = g:CREAM_COLS
			unlet g:CREAM_COLS
		endif
		if exists("g:CREAM_LINES")
			let g:CREAM_LINES_{myos} = g:CREAM_LINES
			unlet g:CREAM_LINES
		endif
		if exists("g:CREAM_WINPOSX")
			let g:CREAM_WINPOSX_{myos} = g:CREAM_WINPOSX
			unlet g:CREAM_WINPOSX
		endif
		if exists("g:CREAM_WINPOSY")
			let g:CREAM_WINPOSY_{myos} = g:CREAM_WINPOSY
			unlet g:CREAM_WINPOSY
		endif
		if exists("g:CREAM_FONT")
			let g:CREAM_FONT_{myos} = g:CREAM_FONT
			unlet g:CREAM_FONT
		endif


		" v.16
		" ----
		" convert var types (string to num) and initialize to 0
		" type(), 0 if Number, 1 if String

		if exists("g:CREAM_WRAP")
			let g:CREAM_WRAP = g:CREAM_WRAP + 0
			" change off state from -1 to 0
			if g:CREAM_WRAP == -1
				let g:CREAM_WRAP = 0
			endif
		endif
		if exists("g:CREAM_AUTOWRAP")
			let g:CREAM_AUTOWRAP = g:CREAM_AUTOWRAP + 0
			" change off state from -1 to 0
			if g:CREAM_AUTOWRAP == -1
				let g:CREAM_AUTOWRAP = 0
			endif
		endif
		if exists("g:CREAM_AUTOWRAP_WIDTH")
			let g:CREAM_AUTOWRAP_WIDTH = g:CREAM_AUTOWRAP_WIDTH + 0
		endif

		if exists("g:CREAM_TABSTOP")
			if type(g:CREAM_TABSTOP) == 1
				" change to number
				let g:CREAM_TABSTOP = g:CREAM_TABSTOP + 0
			endif
		endif
		if exists("g:CREAM_AUTOINDENT")
			let g:CREAM_AUTOINDENT = g:CREAM_AUTOINDENT + 0
			" change off state from -1 to 0
			if g:CREAM_AUTOINDENT == -1
				let g:CREAM_AUTOINDENT = 0
			endif
		endif
		if exists("g:CREAM_LINENUMBERS")
			let g:CREAM_LINENUMBERS = g:CREAM_LINENUMBERS + 0
		endif
		if exists("g:LIST")
			let g:LIST = g:LIST + 0
		endif

		if exists("g:CREAM_TOOLBAR")
			let g:CREAM_TOOLBAR = g:CREAM_TOOLBAR + 0
		endif

		if exists("g:CREAM_EXPERTMODE")
			if type(g:CREAM_EXPERTMODE) == 1
				" change state from "on-off" to "1-0"
				if g:CREAM_EXPERTMODE == "on"
					let g:CREAM_EXPERTMODE = 1
				elseif g:CREAM_EXPERTMODE == "off"
					let g:CREAM_EXPERTMODE = 0
				endif
			endif
			let g:CREAM_EXPERTMODE = g:CREAM_EXPERTMODE + 0
		endif

		if exists("g:CREAM_SPELL_MULTIDICT")
			let g:CREAM_SPELL_MULTIDICT = g:CREAM_SPELL_MULTIDICT + 0
		endif

		if exists("g:CREAM_SINGLESERVER")
			let g:CREAM_SINGLESERVER = g:CREAM_SINGLESERVER + 0
		endif

		if exists("g:CREAM_SEARCH_HIGHLIGHT")
			let g:CREAM_SEARCH_HIGHLIGHT = g:CREAM_SEARCH_HIGHLIGHT + 0
		endif

		" find
		if exists("g:CREAM_FCASE")
			let g:CREAM_FCASE = g:CREAM_FCASE + 0
			" fix -1 state (make 0)
			if g:CREAM_FCASE == -1
				let g:CREAM_FCASE = 0
			endif
		endif
		if exists("g:CREAM_FREGEXP")
			let g:CREAM_FREGEXP = g:CREAM_FREGEXP + 0
			" fix -1 state (make 0)
			if g:CREAM_FREGEXP == -1
				let g:CREAM_FREGEXP = 0
			endif
		endif

		" replace
		if exists("g:CREAM_RCASE")
			let g:CREAM_RCASE = g:CREAM_RCASE + 0
			" fix -1 state (make 0)
			if g:CREAM_RCASE == -1
				let g:CREAM_RCASE = 0
			endif
		endif
		if exists("g:CREAM_RREGEXP")
			let g:CREAM_RREGEXP = g:CREAM_RREGEXP + 0
			" fix -1 state (make 0)
			if g:CREAM_RREGEXP == -1
				let g:CREAM_RREGEXP = 0
			endif
		endif
		if exists("g:CREAM_RONEBYONE")
			let g:CREAM_RONEBYONE = g:CREAM_RONEBYONE + 0
			" fix -1 state (make 0)
			if g:CREAM_RONEBYONE == -1
				let g:CREAM_RONEBYONE = 0
			endif
		endif

		" replace multi-file
		if exists("g:CREAM_RMCASE")
			let g:CREAM_RMCASE = g:CREAM_RMCASE + 0
			" fix -1 state (make 0)
			if g:CREAM_RMCASE == -1
				let g:CREAM_RMCASE = 0
			endif
		endif
		if exists("g:CREAM_RMREGEXP")
			let g:CREAM_RMREGEXP = g:CREAM_RMREGEXP + 0
			" fix -1 state (make 0)
			if g:CREAM_RMREGEXP == -1
				let g:CREAM_RMREGEXP = 0
			endif
		endif

		"......................................................................
		" Hmm... not sure how long these have been obsolete
		if exists("g:CREAM_HI_SEL")
			unlet g:CREAM_HI_SEL
		endif
		if exists("g:CREAM_SHIFTWIDTH")
			unlet g:CREAM_SHIFTWIDTH
		endif


		" v.18
		" ----
		" this never in production
		if exists("g:CREAM_POP")
			unlet g:CREAM_POP
		endif

		" v.21
		" ----
		" never utilized
		if exists("g:CREAM_TAGLIST")
			unlet g:CREAM_TAGLIST
		endif

		" v.22
		" ----
		if exists("g:CREAM_FONT_COLUMNS")
			let myos = Cream_getoscode()
			let g:CREAM_FONT_COLUMNS_{myos} = g:CREAM_FONT_COLUMNS
			unlet g:CREAM_FONT_COLUMNS
		endif

		" array style/structure changes for addon mappings
		let i = 0
		while i < 8
			" if first item exists and is a string
			if exists("g:CREAM_ADDON_MAPS{i}")
			\ && type(g:CREAM_ADDON_MAPS{i}) == 1
				" set new globals to old values
				let g:CREAM_ADDON_MAPS{i + 1}_{1} = MvElementAt(g:CREAM_ADDON_MAPS{i}, "\t", 0)
				let g:CREAM_ADDON_MAPS{i + 1}_{2} = MvElementAt(g:CREAM_ADDON_MAPS{i}, "\t", 1)
				let g:CREAM_ADDON_MAPS{i + 1}_{3} = MvElementAt(g:CREAM_ADDON_MAPS{i}, "\t", 2)
				" remove old globals
				"if i != 0
					unlet g:CREAM_ADDON_MAPS{i}
				"else
				"    " trick to get past init filter at
				"    " Cream_addon_maps_init() so we get a count there
				"    let g:CREAM_ADDON_MAPS{i} = -1
				"endif
			else
				break
			endif
			let i = i + 1
		endwhile
		" don't do this again
		let g:CREAM_VARFIX_022 = 1

	endif


	if !exists("g:CREAM_VARFIX_029")

		" v.29
		" ----
		if exists("g:CREAM_AUTOWRAP_WIDTH")
			let g:CREAM_AUTOWRAP_WIDTH = g:CREAM_AUTOWRAP_WIDTH + 0
		endif

		" don't do this again
		let g:CREAM_VARFIX_029 = 1

	endif

	if !exists("g:CREAM_VARFIX_031")

		" v.31
		" ----
		" fix spelling dictionary dialect name changes
		if exists("g:CREAM_SPELL_LANG")
			let g:CREAM_SPELL_LANG = substitute(g:CREAM_SPELL_LANG, ',us', ',US', 'g')
			let g:CREAM_SPELL_LANG = substitute(g:CREAM_SPELL_LANG, ',ca', ',CA', 'g')
			let g:CREAM_SPELL_LANG = substitute(g:CREAM_SPELL_LANG, ',br', ',GB', 'g')
		endif

		" don't do this again
		let g:CREAM_VARFIX_031 = 1

	endif

	" v.34
	" ----
	" type fix
	if exists("g:CREAM_TOOLBAR") && g:CREAM_TOOLBAR == "1"
		let g:CREAM_TOOLBAR = 1
	endif


endfunction

" Buffer Enter fix {{{1

" fix normal mode hang on buffer enter (fixed in patch 6.0.254)
"*** Broken ***
function! Cream_bufenter_fix()
	if version <= 600
		execute "normal \<Esc>"
	endif
endfunction

" New {{{1

function! Cream_file_new()
" opens new buffer in current window

	" if already in new, stop
	if Cream_buffer_isnewunmod() == 1
		return
	endif

	" window management (prohibit from a special)
	if Cream_buffer_isspecial() == 1
		call Cream_TryAnotherWindow()
		" if already in new, stop
		if Cream_buffer_isnewunmod() == 1
			return
		endif
	endif

	" why would we need to confirm this?
	"confirm enew
	if exists("g:CREAM_TABPAGES") && g:CREAM_TABPAGES == 1
		tabnew
	else
		enew
	endif

	" not needed, handled by BufNew autocmd call
	"" refresh buffer menu
	"call BMShow()

endfunction

" Open/Edit {{{1

function! Cream_file_open(...)
" open file using dialog
" * optional argument is file to be opened (without dialog)
" * disallow opening in Calendar window
" * disallow opening in Opsplorer window

	" window management (not from a special)
	if Cream_buffer_isspecial(bufnr("%"))
		call Cream_TryAnotherWindow()
	endif

	" if argument passed
	if a:0 == 0
		" open dialog
		if exists("g:CREAM_CWD")
			let mydir = g:CREAM_CWD
		else
			"" TODO: did Vim behavior change? (empty works pre-7)
			" let mydir = getcwd()

			"if Cream_has("ms")
			"    let mydir = ""
			"else
			"    if exists("b:cream_pathfilename")
			"        let mydir = fnamemodify(expand(b:cream_pathfilename), ":p:h") . '/'
			"    else
			"        let mydir = ""
			"    endif
			"endif

			" we've fixed automatic directory changing elsewhere, this
			" should always open to the cwd.
			let mydir = getcwd()

		endif

""*** DEBUG:
"let n = confirm(
"    \ "DEBUG:\n" .
"    \ "  mydir  = " . mydir . "\n" .
"    \ "\n", "&Ok\n&Cancel", 1, "Info")
"if n != 1
"    return
"endif
""***

		let myfile = browse(0, "Select file", mydir, "")
		" To open file in path of current buffer
		"let myfile = browse(0, "Select file", fnamemodify(bufname("%"), ":p:h"), "")
	else
		let myfile = a:1
	endif

	" ignore Cancel return
	if myfile == ""
		return
	endif

	" make sure we can edit!
	if     filewritable(myfile) == 2
		call confirm(
			\ "Can not edit a directory!\n" .
			\ "\n", "&Ok", 1, "Info")
		return
	elseif filereadable(myfile) == 0
		let n = confirm(
			\ "File \"" . myfile . "\"\n" .
			\ "does not exist, create it?\n" .
			\ "\n", "&Ok\n&Cancel", 1, "Info")
		if n != 1
			return
		endif
	elseif filewritable(myfile) == 0
		let n = confirm(
			\ "Warning!\n\n" .
			\ "File is not writable (read-only, permissions, etc.). Open anyway?\n" .
			\ "\n", "&Ok\n&Cancel", 1, "Warning")
		if n != 1
			return
		endif
	endif

	" hook for user functionality
	if exists("*Cream_hook_open")
		let fname = Cream_path_fullsystem(myfile)
		let test = Cream_hook_open(fname)
		if test == -1
			" stop
			return -1
		endif
	endif

	" escape spaces
	let myfile = escape(myfile, ' #%<')

	" if using tabs
	if exists("g:CREAM_TABPAGES") && g:CREAM_TABPAGES == 1
		" if new-unmod
		if Cream_buffer_isnewunmod()
			" open in current window
			execute "edit " . myfile
		else
			" new tab
			execute "tabedit " . myfile
		endif
	else
		" open in current window
		execute "edit " . myfile
	endif

	" refresh syntax highlighting
	call Cream_filetype()

endfunction

function! Cream_file_open_readonly(...)
" similar to open() except file is made readonly

	" window management (not from a special)
	if Cream_buffer_isspecial(bufnr("%"))
		call Cream_TryAnotherWindow()
	endif

	" if argument passed
	if a:0 == 0
		" open dialog
		let myfile = browse(0, "Select file", "", '')
	else
		let myfile = a:1
	endif

	" ignore Cancel return
	if myfile == ""
		return
	endif

	" make sure we can edit!
	if filewritable(myfile) == 2
		call confirm(
			\ "Can not edit a directory!\n" .
			\ "\n", "&Ok", 1, "Info")
		return
	endif
	if filereadable(myfile) == 0
		call confirm(
			\ "Error: File does not exist!\n" .
			\ "\n", "&Ok", 1, "Error")
		return
	endif

	" escape spaces
	let myfile = escape(myfile, ' ')

	" open in current window
	if exists("g:CREAM_TABPAGES") && g:CREAM_TABPAGES == 1
		" if new-unmod
		if Cream_buffer_isnewunmod()
			" open in current window
			execute "view " . myfile
		else
			" new tab
			execute "tab view " . myfile
		endif
	else
		execute "view " . myfile
	endif

	" refresh syntax highlighting
	call Cream_filetype()

endfunction

function! Cream_session_new()
" start new session (and over-ride single server mode temporarily)

" TODO: OBSOLETE, we no longer permit multiple sessions. (2007-05-16)


	" solve buffer not found errors from second session startup
	let g:CREAM_LAST_BUFFER = ""
	" write the viminfo so it's remembered!
	wviminfo!

	" don't use if not initialized
	if !exists("g:CREAM_SINGLESERVER")
		if Cream_has("ms")
			execute 'silent! !start gvim --servername "CREAM" -U NONE -u "\$VIMRUNTIME/cream/creamrc"'
		else
			execute 'silent! !gvim --servername "CREAM" -U NONE -u "\$VIMRUNTIME/cream/creamrc"'
		endif
	else

		" TODO: There's a complex bug here: the new session will not
		" have the same settings as this session. But the last session
		" to close will determine the preferences of future sessions.
		" This will result in:
		" o Omissions of Recent File menu entries
		" o Settings toggled
		"
		" Solutions:
		" o Warn the user?
		" o => On close, message any existing sessions with the new
		"   info. (talk about complicated)
		" o Immediately write setting changes back to viminfo?
		"   * Any session beginning with a name other than "CREAM"
		"     should register with the rest?

		" Set up viminfo for new session.
		"
		" so new session doesn't re-merge (see Cream_singleserver())
		let g:CREAM_SERVER_OVERRIDE = 1
		" disable swapfiles
		set noswapfile
		" disable window pos (so not over current session)
		call Cream_screen_unlet()
		" forget current buffers
		if !exists("g:CREAM_LAST_BUFFER_FORGET") || g:CREAM_LAST_BUFFER_FORGET == 1
			let lastbuf = 1
			let g:CREAM_LAST_BUFFER_FORGET = 1
			if exists("g:CREAM_LAST_BUFFER")
				unlet g:CREAM_LAST_BUFFER
			endif
			set viminfo-=%
		endif
		" write viminfo
		wviminfo!
		" restore last buffer remember state
		if exists("lastbuf")
			let g:CREAM_LAST_BUFFER_FORGET = 1
			let &viminfo = &viminfo . ",%"
			"let g:CREAM_LAST_BUFFER = 1
		endif
		" restore GUI set
		call Cream_screen_get()
		" remove global shield
		unlet g:CREAM_SERVER_OVERRIDE

		" add four random digits to server name so unique
		let myserver = "CREAM" . strpart(localtime(), 6)
		" open the new session
		if Cream_has("ms")
			execute 'silent! !start gvim --servername "' . myserver . '" -U NONE -u "\$VIMRUNTIME/cream/creamrc"'
		else
			execute 'silent! !gvim --servername "' . myserver . '" -U NONE -u "\$VIMRUNTIME/cream/creamrc"'
		endif

	endif

endfunction

function! Cream_cwd()
" maintain current working directory (called via autocmd)

	if exists("g:CREAM_CWD")
		execute 'cd "' . g:CREAM_CWD . '"'
	else
		if exists("b:cream_pathfilename")
			" need quotes on unix for unescaped filenames (escaping is harder)
			if Cream_has("ms")
				execute 'cd ' .
				\ fnamemodify(expand(b:cream_pathfilename), ":p:h")
			else
				execute 'cd ' .
				\ escape(fnamemodify(expand(b:cream_pathfilename), ":p:h"), ' \')
			endif
		endif
	endif

endfunction

" File open under cursor {{{1
function! Cream_file_open_undercursor(mode)
" open the file under the cursor.
" * guess where it is if not full path
" * future options to open related (.h from .c, etc.)

	" get full name
	" with selection, we won't guess (much)
	if     a:mode == "v"
		normal gv
		normal "xy
		" trim off accidental leading whitespace
		let myfile = Cream_trim(@x)

		" URL
		if Cream_isURL(myfile)
			let isURL = 1
		" filename
		elseif filereadable(myfile)

		" filename, try file in existing path
		elseif filereadable(expand("%:p:h") . "/" . myfile)
			let myfile = expand("%:p:h") . "/" . myfile

		" further guesses here...

		else

		endif

	" use word under cursor (if file doesn't exist, we might try
	" a little harder in the area, but not much--tell user we can't
	" find spaces or odd characters.)
	elseif a:mode == "i"

		" OPTION 1
		"" try word under cursor
		"let myfile = expand("<cword>")
		"
		"" URL ( http://cream.sf.net )
		"if     match(myfile, "http://", 0) == 0
		"    let isURL = 1
		"elseif match(expand("<cWORD>"), "http://", 0) == 0
		"    let myfile = expand("<cWORD>")
		"    let isURL = 1

		"" filename
		"elseif filereadable(myfile)
		"
		"" filename, try harder
		"elseif filereadable(expand("<cWORD>"))
		"    let myfile = expand("<cWORD>")
		"" filename, try word under cursor, with path
		"elseif filereadable(expand("%:p:h") . "/" . expand("<cword>"))
		"    let myfile = expand("%:p:h") . "/" . expand("<cword>")
		"" filename, try harder, with path
		"elseif filereadable(expand("%:p:h") . "/" . expand("<cWORD>"))
		"    let myfile = expand("%:p:h") . "/" . expand("<cWORD>")
		"
		"" filename, try *really* hard
		"else
		"    " add period as valid part of cword (already part of cWORD)
		"    set iskeyword+=.
		"
		"    let myfile = expand("<cword>")
		"    if     filereadable(myfile)
		"        " good, continue
		"    " try word with path
		"    elseif filereadable(expand("%:p:h") . "/" . expand("<cword>"))
		"        let myfile = expand("%:p:h") . "/" . expand("<cword>")
		"    endif
		"
		"    set iskeyword-=.
		"endif

		" OPTION 2
		let myfile = expand("<cfile>")
		" URL
		if Cream_isURL(myfile)
			let isURL = 1
		endif

	endif

	" ACTIONS
	if exists("isURL")
		call Cream_file_open_defaultapp(myfile)
		return

	" validate (return if file unreadable)
	elseif filereadable(myfile) == 0
		if     a:mode == "v"
			call confirm(
				\ "Unable to find a file to open matching the selection.\n" .
				\ "\n", "&Ok", 1, "Info")
			normal gv
		elseif a:mode == "i"
			call confirm(
				\ "Unable to find a file to open matching the word under the cursor.\n" .
				\ "\n", "&Ok", 1, "Info")
		endif
		return
	endif

	" open validated file
	call Cream_file_open(myfile)

	" refresh buffer menu
	call BMShow()

endfunction

function! Cream_isURL(str)
	if     match(a:str, "http://", 0) == 0
		return 1
	elseif match(a:str, "www.", 0) == 0
		return 1
	endif
endfunction

" Saving {{{1

function! Cream_save()
" save no matter what
	" if new file
	if expand("%") == ""
		if has("browse")
			browse confirm write
		else
			confirm write
		endif
		set filetype=
		call Cream_filetype()
	else
		confirm write
	endif
	" refresh buffer menu
	call BMShow()
endfunction

function! Cream_saveas(...)
" general SaveAs function
" o Unloads original file without saving
" o Optional argument {...} can be path-filename to save to without
"   prompting user for path-filename.
"
" NOTE: If saveas is successful, new buffer name will have old buffer
" number while old buffer name will have a new buffer number.

	" remember original buffer
	let origbufnr = bufnr("%")
	" unnamed buffers will have the cwd as their name, condition here
	let origbufname = Cream_path_fullsystem(bufname(origbufnr))

	" track if this is a new file or not
	let isnewunmod = Cream_buffer_isnewunmod()
	let isnewmod = Cream_buffer_isnewmod()

	" if passed arg and head of it exists
	if a:0 == 1
		let pathname == Cream_path_fullsystem(a:1)
	endif
	if exists("pathname")
		if Cream_pathexists(pathname)
			" save as new file *name*
			execute "confirm saveas! " . pathname
		endif

	else

		" prompt user for path/filename
		if has("browse")
			browse confirm saveas
		else
			confirm saveas
		endif

	endif

	" get new buffer's number
	let newbufnr_new = bufnr("%")
	" get original buffer's new number
	let origbufnr_new = bufnr(origbufname)

	"-----------------------------------------------------------------
	" unnamed saveas success
	if  isnewmod == 1 && &modified == 0
	\|| isnewunmod == 1 && bufname(bufnr("%")) != ""

		" vim leaves a bunch of unnamed around
		call Cream_buffers_delete_untitled()

		" buffer was new
		" (don't try to delete old buffer)
		return

	" unnamed saveas canceled (failed?)
	elseif  isnewmod == 1 && &modified == 1
	\|| isnewunmod == 1 && bufname(bufnr("%")) != ""

		return

	endif

	" make new file name current (*has old buffer number*)
	execute "buffer " . origbufnr

	" remove original file (*now with new number*) *without saving*
	if origbufnr_new != -1
		call Cream_bwipeout(origbufnr_new)
	endif

	" REFRESH...
	" buf name
	if exists("b:cream_pathfilename")
		unlet b:cream_pathfilename
	endif
	" highlighting
	if exists("g:CREAM_SYNTAX") && g:CREAM_SYNTAX == 1
		syntax enable
	endif
	" spell check highlighting if on
	if exists("b:cream_spell") && b:cream_spell == "1"
		call Cream_spellcheck()
	endif
	" MRU menu (BufEnter autocmd doesn't pick up?)
	call MRUAddToList()
	" buffer menu
	call BMShow()
	" windows/tabs
	call Cream_window_setup()

	"" TODO: hmmm... beginning 7.2.60 the new tab is not refreshed for some reason
	"redraw!
	"" weird, it takes twice
	"redraw!
	call Cream_filetype()

endfunction

function! Cream_saveall()
" function so buffer menu can be refreshed
	" we silence command line warning, so warn with dialog
	if Cream_NumberOfBuffers("newmod") > 0
		call confirm(
			\ "Modified buffers exist that are not yet saved as files. " .
			\ "SaveAs in \"[untitled]\" windows to keep your work." .
			\ "\n", "&Ok", 1, "Warning")
	endif
	silent! execute "wall"
	call BMShow()
endfunction

function! Cream_update(mode)
" save only when changes
" * we don't use the :update command because it can't prompt to SaveAs
"   an un-named file

	if &modified == 1
		call Cream_save()
	endif
	if a:mode == "v"
		" reselect
		normal gv
	endif
	" refresh buffer menu
	call BMShow()
endfunction

function! Cream_save_confirm()
" confirm save any modified current document
"
" Returns:
"   -1 if user cancelled in some fashion (meaning "STOP!")
"    1 if doc saved, chose not to save, or doc not modified

	" save if modified
	if &modified == 1
		" unnamed file
		if  Cream_buffer_isnewmod() == 1
			"*** Developer: We want to track Vim dialogs against Cream's
			if exists("g:cream_dev")
				let mychoice = confirm("Cream: Save changes as a new document?", "&Yes\n&No\n&Cancel", 1)
			else
				let mychoice = confirm("Save changes as a new document?", "&Yes\n&No\n&Cancel", 1)
			endif
			"***
		else
			"*** Developer: We want to track Vim dialogs against Cream's
			let filename = fnamemodify(expand("%"), ":p")
			if Cream_has("ms")
				let filename = substitute(filename, '/', '\', 'g')
			endif
			if exists("g:cream_dev")
				let mychoice = confirm("Cream: Save changes to\n \"" . filename . "\" ?\n", "&Yes\n&No\n&Cancel", 1)
			else
				let mychoice = confirm("Save changes to\n \"" . filename . "\" ?\n", "&Yes\n&No\n&Cancel", 1)
			endif
			"***
		endif
		" yes, save
		if mychoice == 1
			" unnamed file
			if Cream_buffer_isnewmod() == 1
				if has("browse")
					" get path-filename via dialog
					"let n = browse("", "", getcwd(), "")
					"" if nothing was returned (error or cancel)
					"if n == ""
					"    return -1
					"endif
					browse saveas
					" if still not saved, error
					if Cream_buffer_isnewmod() == 1
						return -1
					endif
				else
					call confirm(
						\ "Please :saveas before closing." .
						\ "\n", "&Ok", 1, "Warning")
					return -1
				endif
			else
				write
			endif
		" no: don't save changes!
		elseif mychoice == 2
			" nothing
		" cancel or error: do nothing
		else
			return -1
		endif

		" refresh buffer menu
		call BMShow()

	endif

	return 1

endfunction

" Close {{{1

function! Cream_close()
" * Provide options to close unsaved buffers
" * Confirm unnamed files
" * Drop into other open files rather than the unlisted "[No File]"

	" confirm the save
	let n = Cream_save_confirm()
	" required save cancelled or error
	if n == -1
		return 0
	endif

	" hook for user functionality
	if exists("*Cream_hook_close")
		let fname = Cream_path_fullsystem(expand("%"))
		let test = Cream_hook_close(fname)
		if test == -1
			" stop
			return -1
		endif
	endif

	" window management--if help, removal *all* help buffers, even
	" hidden ones
	if Cream_buffer_ishelp()
		let washelp = 1
	endif

	" wipeout buffer (delete)
	call Cream_bwipeout()

	if exists("washelp")
		call Cream_help_hidden_remove()
	endif

	" vim leaves a bunch of unnamed around
	call Cream_buffers_delete_untitled()

	" not necessary, done by focusgained autocmd
	"" reset tabs
	"call Cream_tabpages_refresh()

	" refresh buffer menu
	call BMShow()

	return 1

endfunction

function! Cream_close_all()

	let buffercount = Cream_NumberOfBuffers("all")
	let i = 0
	while i < buffercount
		let return = Cream_close()
		if return != 1
			" user quit at one file, stop closing
			call BMShow()
			return 0
		endif
		let i = i + 1
	endwhile

	" refresh buffer menu
	call BMShow()

	return 1

endfunction

" Exit {{{1

function! Cream_exit()
" * Remember last current buffer for restoration
" * Existing buffers are retained (default nature of Vim)
" * Quit Vim, confirm unsaved buffers with changes

	" only do once! (This function called directly by menu, but also
	" by autocmd in the case of a window manager exit, etc. We just
	" want to do it one time in the first case.)
	if exists("g:Cream_exit")
		return
	endif

	" remember something that's already showing (not just open)
	call Cream_TryAnotherWindow()

	" don't do this on exit, we fix it all on startup
	"""" kill off any non-memorable buffers
	"""call Cream_buffers_delete_special()
	"""call Cream_buffers_delete_untitled()

	" get current buffer
	let mybufnr = bufnr("%")

	" confirm save all modifieds; upon cancel of the confirm, exit
	" doesn't happen. (Was responsible for not-able-to-exit bug where
	" cancel aborts the close all, but check variable is still set!)
	let i = 1
	while i <= bufnr('$')
		if getbufvar(i, "&modified") == 1
		\&& Cream_buffer_isspecial(i) == 0
			execute "buffer " . i
			let valid = Cream_save_confirm()
			if valid == -1
				" save cancelled or error--go back to original buffer
				execute "buffer " . mybufnr
				return
			endif
		endif
		let i = i + 1
	endwhile

	" avoid modified new (already elected not to save it)
	call Cream_TryAnotherWindow("nonewmod")

	" get current buffer filename (if not special, newmod or new unmod)
	" Note: We do this post save, to avoid saving modified un-named
	if  Cream_buffer_isspecial(bufnr("%")) != 1
	\&& Cream_buffer_isnewunmod(bufnr("%")) != 1
	\&& Cream_buffer_isnewmod(bufnr("%")) != 1
		let g:CREAM_LAST_BUFFER = fnamemodify(bufname("%"), ":p")
	else
		if exists("g:CREAM_LAST_BUFFER")
			unlet g:CREAM_LAST_BUFFER
		endif
	endif

	" get screen position and size
	if has("gui_running")
		call Cream_screen_get()
	endif

	" manage MRU menu
	call MRUVimLeavePre()

	" close calendar and remember state (do after confirmations so it
	" can be maintained if there are cancel/problems above.)
	call Cream_calendar_exit()

	" don't save buffer state based on preference
	if exists("g:CREAM_LAST_BUFFER_FORGET") && g:CREAM_LAST_BUFFER_FORGET == 1
		" close all buffers
		let myreturn = Cream_close_all()
		if myreturn != 1
			" user quit somewhere during close process, stop
			return 0
		endif
		if exists("g:CREAM_LAST_BUFFER")
			unlet g:CREAM_LAST_BUFFER
		endif
	endif

	" penultimate: post-everything but the preserving viminfo write
	let g:Cream_exit = 1

	" yes, we're really doing this (can't confirm--if "no" chosen
	" above, would see it twice)
	" * must be last to correctly write viminfo
	qall!

endfunction

function! Cream_save_exit()
" save all files and exit

	silent! call Cream_saveall()
	silent! call Cream_exit()

endfunction

" 1}}}
" Editing {{{1
" Cut/Copy/Paste {{{1

function! Cream_cut(mode)
" cut selection to universal clipboard ("+)

	if a:mode == "v"
		normal gv
		normal "+x
	endif
endfunction

function! Cream_copy(mode)
" copy selection to universal clipboard ("+)

	if a:mode == "v"
		normal gv
		normal "+y
		normal gv
	endif
endfunction

""----------------------------------------------------------------------
"" Source:	mswin.vim
""
"" Pasting blockwise and linewise selections is not possible in Insert
"" and Visual mode without the +virtualedit feature. They are pasted as
"" if they were characterwise instead.
"if has("virtualedit")
"    nnoremap <silent> <SID>Paste :call <SID>Paste()<CR>
"    function! <SID>Paste()
"        let myve = &virtualedit
"        set virtualedit=all
"        normal `^"+gPi
"        let &virtualedit = myve
"    endfunction
"    imap <C-v>	x<BS><Esc><SID>Pastegi
"    vmap <C-v>	"-c<Esc><SID>Paste
"else
"    nnoremap <silent> <SID>Paste "=@+.'xy'<CR>gPFx"_2x
"    imap <C-v>	x<Esc><SID>Paste"_s
"    vmap <C-v>	"-c<Esc>gix<Esc><SID>Paste"_x
"endif
"vmap <S-Insert>	<C-v>
"imap <S-Insert>	<C-v>
""----------------------------------------------------------------------

function! Cream_paste(mode)
" paste selection from universal clipboard ("+)

	if     a:mode == "v"
		normal gv
		normal "+P
		" correct position
		normal l
		" don't re-select, sizes may differ
	elseif a:mode == "i"
		" fix win32 paste from app
		call setreg('+', @+, 'c')
		let myvirtualedit = &virtualedit
		set virtualedit=all
		normal `^"+gP
		let &virtualedit = myvirtualedit
	endif

endfunction

" Undo and Redo {{{1

function! Cream_undo(mode)
	undo
	if a:mode == "v"
		normal gv
	endif
endfunction

function! Cream_redo(mode)
	redo
	if a:mode == "v"
		normal gv
	endif
endfunction

" Delete {{{1
function! Cream_delete(mode)
" for pop up menu item
	if a:mode == "v"
		normal gv
		normal x
	endif
endfunction

" Indent/Unindent {{{1

function! Cream_indent(mode)
	if a:mode == "v"
		normal gv
		normal >
		normal gv
	endif
endfunction

function! Cream_unindent(mode)
	if a:mode == "v"
		normal gv
		normal <
		normal gv
	elseif a:mode == "i"
		let mypos = Cream_pos()
		" select line and unindent
		normal V
		normal <
		execute mypos
		" now adjust for shift
		let i = 0
		let myline = line('.')
		while i < &tabstop
			normal h
			" oops, go back to current line if we jumped up
			if line('.') < myline
				normal l
			endif
			let i = i + 1
		endwhile
		" select one char so
		" * user can see a selection is in place
		" * if tab is used immediately following this routine believes
		"   the whole line is selected rather than inserting a single
		"   tab
		normal vl
		if mode() == "v"
			" change to select mode
			execute "normal \<C-G>"
		endif
	endif
endfunction

" Word Count {{{1

function! Cream_count_word(mode)

	if a:mode == "i"
		" remember position
		let mypos = Cream_pos()
		let myword = expand('<cword>')
	elseif a:mode == "v"
		" use selection
		normal gv
		normal "xy
		let myword = @x
	else
		let myword = ""
	endif

	let pattern = inputdialog("Enter a word to count within the \ncurrent document... (case sensitive)", myword)
	if pattern != ''
		" test
		" search
		if search(pattern, 'w') > 0

			let @/ = pattern
			" capture states
			let mymodified = &modified
			let myreport = &report
			" report all changes
			set report=0
			" capture output of substitution
			redir @x
			"execute 'g/' . pattern . '/s//&/g'
			execute '%substitute/\<' . pattern . '\>/' . pattern . '/geI'
			"execute '%substitute/' . pattern . '/' . pattern . '/geI'
			redir END
			" restore states
			let &modified = mymodified
			let &report = myreport

			" remove initial leading linefeed
			let str = @x
			let str = strpart(str, 1)
			" the initial search above tells us it will be found
			"" parse output
			"if match(str, "Pattern") > 0
			"    " word doesn't exist
			"    let cnt = 0
			"else
				let thepos = match(str, ' ')
				let cnt = strpart(str, 0, thepos)
				"let cnt = matchstr(str, '^\d\+')
				" if highlighting on, match it to show on the screen
				if g:CREAM_SEARCH_HIGHLIGHT == 1
					execute '/\<' . pattern . '\>'
					"execute '/' . pattern . ''
				endif
			"endif

			" if highlighting not on, explain benefit to user
			if g:CREAM_SEARCH_HIGHLIGHT == 0
				call confirm(
					\ "\"" . pattern . "\"  occurs " . cnt . " times." . "\n" .
					\ "\n" .
					\ "(Turn on Settings...Highlight Find to see occurances.)\n" .
					\ "\n", "&Ok", 1, "Info")
			else
				call confirm(
					\ "\"" . pattern . "\"  occurs " . cnt . " times." . "\n" .
					\ "\n", "&Ok", 1, "Info")
				redraw!
			endif

		else
			call confirm(
				\ "\"" . pattern . "\" not found.\n" .
				\ "\n", "&Ok", 1, "Info")
		endif

	endif

	if     a:mode == "i"
		" recall position
		execute mypos
	elseif a:mode == "v"
		normal gv
	endif

endfunction

function! Cream_count_words(mode)

	if a:mode == "i"
		" remember position
		let mypos = Cream_pos()
	endif

	" TODO: if a:mode == "v", just count in selection
	if a:mode == "v"
		normal gv
		let cnttype = "selection"
	else
		let cnttype = "file"
	endif

	" (approach by Piet Delport)
	execute "silent normal! g\<C-G>"
	if a:mode == "v"
		let lines = matchstr(v:statusmsg, '\zs\d\+\ze of \d\+ Line') + 0
		let words = matchstr(v:statusmsg, '\zs\d\+\ze of \d\+ Word') + 0
		let chars = matchstr(v:statusmsg, '\zs\d\+\ze of \d\+ Byte') + 0
	else
		let lines = matchstr(v:statusmsg, 'Line \d\+ of \zs\d\+') + 0
		let words = matchstr(v:statusmsg, 'Word \d\+ of \zs\d\+') + 0
		let chars = matchstr(v:statusmsg, 'Byte \d\+ of \zs\d\+') + 0
	endif
	" subtract NL/CRs
	if &fileformat == "dos"
		let chars = chars - (lines * 2)
	else
		let chars = chars - lines
	endif

	" word length comment
	if words > (chars / 5)
		let wordlencomment = "  (These words are shorter than average.)\n"
	elseif words < (chars / 5)
		let wordlencomment = "  (These words are longer than average.)\n"
	else
		let wordlencomment = ""
	endif

	" strings
	if     words == 0
		let strpages = ""
	elseif words < 250
		let strpages = "Less than one page\n"
	elseif words == 250
		let strpages = "Exactly one typical 250-word page\n"
	elseif words > 250 && words < 500
		let strpages = "Between one and two typical 250-word pages\n"
	else
		let avgwordm = Cream_format_number_commas(((chars / 5) / 250))
		let avgwordt = Cream_format_number_commas(((chars / 5) / 400))
		let strpages =  avgwordm . " average 250-word manuscript pages\n" .
					 \  avgwordt . " average 400-word technical pages\n"
	endif

	let str = Cream_format_number_commas(words) . " actual words\n" .
			\ Cream_format_number_commas((chars / 5)) . " average 5-letter words\n" .
			\ wordlencomment .
			\ "\n" .
			\ Cream_format_number_commas(chars) . " characters including spaces and tabs\n" .
			\ "\n" .
			\ strpages

	" display results
	let n = confirm(
		\ "This " . cnttype . " has:" . "\n" .
		\ "\n" .
		\ str .
		\ "\n", "Copy and Close\n&Close", 2, "Info")
	if n == 1
		let @+ = str
	endif

	if     a:mode == "i"
		" recall position
		execute mypos
	elseif a:mode == "v"
		normal gv
	endif

endfunction


" 1}}}
" Formatting and text handling
" String Handling {{{1

function! Cream_get_char_undercursor()
" return character under cursor
	silent! redir @c
	silent! ascii
	silent! redir END
	return @c[2]
endfunction

function! Cream_str_pad(str, len)
" Format {str} with preceeding spaces to length {len}.
	let str = a:str
	while strlen(str) < a:len
		let str = " " . str
	endwhile
	return str
endfunction

" Formatting {{{1

function! Cream_whitespace_trim_leading(mode, ...)
" default is current document
" (optional) argument, returns argument trimmed

	if     a:0 > 0
		return substitute(a:1, '\(^\|\n\)\zs\s\+', '', 'g')
	elseif a:mode == "v"
		normal gv
		normal "xy
		let @x = substitute(@x, '\(^\|\n\)\zs\s\+', '', 'g')
		" stupid hack to fix Vim
		if virtcol('.') == 1
			normal h
		endif

		normal gv
		normal "xp
	elseif a:mode == "i"
		let mypos = Cream_pos()
		execute ':%substitute/^\s\+//ge'
		execute mypos
	endif

endfunction

function! Cream_whitespace_trim_trailing(mode, ...)
" default is current document
" (optional) argument, returns argument trimmed

	if     a:0 > 0
		return substitute(a:1, '\s\+\ze\($\|\n\)', '', 'g')
	elseif a:mode == "v"
		normal gv
		normal "xy
		let @x = substitute(@x, '\s\+\ze\($\|\n\)', '', 'g')
		" stupid hack to fix Vim
		if virtcol('.') == 1
			normal h
		endif

		normal gv
		normal "xp
	elseif a:mode == "i"
		let mypos = Cream_pos()

		" test if sig line exists
		execute "0"
		if search('^-- $') > 0
			let flag = 1
		endif
		execute ':%substitute/\s\+$//ge'
		if exists("flag")
			" prompt user if should recover
			let n = confirm(
				\ "Email signature seperator lines (\"-- \") were detected in\n" .
				\ "this file. Would you like to recover their trailing space?" .
				\ "\n", "&Yes\n&No", 1, "Warning")
			if n == 1
				execute ':%substitute/^--$/-- /ge'
			endif
		endif

		execute mypos
	endif

endfunction

function! Cream_whitespace_trim(mode, ...)
" trim both leading and trailing whitespace
" default is current document
" (optional) argument, returns argument trimmed

	if     a:0 > 0
		let tmp = a:1
		let tmp = Cream_whitespace_trim_leading("i", tmp)
		let tmp = Cream_whitespace_trim_trailing("i", tmp)
		return tmp
	elseif a:mode == "v"
		normal gv
		normal "xy
		let @x = Cream_whitespace_trim_leading("i", @x)
		let @x = Cream_whitespace_trim_trailing("i", @x)
		" stupid hack to fix Vim
		if virtcol('.') == 1
			normal h
		endif

		normal gv
		normal "xp
	elseif a:mode == "i"
		call Cream_whitespace_trim_leading("i")
		call Cream_whitespace_trim_trailing("i")
	endif

endfunction

function! Cream_emptyline_collapse()
" collapse all empty lines to 1
	let mypos = Cream_pos()
	"silent! execute ':%substitute/\n\s*\n*$/\r/ge'
	"silent! execute ':%substitute/\n*$/\r/ge'
	silent! execute ':%substitute/\n\s\+$/\r/ge'
	silent! execute ':%substitute/\n\+$/\r/ge'
	execute mypos
endfunction

function! Cream_emptyline_delete()
" deletes all empty lines
	let mypos = Cream_pos()
	silent! execute ':%substitute/\n\s*\n*$//ge'
	execute mypos
endfunction

function! Cream_joinlines(mode)
" join selected lines
	if a:mode != "v"
		return
	endif
	normal gv
	let n = confirm(
		\ "Maintain spaces between each line?\n" .
		\ "\n", "&Yes\n&No\n&Cancel", 2, "Info")
	if     n == 1
		" preserve spaces
		normal J
	elseif n == 2
		" don't preserve spaces
		normal gJ
	else
		return
	endif
endfunction

function! Cream_vimscript_compress()
" Compress a Vim script by removing comments and leading whitespace.

	" verify vim filetype
	if &filetype != "vim"
		call confirm(
			\ "This isn't a Vim file. Quitting...\n" .
			\ "\n", "&Ok", 1, "Info")
		return
	endif

	" confirm destruction
	let n = confirm(
		\ "Strip comments and leading/trailing whitespace?\n" .
		\ "\n", "&Ok", 1, "Info")
	if n != 1
		return
	endif

	" strip whitespace
	call Cream_whitespace_trim("i")
	" delete lines beginning with a comment
	silent %substitute/^".*$//ge
	" delete empty lines
	call Cream_emptyline_delete()

endfunction

" File Format {{{1

function! Cream_fileformat(...)
" set &fileformat based on argument:
"   dos   <CR><NL>
"   unix      <NL>
"   mac   <CR>

	" get current file format
	let ffold = &fileformat

	" bypass dialog to get desired fileformat if argument passed
	if a:0 == 1
		let ffnew = a:1
		" don't mod file if already correct
		if ffnew ==? ffold
			return
		endif
	else

		" set default choice
		if     ffold == "unix"
			let mychoice = 1
		elseif ffold == "dos"
			let mychoice = 2
		elseif ffold == "mac"
			let mychoice = 3
		else
			let mychoice = 4
		endif

		" change to new
		let n = confirm(
			\"Convert file from current " . toupper(ffold) . " format to...\n",
			\"&Unix\n&DOS/Windows\n&Apple\n&Cancel", mychoice, "Question")
		if     n == 1
			let ffnew = "unix"
		elseif n == 2
			let ffnew = "dos"
		elseif n == 3
			let ffnew = "mac"
		else
			return
		endif

	endif

	" change current buffer's fileformat based on selection
	if     ffnew ==? "unix"
		set fileformat=unix
	elseif ffnew ==? "dos"
		set fileformat=dos
		execute ":%substitute/" . nr2char(13) . "$//ge"
	elseif ffnew ==? "mac"
		set fileformat=mac
	endif

endfunction

" Encoding {{{1

" Cream_filetypes_list()

function! Cream_convert_encoding(encoding)
" convert the current document's encoding to that passed
" Example:  :call ConvertEncoding("latin1")

	"".................................................................
	"" method 1

	"" select all
	"normal gg
	"normal gH
	"execute "normal \<C-g>"
	"normal G

	"" cut
	"normal "xx

	"" convert
	"let @x = iconv(@x, &encoding, a:encoding)

	"" change document's encoding (must follow above)
	"execute "set encoding=" . a:encoding

	"" paste back the conversion
	"normal "xP

	".................................................................
	" method 2

	execute "set fileencoding=" . a:encoding

endfunction

function! Cream_fileencoding_set(encoding)
" see :help encoding-values

	execute "set encoding=" . a:encoding
	execute "set fileencoding=" . a:encoding

	" reload keymaps
	call Cream_source($CREAM . "cream-keys.vim")
	" reload menus
	call Cream_menus()
	call MRURefreshMenu()
	call BMShow()

endfunction

" Tabs (&expandtab) {{{1

function! Cream_expandtab_init()

	if exists("g:CREAM_EXPANDTAB") && g:CREAM_EXPANDTAB == 1
		set expandtab
	else
		set noexpandtab
		let g:CREAM_EXPANDTAB = 0
	endif

endfunction

function! Cream_expandtab_toggle(mode)

	if exists("g:CREAM_EXPANDTAB")
		if g:CREAM_EXPANDTAB == 1
			set noexpandtab
			let g:CREAM_EXPANDTAB = 0
		else
			set expandtab
			let g:CREAM_EXPANDTAB = 1
		endif
	else
		" error
	endif

	call Cream_menu_settings_expandtab()

	if a:mode == "v"
		normal gv
	endif

endfunction

" Retab {{{1

function! Cream_retab()
" replace all existing tabs with spaces according to current tabstop settings

	" save expandtab
	let myexpandtab = &expandtab
	" turn on
	set expandtab

	silent! retab
	" restore
	let &expandtab = myexpandtab

endfunction

" 1}}}
" Motion, selection
" Positioning {{{1

function! Cream_pos(...)
" return current position in the form of an executable command
" Origins: Benji Fisher's foo.vim, available at
"          http://vim.sourceforge.net

	"let mymark = "normal " . line(".") . "G" . virtcol(".") . "|"
	"execute mymark
	"return mymark

	" current pos
	let curpos = line(".") . "G" . virtcol(".") . "|"

	" mark statement
	let mymark = "normal "

	" go to screen top
	normal H
	let mymark = mymark . line(".") . "G"
	" go to screen bottom
	normal L
	let mymark = mymark . line(".") . "G"

	" go back to curpos
	execute "normal " . curpos

	" cat top/bottom screen marks to curpos
	let mymark = mymark . curpos

	execute mymark
	return mymark

endfunction

" Motion {{{1

function! Cream_motion_doctop()
	normal gg0
endfunction
function! Cream_motion_docbottom()
	normal G$
endfunction

function! Cream_motion_windowtop()
	normal H
endfunction
function! Cream_motion_windowbottom()
	normal L
endfunction
function! Cream_motion_windowmiddle()
	normal M
endfunction

function! Cream_delete_v()
" <BS> deletes selection (in Visual mode)
	normal gvd
endfunction

function! Cream_motion_home()
" Original Concept: http://www.derwok.de/downloads/index.html
	" get current column
	let oldcol = col('.')
	" go to first non-white
	normal g^
	" get current column, new
	let newcol = col('.')
	" if already were at first-non-white, toggle
	if (oldcol == newcol)
		" if we're on the first line, stop, otherwise toggle
		if (&wrap == 1) && (newcol > Cream_linewidth())
			" stop
		else
			"execute "normal gI\<C-o>"
			normal gI
			let lastcol = col('.')
			" already were at first col?
			if (newcol == lastcol)
				" fix being stuck on second column if a one character line
				if (newcol == oldcol)
					" terminate, ensure at beginning
					normal i
				else
					" toggle back to first non-white
					normal g^
				endif
			else
				" toggle to first col
				"execute "normal gI\<C-o>"
				normal gI
			endif
		endif
	endif
endfunction

function! Cream_linewidth()
" calculate text width (not &columns, winwidth(0), col('$'),
" virtcol('$') according to state of signs, fold columns, and line
" numbers.

	let foldcolumn = &foldcolumn
	if Cream_ShowMarks_Exist() == 1
		let signs = 2
	else
		let signs = 0
	endif
	if &number == 1
		let number = 8
	else
		let number = 0
	endif

	return winwidth(0) - signs - foldcolumn - number

endfunction

function! Cream_map_end()

	" get current pos
	let oldcol = virtcol('.')
	" go to last screen column
	normal g$

	" Hack: fix that <C-b> doesn't allow ending up after last char
	" with wrap off (2004-04-23)
	if &wrap == 0
		" if line is shorter than screen
		if virtcol('$') < Cream_linewidth()
			" fix position
			normal $
		" if we were already at screen line end
		elseif oldcol >= virtcol('.')
			" move farther
			normal $
		else
			" nothing, first g$ did what we wanted
		endif
	else
		" if already at screen end, go to line end
		if oldcol >= virtcol('.')
			normal $
		endif
	endif

endfunction

function! Cream_up(mode)
	" if popup menu is visible, move in menu
	if pumvisible()
		return "\<C-p>"
	endif
	if     a:mode == "i"
		normal gk
	elseif a:mode == "v"
		"......................................................................
		" option 1
		" don't reselect, we've already got one!
		normal gv
		" get positions
		let lin1 = line(".")
		let col1 = virtcol(".")
		let pos1 = Cream_pos()
		normal o
		let lin2 = line(".")
		let col2 = virtcol(".")
		let pos2 = Cream_pos()
		normal o
		" drop to normal mode
		execute "normal \<C-\>\<C-n>"
		" adjust only if multi-line
		if     lin1 > lin2
			execute pos2
		elseif lin1 < lin2
			execute pos1
		elseif col1 > col2
			execute pos2
		elseif col2 > col1
			execute pos1
		endif
		"......................................................................
		" option 2
	endif
endfunction

function! Cream_down(mode)
	" if popup menu is visible, move in menu
	if pumvisible()
		return "\<C-n>"
	endif
	if     a:mode == "i"
		normal gj
	elseif a:mode == "v"
		" don't reselect, we've already got one!
		normal gv
		" get positions
		let lin1 = line(".")
		let col1 = virtcol(".")
		let pos1 = Cream_pos()
		normal o
		let lin2 = line(".")
		let col2 = virtcol(".")
		let pos2 = Cream_pos()
		normal o
		" drop to normal mode
		execute "normal \<C-\>\<C-n>"
		" adjust only if multi-line
		if     lin1 < lin2
			execute pos2
		elseif lin1 > lin2
			execute pos1
		elseif col1 > col2
			execute pos1
		elseif col2 > col1
			execute pos2
		endif
	endif
endfunction


function! Cream_scroll_down(...)
" scroll screen up one line but maintain cursor position

	" if visual mode (argument "v")
	if a:0 != 0
		if a:1 == "v"
			" reselect selection
			normal gv
			" move left one (bug fix) and scroll
			execute "normal \<Left>\<C-e>"
			" re-select
			normal gv
		else
			call confirm(
				\ "Error: Invalid argument passed to Cream_scroll_down(). Quitting...\n" .
				\ "\n", "&Ok", 1, "Info")
			return
		endif
	" if insert mode
	else
		" scroll up, go down one
		execute "normal \<C-e>\<Down>"
	endif
endfunction

function! Cream_scroll_up(...)
	"execute "normal \<Left>\<C-y>"
" scroll screen down one line but maintain cursor position

	" if visual mode (argument "v")
	if a:0 != 0
		if a:1 == "v"
			" reselect selection
			normal gv
			" move left one (bug fix) and scroll
			execute "normal \<Left>\<C-y>"
			" re-select
			normal gv
		else
			call confirm(
				\ "Error: Invalid argument passed to Cream_scroll_down(). Quitting...\n" .
				\ "\n", "&Ok", 1, "Info")
			return
		endif
	" if insert mode
	else
		" scroll up, go down one
		execute "normal \<C-y>\<Up>"
	endif
endfunction

function! Cream_pageup()
" PageUp -- ensure motion to top if in first page
	" NOTE: we use Ctrl+U instead of Ctrl+B since that became our key
	" to the universe!
	execute "normal \<C-u>\<C-u>"
	normal H
endfunction

function! Cream_pagedown()
" PageUp -- ensure motion to top if in first page
	execute "normal \<C-f>"
	normal L
endfunction

" Selection {{{1

function! Cream_select_all(...)
" Notes: An optional argument "endvisual" can be passed to force the
" function to end in visual mode and prevent the function from
" returning to select mode.

	" important: drop existing selection if current
	normal i

	normal gg
	normal gH
	" change to visual mode
	execute "normal \<C-g>"
	normal G
	" return to select if we weren't passed an argument or the
	" argument isn't "endvisual"
	if  a:0 == 0
	\|| a:0 > 1 && a:1 ==? "endvisual"
		" change to select mode
		execute "normal \<C-g>"
	endif

endfunction

function! Cream_map_shift_home(mode)
" toggles selection back and forth between first column and first
" non-whitespace column
" * Requires arguement "i" or "v" (for Insert or Visual mode)

	if a:mode == "i"
		normal v
		normal g^
		" go to other end of selection
		normal o
		" while not at last insert position
		while col(".") < col("'^")
			" go right (usually only once)
			normal l
		endwhile
		" go back to front end of selection
		normal o
		" change to select mode
		execute "normal \<C-G>"
	elseif a:mode == "v"
		" reselect
		normal gv
		" capture position
		let oldcol = col(".")
		" go to first screen line position
		normal g0
		" get current column, new
		let newcol = col(".")
		" already were at first column
		if (oldcol == newcol)
			" back up to first char
			normal g^
		endif
	endif

endfunction
function! Cream_map_shift_end()
	" get current column
	let oldcol = col(".")
	" go to last non-white
	normal v
	normal g$
	" get current column, new
	let newcol = col(".")
	" already were at last non-white?
	if (oldcol == newcol)
		" go to first col
		normal $
		let lastcol = col(".")
		" already were at last col?
		if (newcol == lastcol)
			" go back to last non-white
			normal g$
		else
			" go to last col
			normal $
		endif
	endif

	" we want select mode if still in visual so typing replaces
	if mode() == "v"
		execute "normal \<C-g>"
	endif

endfunction

function! Cream_map_shift_up(mode)
" accepts mode argument "i" or "v"

	if a:mode == "i"
		" this is a huge hack to remedy the fact that "normal vgk" omits
		" selection of the final char if beginning from the last pos of
		" a line, mostly because of the <C-b> intro.
		normal v
		normal gk
		normal gj
		normal o
		normal gk

		" we want select mode if still in visual so typing replaces
		if mode() == "v"
			execute "normal \<C-G>"
		endif
	elseif a:mode == "v"
		" reselect
		normal gv
		normal gk
	endif

endfunction
function! Cream_map_shift_down(mode)
" accepts mode argument "i" or "v"

	if a:mode == "i"
		" same hack as Cream_map_shift_up()
		normal v
		normal gj
		normal gk
		normal o
		normal gj

		" we want select mode if still in visual so typing replaces
		if mode() == "v"
			execute "normal \<C-g>"
		endif
	elseif a:mode == "v"
		normal gv
		normal gj
	endif

endfunction


function! Cream_map_shift_pageup(mode)
" accepts mode argument "i" or "v"

	if     a:mode == "i"
		normal vH
	elseif a:mode == "v"
		" reselect
		normal gv
		execute "normal \<C-u>"
	endif

endfunction
function! Cream_map_shift_pagedown(mode)
" accepts mode argument "i" or "v"

	if     a:mode == "i"
		normal vL
	elseif a:mode == "v"
		" reselect
		normal gv
		execute "normal \<C-d>"
	endif

endfunction

function! Cream_visual_swappos(direction)
" from visual mode, go to end of selection specified
" * arguments "up" and "dn"

	normal gv
	" get pos
	let myline = line('.')
	let mycol  = col('.')
	" swap
	normal o
	" get pos, new
	let mylinenew = line('.')
	let mycolnew  = col('.')

	if     a:direction ==? "up"
		if     myline > mylinenew

		elseif myline < mylinenew
			" swap back (original was less)
			normal o
		" equal
		else
			" check columns (selection only on one line)
			if mycol < mycolnew
				normal o
			endif
		endif
	elseif a:direction ==? "dn"
		if     myline < mylinenew

		elseif myline > mylinenew
			" swap back (original was less)
			normal o
		" equal
		else
			" check columns (selection only on one line)
			if mycol > mycolnew
				normal o
			endif
		endif
	endif

endfunction

" Replace mode {{{1

" Note: This is botched by the Ctrl+B key to the universe.
"""function! Cream_replacemode()
"""" used by <Insert> key to replace tabs without moving positions
"""
"""    " initialize
"""    if !exists("g:cream_replacemode")
"""        let g:cream_replacemode = 1
"""    " toggle state
"""    else
"""        if g:cream_replacemode == 1
"""            let g:cream_replacemode = 0
"""        else
"""            let g:cream_replacemode = 1
"""        endif
"""    endif
"""
"""    " condition based on state
"""    if g:cream_replacemode == 1
"""        " start replace mode
"""        if &modified == 1
"""            return "x\<C-\>\<C-n>gR\<Del>"
"""        else
"""            " need to compensate for the insert x pos stuff...
"""            return "x\<C-\>\<C-n>gR\<Del>\<C-b>:set nomodified\<CR>gR"
"""        endif
"""    else
"""        " end replace mode
"""        return "\<C-b>i"
"""    endif
"""
"""endfunction

function! Cream_replacemode_demap()
	execute "imap <silent> <Insert> <C-b>:call Cream_replacemode_remap()<CR><C-b>gR"
endfunction
function! Cream_replacemode_remap()
	execute "imap <silent> <Insert> <C-b>:call Cream_replacemode_demap()<CR><C-b>i"
endfunction


"1}}}
" Navigationy things
" Tags {{{1

" Tag jumping
function! Cream_tag_backward()
" go to previous tag
	if !Cream_has("tagsfile")
		if !Cream_has("tags")
			call s:Cream_tag_warn()
		endif
		return
	endif
	"if exists("g:CREAM_TABPAGES") && g:CREAM_TABPAGES == 1
	"    silent! tab pop
	"else
		silent! pop
	"endif
	call Cream_window_setup()
endfunction

function! Cream_tag_forward()
" go to next tag forward
	if !Cream_has("tagsfile")
		if !Cream_has("tags")
			call s:Cream_tag_warn()
		endif
		return
	endif
	"if exists("g:CREAM_TABPAGES") && g:CREAM_TABPAGES == 1
	"    silent! tab tag
	"else
		silent! tag
	"endif
	call Cream_window_setup()
endfunction

function! Cream_tag_goto()
" go to tag under cursor
	let bufnr = bufnr("%")
	let curpos = line(".") . "G" . virtcol(".")
	if !Cream_has("tagsfile")
		if !Cream_has("tags")
			call s:Cream_tag_warn()
		else
			call confirm(
			\ "No tags file found.\n" .
			\ "(For more information on tags, see http://ctags.sf.net.)\n" .
			\ "\n", "&Ok", 1, "Info")
		endif
		return
	endif
	let mytag = expand("<cword>")
	"if exists("g:CREAM_TABPAGES") && g:CREAM_TABPAGES == 1
	"    execute "silent! tab tag " . mytag
	"else
		execute "silent! tag " . mytag
	"endif
	" see if we moved
	if bufnr("%") == bufnr
		if curpos == line(".") . "G" . virtcol(".")
			" warn no tag found under cursor
			call confirm(
				\ "No tag found under cursor.\n" .
				\ "\n", "&Ok", 1, "Info")
		endif
	else
		" only do this if buffer number changed
		call Cream_window_setup()
	endif
endfunction

function! s:Cream_tag_warn()
	call confirm(
		\ "This feature requires a working installation of ctags.\n" .
		\ "Please see http://ctags.sf.net for more details. \n" .
		\ "\n", "&Ok", 1, "Info")
endfunction

function! Cream_tag_backclose()
" close and go back

	if !Cream_has("tagsfile") &&
	\ getbufvar("%", "&filetype") != "help"
		return
	endif

	" get current buffer
	let mybufno = bufnr("%")

	"" if un-modified, delete buffer
	"if !&modified
	"    " TODO: This area looks suspiciously duplicate of file
	"    " functions elsewhere...
	"    " unnamed file?
	"    if Cream_buffer_isnewunmod() == 1
	"        let mychoice = confirm("Save changes as a new document?", "&Yes\n&No\n&Cancel", 1)
	"    else
	"        let mychoice = confirm("Save changes to \"" . expand("%:p") . "\"?", "&Yes\n&No\n&Cancel", 1)
	"    endif
	"    if mychoice == 1
	"        " if an unnamed file, saveas first
	"        if Cream_buffer_isnewunmod() == 1
	"            " browse confirm w
	"            let n = browse("", "", getcwd(), "")
	"            if n = ""
	"                return
	"            endif
	"        else
	"            write
	"        endif
	"    elseif mychoice == 2
	"        " no: don't save changes!
	"    else
	"        " cancel or error: do nothing
	"        return
	"    endif
	"endif

	" TODO: if the tag is in the same file, we close our current file!

	" go back (in same window)
	silent! pop
	" remember
	let bufnr = bufnr("%")

	" If tag and jump are different files, handle separate tabs.
	" Otherwise, we're done.
	if mybufno != bufnr
		" come back
		silent! tag
		" now go to tab containing that buffer
		call Cream_tab_goto(bufnr)
		" go back to tag
		silent! pop

		" delete referenced buffer if not this file
		if mybufno != bufnr("%")
			" close only if unmodified
			if getbufvar(mybufno, "&modified") == 0
				execute "bwipeout! " . mybufno
			endif
			" refresh buffer list
			call BMShow()
		endif

	endif

endfunction

function! Cream_Tlist_toggle()
" window management wrapper for :Tlist

	" test if open
	let i = 1
	while i <= bufnr('$')
		if bufname(i) == "__Tag_List__"
			let open = 1
		endif
		let i = i + 1
	endwhile

	" open if not
	if !exists("open")

		"let s:tlist_part_of_winmanager = 1
		"let g:tlist_bufhidden = 1

		if !Cream_has("tags")
			call s:Cream_tag_warn()
			return
		endif

		" notify external window management
		call Tlist_Set_App("cream")

		"call Tlist_Update_File_Tags(fnamemodify(expand("%"), ":p"), &filetype)

		" call command, not function!
		execute ":Tlist"

		" remember what buffer we're in (not window! it will change)
		let mybufnr = bufnr("%")
		" reset window configuration
		call Cream_window_setup()
		" restore cursor to original current buffer's new window
		call MoveCursorToWindow(bufwinnr(mybufnr))

	" close it
	else
		" remember current buffer/window
		let mybufnr = bufnr("%")
		let i = 1
		while i <= bufnr('$')
			if bufname(i) == "__Tag_List__"
				" close
				execute "bwipeout! " . i
				break
			endif
			let i = i + 1
		endwhile
		" return to orig buffer's window
		let mywinnr = bufwinnr(bufname(mybufnr))
		if mywinnr > 0
			call MoveCursorToWindow(mywinnr)
		endif

	endif

endfunction

function! Cream_Tlist_prototype()
" Return the current function's prototype without opening the list
" window.

	" if list doesn't exist, initialize
	"if !exists("b:tlist_tag_count")
	"\|| b:tlist_tag_count == 0
	"    " regenerate tags
	"    let b:tlist_tag_count = 0
	"    call setbufvar(bufnr("%"), 'tlist_sort_type', 'order')
	"    call s:Tlist_Process_File(fnamemodify(expand("%"), ":p"), &filetype)
	"endif

	call Tlist_Update_File_Tags(fnamemodify(expand("%"), ":p"), &filetype)

	"let g:Tlist_Process_File_Always = 1

	return Tlist_Get_Tag_Prototype_By_Line()

endfunction

" Goto {{{1

function! Cream_goto()
" go to line number or % of file

	let m = 1
	while m == 1

		" set default empty, unless repeating
		if !exists("n")
			let n = ""
		endif
		let n = inputdialog("Enter line number to go to.\n(End with \"%\" to go to percentage in file.)", n)
		" remove spaces
		let n = substitute(n, " ", "", "g")
		" validate
		" invalid if contains anything but number and "%"
		if match(n, "[^0-9%]") > -1
			let m = confirm("Only numbers and trailing \"%\" allowed.", "&Ok\n&Cancel", 1, "Info")
			if m != 1
				return
			endif
		else
			break
		endif

	endwhile
	" if contains "%" go to percentage
	if match(n, "%") != -1
		" invalid if "%" not at end
		if strpart(n, strlen(n) - 1, 1) != "%"
			call confirm("\"%\" must be last character.", "&Ok", 1, "Info")
			return
		else
			" goto percent
			execute "normal " . n . "%"
		endif
	else
		" goto line number
		execute "normal :" . n . "\<CR>"
	endif

endfunction

" Folding {{{1

function! Cream_fold(direction)
" fold toggle (or go to next above/below)

	let myfold = foldlevel(line('.'))
	" in fold, toggle
	if myfold != 0
		normal za
	" not in fold
	else
		" get initial pos
		let myline = line(".")
		" go up to previous ("up" argument)
		if a:direction == "up"
			" move to end of previous fold
			normal zk
		" go down to next
		else
			" move to start of next fold
			normal zj
		endif
		" if no movement, report
		if myline == line(".")
			call confirm(
			\ "This document contains no folds. To use the folding feature:\n" .
			\ "\n" .
			\ "* Select text and press F9 to create a fold.\n" .
			\ "* Use Alt+F9 to clear a fold at the cursor.\n" .
			\ "\n" .
			\ "See the help document \"Keyboard Shortcuts\" at F9 for additional folding features. \n" .
			\ "\n", "&Ok", 1, "Info")
		endif
	endif

endfunction

function! Cream_fold_set(mode)
	if a:mode != "v"
		return
	endif
	normal gv

	" Note: We'd love to remove foldmethod=marker here and restore on
	" the other side, but it doesn't seem to work.

	normal zf

	" why would we re-select? Don't.
	"normal gv
endfunction

function! Cream_fold_openall()
	normal zr
endfunction

function! Cream_fold_closeall()
	normal zM
endfunction

function! Cream_fold_delete()
	normal zd
endfunction

function! Cream_fold_deleteall()
	normal zE
endfunction

function! Cream_fold_init()
" called by autocmd VimEnter to automatically open the current fold

	if foldlevel(line('.')) > 0
		let mypos = Cream_pos()
		normal za
		execute mypos
	endif

endfunction

" Backspace {{{1

function! Cream_map_key_backspace(...)
" * Shift+Backspace deletes word (see help for difference between
"   "word" and "WORD")
" * Ctrl+Backspace deletes WORD

	" this is stupid. Vim can't keep track of position when at line end
	let oldcol = col('.')
	let colend = col('$')
	let oldline = line('.')

	if exists("a:1")
		if a:1 == "WORD"
			let myword = "W"
		else
			let myword = "w"
		endif
	else
		let myword = "w"
	endif

	" select previous word
	if oldcol == 1
	" 1. at line beginning
		normal X
	else
		normal g^
		" if we're already at the beginning of a line
		if oldcol == col('.')
"call confirm("at begin/white", "&Ok")
			" 2. delete whitespace to beginning of line
			normal h
			execute "normal \<S-Home>"
			execute "normal \<Del>"
			" delete
			normal x
			execute "normal X"
		else
			" go back to original position
			while col('.') < oldcol
				execute "normal l"
			endwhile
			"*** evaluation fails, we never get here :(
			"	" can't detect this (stupid <C-b>/<C-\><C-n> position loss at line end)
			"	if oldcol == colend
			"		"call confirm("else, end", "&Ok")
			"		" 3. at line end
			"		" end of line hoses
			"		normal l
			"		while line('.') > oldline
			"			normal h
			"		endwhile
			"		execute "normal hva" . myword
			"		" delete
			"		normal x
			"	else
			"***
			"call confirm("else, NOT end", "&Ok")
			" 4. everywhere else
			execute "normal va" . myword
			" delete
			normal x
		endif
	endif

endfunction

" 1}}}
" Search
" Find/Replace {{{1

" Note: Both these functions are pointless:
" o We map directly to :promptfind and :promptrepl to preserve their
"   multi-modal nature.
" o :promptfind and :promptrepl appear to use '\V' by default, as
"   discussed in list thread on 2004-10-27.
function! Cream_find_native()
" Dialog Find, using Vim's native find dialog.
	let mymagic = &magic
	set nomagic
	"try
		silent! promptfind
	"    catch /^Vim\%((\a\+)\)\=:E486/
	"        call confirm(
	"            \ "Pattern not found.\n" .
	"            \ "\n", "&Ok", 1, "Info")
	"    finally
	"        call confirm(
	"            \ "Pattern found.\n" .
	"            \ "\n", "&Ok", 1, "Info")
	"endtry
	let &magic = mymagic
endfunction

function! Cream_replace_native()
" Dialog Find, using Vim's native find dialog.
	let mymagic = &magic
	set nomagic
	silent! promptrepl
	let &magic = mymagic
endfunction

" Find Under Cursor {{{1
" Note: From visual mode, selection is extended to next find.

function! Cream_findunder(mode)
	let myincsearch = &incsearch
	set noincsearch
	if a:mode == "v"
		normal gv
		normal "xy
		" test if exists
		if search(@x) != 0
			" highlight it
			normal v
			let i = 0
			while i < strlen(@x)
				normal l
				let i = i + 1
			endwhile
		endif
	else
		silent normal *
	endif
	let &incsearch = myincsearch
endfunction

function! Cream_findunder_reverse(mode)
	if a:mode == "v"
		normal gv
		normal "xy
		" test if exists
		if search(@x, "b") != 0
			" highlight it
			normal v
			let i = 0
			while i < strlen(@x)
				normal l
				let i = i + 1
			endwhile
		endif
	else
		silent normal #
	endif
endfunction

function! Cream_findunder_case(mode)
	let myhls = &hlsearch
	let myic = &ignorecase
	set hlsearch
	set noignorecase
	if a:mode == "v"
		normal gv
		normal "xy
		" test if exists
		if search(@x) != 0
			" highlight it
			normal v
			let i = 0
			while i < strlen(@x)
				normal l
				let i = i + 1
			endwhile
		endif
	else
		silent normal *
	endif
	let &ignorecase = myic
	let &hlsearch = myhls
endfunction

function! Cream_findunder_case_reverse(mode)
	let myhls = &hlsearch
	let myic = &ignorecase
	set hlsearch
	set noignorecase
	if a:mode == "v"
		normal gv
		normal "xy
		" test if exists
		if search(@x, "b") != 0
			" highlight it
			normal v
			let i = 0
			while i < strlen(@x)
				normal l
				let i = i + 1
			endwhile
		endif
	else
		silent normal #
	endif
	let &ignorecase = myic
	let &hlsearch = myhls
endfunction

" Search Highlighting {{{1

" search highlighting
function! Cream_search_highlight_init()
" initialize search highlighting
	if !exists("g:CREAM_SEARCH_HIGHLIGHT")
		" initially off
		set nohlsearch
		let g:CREAM_SEARCH_HIGHLIGHT = 0
	else
		if g:CREAM_SEARCH_HIGHLIGHT == 1
			set hlsearch
		else
			set nohlsearch
		endif
	endif
endfunction
function! Cream_search_highlight_toggle()
" toggle search highlighting
	if exists("g:CREAM_SEARCH_HIGHLIGHT")
		if g:CREAM_SEARCH_HIGHLIGHT == 0
			set hlsearch
			let g:CREAM_SEARCH_HIGHLIGHT = 1
		elseif g:CREAM_SEARCH_HIGHLIGHT == 1
			set nohlsearch
			let g:CREAM_SEARCH_HIGHLIGHT = 0
		endif
	else
		call Cream_error_warning("Error: global uninitialized in Cream_search_highlight_toggle()")
	endif
	call Cream_menu_settings_highlightsearch()
endfunction
function! Cream_search_highlight_reset()
" Reset search highlight to nothing.
	" TODO: Vim bug, this won't work from within a function.
	nohlsearch
endfunction

" 1}}}
" Wrap
" Word Wrap (Line wrap; wrap words at window margin)  {{{1
" * By default, wrap is on, global variable g:CREAM_WRAP remembers session to
"   session
" * Potentially, wrap could be turned on globally after being turned off in a
"   buffer and states could get confused. Account for both.

function! Cream_wrap_init()
" initialize wrap (for statusline)

	" don't change state in special buffers
	" opsplorer
	if bufname("%") == "_opsplorer"
		return
	endif
	" calendar
	if bufname("%") == "__Calendar"
		return
	endif
	" tag list
	if bufname("%") == "__Tag_List__"
		return
	endif

	" used before
	if exists("g:CREAM_WRAP")
		if g:CREAM_WRAP == 1
			set wrap
			set guioptions-=b
		else
			set nowrap
			"execute "set lines=" . (&lines - 2)
			set guioptions+=b
		endif
	else
		" initialize on
		set wrap
		set guioptions-=b
		let g:CREAM_WRAP = 1
	endif

endfunction

function! Cream_wrap(mode)
" toggle word wrap

	if g:CREAM_WRAP == 1
		let g:CREAM_WRAP = 0
		set nowrap
		" save position on Windows
		if Cream_has("ms")
			call Cream_screen_get()
		endif
		" make window shorter
		execute "set lines=" . (&lines - 2)
		" add horiz. scroll bar
		set guioptions+=b
		" restore position on Windows
		if Cream_has("ms")
			call Cream_screen_init()
		endif
	else
		let g:CREAM_WRAP = 1
		set wrap
		" save position on Windows
		if Cream_has("ms")
			call Cream_screen_get()
		endif
		" make window longer
		execute "set lines=" . (&lines + 2)
		" remove horiz. scroll bar
		set guioptions-=b
		" restore position on Windows
		if Cream_has("ms")
			call Cream_screen_init()
		endif
	endif
	call Cream_menu_settings_wordwrap()

	if a:mode == "v"
		normal gv
	endif

endfunction

" Auto Wrap (break lines automatically while typing) {{{1

function! Cream_autowrap_init()
" initialize autowrap (for statusline)

	" don't change state in special buffers
	" opsplorer
	if bufname("%") == "_opsplorer"
		return
	endif
	" calendar
	if bufname("%") == "__Calendar"
		return
	endif

	" initialize wrap width
	if !exists("g:CREAM_AUTOWRAP_WIDTH")
		let g:CREAM_AUTOWRAP_WIDTH = 80
	endif

	" used before
	if exists("g:CREAM_AUTOWRAP")
		if g:CREAM_AUTOWRAP == 1
			execute "set textwidth=" . g:CREAM_AUTOWRAP_WIDTH
			set expandtab
			let g:CREAM_EXPANDTAB = 1
		else
			set textwidth=0
			set noexpandtab
			let g:CREAM_EXPANDTAB = 0
		endif
	else
		" initialize off (use wrap settings)
		set textwidth=0
		set noexpandtab
		let g:CREAM_AUTOWRAP = 0
		let g:CREAM_EXPANDTAB = 0
	endif
endfunction

function! Cream_autowrap(mode)
" toggle autowrap
" * Autowrap is currently global (buffer specific will mean a global for each buffer!)
" * Dependent upon formatoptions-=tcrqn being set elsewhere!

	if g:CREAM_AUTOWRAP == 1
		let g:CREAM_AUTOWRAP = 0
		set textwidth=0
		set noexpandtab
		let g:CREAM_EXPANDTAB = 0
	else
		let g:CREAM_AUTOWRAP = 1
		execute "set textwidth=" . g:CREAM_AUTOWRAP_WIDTH
		set expandtab
		let g:CREAM_EXPANDTAB = 1
	endif
	call Cream_menu_settings_autowrap()

	if a:mode == "v"
		normal gv
	endif

endfunction

" Wrap Width {{{1
function! Cream_autowrap_setwidth()
" Dialog to set g:CREAM_AUTOWRAP_WIDTH (textwidth)
" NOTE: g:CREAM_AUTOWRAP_WIDTH is initialized at Cream_autowrap_init()
" o Turns autowrap on (or resets) if used

	let n = Inputdialog("Set new wrap width for autowrap (when on):", g:CREAM_AUTOWRAP_WIDTH)
	" do nothing on cancel
	if     n == "{cancel}"
		return
	" warn only numbers allowed
	elseif match(n, '[^0-9]') != -1
		call confirm(
			\ "Only numbers are allowed.\n" .
			\ "\n", "&Ok", 1, "Error")
	" warn can not set to 0
	elseif n == 0
		call confirm(
			\ "Can not set to 0.", "&Ok", 1)
	" set
	else
		let g:CREAM_AUTOWRAP_WIDTH = n + 0
		" turn on
		let g:CREAM_AUTOWRAP = 1
		execute "set textwidth=" . g:CREAM_AUTOWRAP_WIDTH
		set expandtab
		" update column highlighting
		" NOTE: Super hack. We do this twice since this feature uses a
		" simple match (not a syntax highlighting group) so it must be
		" toggled on and off to reset.
		call Cream_highlight_columns(g:CREAM_AUTOWRAP_WIDTH)
		call Cream_highlight_columns(g:CREAM_AUTOWRAP_WIDTH)
	endif

endfunction

" Quick Wrap (Re-format existing text) {{{1

function! Cream_quickwrap(mode)
" re-formats text

	" remember
	let mytextwidth = &textwidth
	let myautoindent = &autoindent
	let mysmartindent = &smartindent
	let myexpandtab = &expandtab

	" sets
	execute "set textwidth=" . g:CREAM_AUTOWRAP_WIDTH
	set autoindent
	set nosmartindent
	set expandtab
	let mypos = Cream_pos()

	" select
	if     a:mode ==? "i"
		" select inner paragraph
		normal vip
		"" go to end (simplifies calcs)
		"normal '>
		" get range ( marks "'<" and "'>" are pre-function!)
		let myfirstline = line(".")
		normal o
		let mylastline = line(".")
		normal o
	elseif a:mode ==? "v"
		normal gv
		let myfirstline = line("'<")
		let mylastline = line("'>")
	else
		return -1
	endif

	" flipped, put first range value first
	if mylastline < myfirstline
		let tmp = myfirstline
		let myfirstline = mylastline
		let mylastline = tmp
	else
		if virtcol('.') == 1
			let atend = 1
		endif
	endif

	" initialize justify (instance specific)
	if !exists("g:cream_justify")
		let g:cream_justify = "left"
	endif

	" condition wrap of content with numbered list
	" retain &formatoptions
	let myformatoptions = &formatoptions
	" Note: we still have a selection... yank it to register
	normal "xy
	normal gv
	if match(@x, '^[ \t]*\d\+[\.\:\)\]\}]\{1}[ \t]\+') != -1
		set formatoptions+=n
	else
		set formatoptions-=n
	endif
	" clear register
	let @x = ""

	" wrap
	if     g:cream_justify ==? "left"
		call s:Cream_wrap_left(myfirstline . "," . mylastline)
	elseif g:cream_justify ==? "center"
		call s:Cream_wrap_center(myfirstline . "," . mylastline)
	elseif g:cream_justify ==? "right"
		call s:Cream_wrap_right(myfirstline . "," . mylastline)
	elseif g:cream_justify ==? "full"
		execute myfirstline . "," . mylastline . "call Justify('tw', 3)"
	"else
	"    call confirm(
	"        \ "Error: Incorrect justification value in Cream_quickwrap()\n" .
	"        \ "\n", "&Ok", 1, "Info")
	"    return
	endif

	" restore &formatoptions
	let &formatoptions = myformatoptions

	" recover
	if a:mode ==? "i"
		"*** BROKEN:
		"startinsert
		"normal i
		"***
		execute "normal \<Esc>"
		" position
		execute mypos
	else
		execute mypos
		" selection
		normal gv
	endif

	" restore
	let &textwidth = mytextwidth
	let &autoindent = myautoindent
	let &smartindent = mysmartindent
	let &expandtab = myexpandtab

endfunction

function! s:Cream_wrap_left(range)
	" remove extra inner spaces if requested twice
	if Cream_delay() == 1
		execute "silent! " . a:range . 'substitute/\(^[\s]\+\)\@!\(\S\+\) \+/\2 /gei'
	endif
	" format
	normal gq
endfunction
function! s:Cream_wrap_center(range)
	" remove extra inner spaces if requested twice
	if Cream_delay() == 1
		execute "silent! " . a:range . 'substitute/\(^[\s]\+\)\@!\(\S\+\) \+/\2 /gei'
	endif
	" format
	execute "silent! " . a:range . "center"
endfunction
function! s:Cream_wrap_right(range)
	" remove extra inner spaces if requested twice
	if Cream_delay() == 1
		execute "silent! " . a:range . 'substitute/\(^[\s]\+\)\@!\(\S\+\) \+/\2 /gei'
	endif
	" format
	execute "silent! " . a:range . "right"
endfunction

function! Cream_delay()
" returns 1 if has been called before within two seconds

	" init timing
	if !exists("g:delaytime")
		let g:delaytime = localtime()
		return 0
	endif
	" only eliminate spaces if called twice within two seconds
	if localtime() - g:delaytime < 2
		let myreturn = 1
	else
		let myreturn = 0
	endif
	" record new time
	let g:delaytime = localtime()

	return myreturn + 0

endfunction

function! Cream_quickwrap_set(mode, justification)
" sets justification type and then quick wraps if selection (mode == "v")

	" insert mode: set mode, don't justify (user should quick wrap)
	if     a:mode ==? "i"
		let g:cream_justify = a:justification
		return
	" visual mode: temp set mode, justify
	elseif a:mode ==? "v"
		" save environment
		if exists("g:cream_justify")
			let myjustify = g:cream_justify
		else
			let myjustify = "left"
		endif
		let g:cream_justify = a:justification
		call Cream_quickwrap(a:mode)
		let g:cream_justify = myjustify
	endif

endfunction

" Quick UnWrap {{{1
function! Cream_quickunwrap(mode)

	" remember
	let mytextwidth = &textwidth

	" set textwidth ridiculously high (100 lines of 100 chars)
	set textwidth=10000

	" main
	if a:mode == "i"
		let mypos = Cream_pos()
		" select inner paragraph
		normal vip
	elseif a:mode == "v"
		normal gv
	else
		return -1
	endif

	normal gq

	if     a:mode == "i"
		execute mypos
	elseif a:mode == "v"
		normal gv
	endif

	" restore
	let &textwidth = mytextwidth

endfunction

" 1}}}
" GUI
" GUI functions {{{1

" font
function! Cream_font_init()

	if !has("gui_running")
		return -1
	endif

	let myos = Cream_getoscode()

	" set font based on global
	if exists("g:CREAM_FONT_" . myos)
		" had a problem with guifont being set to * in an error.
		"*** this fixed in patch 6.1.296 ***
		if g:CREAM_FONT_{myos} != "*"
			"execute "set guifont=" . g:CREAM_FONT_{myos}
			"execute "set printfont=" . g:CREAM_FONT_{myos}
			" fix fonts with spaces in names
			let g:CREAM_FONT_{myos} = substitute(g:CREAM_FONT_{myos}, " ", "\ ", "g")
			let &guifont = g:CREAM_FONT_{myos}
			let &printfont = g:CREAM_FONT_{myos}
		else
			" clean out possible error value
			unlet g:CREAM_FONT_{myos}
		endif
	else
		" Don't prompt to initialize font... use default.
		let g:CREAM_FONT_{myos} = ""
	endif

endfunction

function! Cream_font_set()

	if !has("gui_running")
		return -1
	endif

	let myos = Cream_getoscode()

	" save screen pos
	" HACK: set guifont=* moves screen on WinXP
	call Cream_screen_get()

	silent! set guifont=*
	" if still empty or a "*", user may have cancelled; do nothing
	if  &guifont == "*"
	\|| g:CREAM_FONT_{myos} == "" && &guifont == ""
		" do nothing
	else
		let g:CREAM_FONT_{myos} = &guifont
	endif

	" restore screen pos
	" HACK: set guifont=* moves screen on WinXP
	call Cream_screen_init()

	return

endfunction

" window positioning
function! Cream_screen_init()
" * Dynamically set based on previous settings
" * Remembers and restores winposition, columns and lines stored in global
"   variables written to viminfo
" * Must follow Cream_font_init() so that columns and lines are accurate based
"   on font size.

	if !has("gui_running")
		return -1
	endif

	let myos = Cream_getoscode()

	" initialize
	if !exists("g:CREAM_COLS_" . myos)
		let g:CREAM_COLS_{myos} = 80
	endif
	if !exists("g:CREAM_LINES_" . myos)
		let g:CREAM_LINES_{myos} = 30
	endif
	if !exists("g:CREAM_WINPOSX_" . myos)
		" don't set to 0, let window manager decide
		let g:CREAM_WINPOSX_{myos} = ""
	endif
	if !exists("g:CREAM_WINPOSY_" . myos)
		" don't set to 0, let window manager decide
		let g:CREAM_WINPOSY_{myos} = ""
	endif

	" set
	if exists("g:CREAM_WINPOS") && g:CREAM_WINPOS == 1
		execute "set columns=" . g:CREAM_COLS_{myos}
		execute "set lines=" . g:CREAM_LINES_{myos}
		execute "winpos " . g:CREAM_WINPOSX_{myos} . " " . g:CREAM_WINPOSY_{myos}
	endif

endfunction

function! Cream_screen_get()
" used by Cream_exit() to retain window position and size

	if !has("gui_running")
		return -1
	endif

	let myos = Cream_getoscode()

	let g:CREAM_COLS_{myos} = &columns
	let g:CREAM_LINES_{myos} = &lines

	let g:CREAM_WINPOSX_{myos} = getwinposx()
	" filter error condition
	if g:CREAM_WINPOSX_{myos} < 0
		let g:CREAM_WINPOSX_{myos} = 0
	endif

	let g:CREAM_WINPOSY_{myos} = getwinposy()
	" filter error condition
	if g:CREAM_WINPOSY_{myos} < 0
		let g:CREAM_WINPOSY_{myos} = 0
	endif

endfunction

function! Cream_screen_unlet()
" Used by singleserver to unset window position and size.

	if !has("gui_running")
		return -1
	endif

	let myos = Cream_getoscode()

	unlet g:CREAM_COLS_{myos}
	unlet g:CREAM_LINES_{myos}

	unlet g:CREAM_WINPOSX_{myos}
	unlet g:CREAM_WINPOSY_{myos}

endfunction

" window positioning preference (on/off)
function! Cream_winpos_init()
" initialize window positioning
	if !exists("g:CREAM_WINPOS")
		" initiallize on
		let g:CREAM_WINPOS = 1
	endif
endfunction

function! Cream_winpos_toggle()
" toggle window positioning
	if exists("g:CREAM_WINPOS")
		if g:CREAM_WINPOS == 0
			let g:CREAM_WINPOS = 1
		else
			let g:CREAM_WINPOS = 0
		endif
	else
		call Cream_error_warning("Error: global uninitialized in Cream_winpos_toggle()")
	endif
	" update the menu item
	call Cream_menu_settings_preferences()
endfunction


" Mouse {{{1

function! Cream_mouse_middle_init()
" initialize middle mouse button to one of two behavior styles

	if exists("g:CREAM_MOUSE_XSTYLE") && g:CREAM_MOUSE_XSTYLE == 1
		" X-compatible paste (paste "* [selection] register)
		set guioptions+=a
		silent! iunmap <MiddleMouse>
		silent! vunmap <MiddleMouse>
		vnoremap <silent> <MiddleMouse> <MiddleMouse><BS>
	else
		" "normal" middle mouse behavior
		set guioptions-=a
		imap <silent> <MiddleMouse> <Nop>
		vmap <silent> <MiddleMouse> <Nop>
		imap <silent> <2-MiddleMouse> <Nop>
		vmap <silent> <2-MiddleMouse> <Nop>
	endif

endfunction

function! Cream_mouse_middle_toggle()
" toggles middle mouse button between one of two behavior styles

	if exists("g:CREAM_MOUSE_XSTYLE") && g:CREAM_MOUSE_XSTYLE == 1
		unlet g:CREAM_MOUSE_XSTYLE
	else
		let g:CREAM_MOUSE_XSTYLE = 1
	endif

	call Cream_mouse_middle_init()

	call Cream_menu_settings_preferences()

endfunction


" 1}}}
" Settings
" Tabstop settings {{{1

" tabstop AND shift width (force the same)
function! Cream_tabstop_init()
" Initialize tab settings, called via autocmd each BufEnter.

	" don't change if in help file
	if &filetype == "help"
		return 1
	endif

	if exists("g:CREAM_TABSTOP")
		execute "set tabstop=" . g:CREAM_TABSTOP
	else
		" never initialized before
		set tabstop=4
		let g:CREAM_TABSTOP = 4
	endif

	if exists("g:CREAM_SOFTTABSTOP")
		execute "set shiftwidth=" . g:CREAM_SOFTTABSTOP
		execute "set softtabstop=" . g:CREAM_SOFTTABSTOP
	else
		" use tabstop
		execute "set shiftwidth=" . g:CREAM_TABSTOP
		execute "set softtabstop=" . g:CREAM_TABSTOP
	endif

endfunction
function! Cream_tabstop()
" Set tabstop and associated via dialog (and Cream global variables).

	if !exists("g:CREAM_TABSTOP")
		let n = Inputdialog("Enter tabstop width:", 4)
	else
		let n = Inputdialog("Enter tabstop width:", g:CREAM_TABSTOP)
	endif

	" cancel
	if     n == "{cancel}"
		return
	" empty or 0
	elseif n == "" || n == "0"
		" if empty
		if !exists("g:CREAM_TABSTOP")
			" previous setting didn't exist, auto set to 4 if global didn't previously exist
			call confirm("Setting to default (4).", "&Ok", 1)
			call Cream_tabstop_init()
		else
			" previous setting existed, just use it
			call confirm("0 not allowed, defaulting to previous setting (" . g:CREAM_TABSTOP . ")", "&Ok", 1)
		endif
	" non-digits
	elseif n =~ '\D'
		call confirm(
			\ "Only number values accepted.\n" .
			\ "\n", "&Ok", 1, "Info")
		return
	" use new setting (allows "stupid" settings, ie, 1000)
	else
		let g:CREAM_TABSTOP = n
		execute "set tabstop=" . g:CREAM_TABSTOP
		if exists("g:CREAM_SOFTTABSTOP")
			execute "set shiftwidth=" . g:CREAM_SOFTTABSTOP
			execute "set softtabstop=" . g:CREAM_SOFTTABSTOP
		else
			execute "set shiftwidth=" . g:CREAM_TABSTOP
			execute "set softtabstop=" . g:CREAM_TABSTOP
		endif
	endif

	return g:CREAM_TABSTOP

endfunction

function! Cream_softtabstop()
" Set softtabstop and shiftwidth via dialog (and Cream global variables)

	let introstr = "Soft tabstop allows new editing to use different \n" .
				 \ "tabstop settings than existing text. Enter your new \n" .
				 \ "tab width below, the main tabstop setting will \n" .
				 \ "maintain existing text at the old tab spacing.\n" .
				 \ "\n" .
				 \ "To turn off soft tabstop, enter 0 or an empty box.\n" .
				 \ "\n"
	if !exists("g:CREAM_SOFTTABSTOP")
		let n = Inputdialog(introstr, 4)
	else
		let n = Inputdialog(introstr, g:CREAM_SOFTTABSTOP)
	endif

	" cancel
	if     n == "{cancel}"
		return
	" empty or 0
	elseif n == "" || n == "0"
		if exists("g:CREAM_SOFTTABSTOP")
			unlet g:CREAM_SOFTTABSTOP
			call Cream_tabstop_init()
		endif
	" non-digits
	elseif n =~ '\D'
		call confirm(
			\ "Only number values accepted.\n" .
			\ "\n", "&Ok", 1, "Info")
		return
	" use new setting
	else
		let g:CREAM_SOFTTABSTOP = n
		execute "set shiftwidth=" . g:CREAM_SOFTTABSTOP
		execute "set softtabstop=" . g:CREAM_SOFTTABSTOP
	endif

endfunction

" Auto Indentation {{{1

" autoindent
function! Cream_autoindent_init()
" initialize autoindent (according to whatever on/off state currently
" exists)

	" don't change state in special buffers
	" opsplorer
	if bufname("%") == "_opsplorer"
		return
	endif
	" calendar
	if bufname("%") == "__Calendar"
		return
	endif

	if !exists("g:CREAM_AUTOINDENT")
		" initially on
		call s:Cream_indent_on()
	else
		if g:CREAM_AUTOINDENT == 1
			call s:Cream_indent_on()
		else
			call s:Cream_indent_off()
		endif
	endif
endfunction

function! Cream_autoindent_toggle()
" toggle autoindent
	if exists("g:CREAM_AUTOINDENT")
		if g:CREAM_AUTOINDENT == 0
			call s:Cream_indent_on()
		elseif g:CREAM_AUTOINDENT == 1
			call s:Cream_indent_off()
		endif
	else
		call Cream_error_warning("Error: global uninitialized in Cream_autoindent_toggle()")
	endif
	call Cream_menu_settings_autoindent()
endfunction

function! s:Cream_indent_on()
	if exists("did_indent_on")
		unlet did_indent_on
	endif
	if exists("b:did_indent")
		unlet b:did_indent
	endif
	filetype indent on
	let g:CREAM_AUTOINDENT = 1
	if &indentexpr == ""
		setlocal autoindent
		"setlocal smartindent
	endif
endfunction
function! s:Cream_indent_off()
	setlocal noautoindent
	"setlocal nosmartindent
	filetype indent off
	let g:CREAM_AUTOINDENT = 0
endfunction

" Line numbers {{{1

function! Cream_linenumbers_init()
" initialize line numbers
	if !exists("g:CREAM_LINENUMBERS")
		" initiallize on
		set number
		let g:CREAM_LINENUMBERS = 1
	else
		if g:CREAM_LINENUMBERS == 1
			set number
		else
			set nonumber
		endif
	endif
endfunction

function! Cream_linenumbers_init_buffer()
" initialize line numbers upon entering buffer
	if g:CREAM_LINENUMBERS == 1
		" turn on in buffer
		set number
		let b:cream_linenumbers = 1
	else
		" turn off in buffer
		if exists("b:cream_linenumbers")
			let b:cream_linenumbers = 0
		endif
		set nonumber
	endif
endfunction

function! Cream_linenumbers_toggle()
" toggle line numbers
	if exists("g:CREAM_LINENUMBERS")
		if g:CREAM_LINENUMBERS == 0
			set number
			let g:CREAM_LINENUMBERS = 1
		elseif g:CREAM_LINENUMBERS == 1
			set nonumber
			let g:CREAM_LINENUMBERS = 0
		endif
	else
		call Cream_error_warning("Error: global uninitialized in Cream_linenumbers_toggle()")
	endif
	call Cream_menu_settings_linenumbers()
endfunction

" Syntax highlighting {{{1
function! Cream_syntax_init()
" initialize syntax highlighting
	if !exists("g:CREAM_SYNTAX")
		" initialize on
		syntax on
		let g:CREAM_SYNTAX = 1
	else
		if g:CREAM_SYNTAX == 1
			" no need to initialize (besides, it hoses custom
			" highlighting in current file on startup)
			"syntax on
		else
			syntax off
		endif
	endif
endfunction

function! Cream_syntax_toggle(mode)
" toggle syntax highlighting
	if exists("g:CREAM_SYNTAX")
		if g:CREAM_SYNTAX == 0
			syntax on
			let g:CREAM_SYNTAX = 1
			call Cream_filetype()
		elseif g:CREAM_SYNTAX == 1
			syntax off
			let g:CREAM_SYNTAX = 0
		endif
	else
		call Cream_error_warning("Error: global uninitialized in Cream_syntax_toggle()")
	endif
	if a:mode == "v"
		normal gv
	endif
	call Cream_menu_settings_syntax()
endfunction

" Highlight current line {{{1

function! Cream_highlight_currentline_init()
" Initialize current line highlighting, Vim 7.0+, called by autocmd.

	if exists("g:CREAM_HIGHLIGHT_CURRENTLINE")
		if &cursorline == 0
			set cursorline
		endif
	else
		if &cursorline == 1
			set nocursorline
		endif
	endif
endfunction

function! Cream_winline_start(linewidth)
" return start of current screen line

	let len = virtcol('$')
	let pos = virtcol('.')

	" if line shorter than window, only one screen line
	if len <= a:linewidth
		return 1
	endif

	" if position is less than line width, on first line
	if pos <= a:linewidth
		return 1
	endif

	" we're beyond the first screen line
	let i = 1
	while i < pos + 1
		let i = i + a:linewidth
	endwhile
	return i - a:linewidth

endfunction

function! Cream_highlight_currentline_toggle()

	if exists("g:CREAM_HIGHLIGHT_CURRENTLINE")
		unlet g:CREAM_HIGHLIGHT_CURRENTLINE
		set nocursorline
	else
		let g:CREAM_HIGHLIGHT_CURRENTLINE = 1
		set cursorline
	endif

	call Cream_menu_settings_highlightcurrentline()

endfunction

" Highlight columns {{{1
function! Cream_highlight_columns(column)
" toggle highlighting of column {column} and beyond

	if !exists("b:cream_col_highlight")
		let b:cream_col_highlight = 1
	else
		unlet b:cream_col_highlight
	endif
	"let b:cream_col_highlight =! b:cream_col_highlight
	if exists("b:cream_col_highlight")
		highlight! link ColumnBeyond Todo
		execute 'match ColumnBeyond /.\%>' . (a:column+1) . 'v/'
	else
		match none
	endif

	call Cream_menu_settings_highlightwrapwidth()

endfunction

" Toolbar toggle {{{1

" NOTE: Cream_toolbar_init() is in cream-settings.vim!

function! Cream_toolbar_toggle()
" toggle toolbar (initialization in cream-menu.vim

	" initialize
	if !exists("g:CREAM_TOOLBAR")
		let g:CREAM_TOOLBAR = 1
	endif

	" reverse current status
	if g:CREAM_TOOLBAR == 1
		set guioptions-=T	" no toolbar
		let g:CREAM_TOOLBAR = 0
	else
		set guioptions+=T	" toolbar
		let g:CREAM_TOOLBAR = 1
	endif
	call Cream_menu_settings_preferences()
endfunction

" Statusline toggle {{{1

function! Cream_statusline_init()
" initialize statusline state
	if !exists("g:CREAM_STATUSLINE")
		" initialize on
		set laststatus=2
		let g:CREAM_STATUSLINE = 1
	else
		if g:CREAM_STATUSLINE == 1
			set laststatus=2
		else
			set laststatus=0
		endif
	endif
endfunction

function! Cream_statusline_toggle()
" toggle syntax highlighting
	if exists("g:CREAM_STATUSLINE")
		if g:CREAM_STATUSLINE == 0
			set laststatus=2
			let g:CREAM_STATUSLINE = 1
		elseif g:CREAM_STATUSLINE == 1
			set laststatus=0
			let g:CREAM_STATUSLINE = 0
		endif
	else
		call Cream_error_warning("Error: global uninitialized in Cream_statusline_toggle()")
	endif
	call Cream_menu_settings_preferences()
endfunction

" Bracket matching {{{1

" initialize environment
function! Cream_bracketmatch_init()

	set matchtime=1

	if !exists("g:CREAM_BRACKETMATCH")
		" initially on
		set showmatch
		let g:CREAM_BRACKETMATCH = 1
	else
		if g:CREAM_BRACKETMATCH == 1
			set showmatch
		else
			set noshowmatch
		endif
	endif

endfunction

" toggle on/off
function! Cream_bracketmatch_toggle()
	if exists("g:CREAM_BRACKETMATCH")
		if g:CREAM_BRACKETMATCH == 0
			set showmatch
			let g:CREAM_BRACKETMATCH = 1
		elseif g:CREAM_BRACKETMATCH == 1
			set noshowmatch
			let g:CREAM_BRACKETMATCH = 0
		endif
	else
		call confirm(
		\"Error: global uninitialized in Cream_bracketmatch_toggle()", "&Ok", 1, "Error")
	endif
	call Cream_menu_settings_preferences()
endfunction

" Errorbells {{{1

function! Cream_errorbells_off(...)
" control Vim's audio and visual warnings
" * Arguments:
"     "beep":   turn off just beeping
"     "flash":  turn off just flashing
"     (empty):  both off
" * Must be initialized after the GUI starts!

	" off
	if a:0 == 0
		let myeb = ""
	else
		let myeb = a:1
	endif

	if     myeb ==? "flash"
		" audibly beep on error messages
		set errorbells
		" Screen flash on error (See also 't_vb')
		set novisualbell
		set t_vb=
	elseif myeb ==? "beep"
		" audibly beep on error messages
		set noerrorbells
		" Screen flash on error (See also 't_vb')
		set visualbell
		set t_vb&
	elseif myeb ==? ""
		" audibly beep on error messages
		set noerrorbells
		" Screen flash on error (See also 't_vb')
		set visualbell
		set t_vb=
	endif

endfunction

" Keymap {{{1

function! Cream_keymap(map)
" change the setting of &keymap
	let g:CREAM_KEYMAP = a:map
	execute "set keymap=" . g:CREAM_KEYMAP
	" refresh menu to indicate new setting
	call Cream_menu_settings_preferences()
endfunction

function! Cream_keymap_init()
" initialize &keymap
	if !exists("g:CREAM_KEYMAP")
		let g:CREAM_KEYMAP = ""
	else
		execute "set keymap=" . g:CREAM_KEYMAP
	endif
endfunction

" 1}}}
" Inserts
" Char Inserts {{{1

" Insert Digraph
function! Cream_digraph_insert()
" only available from insert mode

"*********************************************************************
" The insertion of digraphs does not occur via function! Since the
" native keystroke <C-k> is only available via insertmode, we're
" unable to functionalize it.
"*********************************************************************
	"normal i
	"execute "i\<C-k>IE"

endfunction

" List Digraphs
function! Cream_digraph_list()
	let str =
		\ "\n" .
		\ " The following digraph list is in the format:\n" .
		\ "\n" .
		\ "    At @ 64\n" .
		\ "\n" .
		\ " where first two letters (\"digraph\") are typed to insert the following \n" .
		\ " character. The trailing digits indicate that character's decimal value.\n" .
		\ "\n" .
		\ " Note that proper display is dependant on operating system, font, and \n" .
		\ " file encoding. Characters beyond decimal 126 are generally unreliable.\n" .
		\ "\n"
	echo str
	digraphs
endfunction

function! Cream_insert_char()
" Insert a char at cursor based on decimal, hex, or octal value. Uses
" inputdialog for prompt.

	" get string representing value
	let x = Inputdialog("Input decimal value of character to insert:\n" .
		\ "(preceed with \"0x\" for hexidecimal, \"0\" for octal)\n" .
		\ "", "")
	" convert to char
	let @x = nr2char(x)
	" TODO: work around errant code for <M-.> map
	if x == "174"
		let @x = "®"
	endif

	" get pos
	let line = line('.')
	let vcol = virtcol('.')
	" insert char
	if vcol == col('$') - 1
		normal "xgp
	else
		normal "xgP
	endif

endfunction

function! Cream_allchars_list()

	" only use for > ascii
	if &encoding != "latin1"
		echo "\n"
		echo " (Use [Enter] to scroll by line, [Space] by page, or [Esc] to close.)"
		echo "\n"
	endif

	echo Cream_ascii_table()

	" limitations
	let min = 32
	if &encoding == "latin1"
		echo "\n"
		echo " Set another file encoding (e.g. Unicode UTF-8) to test characters "
		echo " beyond decimal 255."
		echo "\n"
		return
	else
		let max = 65375
	endif
	let str = ""

	" display setup
	echo "\n"
	echo " All characters available on this computer up to " . max . " places \n"
	echo " are listed below by HEXIDECIMAL VALUE. (Proper display is dependant on \n"
	echo " the current encoding and font.)"
	echo "\n"

	let numlen = strlen(Nr2Hex(max))
	let num = ""
	if &columns < 80
		let wid = &columns / 2
	else
		let wid = 21  " 37
	endif

	let i = 0
	while min+i < max
		if i % (wid - strlen(max)) == 0
			" headings
			if i == 0 || (i+16) % 256 == 0
				echo "     _________________________________________________"
				echo "      0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f  "
				echo "     ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯"
			endif
			echo str
			let num = min+i
			" do in hex if bigger than decimal (:help i_CTRL-V_digit)
			if max > 255
				let num = Nr2Hex(num)
			endif
			" pad
			let num = Cream_str_pad(num, numlen)
			let str = num . ": "
		endif
		let str = str . nr2char(min+i) . "  "
		let i = i + 1
	endwhile
	" print any remainder
	echo str
endfunction

" Date/Time {{{1

function! Cream_insert_datetime(...)
" Insert the date/time at the cursor. If {...} is 1, use dialog to
" select format. Otherwise, use default/last picked.
"
" Formatting References:
" o http://www.saqqara.demon.co.uk/datefmt.htm
" o http://www.opengroup.org/onlinepubs/7908799/xsh/strftime.html
" o http://www.cl.cam.ac.uk/~mgk25/iso-time.html
" o http://www.w3.org/TR/NOTE-datetime

	" retain existing dialog button display orientation
	let sav_go = &guioptions
	" make vertical
	set guioptions+=v

	" set default pick if no previous preference
	if !exists("g:CREAM_DATETIME_STYLE")
		let g:CREAM_DATETIME_STYLE = 1
	endif

	if   a:0 == 1
	\ && a:1 == 1
		" let button text equal current values
		let mybuttons =
			\ substitute(strftime('%d %B %Y'), '^0*', '', 'g') . "\n" .
			\ substitute(strftime('%d %b %Y'), '^0*', '', 'g') . "\n" .
			\ strftime('%Y %B %d') . "\n" .
			\ strftime('%Y %b %d') . "\n" .
			\ strftime('%Y-%b-%d') . "\n" .
			\ strftime("%Y-%m-%d") . "\n" .
			\ strftime("%Y-%m-%d, %I:%M") . tolower(strftime("%p")) . "\n" .
			\ strftime("%Y-%m-%d, %H:%M") . "\n" .
			\ strftime("%Y-%m-%d %H:%M:%S") . "\n" .
			\ strftime("%Y-%m-%d %H:%M:%S") . Cream_timezone() . "\n" .
			\ strftime("%Y%m%dT%H%M%S") . Cream_timezone() . "\n" .
			\ "&Cancel"

		let n = confirm(
			\ "Select the date/time format to insert: \n" .
			\ "\n" .
			\ "\n", mybuttons, g:CREAM_DATETIME_STYLE, "Info")
	else
		let n = g:CREAM_DATETIME_STYLE
	endif

	if     n == 1
		" 14 February 2003 (minus leading 0's)
		let mytime = substitute(strftime('%d %B %Y'), '^0*', '', 'g')
	elseif n == 2
		" 14 Feb 2003 (minus leading 0's)
		let mytime = substitute(strftime('%d %b %Y'), '^0*', '', 'g')
	elseif n == 3
		" 2003 February 14
		let mytime = strftime('%Y %B %d')
	elseif n == 4
		" 2003 Mar 14
		let mytime = strftime("%Y %b %d")
	elseif n == 5
		" 2003-Mar-14
		let mytime = strftime("%Y-%b-%d")
	elseif n == 6
		" 2003-03-14
		let mytime = strftime("%Y-%m-%d")
	elseif n == 7
		" 2003-02-14, 10:28pm
		let mytime = strftime("%Y-%m-%d, %I:%M") . tolower(strftime("%p"))
	elseif n == 8
		" 2003-02-14, 22:28
		let mytime = strftime("%Y-%m-%d, %H:%M")
	elseif n == 9
		" 2003-02-14 22:28:14
		let mytime = strftime("%Y-%m-%d %H:%M:%S")
	elseif n == 10
		" 2003-02-14 22:28:23-0500
		let mytime = strftime("%Y-%m-%d %H:%M:%S") . Cream_timezone()
	elseif n == 11
		" 20030214T102823-0500
		let mytime = strftime("%Y%m%dT%H%M%S") . Cream_timezone()
	else

	endif

	" restore gui buttons
	let &guioptions = sav_go

	" insert register "i" (condition on col position)
	if exists("mytime")
		let @i = mytime
		if virtcol('.') == virtcol('$') - 1
			execute "normal a" . @i
			normal $
		else
			execute "normal i" . @i
			normal l
		endif
		" remember selection
		if 0 < n && n < 11
			let g:CREAM_DATETIME_STYLE = n
		endif
	endif

endfunction

" Character Lines {{{1
function! Cream_charline(...)
" insert character line, full width

	" accepting argument allows function to be called with a supplied
	" character as the argument
	if exists("a:1")
		let mychar = a:1
	else
		let mychar = nr2char(getchar())
	endif

	" insert character &textwidth number of times *minus the current
	" position*
	let mylen = g:CREAM_AUTOWRAP_WIDTH - virtcol('.')

	call Cream_charline_insert(mychar, mylen)

endfunction
function! Cream_charline_insert(str, len)
" insert a:str a:len number of times

	let i = 0
	while i < a:len
		execute "normal a" . a:str
		let i = i + 1
	endwhile

	normal $

endfunction
function! Cream_charline_prompt()
" a wrapper for Cream_charline() to prompt the user

	let n = confirm(
		\ "Enter any character after Continue to insert it as a line. \n" .
		\ "(Use the keyboard shortcut to avoid this prompt.)\n" .
		\ "\n", "Continue...\n&Cancel", 1, "Info")
	if n != 1
		return
	endif

	call Cream_charline()

endfunction

function! Cream_charline_lineabove(...)
" insert character line the length of the line above

	" set width based on line above
	" get str
	let mystr = getline(line(".") - 1)
	" substitute tabs for spaces
	let i = 0
	let mytab = ""
	while i < &tabstop
		let mytab = mytab . " "
		let i = i + 1
	endwhile
	let mystr = substitute(mystr, '\t', mytab, 'g')
	let mylen = strlen(mystr)

	" accepting argument allows function to be called with a supplied
	" character as the argument
	if exists("a:1")
		let mychar = a:1
	else
		let mychar = nr2char(getchar())
	endif

	" insert character mylen number of times *minus the current
	" position*
	let i = 0

	" let pad equal 1 unless at line end
	let mystr = getline('.')
	let mystr = substitute(mystr, '\t', mytab, 'g')
	if virtcol('.') != strlen(mystr)
		let mypad = 1
	else
		let mypad = 0
	endif

	" calculate length
	let mylen = mylen - virtcol('.') + mypad

	call Cream_charline_insert(mychar, mylen)

endfunction
function! Cream_charline_lineabove_prompt()
" a wrapper for Cream_charline_lineabove() to prompt the user

	let n = confirm(
		\ "Enter any character after Continue to insert it as a line the \n" .
		\ "length of the line above.\n" .
		\ "(Use the keyboard shortcut Ctrl+Shift+F4 to avoid this prompt. )\n" .
		\ "\n", "Continue...\n&Cancel", 1, "Info")
	if n != 1
		return
	endif

	call Cream_charline_lineabove()

endfunction

" 1}}}
" Other
" Character codes {{{1
function! Cream_char_codes(mode)
" display character codes

	" position
	if a:mode == "v"
		"normal gv
	"elseif a:mode == "i"
	"    normal h
	endif

	redir @x
	normal ga
	redir END

	let str = @x

	let chr = matchstr(str, '.\ze> ')
	let dec = matchstr(str, '\d\+,\@=')
	let hex = matchstr(str, '\(Hex \)\@<=\x\+')
	let oct = matchstr(str, '\(Octal \)\@<=\o\+')

	if     dec == 9
		let chr = "(tab)"
	elseif dec == 10
		let chr = "(line feed)"
	elseif dec == 13
		let chr = "(carriage return)"
	elseif dec == 32
		let chr = "(space)"
	elseif chr == ""
		call confirm(
		\ "No character found.\n" .
		\ "\n", "&Ok", 1, "Info")
		return
	endif

	" windows
	if chr == "&" && Cream_has("ms")
		let chr = "&&"
	endif

	call confirm(
		\ "character:   " . chr . "       \n" .
		\ "\n" .
		\ "  dec:   " . dec . "\n" .
		\ "  hex:   " . hex . "\n" .
		\ "  oct:   " . oct . "\n" .
		\ "\n", "&Ok", 1, "Info")

endfunction
" Version {{{1
function! Cream_version(form)
" returns version, subversion, and/or patch number:
" arguments:
"   "version"    -- return numerical version number: "6"
"   "subversion" -- return numerical subversion number: "2"
"   "patchlevel" -- return numerical patch level number: "57"
"   "string"     -- return version in string form: "6.2.71"
"

	" find version (everything prior to last two digits)
	let myversion = strpart(v:version, 0, strlen(v:version) - 2) + 0

	" find sub-version (last two digits of version)
	let mysubversion = strpart(v:version, strlen(v:version) - 2) + 0

	" find patch level
	" which will be their problem ;)
	" Note: This function works only in Vim 6.2 or after since
	"       has("patch##") was introduced in patch 6.1.384.
	let mypatchlevel = 0
	if v:version < 602
		" hack :version output
		redir @x
		silent! version
		redir END
		let mypos1 = matchend(@x, "Included patches: ")
		if mypos1 != -1
			" get pos of line end following
			let mypos2 = match(@x, "\n", mypos1)
			" get entire patch level string
			let tmp = strpart(@x, mypos1, mypos2 - mypos1)
			" count digits at end (the last numbers in this string are
			" the highest patch) (add 1, it's 0-based)
			let cnt = match(tmp, '\d*$') + 1
			" strip down to only those
			let mypatchlevel = strpart(tmp, strlen(tmp) - cnt)
		else
			let mypatchlevel = "0"
		endif
	else
		" loop through 999 (some people skip patches,
		let i = 0
		while i < 1000
			if has("patch" . i)
				let mypatchlevel = i
			endif
			let i = i + 1
		endwhile
	endif

	" find string version
	let mystring = myversion . "." . mysubversion . "." . mypatchlevel

	""*** DEBUG:
	"let n = confirm(
	"    \ "DEBUG:\n" .
	"    \ "  myversion     = \"" . myversion . "\"\n" .
	"    \ "  mysubversion  = \"" . mysubversion . "\"\n" .
	"    \ "  mypatchlevel  = \"" . mypatchlevel . "\"\n" .
	"    \ "  mystring      = \"" . mystring . "\"\n" .
	"    \ "\n", "&Ok\n&Cancel", 1, "Info")
	"if n != 1
	"    return
	"endif
	""***

	if     a:form == "version"
		return myversion
	elseif a:form == "subversion"
		return mysubversion
	elseif a:form == "patchlevel"
		return mypatchlevel
	elseif a:form == "string"
		return mystring
	else
		call confirm(
			\ "Error: Invalid argument passed to Cream_version()\n" .
			\ "\n", "&Ok", 1, "Info")
		return -1
	endif

endfunction

" OS code {{{1
" establish os for variable
function! Cream_getoscode()
" return a mneumonic code for the OS in use

	if Cream_has("ms")
		return "WIN"
	elseif has("unix")
		return "UNIX"
	else
		return "OTHER"
	endif
endfunction

" Date, Time, Timezone {{{1

function! Cream_timezone()
" Return the current timezone value (system dependent).
" o Override with g:CREAM_TIMEZONE.
" o Otherwise, most system return strftime("%Z").
" o If system (Windows) does not have strftime("%Z"), empty string ""
"   is returned.

	if exists("g:CREAM_TIMEZONE")
		let timezone = g:CREAM_TIMEZONE
	else
		let timezone = strftime("%Z")
	endif
	return timezone

endfunction

function! Cream_daysinyear()
" returns leap year status, 0 for false, 1 for true
	let leap = ""
	let myyear = strftime("%Y")
	if myyear % 400 != 0
		if myyear % 100 != 0
			if myyear % 4 != 0
				let leap = 364
			else
				let leap = 365
			endif
		else
			let leap = 364
		endif
	else
		let leap = 365
	endif
	return leap
endfunction

function! Cream_time_convertformat(mode, from, to, ...)
" o converts a time format to another
" o returns a string
" o {...} is the string to be converted (overrides selection in "v" mode)

	if a:mode == "v"
		normal gv
		normal "xy
		let str = @x
	endif

	if a:0 > 0
		let str = a:1
	endif

	" convert to seconds (number, not string!)
	let seconds = ""
	if     a:from == "24"

		if strlen(str) != 4
			call confirm(
				\ "Error in Cream_time_convertformat().\n" .
				\ "24-hour times expected to be 4 digits long.\n" .
				\ "\n", "&Ok", 1, "Info")
			return
		endif

		" get minutes
		let minutes = matchstr(str, '\d\d$')
		" strip leading 0 to avoid octal mis-interpretation
		if match(minutes, '^0') != -1
			let minutes = matchstr(minutes, '\d$')
		endif

		" get hours
		let hours = matchstr(str, '^\d\+\ze\d\d')
		" strip leading 0 to avoid octal mis-interpretation
		if match(hours, '^0') != -1
			let hours = matchstr(hours, '\d$')
		endif

		" convert hours and minutes to minutes
		let minutes = ((hours + 0) * 60) + (minutes + 0)
		" convert to seconds, string
		let seconds = minutes * 60

	elseif a:from == "minutes"

		let seconds = str * 60

	else
		call confirm(
			\ "Unhandled \"from\" time format in Cream_time_convertformat().\n" .
			\ "\n", "&Ok", 1, "Info")
		return
	endif


	" convert from seconds
	let time = ""
	if     a:to == "seconds"
		let time = seconds

	elseif a:to == "minutes"
		let time = seconds / 60

	elseif a:to == "24"
		" convert to minutes
		let time = seconds / 60

		" obtain minutes remainder
		let minutes = time % 60
		" pad to two digits
		while strlen(minutes) < 2
			let minutes = "0" . minutes
		endwhile

		let hours = time / 60

		let time = hours . minutes

	else
		call confirm(
			\ "Unhandled \"to\" time format in Cream_time_convertformat().\n" .
			\ "\n", "&Ok", 1, "Info")
		return
	endif


""*** DEBUG:
"let n = confirm(
"    \ "DEBUG:\n" .
"    \ "  str  = \"" . str . "\"\n" .
"    \ "  a:from  = \"" . a:from . "\"\n" .
"    \ "  a:to  = \"" . a:to . "\"\n" .
"    \ "  seconds  = \"" . seconds . "\"\n" .
"    \ "  time  = \"" . time . "\"\n" .
"    \ "\n", "&Ok\n&Cancel", 1, "Info")
"if n != 1
"    return
"endif
""***


	if a:mode == "v"
		let @x = time
		normal gv
		normal "xp
	else
		" convert to string
		return strpart(time, 0)
	endif

endfunction

" Math {{{1

function! Cream_isfactor(dividend, divisor)
" Return 1 if {divisor} is a factor of {dividend}, 0 if not.

	if     a:dividend < a:divisor
		return 0
	elseif a:dividend == a:divisor
		return 1
	endif

	let i = 1
	while 1
		" raise divisor to power
		let divisor_n = a:divisor
		let j = 1
		while j < i
			let divisor_n = (divisor_n * a:divisor)
			let j = j + 1
		endwhile
		let modu = a:dividend % divisor_n
		" if we get to number before non-0
		if     modu == a:dividend
			return 1
		elseif modu == 0
			" still factor, continue
		else
			" nope
			return 0
		endif
		let i = i + 1
	endwhile

endfunction

" Evaluate &statusline/&printheader {{{1

function! Cream_statusline_eval(option)
" evaluate &statusline or &printheader value and return an equivelent string

	if    a:option != "statusline" && a:option != "stl"
	\  && a:option != "printheader" && a:option != "pheader"
		" invalid
		return
	endif

	execute "let str = &" . a:option
	" wrap/hide
	if match(str, "%<") != -1
		let str = substitute(str, "%<", '', '')
	endif
	" path
	if match(str, "%f") != -1
		let str = substitute(str, "%f", fnamemodify(expand("%"), ":p:h"), 'g')
	endif
	" filename
	if match(str, "%h") != -1
		let str = substitute(str, "%h", fnamemodify(expand("%"), ":p:t"), 'g')
	endif
	" split left/right
	if match(str, "%=") != -1
		let str = substitute(str, "%=", '         ', '')
	endif
	" page numbers
	if match(str, "%N") != -1
		let str = substitute(str, "%N", '1', '')
	endif
	" modified
	if match(str, "%m") != -1
		if &modified
			let str = substitute(str, "%m", ' [+]', '')
		else
			let str = substitute(str, "%m", '    ', '')
		endif
	endif

	return str

endfunction


" Diff Mode {{{1

" called by setting diffexpr=MyDiff() only if has("win32")
function! MyDiff()
	if has("win32")
		let opt = "-a --binary "
		if &diffopt =~ "icase" | let opt = opt . "-i " | endif
		if &diffopt =~ "iwhite" | let opt = opt . "-b " | endif
		let myfname_in   = substitute(v:fname_in,  '/', '\', 'g')
		let myfname_new  = substitute(v:fname_new, '/', '\', 'g')
		let myfname_out  = substitute(v:fname_out, '/', '\', 'g')
		let myVIMRUNTIME = substitute($VIMRUNTIME, '/', '\', 'g')
		silent execute '!"' . myVIMRUNTIME . '\diff" ' . opt .
			\ myfname_in . ' ' . myfname_new . ' > ' . myfname_out
	endif
endfunction

function! Cream_diffmode_toggle()
" toggles diff mode (detects existing state)
" * diff mode is *not* retained session to session
" * diff mode is *buffer-specific*
" * g:cream_diffmode exists if on in any buffer

	if !exists("b:cream_diffmode")

		" Win95 error trap
		let myshellslash = &shellslash
		set noshellslash

		" turn on
		call Cream_diffmode(1)

		let &shellslash = myshellslash

		let b:cream_diffmode = 1
		" keep track of how many diff mode buffers there are
		if !exists("g:cream_diffmode")
			let g:cream_diffmode = 1
		else
			let g:cream_diffmode =  g:cream_diffmode + 1
		endif
	else
		" turn off
		call Cream_diffmode(0)
		unlet b:cream_diffmode
		" keep track of how many diff mode buffers there are
		let g:cream_diffmode =  g:cream_diffmode - 1
		if g:cream_diffmode == 0
			unlet g:cream_diffmode
		endif
	endif

endfunction

function! Cream_diffmode(state)
" turns diff mode on (1) or off (0)
" abstracted from Cream_diffmode_toggle() so that autocmd can call
" here for new buffers (behind the scenes conformance ;)

	if a:state == 1

		diffthis
		"" same as using :diffthis
		"set diff
		"set scrollbind
		"set scrollopt=hor
		"set nowrap
		"set foldmethod=diff
		"set foldcolumn=2

		let g:CREAM_WRAP = 0

	elseif a:state == 0

		" diffthis off
		set nodiff
		set noscrollbind
		set scrollopt=ver,jump
		"set nowrap
		set foldmethod=manual
		set foldcolumn=0

		"let g:CREAM_WRAP = 0

	endif

endfunction

function! Cream_diffmode_update()
" update diff state
	if exists("b:cream_diffmode")
		diffupdate
	endif
endfunction

" Trim word {{{1

function! Cream_trim(word)
" trim whitespace off both ends of the argument
	return substitute(a:word, '^\s*\(\S*\)\s*$', '\1', 'g')
endfunction

" Sort and Uniq {{{1

" Line Sort (BISort2)
" Source: Piet Delport, vim@vim.org list, 2003-05-01
function! BISort2(start, end)
" Sort a selection. Required to be in visual mode.
"
" Note: The four "toupper()" statements make this sort case-insenstive.
"       Remove for case-sensitive.
	if col("'>") == 1
		let fix = 1
		"call confirm(
		"    \ "FIX!\n" .
		"    \ "\n", "&Ok", 1, "Info")
	else
		let fix = 0
	endif
	let i = a:start - 1
	while i <= a:end - fix
		" find insertion point via binary search
		let i_val = getline(i)
		let lo = a:start
		let hi = i
		while lo < hi
			let mid = (lo + hi) / 2
			let mid_val = getline(mid)
			if toupper(i_val) < toupper(mid_val)
				let hi = mid
			else
				let lo = mid + 1
				if toupper(i_val) == toupper(mid_val) | break | endif
			endif
		endwhile
		" do insert
		if lo < i
			exec i.'d_'
			call append(lo - 1, i_val)
		endif
		let i = i + 1
	endwhile

	"normal gv

endfunction
command! -nargs=0 -range BISort2 call BISort2(<line1>, <line2>)

function! BISort2Reverse(start, end)
	call BISort2(a:start, a:end)
	call Invert(a:start, a:end)
endfunction
command! -nargs=0 -range BISort2Reverse call BISort2Reverse(<line1>, <line2>)

function! Invert(start, end)
	if col("'>") == 1
		let fix = 1
	else
		let fix = 0
	endif
	let i = a:end
	while i >= a:start - fix
		let i_val = getline(i)
		exec i.'d_'
		call append(a:end - 1, i_val)
		let i = i - 1
	endwhile
endfunction
command! -nargs=0 -range Invert call Invert(<line1>, <line2>)

" Source: genutils.vim (included with Cream)
" Note:   Functions are declared local in genutils. (These are global.)
function! CmpByString(line1, line2, direction)
	if a:line1 < a:line2
		return -a:direction
	elseif a:line1 > a:line2
		return a:direction
	else
		return 0
	endif
endfunction

function! CmpByNumber(line1, line2, direction)
	let num1 = a:line1 + 0
	let num2 = a:line2 + 0

	if num1 < num2
		return -a:direction
	elseif num1 > num2
		return a:direction
	else
		return 0
	endif
endfunction

""*** obsoleted by above ***
"function! Cream_sort_compare(idx1, idx2)
"" return the index (1 or 2) for which of two arguments come first
"" alphabetically
"" * An empty value will be returned as first, too.
"
"    let i = 0
"    while i < strlen(a:idx1)
"        let myval1 = strpart(a:idx1, i, 1)
"        let myval2 = strpart(a:idx2, i, 1)
"        if char2nr(myval1) == char2nr(myval2)
"            let i = i + 1
"            continue
"        elseif char2nr(myval2) > char2nr(myval1)
"            return 1
"        else
"            return 2
"        endif
"        let i = i + 1
"    endwhile
"
"    " two are equal for the length of a:idx1
"    if strlen(a:idx2) > strlen(a:idx1)
"        " 2 is longer, so after
"        return 2
"    else
"        " equal!
"        return 0
"    endif
"
"endfunction

function! Cream_uniq(mode)
" call function to uniq a selection

	if a:mode != "v"
		return
	endif

	"normal gv

	" cursor in first column of last line implies not selected
	let mycol = col('.')
	let myline1 = line("'<")
	let myline2 = line("'>")
	if mycol == "1"
		let myline2 = myline2 - 1
	endif

	let n = confirm(
		\ "Uniq only works on sorted lines. Do you want to sort the selection first?\n" .
		\ "\n", "&Yes\n&No\n&Cancel", 1, "Info")
	if     n == 1
		call BISort2(myline1, myline2)
	elseif n == 2
		" do nothing, proceed with uniq
	else
		return
	endif

	call Uniq(myline1, myline2)

	" DON'T RESELECT! Lines deleted so it can't be the same.

endfunction

" Note: we use this as a library function, see Cream_uniq() for the
"       call.
"
" Author:  Jean Jordaan <jean(at)upfrontsystems.co.za>
" Source:  http://groups.yahoo.com/group/vim/message/16444
" Date:    Mon Feb 26, 2001  5:34 am
" Subject: Some functions to read mailing list digests with Vim.
"
function! Uniq(...) range

	" use arguments if two passed
	if a:0 == 2
		let a = a:1
		let z = a:2
	" use range
	else
		let a = a:firstline
		let z = a:lastline
	endif
	while (a <= z)
		let str1 = getline(a)
		let str2 = getline(a+1)
		if (str1 == str2)
			execute a . "delete"
			let z = z - 1
		else
			let a = a + 1
		endif
	endwhile

endfunction
" :Uniq takes a range of sorted lines and discards duplicates.
command! -nargs=0 -range Uniq <line1>,<line2>call Uniq()

" Completion {{{1

function! Cream_complete_forward()
	return "\<C-n>"
endfunction

function! Cream_complete_backward()
	return "\<C-p>"
endfunction

function! Cream_complete_omni_forward()
	if pumvisible()
		return "\<C-n>"
	endif
	if &omnifunc != ""
		return "\<C-x>\<C-o>"
	else
		return ""
	endif
endfunction
function! Cream_complete_omni_backward()
	if pumvisible()
		return "\<C-p>"
	endif
	if &omnifunc != ""
		return "\<C-x>\<C-o>"
	else
		return ""
	endif
endfunction

" Progress Bars {{{1

function! ProgressBar(percent, string, char, barlen)
" Draw a progress bar in the command line similar to:
"
"   Loading file... [####################             ] 55%
"
" Example Call:
"
"   call ProgressBar(55, "Loading file...", "#", 0)
"
" Arguments:
" {percent} -- percent of bar complete
" {string}  -- leading description string (empty acceptable)
" {char}    -- character to use as bar (suggest "#", "|" or "*")
" {barlen}  -- bar length in columns, use 0 to use window width
"

	" establish bar length (if no value passed)
	if a:barlen == 0
		"____________window width__leading string___"["__"] "_63%_ margin
		let barlen = winwidth(0) - strlen(a:string) - 1 - 2 - 3 - 2
	else
		let barlen = a:barlen
	endif

	" define progress
	let chrs = barlen * a:percent / 100
	" define progress to go
	let chrx = barlen * (100 - a:percent) / 100

	" bar, initial string and start line
	let bar = a:string . "["
	" bar, progress
	while chrs
		let bar = bar . a:char
		let chrs = chrs - 1
	endwhile
	" bar, progress to go
	while chrx
		let bar = bar . " "
		let chrx = chrx - 1
	endwhile
	" bar, end line
	let bar = bar . "] "
	" bar, extra space if single digit
	if a:percent < 10
		let bar = bar . " "
	endif
	" bar, percentage
	let bar = bar . a:percent . "%"

	" clear command line
	execute "normal \<C-\>\<C-n>:\<C-u>"
	" Yikes, this changes window/buffers, saves, other blechy stuff.
	"execute "normal \<C-b>:\<C-u>"

	" capture cmdheight
	let cmdheight = &cmdheight
	" setting to 2+ avoids 'Press Enter...'
	if cmdheight < 2
		let &cmdheight = 2
	endif

	" show on command line
	echon bar

	" restore cmdheight
	let &cmdheight = cmdheight

endfunction

"function! ProgressBarBuffer(string, progress, char, barlen)
"" draw a progress bar *in a blank buffer* based on a percentage
"" * WARNING: Draws in a previously opened and current empty buffer!
""   ALL CONTENTS OF THE CURRENT BUFFER WILL BE DELETED!!
"" * (a:string)   -- string preceding bar
"" * (a:progress) -- percentage of completion
"" * (a:char)     -- character used to draw the bar
"" * (a:barlen)   -- bar length (in chars)
"
"    " loop to get current bar length
"    let mybar = ""
"    let i = 0
"    while i < a:barlen
"        " bar characters
"        if i < (a:progress * a:barlen) / 100
"            let mybar = mybar . a:char
"        " remaining spaces
"        else
"            let mybar = mybar . " "
"        endif
"        let i = i + 1
"    endwhile
"
"    " set echo string
"    let @x = a:string . "|" . mybar . "| " . a:progress . "%"
"
"    " delete everything in buffer
"    " go to top, linewise select
"    normal gggH
"    " we need to change modes if in select
"    if mode() == "s" || mode() == "S"
"        execute "normal \<C-g>"
"    endif
"    " go to bottom, delete
"    execute "normal G\<Del>"
"    " paste new bar
"    normal "xp
"    " redraw screen (and refresh background to remove cursor)
"    redraw!
"
"endfunction


" Variable Type Conversions {{{1

function! Hex2Nr(hex)
" return the decimal value of a Hex string
	"execute "return 0x" . a:hex
	let val = ("0x" . a:hex) + 0
	"while strlen(val) < 2
	"    let val = "0" . val
	"endwhile
	return val
endfunction

function! Nr2Hex(nr)
" return the Hex string of a number
" Inspiration:  :help eval-examples
	let n = a:nr
	let r = ""
	while n
		let r = '0123456789ABCDEF'[n % 16] . r
		let n = n / 16
	endwhile
	" return a real 0, not empty!
	if r == ""
		return "0"
	else
		" ensure value padded to two digits
		while strlen(r) < 2
			let r = "0" . r
		endwhile
		return tolower(r)
	endif
endfunction

function! String2Hex(str)
" convert each character in a string to a two character Hex string
" Inspiration:  from :help eval-examples
	let out = ''
	let i = 0
	while i < strlen(a:str)
		let mychar = Nr2Hex(char2nr(a:str[i]))
		" pad to two characters
		while strlen(mychar) < 2
			let mychar = "0" . mychar
		endwhile
		let out = out . mychar
		let i = i + 1
	endwhile
	return out
endfunction

function! Hex2String(hex, ...)
" convert each two character pair in a string to a character
" allows optional argument "silent" to avoid printing error

	let hex = a:hex
	" strip whitespace and ctrl chars
	let hex = substitute(hex, '[ [:cntrl:]]', '', 'g')

	" ensure pairs of chars
	if strlen(hex) % 2 != 0
		if a:0 == 0 || a:1 != "silent"
			call confirm(
			\ "Error in Hex2String(), unable to process unpaired values.\n" .
			\ "Found: decimal " . "" . "\n" .
			\ "\n", "&Ok", 1, "Info")
		endif
		return a:hex
	endif
	let out = ''
	let i = 0
	while i < strlen(hex)
		" char assumed to be two digits, but only use last if first is 0
		if hex[i] == "0"
			let mychar = hex[i + 1]
		else
			let mychar = hex[i] . hex[i + 1]
		endif
		let out = out . nr2char(Hex2Nr(mychar))
		let i = i + 2
	endwhile
	return out
endfunction

function! Cream_nonoctal(str)
" Returns number from string stripped of leading 0's (potentially
" mis-interpreted as octal).

	"return substitute(a:str, '\(^\|\n\|\s\)\zs0\+', '', 'g')

	" Alternate: strip leading 0's in a word
	return substitute(a:str, '\<0\+', '', 'g') + 0

endfunction

function! Cream_str2dec_pad(str)
" convert a string to a string of decimal numbers, padded to three
" registers ( "7" => "007")

	let newstr = a:str

	let mypos = 0
	let i = 0
	let mylength = strlen(newstr) * 3
	while mypos < mylength

		" get first part
		let mystrfirst = strpart(newstr, 0, mypos)
		" get middle part (always one char)
		let mystrmiddle = strpart(newstr, mypos, 1)
		" get last part
		let mystrlast = strpart(newstr, mypos + 1)

		" convert to decimal
		let mystrmiddle = char2nr(mystrmiddle)

		" expand to three characters
		while strlen(mystrmiddle) < 3
			let mystrmiddle = "0" . mystrmiddle
		endwhile

		" concatenate
		let newstr = mystrfirst . mystrmiddle . mystrlast
		" find new pos
		let mypos = mypos + 3

	endwhile

	return newstr

endfunction

function! Cream_dec2str_pad(dec)
" convert a string of decimal numbers, padded to three registers, to a
" string, see Cream_str2dec_pad() ( "007" => "7")

	let newstr = a:dec

	" strip all non-digits
	let newstr = substitute(newstr, "\\D", '', 'g')

""*** DEBUG:
"let n = confirm(
"    \ "DEBUG:\n" .
"    \ "  newstr  = \"" . newstr . "\"\n" .
"    \ "  a:dec   = \"" . a:dec . "\"\n" .
"    \ "\n", "&Ok\n&Cancel", 1, "Info")
"if n != 1
"    return
"endif
""***

	" un-3 digit ascii, char by char
	let mylen = strlen(newstr)
	let mypos = 0
	while mypos < mylen
		" get first part
		let mystrfirst = strpart(newstr, 0, mypos)
		" get middle part (always four chars)
		let mystrmiddle = strpart(newstr, mypos, 3)
		" get last part
		let mystrlast = strpart(newstr, mypos + 3)

		" strip leading "0"s
"let tmp1 = ""
		while strpart(mystrmiddle, 0, 1) == "0"
			let mystrmiddle = strpart(mystrmiddle, 1)
"let tmp1 = mystrmiddle
		endwhile
		" convert to char
"let tmp2 = mystrmiddle + 0
		let mystrmiddle = nr2char(mystrmiddle)

""*** DEBUG:
"let n = confirm(
"    \ "DEBUG:\n" .
"    \ "  mystrfirst   = \"" . mystrfirst . "\"\n" .
"    \ "  tmp1         = \"" . tmp1 . "\"\n" .
"    \ "  tmp2         = \"" . tmp2 . "\"\n" .
"    \ "  mystrmiddle  = \"" . mystrmiddle . "\"\n" .
"    \ "\n", "&Ok\n&Cancel", 1, "Info")
"if n != 1
"    return
"endif
""***
		" concatenate
		let newstr = mystrfirst . mystrmiddle . mystrlast
		" find new pos
		let mypos = mypos + 1
	endwhile

	return newstr

endfunction

function! Cream_str2nr_sum(str)
" convert a string to a number, the equivelent of each character's
" decimal value summed.

	let nr = 0
	let i = 0
	while i < strlen(a:str)
		let nr = nr + char2nr(strpart(a:str, i, 1))
		let i = i + 1
	endwhile

	return nr

endfunction

function! Cream_format_number_commas(str)
" Add commas to a string every three characters. ("75300242" => "75,300,242")

	" don't process less than 4
	if strlen(a:str) < 4
		return a:str
	endif

	let tmp = a:str
	let str = ""
	while strlen(tmp) > 3
		" last three chars
		let str = "," . matchstr(tmp, '...$') . str
		" chew off 3 at a time
		let tmp = substitute(tmp, '...$', '', "")
	endwhile
	" tack on remaining
	let str = tmp . str

	return str

endfunction


" 1}}}
" vim:foldmethod=marker
