"
" Filename: cream-lib-os.vim
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

" Local functions {{{1

"function! s:OnMS()
"" NOTE: This localized function a duplicate of that from genutils.vim.
""
"    return has("win32") || has("dos32") || has("win16") || has("dos16") || has("win95")
"endfunction


" Path and filenames {{{1

function! Cream_path_fullsystem(path, ...)
" Return a completely expanded and valid path for the current system
" from string {path}.
" o Path does not have to exist, although generally invalid formats
"   will not usually be properly expanded (e.g., backslash path separators
"   on Unix).
" o Both files and directories may be processed. (Paths will not be
"   returned with a trailing path separator.)
" o Preserves Windows UNC server name preceding "\\".
" o {optional} argument can be used to override system settings:
"   * "win" forces return in Windows format as possible:
"     - No drive letter is added (none can be assumed)
"   * "unix" forces return to Unix format as possible:
"     - Windows drive letter is not removed
"

	" format type
	" forced with argument
	if a:0 == 1
		if     a:1 == "win"
			let format = "win"
		elseif a:1 == "unix"
			let format = "unix"
		endif
	endif
	" detected if not forced
	if !exists("format")
		if Cream_has("ms")
			let format = "win"
		else
			let format = "unix"
		endif
	endif

	" expand to full path
	let path = fnamemodify(a:path, ":p")

	" make Windows format (assume is Unix)
	if format == "win"

		" remove escaping of spaces
		let path = substitute(path, '\\ ', ' ', "g")

		" convert forward slashes to backslashes
		let path = substitute(path, '/', '\', "g")

	" make Unix format (assume is Windows)
	else

		"" strip drive letter
		"let path = substitute(path, '^\a:', '', '')

		" convert backslashes to forward slashes
		let path = substitute(path, '\', '/', "g")

		" escape spaces (but not twice)
		let path = substitute(path, '[/\\]* ', '\\ ', "g")

	endif

	" remove duplicate separators
	let path = substitute(path, '\\\+', '\', "g")
	let path = substitute(path, '/\+', '/', "g")

	" remove trailing separators
	let path = substitute(path, '[/\\]*$', '', "")

	" maintain Windows UNC servername
	if Cream_path_isunc(a:path)
		let path = substitute(path, '^\', '\\\\', "")
		let path = substitute(path, '^/', '\\\\', "")
	endif

	return path

endfunction

function! Cream_path_isunc(path)
" Returns 1 if {path} is in Windows UNC format, 0 if not.
	if match(a:path, '^\\\\') != -1 || match(a:path, '^//') != -1
		return 1
	endif
endfunction

function! Cream_pathexists(path)
" Returns 1 if {path} is an existing file or directory. (Ignores
" ability to write or read to it, only if exists.)

	" if both unreadable and not a directory
	if !filereadable(a:path) && filewritable(a:path) != 2
		return 0
	else
		return 1
	endif

endfunction

" Open with default app {{{1

function! Cream_file_open_defaultapp(...)
" Open file {...} with OS default application. If none passed, current
" file used.

	if a:0 == 0
		" use current file
		let file = Cream_path_fullsystem(expand("%"))
	else
		let file = a:1
	endif

	" GNOME2
	if     has("gui_gtk2")
		let cmd = 'silent! !gnome-open ' . escape(file, ' ')
	" Windows
	elseif Cream_has("ms")
		" 95/98/ME
		if has("win95")
			let cmd = 'silent! !command /c start "' . file . '"'
		" NT/2K/XP
		else
			" handle URLs differently (http://www.google.com)
			if Cream_isURL(file)
				" use start, don't quote
				let cmd = 'silent! !cmd /c start ' . file
			else
				let cmd = 'silent! !cmd /c "' . file . '"'
			endif
		endif
	else
		" no command found, exit
		call confirm(
			\ "Platform not identified, unable to open file.\n" .
			\ "\n", "&Ok", 1, "Info")
		return
	endif
		if Cream_has("ms")
			let myshellslash = &shellslash
			set noshellslash
			silent! execute cmd
			let &shellslash = myshellslash
		else
			silent! execute cmd
		endif
	return 1

endfunction

" Open file explorer {{{1
"function! Cream_open_fileexplorer()
"" open the OS file manager to the current file's directory
"
"    " parse b:cream_pathfilename (respects links as opposed to
"    " getcwd())
"    if exists("b:cream_pathfilename")
"        let mypath = fnamemodify(b:cream_pathfilename, ":p:h")
"    else
"        let mypath = getcwd()
"    endif
"
"    call Cream_file_open_defaultapp(mypath)
"
"endfunction
function! Cream_open_fileexplorer()
" open the OS file manager to the current file's directory

	" parse b:cream_pathfilename (respects links as opposed to
	" getcwd())
	if exists("b:cream_pathfilename")
		let mypath = fnamemodify(b:cream_pathfilename, ":p:h")
	else
		let mypath = getcwd()
	endif

	let mypath = Cream_path_fullsystem(mypath)

	" NOTE: On Windows XP (2008-01-07), pointing to a cmd.exe to a
	" directory fails to open explorer--call explorer specifically
	"
	"call Cream_file_open_defaultapp(mypath)
	"
	" GNOME2
	if     has("gui_gtk2")
		let cmd = 'silent! !gnome-open ' . escape(mypath, ' ')
	" Windows
	elseif Cream_has("ms")
		" 95/98/ME
		if has("win95")
			let cmd = 'silent! !command /c start "' . mypath . '"'
		" NT/2K/XP
		else
			"let cmd = 'silent! !cmd /c "' . mypath . '"'
			let cmd = 'silent! !explorer /e,"' . mypath . '"'
		endif
	endif

	" no command found, exit
	if !exists("cmd")
		call confirm("Platform not identified, unable to open file.\n", "&Ok", 1, "Info")
		return
	endif
	silent! execute cmd
	return 1

endfunction

" File operations {{{1

"function! Cream_touch(pathfile)
"" create an empty file {pathfile}
"" WARNING: Will overwrite an existing file of the same name!
"
"    " test that head exists
"    if !Cream_pathexists(fnamemodify(a:pathfile, ":p:h"))
"        call confirm(
"            \ "Error: Invalid path passed to Cream_touch()\n" .
"            \ "Touch file not created\n" .
"            \ "\n", "&Ok", 1, "Warning")
"        return -1
"    endif
"
"    " save position
"    if exists("$CREAM")
"        let mypos = Cream_pos()
"    endif
"
"    silent! enew
"    silent! execute "saveas! " . a:pathfile
"    " close buffer
"    silent! bwipeout!
"
"    " restore pos
"    if exists("$CREAM")
"        execute mypos
"    endif
"
"endfunction

function! Cream_touch(pathfile)
" create an empty file {pathfile}, prompting if it exists

	" test that head exists
	if !Cream_pathexists(fnamemodify(a:pathfile, ":p:h"))
		call confirm(
			\ "Error: Invalid path passed to Cream_touch().\n" .
			\ "File not created.\n" .
			\ "\n", "&Ok", 1, "Warning")
		return -1
	endif

	if Cream_has("ms")
		let pathfile = fnamemodify(a:pathfile, ":p:8")
	else
		let pathfile = fnamemodify(a:pathfile, ":p")
	endif

	execute "silent! confirm 0write " . pathfile

endfunction

" Windows drive letters {{{1
function! Cream_windrives()
" Return string listing existing drive letters on MS Windows. Each is
" returned followed by a newline.

	if !Cream_has("ms")
		return ""
	endif

	let cmd = 'for %A in (C D E F G H I J K L M N O P Q R S T U V W X Y Z) do if exist %A:\CON echo %A: >nul'

	redir @x
	silent! echo system(cmd)
	redir END

	let drives = @x
	" remove DOS echos
	let drives = substitute(drives, '\u:[[:print:]]\{-1,}>if exist \u:\\CON echo \u:', '', 'g')
	" remove extra lines
	let drives = substitute(drives, '\n\n', '', 'g')
	" remove leading spaces
	let drives = substitute(drives, '^[ \r\n]\+', '', 'g')

	return drives

endfunction

" Windows Shortcut {{{1
function! Cream_make_shortcut(spath, sname, target, key, desc)
" Make a windows shortcut.
"
" Example:
"   call Cream_make_shortcut( .
"   \ "C:\WINDOWS\Desktop", .
"   \ "MyShortcut", .
"   \ "C:\WINDOWS\notepad.exe", .
"   \ "Ctrl+Shift+N", .
"   \ "My Notepad".
"   \ )
"
" Arguments:
" o {spath}  :: Path to shortcut, must exist.
" o {sname}  :: Name of shortcut, extension ".lnk" not required.
" o {target} :: Target (exe) of shortcut (can be empty "").
" o {key}    :: Keyboard shortcut, e.g. "Ctrl+Shift+N".
" o {desc}   :: Brief description of shortcut, can be empty.

	" only if on Windows
	if !Cream_has("ms")
		call confirm(
			\ "Only available on Windows systems.\n" .
			\ "\n", "&Ok", 1, "Info")
	endif
	" warn if not 9x/NT/64
	if has("win95") || has("win16") || has("win32unix")
		let n = confirm(
			\ "This has not been tested on this version of Windows, continue?\n" .
			\ "\n", "&Ok", 1, "Info")
		if n != 1
			return
		endif
	endif

	" verify a:spath has trailing slash
	let spath = Cream_path_addtrailingslash(a:spath)

	" reject if a:spath doesn't point to a real directory
	if filewritable(spath) != 2
		let n = confirm(
		\ "Path for shortcut doesn't exist, unable to continue.\n" .
		\ "\n", "&Ok", 1, "Info")
		return
	endif

	" a:sname--add .lnk if doesn't exist
	let sname = a:sname
	if strpart(sname, strlen(sname)-4) !=? ".lnk"
		let sname = sname . ".lnk"
	endif

	" warn if a:target doesn't point to a real file
	if filereadable(a:target) != 1
		if filewritable(a:target) == 2
			let n = confirm(
			\ "Ok that target points to a directory?\n" .
			\ "\n", "&Ok\n&Cancel", 1, "Info")
			if n != 1
				return
			endif
		else
			let n = confirm(
			\ "Target does not exist, create anyway?\n" .
			\ "\n", "&Ok\n&Cancel", 1, "Info")
			if n != 1
				return
			endif
		endif
	endif

	" warn if a:desc is too long
	if strlen(a:desc) > 200
		let n = confirm(
			\ "Description is too long for a tooltip, continue anyway?\n" .
			\ "\n", "&Ok", 1, "Info")
		if n != 1
			return
		endif
	endif

	" cat valid VBS statements
	let @x = ''
	let @x = @x . 'set WshShell = WScript.CreateObject("WScript.Shell")' . "\n"
	let @x = @x . 'set oShortCutLink = WshShell.CreateShortcut("' . spath . '" & "' . sname . '")' . "\n"
	let @x = @x . 'oShortCutLink.TargetPath = "' . a:target . '"' . "\n"
	let @x = @x . 'oShortCutLink.WindowStyle = 1' . "\n"
	let @x = @x . 'oShortCutLink.Hotkey = "' . a:key . '"' . "\n"
	let @x = @x . 'oShortCutLink.Description = "' . a:desc . '"' . "\n"
	let @x = @x . 'oShortCutLink.Save' . "\n"

	" write as temp VBS file
	call Cream_file_new()
	put x
	let fname = tempname() . ".vbs"
	execute "saveas " . fname

	" run
	call Cream_file_open_defaultapp()

	" close it
	silent bwipeout!

	" delete file
	call delete(fname)

	" verify .lnk made
	if !filereadable(spath . sname)
		call confirm(
			\ "Error: Shortcut not made.\n" .
			\ "\n", "&Ok", 1, "Info")
	endif

endfunction

" 1}}}
" vim:foldmethod=marker
