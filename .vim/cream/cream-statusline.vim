"
" cream-statusline.vim
"
" Cream -- An easy-to-use configuration of the famous Vim text  editor
" [ http://cream.sourceforge.net ] Copyright (C) 2001-2011 Steve Hall
"
" License:
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of  the  License,  or
" (at your option) any later version.
" [ http://www.gnu.org/licenses/gpl.html ]
"
" This program is distributed in the hope that it will be useful,  but
" WITHOUT  ANY  WARRANTY;  without  even  the  implied   warranty   of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the  GNU
" General Public License for more details.
"
" You should have received a copy of the GNU  General  Public  License
" along with  this  program;  if  not,  write  to  the  Free  Software
" Foundation,  Inc.,  59  Temple  Place  -  Suite  330,   Boston,   MA
" 02111-1307, USA.
"
" Description:
" o WARNING!! Statuslines prior to version 6.2.071 are unable to
"   handle more than 50 items. (An items is an "%" object, defined by
"   begin group, enclosee [, close group]). In fact, prior to some
"   version of 6.0.? this limit was even lower.
" o Close of a highlighting group (%*) is not necessary--Vim always
"   assumes the beginning of the next ends the previous.
" o Cream's statusline colors are dependent on colors definitions
"   elsewhere for User highlight groups 1-4, represented as "%N*" ...
"   "%*" corresponding to the highlight group "UserN"
" o Changing highlighting groups dynamically in the status bar has far
"   too much overhead and is too slow. So we:
"   * Preset all the highlight groups (elsewhere)
"   * Pair two calls to two corresponding ON/OFF functions for *each*
"     evaluation
"   * Return a value for the actual condition (displayed)
"   * Return empty for reverse (isn't displayed)
" o :set statusline= requires a very specific syntax and can not
"   accept a function since it is specially evaluated by Vim, probably
"   because it happens so often. Examples of illegal/non-functioning:
"
"     set statusline=Cream_statusline()
"     execute "set statusline=" . Cream_statusline()
"
"   It's the internal components of the statusline that continually
"   get re-evaluated, not the entire statusline. (&stl is not
"   continually executed, only the components *within*.)

" initialize statusline on load (autocmd will handle state by retained
" preference)
set laststatus=2

" evaluation functions
" path/file {{{1
function! Cream_statusline_path()
    call Cream_buffer_pathfile()
    " ignore path in gui (it's in titlebar)
    if has("gui_running")
        return " "
    endif
    " strip filename
    let path = fnamemodify(b:cream_pathfilename, ":h")
    " ensure trailing slash
    let path = Cream_path_addtrailingslash(path)
    return path
endfunction

" file state conditions
function! Cream_statusline_filestate()
    let state = ""

    " test read-only state once
    if !exists("b:cream_pathfilename") || b:cream_pathfilename == "(Untitled)"
        let b:cream_readonly = 0
    else
        let b:cream_readonly = filewritable(b:cream_pathfilename)
    endif

    " help file
    if &buftype == "help"
        return 'H'
    " writable
    elseif b:cream_readonly == 0
        \ || &readonly || &buftype == "nowrite"
        return '-'
    " modified
    elseif &modified != 0
        return '*'
    " unmodified
    else
        return ' '
    endif
endfunction

function! Cream_statusline_filename()
    if !exists("b:cream_pathfilename")
        return "(Untitled)"
    elseif b:cream_pathfilename == ""
        return "(Untitled)"
    endif
    return fnamemodify(b:cream_pathfilename, ":t")
endfunction

" file properties {{{1
" grouped to preserve group count

" fileformat (three characters only)
function! Cream_statusline_fileformat()
    if &fileformat == ""
        return "--"
    else
        return &fileformat
    endif

endfunction

" fileencoding (three characters only)
function! Cream_statusline_fileencoding()
    if &fileencoding == ""
        if &encoding != ""
            return &encoding
        else
            return "--"
        endif
    else
        return &fileencoding
    endif
endfunction

" file type
function! Cream_statusline_filetype()
    if &filetype == ""
        return "--"
    else
        return &filetype
    endif
endfunction

function! Cream_statusline_fileinfo()

    return Cream_statusline_fileformat() . ":" .
        \  Cream_statusline_fileencoding() . ":" .
        \  Cream_statusline_filetype()

endfunction


" specials {{{1

" indicate expert mode
function! Cream_statusline_expert()
    if exists("g:CREAM_EXPERTMODE") && g:CREAM_EXPERTMODE == 1
        return "expert "
    endif
    return ""
endfunction

" diff mode
function! Cream_statusline_diffmode()
    if exists("b:cream_diffmode")
        return "diff "
    endif
    return ""
endfunction

function! Cream_statusline_specials()
" this function collects multiple special states together so that we
" are able to minimize the number of items in the final statusline

    let myspecials = ""
    let myspecials = myspecials . Cream_statusline_expert()
    let myspecials = myspecials . Cream_statusline_diffmode()
    return myspecials

endfunction

" right side {{{1

" show invisibles
function! Cream_statusline_showON()
    "if exists("g:LIST") && g:LIST == 1
    if &list
        if     &encoding == "latin1"
            "return "¶"
            return nr2char(182)
        elseif &encoding == "utf-8"
        \ && v:version >= 602
        \ || v:version == 601
        \ && has("patch469")
            " decimal 182
            return nr2char(182)
        else
            return "$"
        endif
    else
        return ""
    endif
endfunction
function! Cream_statusline_showOFF()
    "if exists("g:LIST") && g:LIST == 1
    if &list
        return ""
    else
        if &encoding == "latin1"
            "return "¶"
            return nr2char(182)
        elseif &encoding == "utf-8"
        \ && v:version >= 602
        \ || v:version == 601
        \ && has("patch469")
            " decimal 182
            return nr2char(182)
        else
            return "$"
        endif
    endif
endfunction

" Word wrap
function! Cream_statusline_wrapON()
    if &wrap
        return "wrap"
    else
        return ""
    endif
endfunction
function! Cream_statusline_wrapOFF()
    if &wrap
        return ""
    else
        return "wrap"
    endif
endfunction

" Auto Wrap
function! Cream_statusline_autowrapON()
    if &textwidth
        return "auto " . &textwidth
    else
        return ""
    endif
endfunction
function! Cream_statusline_autowrapOFF()
    if &textwidth
        return ""
    else
        if exists("g:CREAM_AUTOWRAP_WIDTH")
            " use global, actual width is 0
            return "auto " . g:CREAM_AUTOWRAP_WIDTH
        else
            return "auto " . &textwidth
        endif
    endif
endfunction

" wrap width
function! Cream_statusline_wrap_width()
" return wrap width setting
    " (don't check existance of g:CREAM_AUTOWRAP)
    "if exists("g:CREAM_AUTOWRAP_WIDTH")
    "    return g:CREAM_AUTOWRAP_WIDTH
    "endif
    " respect Vim actual settings (modelines)
    return &textwidth
endfunction

" justification
function! Cream_statusline_wrap_justifyON()
" return justification mode if not "left"
    if !exists("g:cream_justify")
        return ""
    endif
    if     g:cream_justify == "center"
        return "cntr"
    elseif g:cream_justify == "right"
        return "rght"
    elseif g:cream_justify == "full"
        return "full"
    else
        return ""
    endif
endfunction
function! Cream_statusline_wrap_justifyOFF()
" return justification mode if not "left"
    if !exists("g:cream_justify")
        return "left"
    endif
    if     g:cream_justify == "left"
        return "left"
    else
        return ""
    endif
endfunction

" tabs
" &expandtab
function! Cream_statusline_expandtabON()
    if &expandtab == 0
        return "tabs"
    else
        return ""
    endif
endfunction
function! Cream_statusline_expandtabOFF()
    if &expandtab == 0
        return ""
    else
        return "tabs"
    endif
endfunction

" tabstop and softtabstop
function! Cream_statusline_tabstop()

    " show by Vim option, not Cream global (modelines)
    let str = "" . &tabstop
    " show softtabstop or shiftwidth if not equal tabstop
    if   (&softtabstop && (&softtabstop != &tabstop))
    \ || (&shiftwidth  && (&shiftwidth  != &tabstop))
        if &softtabstop
            let str = str . ":sts" . &softtabstop
        endif
        if &shiftwidth != &tabstop
            let str = str . ":sw" . &shiftwidth
        endif
    endif
    return str

endfunction

" autoindent
"function! Cream_statusline_autoindentON()
"    if exists("g:CREAM_AUTOINDENT")
"        if g:CREAM_AUTOINDENT == "1"
"            return "indt"
"        else
"            return ""
"        endif
"    else
"        " wrap is on if never initialized
"        return "wrap"
"    endif
"endfunction
"function! Cream_statusline_autoindentOFF()
"    if exists("g:CREAM_AUTOINDENT")
"        if g:CREAM_AUTOINDENT == "1"
"            return ""
"        else
"            return "indt"
"        endif
"    else
"        " autoindent is on if never initialized
"        return ""
"    endif
"endfunction
function! Cream_statusline_autoindentON()
    if &autoindent
        return "indt"
    else
        return ""
    endif
endfunction
function! Cream_statusline_autoindentOFF()
    if &autoindent
        return ""
    else
        return "indt"
    endif
endfunction

" mode (Insert/Visual/Select/Replace v. Normal)
" ( see :help mode() for return strings)
function! Cream_statusline_modeNO()
    let mymode = mode()
    if mymode ==? "i"
        return ""
    elseif mymode ==? "v"
        return ""
    elseif mymode ==? "s"
        return ""
    elseif mymode ==? "R"
        return ""
    elseif mymode == ""
        return "C"
    elseif mymode ==? "n"
        return "N"
    else
        return "  " . mymode . "  "
    endif
endfunction
"function! Cream_statusline_modeCOL()
"    let mymode = mode()
"    else
"        return ""
"    endif
"endfunction
function! Cream_statusline_modeOK()
    let mymode = mode()
    if     mymode ==? "i"
        return "I"
    elseif mymode ==? "v"
        return "V"
    elseif mymode ==? "s"
        return "S"
    elseif mymode ==? "R"
        return "R"
    elseif mymode == ""
        return ""
    else
        return ""
    endif
endfunction

function! Cream_statusline_bufsize()
    let bufsize = line2byte(line("$") + 1) - 1
    " prevent negative numbers (non-existant buffers)
    if bufsize < 0
        let bufsize = 0
    endif
    " add commas
    let remain = bufsize
    let bufsize = ""
    while strlen(remain) > 3
        let bufsize = "," . strpart(remain, strlen(remain) - 3) . bufsize
        let remain = strpart(remain, 0, strlen(remain) - 3)
    endwhile
    let bufsize = remain . bufsize
    " too bad we can't use "¿" (nr2char(1068)) :)
    let char = "b"
    return bufsize . char
endfunction

" 1}}}

" utility function ("real time") {{{1

function! Cream_cursor_pos(mode, cursor)
" NOTE: Function must be global.

    if     a:mode == "\<C-V>"
        let mode = "V"
    elseif a:mode == "\<C-S>"
        let mode = "S"
    else
        let mode = a:mode
    endif
    let b:cursor_{mode} = a:cursor
    return ""
endfunction

" 1}}}

" set statusline {{{1

if v:version < 603
" limited number of fields

    set statusline=
        \%{Cream_cursor_pos(mode(),virtcol(\".\"))}
        \%2*%{Cream_statusline_filename()}
        \\ %3*%{Cream_statusline_filestate()}
        \%1*\|%{Cream_statusline_fileinfo()}\|
        \%{Cream_statusline_bufsize()}\ %=
        \%3*%{Cream_statusline_specials()}
        \%1*\|
        \%2*%{Cream_statusline_showON()}
        \%1*%{Cream_statusline_showOFF()}\|
        \%2*%{Cream_statusline_wrapON()}
        \%1*%{Cream_statusline_wrapOFF()}:
        \%2*%{Cream_statusline_autowrapON()}
        \%1*%{Cream_statusline_autowrapOFF()}:
        \%{Cream_statusline_wrap_width()}\|
        \%2*%{Cream_statusline_expandtabON()}
        \%1*%{Cream_statusline_expandtabOFF()}:
        \%{Cream_statusline_tabstop()}\|
        \%05(%l%),%03(%v%)
        \%2*\ %P

else

    set statusline=
        \%{Cream_cursor_pos(mode(),virtcol(\".\"))}
        \%{fugitive#statusline()}
        \%1*%{Cream_statusline_path()}
        \%2*%{Cream_statusline_filename()}
        \\ %3*%{Cream_statusline_filestate()}
        \%1*\|%{Cream_statusline_fileinfo()}\|
        \%{Cream_statusline_bufsize()}\ %=
        \%3*%{Cream_statusline_specials()}
        \%1*\|
        \%2*%{Cream_statusline_showON()}
        \%1*%{Cream_statusline_showOFF()}\|
        \%2*%{Cream_statusline_wrapON()}
        \%1*%{Cream_statusline_wrapOFF()}:
        \%2*%{Cream_statusline_autowrapON()}
        \%1*%{Cream_statusline_autowrapOFF()}:
        \%3*%{Cream_statusline_wrap_justifyON()}
        \%1*%{Cream_statusline_wrap_justifyOFF()}\|
        \%2*%{Cream_statusline_expandtabON()}
        \%1*%{Cream_statusline_expandtabOFF()}:
        \%{Cream_statusline_tabstop()}:
        \%2*%{Cream_statusline_autoindentON()}
        \%1*%{Cream_statusline_autoindentOFF()}\|
        \%4*%{Cream_statusline_modeNO()}
        \%1*%{Cream_statusline_modeOK()}\|
        \%05(%l%),%03(%v%)
        \%2*\ %P

endif

" 1}}}
" vim:foldmethod=marker
