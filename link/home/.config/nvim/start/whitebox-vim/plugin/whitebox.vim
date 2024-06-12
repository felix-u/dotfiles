let g:whitebox_enabled = 1
let g:whitebox_port    = 19013
let g:whitebox_debug   = 0

function! WBSetPortFromFile()
    if (!empty(glob($HOME . '/.whitebox/port')))
        let port_string = readfile($HOME . '/.whitebox/port')
        if len(port_string) > 0
            " echom str2nr(port_string[0])
            let new_port = str2nr(port_string[0])
            if new_port > 0 && new_port < 65536
                let g:whitebox_port = str2nr(port_string[0])
            endif
        endif
    endif
endfunction

if has('nvim')
    let s:editor_name='Neovim'

    function! WBSockConnect()
        call WBSetPortFromFile()
        return sockconnect("tcp", "localhost:".g:whitebox_port)
    endfunction

    function! WhiteBoxIsConnected()
        return exists('s:wb_ch') && (s:wb_ch > 0)
    endfunction
    function! WhiteBoxConnectionCheck()
        echom "WhiteBox connection ".(WhiteBoxIsConnected() ? "open" : "closed")
    endfunction

    function! WhiteBoxSendData(data)
        call WhiteBoxDbg('sending: '.a:data)
        call chansend(s:wb_ch, a:data)
    endfunction

    function! WhiteBoxDisconnect()
        call chanclose(s:wb_ch)
    endfunction

else " Vim
    let s:editor_name='Vim'

    function! WBSockConnect()
        call WBSetPortFromFile()
        return ch_open("localhost:".g:whitebox_port, {"mode":"nl"}) " newline mode (messages separated by '\n')
    endfunction

    function! WhiteBoxIsConnected()
        return exists('s:wb_ch') && (ch_status(s:wb_ch) ==# 'open')
    endfunction
    function! WhiteBoxConnectionCheck()
        echom "WhiteBox connection ".ch_status(s:wb_ch)." (port ".g:whitebox_port.")"
    endfunction

    function! WhiteBoxSendData(data)
        call WhiteBoxDbg('sending: '.a:data)
        call ch_sendraw(s:wb_ch, a:data)
    endfunction

    function! WhiteBoxDisconnect()
        call ch_close(s:wb_ch)
    endfunction
endif

function! WhiteBoxDbg(msg)
    if g:whitebox_debug
        echom a:msg
    endif
    return a:msg
endfunction


function! WhiteBoxPluginJSON()
    let curpos = getpos('.')
    let rngpos = getpos('v')
    let json_dict = {
                \ 'editor': s:editor_name,
                \ 'path': expand('%:p'),
                \ 'selection': [
                \     { 'line': curpos[1], 'column': curpos[2] },
                \     { 'line': rngpos[1], 'column': rngpos[2] },
                \ ],
                \ 'dirty': (&mod ? ['unsaved'] : []),
                \ }
    return json_encode(json_dict)
endfunction

function! WhiteBoxFnCaller(name, text)
    let json_dict = {
                \   'fn_caller' : {
                \       'text': a:text,
                \       'name': a:name,
                \   }
                \ }
    return json_encode(json_dict)
endfunction

function! WhiteBoxPluginData(editor)
    let curpos = getpos('.')
    let rngpos = getpos('v')
    let fpath  = expand('%:p')
    let dir    = expand('%:p:h')
    let fout   = dir . '\cursor.pos'
    " [1]: line number, [2]: col number
    " if there is no selection, these will be the same
    let select_rng = curpos[1].','.curpos[2] .'-'. rngpos[1].','.rngpos[2]

    " let text = '"'.fpath.' '.select_rng.'"'
    let text = a:editor.' '.&mod.' '.select_rng.' '.fpath."\n"
    return text
endfunction

function! WhiteBoxOnChange()
    if ! g:whitebox_enabled
        return
    endif

    if WhiteBoxIsConnected()
        call WhiteBoxSendData(WhiteBoxPluginJSON())
    endif
endfunction

function! WhiteBoxConnect()
    if ! WhiteBoxIsConnected()
        let s:wb_ch = WBSockConnect()
    endif
    call WhiteBoxConnectionCheck()
    call WhiteBoxOnChange()
endfunction

augroup whitebox
	au!
    autocmd CursorMoved,BufWritePost * call WhiteBoxOnChange()
augroup end
