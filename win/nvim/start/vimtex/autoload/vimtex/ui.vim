" VimTeX - LaTeX plugin for Vim
"
" Maintainer: Karl Yngve Lervåg
" Email:      karl.yngve@gmail.com
"

function! vimtex#ui#echo(input, ...) abort " {{{1
  if empty(a:input) | return | endif
  let l:opts = extend({'indent': 0}, a:0 > 0 ? a:1 : {})

  if type(a:input) == v:t_string
    call s:echo_string(a:input, l:opts)
  elseif type(a:input) == v:t_list
    call s:echo_formatted(a:input, l:opts)
  elseif type(a:input) == v:t_dict
    call s:echo_dict(a:input, l:opts)
  else
    call vimtex#log#warn('Argument not supported: ' . type(a:input))
  endif
endfunction

" }}}1

function! vimtex#ui#confirm(prompt) abort " {{{1
  return vimtex#ui#{g:vimtex_ui_method.confirm}#confirm(a:prompt)
endfunction

" }}}1
function! vimtex#ui#input(options) abort " {{{1
  let l:options = extend({
        \ 'prompt': '> ',
        \ 'text': '',
        \ 'info': '',
        \}, a:options)

  return vimtex#ui#{g:vimtex_ui_method.input}#input(l:options)
endfunction

" }}}1
function! vimtex#ui#menu(actions) abort " {{{1
  " Argument: The 'actions' argument is a dictionary/object which contains
  "   a list of menu items and corresponding actions (dict functions).
  "   Something like this:
  "
  "   let a:actions = {
  "         \ 'prompt': 'Prompt string for menu',
  "         \ 'menu': [
  "         \   {'name': 'My first action',
  "         \    'func': 'action1'},
  "         \   {'name': 'My second action',
  "         \    'func': 'action2'},
  "         \   ...
  "         \ ],
  "         \ 'action1': Func,
  "         \ 'action2': Func,
  "         \ ...
  "         \}
  if empty(a:actions) | return | endif

  let l:choice = vimtex#ui#select(a:actions.menu, {
        \ 'prompt': a:actions.prompt,
        \})
  if empty(l:choice) | return | endif

  try
    call a:actions[l:choice.func]()
  catch
    " error here
  endtry
endfunction

" }}}1
function! vimtex#ui#select(container, ...) abort " {{{1
  let l:options = extend(
        \ {
        \   'prompt': 'Please choose item:',
        \   'return': 'value',
        \   'force_choice': v:false,
        \   'auto_select': v:true,
        \ },
        \ a:0 > 0 ? a:1 : {})

  let l:list = type(a:container) == v:t_dict
        \ ? values(a:container)
        \ : a:container
  let [l:index, l:value] = empty(l:list)
        \ ? [-1, '']
        \ : (len(l:list) == 1 && l:options.auto_select
        \   ? [0, l:list[0]]
        \   : vimtex#ui#{g:vimtex_ui_method.select}#select(l:options, l:list))

  if l:options.return ==# 'value'
    return l:value
  endif

  if type(a:container) == v:t_dict
    return l:index >= 0 ? keys(a:container)[l:index] : ''
  endif

  return l:index
endfunction

" }}}1

function! vimtex#ui#get_number(max, digits, force_choice, do_echo) abort " {{{1
  let l:choice = ''

  if a:do_echo
    echo '> '
  endif

  while len(l:choice) < a:digits
    if len(l:choice) > 0 && (l:choice . '0') > a:max
      return l:choice - 1
    endif

    let l:input = nr2char(getchar())

    if !a:force_choice && index(["\<C-c>", "\<Esc>", 'x'], l:input) >= 0
      if a:do_echo
        echon 'aborted!'
      endif
      return -2
    endif

    if len(l:choice) > 0 && l:input ==# "\<cr>"
      return l:choice - 1
    endif

    if l:input !~# '\d' | continue | endif

    if (l:choice . l:input) > 0
      let l:choice .= l:input
      if a:do_echo
        echon l:input
      endif
    endif
  endwhile

  return l:choice - 1
endfunction

" }}}1

function! vimtex#ui#get_winwidth() abort " {{{1
  let l:numwidth = (&number || &relativenumber)
        \ ? max([&numberwidth, strlen(line('$')) + 1])
        \ : 0
  let l:foldwidth = str2nr(matchstr(&foldcolumn, '\d\+$'))

  " Get width of signcolumn
  " Note: A signcolumn is 2-char wide, so in some cases we multiply by 2
  if &signcolumn ==# 'yes'
    let l:signwidth = 2
  elseif &signcolumn =~# 'yes'
    let l:signwidth = 2*split(&signcolumn, ':')[1]
  elseif &signcolumn ==# 'auto'
    let l:signlist = split(execute(
          \ printf('sign place %s buffer=%d',
          \   has('nvim-0.4.2') || has('patch-8.1.614') ? 'group=*' : '',
          \   bufnr())), "\n")
    let l:signwidth = len(l:signlist) > 2 ? 2 : 0
  elseif &signcolumn =~# 'auto'
    " Get number of signs on each line that has a sign
    let l:sign_lenths = map(
          \ sign_getplaced(bufnr(), {'group': '*'})[0].signs,
          \ { _, x -> len(
          \   sign_getplaced(bufnr(),
          \                  {'group': '*', 'lnum': x.lnum})[0].signs)})
    let l:signwidth = 2*max(l:sign_lenths)
  else
    let l:signwidth = 0
  endif

  return winwidth(0) - l:numwidth - l:foldwidth - l:signwidth
endfunction

" }}}1

function! s:echo_string(msg, opts) abort " {{{1
  echohl VimtexMsg
  echo repeat(' ', a:opts.indent) . a:msg
  echohl None
endfunction

" }}}1
function! s:echo_formatted(parts, opts) abort " {{{1
  echo repeat(' ', a:opts.indent)
  try
    for l:part in a:parts
      if type(l:part) == v:t_string
        echohl VimtexMsg
        echon l:part
      else
        execute 'echohl' l:part[0]
        echon l:part[1]
      endif
      unlet l:part
    endfor
  finally
    echohl None
  endtry
endfunction

" }}}1
function! s:echo_dict(dict, opts) abort " {{{1
  for [l:key, l:val] in items(a:dict)
    call s:echo_formatted([['Label', l:key . ': '], l:val], a:opts)
  endfor
endfunction

" }}}1
