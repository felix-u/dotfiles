" VimTeX - LaTeX plugin for Vim
"
" Maintainer: Karl Yngve Lervåg
" Email:      karl.yngve@gmail.com
"

function! vimtex#misc#init_buffer() abort " {{{1
  command! -buffer                VimtexReload call vimtex#misc#reload()
  command! -buffer -bang -range=% VimtexCountWords
        \ call vimtex#misc#wordcount_display({
        \   'range' : [<line1>, <line2>],
        \   'detailed' : <q-bang> == '!',
        \   'count_letters' : 0,
        \ })
  command! -buffer -bang -range=% VimtexCountLetters
        \ call vimtex#misc#wordcount_display({
        \   'range' : [<line1>, <line2>],
        \   'detailed' : <q-bang> == '!',
        \   'count_letters' : 1,
        \ })

  nnoremap <buffer> <plug>(vimtex-reload) :VimtexReload<cr>
endfunction

" }}}1

function! vimtex#misc#get_graphicspath(fname) abort " {{{1
  for l:root in b:vimtex.graphicspath + ['.']
    let l:candidate = simplify(b:vimtex.root . '/' . l:root . '/' . a:fname)
    for l:suffix in ['', '.jpg', '.png', '.pdf']
      if filereadable(l:candidate . l:suffix)
        return l:candidate . l:suffix
      endif
    endfor
  endfor

  return a:fname
endfunction

" }}}1
function! vimtex#misc#wordcount(...) abort " {{{1
  let l:opts = a:0 > 0 ? a:1 : {}

  let l:range = get(l:opts, 'range', [1, line('$')])
  if l:range == [1, line('$')]
    let l:file = b:vimtex
  else
    let l:file = vimtex#parser#selection_to_texfile({'range': l:range})
  endif

  let l:cmd = 'texcount -nosub -sum '
        \ . (get(l:opts, 'count_letters') ? '-letter ' : '')
        \ . (get(l:opts, 'detailed') ? '-inc ' : '-q -1 -merge ')
        \ . g:vimtex_texcount_custom_arg . ' '
        \ . vimtex#util#shellescape(l:file.base)
  let l:lines = vimtex#jobs#capture(l:cmd, {'cwd': l:file.root})

  if l:file.base !=# b:vimtex.base
    call delete(l:file.tex)
  endif

  if get(l:opts, 'detailed')
    return l:lines
  else
    call filter(l:lines, 'v:val !~# ''ERROR\|^\s*$''')
    return join(l:lines, '')
  endif
endfunction

" }}}1
function! vimtex#misc#wordcount_display(opts) abort " {{{1
  let output = vimtex#misc#wordcount(a:opts)

  if !get(a:opts, 'detailed')
    call vimtex#log#info('Counted '
          \ . (get(a:opts, 'count_letters') ? 'letters: ' : 'words: ')
          \ . output)
    return
  endif

  " Create wordcount window
  if bufnr('TeXcount') >= 0
    bwipeout TeXcount
  endif
  split TeXcount

  " Add lines to buffer
  for line in output
    call append('$', printf('%s', line))
  endfor
  0delete _

  " Set mappings
  nnoremap <silent><buffer><nowait> q :bwipeout<cr>

  " Set buffer options
  setlocal bufhidden=wipe
  setlocal buftype=nofile
  setlocal cursorline
  setlocal nobuflisted
  setlocal nolist
  setlocal nospell
  setlocal noswapfile
  setlocal nowrap
  setlocal tabstop=8
  setlocal nomodifiable

  " Set highlighting
  syntax match TexcountText  /^.*:.*/ contains=TexcountValue
  syntax match TexcountValue /.*:\zs.*/
  highlight link TexcountText  VimtexMsg
  highlight link TexcountValue Constant
endfunction

" }}}1
" {{{1 function! vimtex#misc#reload()
if get(s:, 'reload_guard', 1)
  function! vimtex#misc#reload() abort
    let s:reload_guard = 0

    " Stop all compiler processes
    silent call vimtex#compiler#stop_all()

    for l:file in glob(s:file . '/**/*.vim', 0, 1)
      execute 'source' l:file
    endfor

    " Temporarily unset b:current_syntax (if active)
    let l:reload_syntax = get(b:, 'current_syntax', '') ==# 'tex'
    if l:reload_syntax
      unlet b:current_syntax
    endif

    call vimtex#init()

    " Reload syntax
    if l:reload_syntax
      syntax clear
      runtime! syntax/tex.vim
    endif

    " Reload indent file
    if exists('b:did_vimtex_indent')
      unlet b:did_indent
      runtime indent/tex.vim
    endif

    call vimtex#log#info('The plugin has been reloaded!')
    unlet s:reload_guard
  endfunction
endif

" }}}1


let s:file = expand('<sfile>:h:h')
