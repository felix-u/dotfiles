" VimTeX - LaTeX plugin for Vim
"
" Maintainer: Karl Yngve Lervåg
" Email:      karl.yngve@gmail.com
"

function! vimtex#syntax#p#pythontex#load(cfg) abort " {{{1
  call vimtex#syntax#nested#include('python')

  syntax match texCmdPythontex /\\py[bsc]\?/ nextgroup=texPythontexArg skipwhite skipnl
  call vimtex#syntax#core#new_arg('texPythontexArg', {
        \ 'contains': '@vimtex_nested_python',
        \ 'opts': 'contained keepend'
        \})
  syntax region texPythontexArg matchgroup=texDelim
        \ start='\z([#@]\)' end='\z1'
        \ contained contains=@vimtex_nested_python keepend

  call vimtex#syntax#core#new_env({
        \ 'name': 'py\%(block\|code\)',
        \ 'region': 'texPythontexZone',
        \ 'contains': '@vimtex_nested_python'
        \})

  highlight def link texCmdPythontex texCmd
endfunction

" }}}1
