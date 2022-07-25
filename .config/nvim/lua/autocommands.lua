-- remember cursor position
vim.cmd([[
autocmd BufRead * autocmd FileType <buffer> ++once
      \ if &ft !~# 'commit\|rebase' && line("'\"") > 1 && line("'\"") <= line("$") | exe 'normal! g`"' | endif
]])

-- remove trailing whitespace on save
vim.cmd([[autocmd BufWritePre * :%s/\s\+$//e]])
