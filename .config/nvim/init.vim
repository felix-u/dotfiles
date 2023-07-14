set autoindent
set autoread
set backspace=indent,eol,start " backspace in insert mode
set clipboard="unnamedplus"
set complete-=i " don't complete keywords in included files
set cursorline
set display+=lastline
set display+=truncate
set expandtab
set guicursor=n-v-c-i:block
set history=1000
set hlsearch
set ignorecase
set incsearch
set laststatus=0
set lazyredraw
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set mouse=a
set nocompatible
set nolangremap
set nonumber
set norelativenumber
set noshowcmd
set noswapfile
set nowrap
set nowritebackup
set nrformats-=octal
set ruler
set scrolloff=1
set sessionoptions-=options
set shiftwidth=4
set showmatch
set showtabline=2
set sidescrolloff=2
set smartindent
set smarttab
set splitbelow
set splitright
set tabpagemax=50
set tabstop=4
set timeout
set timeoutlen=400
set undofile
set viewoptions-=options
set viminfo^=!
set wildmenu
setglobal tags-=./tags tags-=./tags; tags^=./tags;	
filetype plugin indent on

function! s:tweak_default_colours()
  highlight clear
  for hlgroup in getcompletion('', 'highlight')
      execute 'highlight' hlgroup 'NONE'
  endfor
  hi link zigDummyVariable NONE
  hi Comment      cterm=bold   ctermfg=White ctermbg=00
  hi CursorLine   ctermbg=00
  hi LineNr       ctermfg=07   ctermbg=00
  hi MatchParen   cterm=bold,underline ctermfg=Black ctermbg=White
  hi netrwDir     ctermfg=04
  hi Pmenu        ctermfg=White ctermbg=Black
  hi PmenuSbar    ctermbg=08
  hi PmenuSel     ctermfg=Black ctermbg=White
  hi PmenuThumb   ctermfg=08 ctermbg=08
  hi Search       cterm=bold   ctermfg=Black ctermbg=03
  hi StatusLine   cterm=NONE   ctermfg=White ctermbg=00
  hi TabLine      cterm=NONE   ctermfg=White ctermbg=00
  hi TabLineFill  cterm=NONE   ctermfg=NONE  ctermbg=NONE
  hi TabLineSel   cterm=bold   ctermfg=Black ctermbg=White
  hi TermCursor   ctermfg=Black ctermbg=White
  hi Visual       ctermbg=08
  hi WildMenu     ctermfg=Black ctermbg=White
  autocmd BufReadPost *.c,*.h hi cError cterm=NONE
endfunction
autocmd! ColorScheme default call s:tweak_default_colours()
colorscheme default

let mapleader = " "

set commentstring=#\ %s
autocmd FileType c   set commentstring=//\ %s
autocmd FileType cpp set commentstring=//\ %s

" print syntax grouping under cursor
function! SynStack()
if !exists("*synstack")
    return
  endif
echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
noremap <leader>thg :call SynStack()<CR>

" Remember edit position.
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \ exe "normal! g`\"" |
  \ endif

noremap d  "_d
noremap dd "_dd
noremap D  "_d$
noremap c  "_c
noremap cc "_cc
noremap C  "_c$
noremap <leader>d  "+d
noremap <leader>dd "+dd
noremap <leader>D  "+d$
noremap <leader>c  "+c
noremap <leader>cc "+cc
noremap <leader>C  "+c$

noremap y "+y
noremap yy "+yy
noremap Y "+y$
noremap p "+p
noremap P "+P

nnoremap j gj
nnoremap k gk
nnoremap <Down> gj
nnoremap <Up> gk

nnoremap n nzzzv
nnoremap N Nzzzv

nnoremap <leader>stf :set linebreak!<CR> :set fo+=t<CR> :set tw=120<CR> :set wrap!<CR>

nnoremap <C-y> :%y+<CR>

nnoremap gj <C-^>

" More breakpoints for undo
inoremap , ,<C-g>u
inoremap . .<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u

" Stay in visual mode when indenting
vnoremap < <gv
vnoremap > >gv

nnoremap <leader>ts :setlocal spell! spelllang=en_gb<CR>
nnoremap <leader>tls :set number<CR> :set relativenumber<CR>
nnoremap <leader>tlh :set nonumber<CR> :set norelativenumber<CR>
nnoremap <leader>tn :Lex<CR>

nnoremap <C-w><Left> <C-w>h
nnoremap <C-w><Down> <C-w>j
nnoremap <C-w><Up> <C-w>k
nnoremap <C-w><Right> <C-w>l

nnoremap <C-1> :tabn 1<CR>
nnoremap <C-2> :tabn 2<CR>
nnoremap <C-3> :tabn 3<CR>
nnoremap <C-4> :tabn 4<CR>
nnoremap <C-5> :tabn 5<CR>
nnoremap <C-6> :tabn 6<CR>
nnoremap <C-7> :tabn 7<CR>
nnoremap <C-8> :tabn 8<CR>
nnoremap <C-9> :tabn 9<CR>
nnoremap <C-0> :tabn 10<CR>
inoremap <C-1> <ESC>:tabn 1<CR>
inoremap <C-2> <ESC>:tabn 2<CR>
inoremap <C-3> <ESC>:tabn 3<CR>
inoremap <C-4> <ESC>:tabn 4<CR>
inoremap <C-5> <ESC>:tabn 5<CR>
inoremap <C-6> <ESC>:tabn 6<CR>
inoremap <C-7> <ESC>:tabn 7<CR>
inoremap <C-8> <ESC>:tabn 8<CR>
inoremap <C-9> <ESC>:tabn 9<CR>
inoremap <C-0> <ESC>:tabn 10<CR>
nnoremap <leader>bm :make<CR>
nnoremap <leader>bn :tabnew<CR>
nnoremap <leader>be :tabnew<CR>:Explore<CR>
nnoremap <leader>bf :tabnew<CR>:Files<CR>

" Switch to last active tab
if !exists('g:Lasttab')
    let g:Lasttab = 1
    let g:Lasttab_backup = 1
endif
autocmd! TabLeave * let g:Lasttab_backup = g:Lasttab | let g:Lasttab = tabpagenr()
autocmd! TabClosed * let g:Lasttab = g:Lasttab_backup
nnoremap gl :exe "tabn ".g:Lasttab<CR>
nnoremap <leader>bo :tabonly<CR>

" show tab numbers in tab line
function! MyTabLine()
  let s = ""
  for i in range(tabpagenr('$'))
    let tabnum = i + 1
    let buflist = tabpagebuflist(i + 1)
    let winnr = tabpagewinnr(i + 1)
    let s .= '%' . tabnum . 'T'
    let s .= (tabnum == tabpagenr() ? '%#TabLineSel#' : '%#TabLine#')
    let s .= ' ' . tabnum . ' '
    let bufnr = buflist[winnr - 1]
    if getbufvar(bufnr, '&modified')
      let s .= '+ '
    endif
    let file = bufname(bufnr)
    let file = fnamemodify(file, ':t')
    if file == ""
      let file = '[No Name]'
    endif
    let s .= file . ' '
  endfor
  let s .= '%T%#TabLineFill#%='
  return s
endfunction
set tabline=%!MyTabLine()

nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" netrw is far too large by default (50%)
let g:netrw_winsize = 16

" escape to enter normal mode in terminal mode
tnoremap <Esc> <C-\><C-n>

" vimtex
let maplocalleader="\\"
let g:vimtex_view_method = 'zathura'

" fzf
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>fg :Rg<CR>
nnoremap <leader>fl :Lines<CR>
nnoremap <leader>fc :Commands<CR>
nnoremap <leader>fm :Maps<CR>
nnoremap <leader>fh :Helptags<CR>
nnoremap <leader>ft :Filetypes<CR>

" Add empty lines without leaving normal mode.
nnoremap [<space> O<Esc>
nnoremap ]<space> o<Esc>

autocmd BufWritePost *.nix silent !nixpkgs-fmt %

source ~/.config/nvim/pack/plugins/start/fzf/fzf.vim
syntax off
