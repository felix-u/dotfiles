set autoindent
set autoread
set backspace=indent,eol,start " backspace in insert mode
set clipboard="unnamedplus"
set complete-=i " don't complete keywords in included files
set cursorline
set display+=lastline
set display+=truncate
set expandtab
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
set noshowcmd
set noswapfile
set nowrap
set nowritebackup
set nrformats-=octal
set number
set relativenumber
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
set timeoutlen=200
set undofile
set viewoptions-=options
set viminfo^=!
set wildmenu
setglobal tags-=./tags tags-=./tags; tags^=./tags;	
filetype plugin indent on

function! s:tweak_default_colours()
  highlight clear
  hi Comment      cterm=italic ctermfg=02
  hi Constant     cterm=NONE   ctermfg=06
  hi CursorLine   cterm=NONE   ctermbg=00
  hi CursorLineNr cterm=NONE   ctermfg=15 ctermbg=00
  hi Delimiter    cterm=NONE   ctermfg=White 
  hi Function     cterm=NONE   ctermfg=White
  hi Identifier   cterm=NONE   ctermfg=White
  hi LineNr       ctermfg=07   ctermbg=00
  hi MatchParen   cterm=bold,underline ctermfg=Black ctermbg=04
  hi netrwDir     ctermfg=04
  hi Pmenu        ctermfg=White ctermbg=Black
  hi PmenuSbar    ctermbg=08
  hi PmenuSel     ctermfg=Black ctermbg=White
  hi PmenuThumb   ctermfg=08 ctermbg=08
  hi PreProc      cterm=NONE   ctermfg=01
  hi Search       cterm=bold   ctermfg=Black ctermbg=03
  hi Special      cterm=NONE   ctermfg=05
  hi Statement    cterm=NONE   ctermfg=White
  hi StatusLine   cterm=NONE   ctermfg=White ctermbg=00
  hi String       cterm=NONE   ctermfg=06
  hi TabLine      cterm=NONE   ctermfg=White ctermbg=00
  hi TabLineFill  cterm=NONE   ctermfg=NONE  ctermbg=NONE
  hi TabLineSel   cterm=bold   ctermfg=Black ctermbg=White
  hi TermCursor   ctermfg=Black ctermbg=White
  hi Type         cterm=NONE   ctermfg=White
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

" vim-cutlass
noremap <leader>d  "+d
noremap <leader>dd "+dd
noremap <leader>D  "+d$
noremap <leader>c  "+c
noremap <leader>cc "+cc
noremap <leader>C  "+c$

" Copy and paste from system clipboard
noremap y "+y
noremap yy "+yy
noremap Y "+y$
noremap p "+p
noremap P "+P

" Plugins

" Directional keys should navigate visual lines, not actual lines
nnoremap j gj
nnoremap k gk
nnoremap <Down> gj
nnoremap <Up> gk

" Keep search results centred
nnoremap n nzzzv
nnoremap N Nzzzv

" Text formatting
nnoremap <leader>stf :set linebreak!<CR> :set fo+=t<CR> :set tw=120<CR> :set wrap!<CR>

" Yank whole file
nnoremap <C-y> :%y+<CR>

" Swap more easily between last two buffers
nnoremap gj <C-^>

" More breakpoints for undo
inoremap , ,<C-g>u
inoremap . .<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u

" Stay in visual mode when indenting
vnoremap < <gv
vnoremap > >gv

" Shift+Up/Down moves text
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '>-2<CR>gv=gv
vnoremap <S-Down> :m '>+1<CR>gv=gv
vnoremap <S-Up> :m '>-2<CR>gv=gv

" toggling various things
nnoremap <leader>ts :setlocal spell! spelllang=en_gb<CR>
nnoremap <leader>tls :set number<CR> :set relativenumber<CR>
nnoremap <leader>tlh :set nonumber<CR> :set norelativenumber<CR>
nnoremap <leader>tn :Lex<CR>

" window navigation
nnoremap <C-w><Left> <C-w>h
nnoremap <C-w><Down> <C-w>j
nnoremap <C-w><Up> <C-w>k
nnoremap <C-w><Right> <C-w>l

" better tab navigation
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
" nnoremap <leader>bf :tabnew<CR>:Telescope find_files<CR>
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

" " debugging
" packadd termdebug
" nnoremap <leader>db :Break<CR>
" nnoremap <leader>dgv :vsplit<CR>:TermdebugCommand
" nnoremap <leader>dgs :split<CR>:TermdebugCommand

" always switch to last window with <C-^>, insert mode or not
nnoremap <C-^> <C-w>w
inoremap <C-^> <C-\><C-n><C-w>w
tnoremap <C-^> <C-\><C-n><C-w>w

nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" convenience
nnoremap <leader>q :q<CR>

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

" Nicer binds to add empty lines without leaving normal mode.
nnoremap [<space> O<Esc>
nnoremap ]<space> o<Esc>

" filetype plugins
autocmd BufWritePost *.nix silent !nixpkgs-fmt %
