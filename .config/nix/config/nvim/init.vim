set autoindent
set autoread
set backspace=indent,eol,start " backspace in insert mode
set clipboard="unnamedplus"
set colorcolumn=80
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
set lazyredraw
set linebreak
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set mouse=a
set nocompatible
set nolangremap
set nonumber
set norelativenumber
set noshowcmd
set noswapfile
set nowritebackup
set nrformats-=octal
set ruler
set scrolloff=1
set sessionoptions-=options
set shiftwidth=4
set showbreak=↳•
set showmatch
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

let mapleader = " "

set commentstring=//\ %s
autocmd FileType c   set commentstring=//\ %s
autocmd FileType cpp set commentstring=//\ %s

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

nnoremap yf :%y+<CR>

" Stay in visual mode when indenting
vnoremap < <gv
vnoremap > >gv

nnoremap <leader>ts :setlocal spell! spelllang=en_gb<CR>
nnoremap <leader>tls :set number<CR> :set relativenumber<CR>
nnoremap <leader>tlh :set nonumber<CR> :set norelativenumber<CR>

" escape to enter normal mode in terminal mode
tnoremap <Esc> <C-\><C-n>

" vimtex
let maplocalleader="\\"
let g:vimtex_view_method = 'zathura'

autocmd BufWritePost *.nix silent !nixpkgs-fmt %

set makeprg=build
nnoremap <leader>bm :Make<CR>
nnoremap <leader>bc :cc<CR>
nnoremap <leader>bl :cl<CR>
nnoremap <leader>bn :cn<CR>
nnoremap <leader>bp :cp<CR>

syntax off
colorscheme delek
set background=light

set guifont=CommitMono\ Nerd\ Font\ Mono:h9
if exists("g:neovide")
    let padding_horizontal = 16
    let padding_vertical = 5
    let g:neovide_padding_top = padding_vertical
    let g:neovide_padding_bottom = padding_vertical
    let g:neovide_padding_right = padding_horizontal
    let g:neovide_padding_left = padding_horizontal

    let g:neovide_scale_factor=1.0
    function! ChangeScaleFactor(delta)
      let g:neovide_scale_factor = g:neovide_scale_factor * a:delta
    endfunction
    nnoremap <expr><C-=> ChangeScaleFactor(1.1)<CR>
    nnoremap <expr><C--> ChangeScaleFactor(1/1.1)<CR>

    let g:neovide_floating_shadow = v:false

    let g:neovide_scroll_animation_length = 0.1
    let g:neovide_cursor_animation_length = 0
endif
