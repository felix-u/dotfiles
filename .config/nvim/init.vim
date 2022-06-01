"
" _ ___ _(_)_ __
"| ' \ V / | '  \
"|_||_\_/|_|_|_|_|
"
"
"

" spEEED
lua require 'impatient'

" syntax highlighting and colour scheme
syntax enable
set termguicolors
set background=dark

" use theme generated from xresources
source /home/felix/.config/nvim/colors/xresources.vim
" NOTE: in case of emergency, break double quotes:
" colo NeoSolarized
" highlight Comment cterm=italic gui=italic

set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath

" essentially, indent = 4 spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" showcmd in bottom
set showcmd

" auto-indent
filetype indent on

" command completion
set wildmenu

" buffer screen updates
set lazyredraw

" highlight matching brackets
set showmatch

" some safe and sensible settings
set nocompatible
filetype plugin on
set mouse=a
set clipboard+=unnamedplus

" start searching while typing and highlight search target
set incsearch
set hlsearch

" map leader to space, which is better than \
let mapleader = " "

" folds
set foldenable
set foldlevelstart=10
set foldnestmax=10
nnoremap <leader>ii za
nnoremap <leader>ip zf}
set foldmethod=manual
hi! link SignColumn Normal
" remember folds from previous sessions
augroup remember_folds
    autocmd!
    autocmd BufWinLeave *.* mkview
    autocmd BufWinEnter *.* silent! loadview
augroup END

" fix indenting
vmap < <gv
vmap > >gv

" remap directional keys to navigate visual lines, not actual lines
nnoremap j gj
nnoremap k gk
nnoremap <Down> gj
nnoremap <Up> gk

" keep search result centred
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap J mzJ`z

" change poor default autopair shortcut conflicting with rapid ESC-paste
" let g:AutoPairsShortcutToggle = '<C-q>'

" undo break points for less painful undoing
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ! !<c-g>u
inoremap ? ?<c-g>u

" jumplist mutations
nnoremap <expr> k (v:count > 5 ? "m'" . v:count : "") . 'k'
nnoremap <expr> j (v:count > 5 ? "m'" . v:count : "") . 'j'

" moving text
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" split navigation
nnoremap <leader><Left>  <C-w>h
nnoremap <leader><Down>  <C-w>j
nnoremap <leader><Up>    <C-w>k
nnoremap <leader><Right> <C-w>l
" split resizing
nnoremap <leader><M-Up>    10<C-w>-
nnoremap <leader><M-Down>  10<C-w>+
nnoremap <leader><M-Left>  10<C-w><
nnoremap <leader><M-Right> 10<C-w>>
" split creation
nnoremap <leader><C-Down>  :split<Space>
nnoremap <leader><C-Right> :vsplit<Space>
" split swapping (ish)
nnoremap <leader><S-Left>  <C-w>H
nnoremap <leader><S-Down>  <C-w>J
nnoremap <leader><S-Up>    <C-w>K
nnoremap <leader><S-Right> <C-w>L
nnoremap <leader><C-s>     <C-w>r
set splitbelow splitright

" tab navigation
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>0 :tablast<cr>
noremap <leader><C-n> :tabnew<cr>

" better shortcut to copy everything in file
nnoremap <C-y> :%y+<CR>

" " case insensitive spellcheck and last word correction with ctrl-l
inoremap <C-c> <c-g>u<Esc>[s1z=`]a<c-g>u
map <leader>cs :setlocal spell! spelllang=en_gb<CR>
set ic

" Remove trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

" paging
let $PAGER=''

" cursor always in the centre of the screen
set scrolloff=16

lua << EOF
require 'plugins'
require 'packer_compiled'
require'cutlass'.setup()
require'Comment'.setup()
require'telescope'.load_extension('media_files')
require("luasnip.loaders.from_vscode").lazy_load()
EOF

" cutlass - binding for cutting
nnoremap <leader>d d
xnoremap <leader>d d
nnoremap <leader>dd dd
nnoremap <leader>D D

" remap increment and decrement to free up C-x and C-a for tmux/terminal
nnoremap <leader>vi <C-a>
nnoremap <leader>va <C-x>

" keep selection when indenting
vnoremap < <gv
vnoremap > >gv

" lightspeed
map s <Plug>Lightspeed_s
map S <Plug>Lightspeed_S

" harpoon
nnoremap <leader>mt :lua require("harpoon.mark").add_file()<CR>
nnoremap <leader>mv :lua require("harpoon.ui").toggle_quick_menu()<CR>
nnoremap <leader>mr <C-^>
nnoremap <leader>mb :lua require("harpoon.ui").nav_prev()<CR>
nnoremap <leader>mf :lua require("harpoon.ui").nav_next()<CR>
" colemak
nnoremap <leader>mgm :lua require("harpoon.ui").nav_file(1)<CR>
nnoremap <leader>mgn :lua require("harpoon.ui").nav_file(2)<CR>
nnoremap <leader>mge :lua require("harpoon.ui").nav_file(3)<CR>
nnoremap <leader>mgi :lua require("harpoon.ui").nav_file(4)<CR>
" qwerty
nnoremap <leader>mgh :lua require("harpoon.ui").nav_file(1)<CR>
nnoremap <leader>mgj :lua require("harpoon.ui").nav_file(2)<CR>
nnoremap <leader>mgk :lua require("harpoon.ui").nav_file(3)<CR>
nnoremap <leader>mgl :lua require("harpoon.ui").nav_file(4)<CR>

let g:Hexokinase_highlighters = [ 'backgroundfull' ]
nnoremap <leader>cc :HexokinaseToggle<CR>

" lsp stuff
let g:coq_settings = {'auto_start': 'shut-up'}

" use trouble.nvim instead
nnoremap <silent> <leader>lrf :Trouble lsp_references<CR>
nnoremap <silent> <leader>ldf :Trouble lsp_definitions<CR>
nnoremap <silent> <leader>ldn :Trouble document_diagnostics<CR>
nnoremap <silent> <leader>lt <cmd>TroubleToggle<CR>
nnoremap <silent> <leader>lrn <cmd>lua vim.lsp.buf.rename()<CR>

" hybrid numbers when in normal mode
set number

augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
augroup END

set cursorline
" don't highlight current line number
" autocmd VimEnter * hi clear CursorLine

let g:user_emmet_leader_key='<C-f>'

let g:tex_flavor = 'latex'
let g:vimtex_view_method = 'zathura'
let g:vimtex_view_general_viewer = 'zathura'
let g:vimtext_quickfix_mode=0
let g:tex_conceal='abdmg'
let g:vimtex_quickfix_mode = 2
let g:vimtex_quickfix_ignore_filters = [
    \ 'Unused global option(s)'
    \]
set conceallevel=0


let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<c-j>'
let g:UltiSnipsJumpBackwardTrigger = '<c-k>'
let g:UltiSnipsSnippetDirectories=["UltiSnips", "mySnippets"]

set timeoutlen=400


" let g:nvim_tree_icons = {
"     \ 'default': '',
"     \ 'symlink': '',
"     \ 'git': {
"     \   'unstaged': "✗",
"     \   'staged': "✓",
"     \   'unmerged': "",
"     \   'renamed': "➜",
"     \   'untracked': "★"
" \   },
" \ 'folder': {
" \   'default': "",
" \   'open': "",
" \   'empty': "",
" \   'empty_open': "",
" \   'symlink': "",
" \   }
" \ }
" nnoremap <leader>tt :NvimTreeToggle<CR>
" nnoremap <leader>tr :NvimTreeRefresh<CR>
" nnoremap <leader>tn :NvimTreeFindFile<CR>
" NvimTreeOpen and NvimTreeClose are also available if you need them
" a list of groups can be found at `:help nvim_tree_highlight`

" nnn.nvim
nnoremap <C-t> <cmd>NnnExplorer<CR>
tnoremap <C-t> <cmd>NnnExplorer<CR>
tnoremap <C-p> <cmd>:NnnPicker<CR>
nnoremap <C-p> <cmd>NnnPicker<CR>

nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>
nnoremap <leader>fm <cmd>Telescope harpoon marks<cr>

" markdown preview (actually incredible though)
let g:mkdp_browser = 'qutebrowser'
let g:mkdp_auto_start = 0
let g:mkdp_markdown_css = '/home/felix/.config/nvim/ignore/markdown.css'
nnoremap <leader>pm :MarkdownPreviewToggle<cr>

" zettelkasten
nnoremap <leader>zfn :Telekasten find_notes<cr>
nnoremap <leader>zfd :Telekasten find_daily_notes<cr>
nnoremap <leader>zfw :Telekasten find_weekly_notes<cr>
nnoremap <leader>zff :Telekasten find_friends<cr>
nnoremap <leader>zfa :Telekasten search_notes<cr>
nnoremap <leader>zgl :Telekasten follow_link<cr>
nnoremap <leader>zgt :Telekasten goto_today<cr>
nnoremap <leader>zgw :Telekasten goto_thisweek<cr>
nnoremap <leader>znn :Telekasten new_note<cr>
nnoremap <leader>znt :Telekasten new_templated_note<cr>
nnoremap <leader>zc :Telekasten show_calendar<cr>
nnoremap <leader>zpi :Telekasten paste_img_and_link<cr>
nnoremap <leader>zii :Telekasten inset_img_link({ i=true })<cr>
nnoremap <leader>zt :Telekasten toggle_todo<cr>
nnoremap <leader>zb :Telekasten show_backlinks<cr>
nnoremap <leader>zsi :Telekasten preview_img<cr>
nnoremap <leader>zsm :Telekasten browse_media<cr>
nnoremap <leader>zst :Telekasten show_tags<cr>
nnoremap <leader>zr :Telekasten rename_note<cr>
nnoremap <leader>zml :Telekasten insert_link({ i=true })<cr>
nnoremap <leader>z :Telekasten panel<cr>

" These commands will move the current buffer backwards or forwards in the bufferline
" disabled
" autocmd VimEnter * highlight BufferLineFill guibg=none
" nnoremap <silent><mymap> :BufferLineMoveNext<CR>
" nnoremap <silent><mymap> :BufferLineMovePrev<CR>

let g:pencil#wrapModeDefault = 'soft'   " default is 'hard'

augroup pencil
  autocmd!
  autocmd FileType text         call pencil#init() autocmd FileType
  autocmd FileType markdown,mkd call pencil#init()
augroup END

" syntax highlighting for rofi config
au BufRead,BufNewFile *.rasi set syntax=css

" restore cursor position
autocmd BufReadPost *
      \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
      \ |   exe "normal! g`\""
      \ | endif

" godot stuff
func! GodotSettings() abort
    setlocal foldmethod=expr
    setlocal tabstop=4
    nnoremap <buffer> <F4> :GodotRunLast<CR>
    nnoremap <buffer> <F5> :GodotRun<CR>
    nnoremap <buffer> <F6> :GodotRunCurrent<CR>
    nnoremap <buffer> <F7> :GodotRunFZF<CR>
endfunc
augroup godot | au!
    au FileType gdscript call GodotSettings()
augroup end

" mapping to find highlight group under cursor
function! SynGroup()
    let l:s = synID(line('.'), col('.'), 1)
    echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfun
nnoremap <silent> <leader>gsn :call SynGroup()<CR>
nnoremap <silent> <leader>gst :TSHighlightCapturesUnderCursor<CR>

" colourscheme shortcuts
nnoremap <silent> <leader>sl :source ~/.config/nvim/colors/lightxresources.vim<CR>
nnoremap <silent> <leader>sd :source ~/.config/nvim/colors/xresources.vim<CR>

" handy sorting shortcut
vnoremap <F2> d:execute 'normal i' . join(sort(split(getreg('"'))), ' ')<CR>

" nvim-cmp
set completeopt=menu,menuone,noselect
