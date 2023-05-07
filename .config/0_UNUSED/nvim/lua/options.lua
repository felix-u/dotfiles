vim.opt.autoindent = true -- keeps indent level when going onto next line
vim.opt.background = "dark" -- self-explanatory
vim.opt.clipboard = "unnamedplus" -- system clipboard
vim.opt.completeopt = { "menuone", "noselect" } -- just for cmp
vim.opt.conceallevel = 2 -- mostly for markdown files
vim.opt.cursorline = true -- highlight current line
vim.opt.expandtab = true -- convert tabs to spaces
vim.opt.fileencoding = "utf-8" -- self-explanatory
vim.opt.fillchars = "fold: " -- fold eye-candy
vim.opt.fillchars.eob = " " -- separation between splits, I think
vim.opt.foldenable = true -- enable folds
vim.opt.foldexpr = "nvim_treesitter#foldexpr()" -- https://www.reddit.com/r/neovim/comments/psl8rq/sexy_folds/
vim.opt.foldlevel = 99 -- essentially, unfold by default
vim.opt.foldmethod = "expr"
vim.opt.foldminlines = 3
vim.opt.foldnestmax = 4
vim.opt.foldtext = "getline(v:foldstart).'...'.trim(getline(v:foldend))"
vim.opt.hlsearch = true -- highlight search pattern matches
vim.opt.ignorecase = true -- ignore case in search patterns
vim.opt.incsearch = true -- show searches as typing
vim.opt.iskeyword:append("-") -- see help page. controls which keys are regex and must be escaped in searches
vim.opt.laststatus = 3 -- influences which windows have status lines
vim.opt.lazyredraw = true -- might be enabled by default, but apparently speeds up rendering
vim.opt.mouse = "a" -- allow mouse use
vim.opt.numberwidth = 2 -- minimum width gutter can occupy (default 4)
vim.opt.pumheight = 10 -- popup menu height
vim.opt.number = false -- when used with "relativenumber", only the current line number is shown in its absolute form
vim.opt.relativenumber = false -- enable line numbers and show relative numbers
vim.opt.ruler = true -- position information at the bottom of the screen
vim.opt.scrolloff = 5 -- keep 5 lines above or below the cursor at all times
vim.opt.shiftwidth = 4 -- number of spaces inserted per indentation
vim.opt.shortmess:append "c" -- see help page
vim.opt.showcmd = false -- don't need to see my last command at the bottom
vim.opt.showmatch = true -- highlight matching braces
vim.opt.showtabline = 0 -- always show tab line
vim.opt.sidescrolloff = 5 -- same as scrolloff, but for columns rather than lines
vim.opt.signcolumn = "yes" -- always show sign column to avoid shifting text every time it appears/disappears
vim.opt.softtabstop = 4 -- four spaces for a tab
vim.opt.smartindent = true -- reacts to programming style. goes hand-in-hand with autoindent
vim.opt.splitbelow = true -- horizontal splits go below the current window
vim.opt.splitright = true -- vertical splits go to the right of the current window
vim.opt.swapfile = false -- don't keep swap files
vim.opt.tabstop = 4 -- four spaces for a tab
vim.opt.termguicolors = true -- truecolour
vim.opt.timeoutlen = 400 -- time to wait for mapped sequence to complete
vim.opt.undofile = true -- persistent undo history
vim.opt.updatetime = 300 -- faster completion (4000ms default)
vim.opt.whichwrap:append("<,>,[,],h,l") -- see help page. controls which keys can shift horizontal view
vim.opt.wildmenu = true -- command mode completion
vim.opt.wrap = false -- lines can only take up one visual line, so long lines will go off-screen rather than wrapping
vim.opt.writebackup = false -- if a file is being written to by another program, it cannot be edited

-- PLUGIN OPTIONS

vim.g.vimtex_view_method = "zathura"
vim.g.vimtex_view_general_viewer = "zathura"
vim.g.tex_conceal = "abdmg"
vim.g.tex_flavor = "latex"
-- vim.g.vimtex_quickfix_enabled = 0
vim.g.vimtex_quickfix_open_on_warning = 0

-- vim.g.mkdp_browser = "qutebrowser"
-- vim.g.mkdp_auto_start = 0
-- vim.g.mdkp_markdown_css = "/home/felix/.config/nvim/lua/colours/markdown.css"