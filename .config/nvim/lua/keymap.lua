-- shorten function name
local keymap = vim.keymap.set
-- silent keymap option
local opts = { silent = true }

-- space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c"


-- Normal --

-- directional keys should navigate visual lines, not actual lines
keymap("n", "j", "gj", opts)
keymap("n", "k", "gk", opts)
keymap("n", "<Down>", "gj", opts)
keymap("n", "<Up>", "gk", opts)

-- keep search results centred
keymap("n", "n", "nzzzv", opts)
keymap("n", "N", "Nzzzv", opts)

-- split navigation
keymap("n", "<leader><Left>", "<C-w>h", opts)
keymap("n", "<leader><Down>", "<C-w>j", opts)
keymap("n", "<leader><Up>", "<C-w>k", opts)
keymap("n", "<leader><Right>", "<C-w>l", opts)
keymap("n", "<leader>i", "<C-w>w", opts) -- "i" for "into"
-- split resizing
keymap("n", "<leader><M-Left>", "9<C-w><", opts)
keymap("n", "<leader><M-Down>", "9<C-w>+", opts)
keymap("n", "<leader><M-Up>", "9<C-w>-", opts)
keymap("n", "<leader><M-Right>", "9<C-w>>", opts)
-- split creation
keymap("n", "<leader><PageDown>", ":split scratch<CR>", opts)
keymap("n", "<leader><End>", ":vsplit scratch<CR>", opts)

-- tab navigation
keymap("n", "<leader>1", "1gt", opts)
keymap("n", "<leader>2", "2gt", opts)
keymap("n", "<leader>3", "3gt", opts)
keymap("n", "<leader>4", "4gt", opts)
keymap("n", "<leader>5", "5gt", opts)
keymap("n", "<leader>6", "6gt", opts)
keymap("n", "<leader>7", "7gt", opts)
keymap("n", "<leader>8", "8gt", opts)
keymap("n", "<leader>9", "9gt", opts)
keymap("n", "<leader>0", ":tablast<CR>", opts)
-- tab creation
keymap("n", "<leader><C-n>", ":tabnew<CR>", opts)

-- nice shortcut to yank entire file
keymap("n", "<C-y>", ":%y+<CR>", opts)

-- close and delete buffer
keymap("n", "<leader>q", ":bd<CR>", opts)

-- increment with <C-i> to free <C-a> for tmux
keymap("n", "<C-i>", "<C-a>", opts)

-- folding is pretty sick
keymap("n", "<leader>fo", "za", opts) -- toggle current fold
keymap("n", "<leader>fi", "zA", opts) -- toggle all folds from cursor's level outwards
keymap("n", "<leader>fa", "zR", opts) -- open all folds
keymap("n", "<leader>fm", "zM", opts) -- close all folds

-- as is LSP
keymap("n", "<leader>lo", ":lua vim.diagnostic.open_float()<CR>", opts)
keymap("n", "<leader>ln", ":lua vim.diagnostic.goto_next()<CR>", opts)
keymap("n", "<leader>lp", ":lua vim.diagnostic.goto_prev()<CR>", opts)
keymap("n", "<leader>ll", ":lua require('lsp_lines').toggle()<CR>", { desc = "Toggle lsp_lines" })
keymap("n", "<leader>lr", ":lua vim.lsp.buf.rename()<CR>", opts)
keymap("n", "gd", ":lua vim.lsp.buf.definition()<CR>", opts)
keymap("n", "gi", ":lua vim.lsp.buf.implementation()<CR>", opts)
keymap("n", "gr", ":lua vim.lsp.buf.references()<CR>", opts)
keymap("n", "<leader>a", ":lua vim.lsp.buf.code_action()<CR>", opts)
keymap("n", "<leader>g", ":lua vim.diagnostic.open_float()<CR>", opts)
keymap("n", "<leader>k", ":lua vim.lsp.buf.hover()<CR>", opts)
vim.diagnostic.config({ virtual_text = false }) -- redundant due to lsp_lines

-- swap ("j" for "jump") more easily between the last two buffers
keymap("n", "<leader>j", "<C-^>", opts)

-- append and prepend lines without leaving normal mode or current position
keymap("n", "]<Space>", ":<C-u>put =repeat(nr2char(10),v:count)<Bar>execute \"'[-1\"<CR>", opts)
keymap("n", "[<Space>", ":<C-u>put!=repeat(nr2char(10),v:count)<Bar>execute \"']+1\"<CR>", opts)


-- Insert --

-- insert more undo breakpoints whilst typing
keymap("i", ",", ",<C-g>u", opts)
keymap("i", ".", ".<C-g>u", opts)
keymap("i", "!", "!<C-g>u", opts)
keymap("i", "?", "?<C-g>u", opts)


-- Visual --

-- stay in visual mode when indenting
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- shift+Up/Down (J/K) moves text and reformats
keymap("v", "J", ":m '>+1<CR>gv=gv", opts)
keymap("v", "K", ":m '>-2<CR>gv=gv", opts)
keymap("v", "<S-Down>", ":m '>+1<CR>gv=gv", opts)
keymap("v", "<S-Up>", ":m '>-2<CR>gv=gv", opts)


-- Plugins --

-- cutlass lets you delete without yanking; these new bindings will allow deleting AND yanking
keymap("n", "<leader>d", "d", opts)
keymap("x", "<leader>d", "d", opts)
keymap("n", "<leader>dd", "dd", opts)
keymap("n", "<leader>D", "D", opts)

-- emmet (HTML and CSS expander)
vim.g.user_emmet_leader_key = "<C-f>"

-- harpoon
keymap("n", "<leader>hb", ":lua require('harpoon.ui').toggle_quick_menu()<CR>", opts)
keymap("n", "<leader>ha", ":lua require('harpoon.mark').add_file()<CR>", opts)
keymap("n", "<leader>hf", ":lua require('harpoon.ui').toggle_quick_menu()<CR>", opts)
keymap("n", "<leader>h<Left>", ":lua require('harpoon.ui').nav_file(1)<CR>", opts)
keymap("n", "<leader>h<Down>", ":lua require('harpoon.ui').nav_file(2)<CR>", opts)
keymap("n", "<leader>h<Up>", ":lua require('harpoon.ui').nav_file(3)<CR>", opts)
keymap("n", "<leader>h<Right>", ":lua require('harpoon.ui').nav_file(4)<CR>", opts)

-- lightspeed
keymap("n", "s", "<Plug>Lightspeed_s", opts)
keymap("n", "S", "<Plug>Lightspeed_S", opts)
-- keymap("n", "s", "<Plug>Lightspeed_omni_s", opts)
keymap("n", "gs", "<Plug>Lightspeed_omni_gs", opts)
keymap("n", "<Tab>", "<Plug>Lightspeed_;_sx", opts)
keymap("n", "<Backspace>", "<Plug>Lightspeed_,_sx", opts)

-- nnn
keymap("n", "<C-t>", ":NnnPicker<CR>", opts)

-- telescope
keymap("n", "<leader>ob", ":Telescope buffers<CR>", opts)
keymap("n", "<leader>of", ":Telescope find_files<CR>", opts)
keymap("n", "<leader>og", ":Telescope live_grep<CR>", opts)
keymap("n", "<leader>ot", ":Telescope help_tags<CR>", opts)
keymap("n", "<leader>om", ":Telescope harpoon marks<CR>", opts)
keymap("n", "<leader>oh", ":Telescope help_tags<CR>", opts)

-- toggling various plugins
keymap("n", "<leader>tc", ":HexokinaseToggle<CR>", opts)
keymap("n", "<leader>ts", ":setlocal spell! spelllang=en_gb<CR>", opts)
keymap("n", "<leader>tls", ":set number<CR>:set relativenumber<CR>", opts)
keymap("n", "<leader>tlh", ":set nonumber<CR>:set norelativenumber<CR>", opts)
keymap("n", "<leader>thn", ":call SynGroup()<CR>", opts)
keymap("n", "<leader>tht", ":TSHighlightCapturesUnderCursor<CR>", opts)
keymap("n", "<leader>tm", ":MarkdownPreviewToggle<CR>", opts)

-- Theme
keymap("n", "<leader>sl", ":colo flattened_light<CR>:hi Statusline ctermbg=none cterm=bold guibg=none gui=bold<CR>", opts)
keymap("n", "<leader>sd", ":colo flattened_dark<CR>:hi Statusline ctermbg=none cterm=bold guibg=none gui=bold<CR>", opts)
