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
-- split resizing
keymap("n", "<leader><M-Left>", "16<C-w><", opts)
keymap("n", "<leader><M-Down>", "16<C-w>+", opts)
keymap("n", "<leader><M-Up>", "16<C-w>-", opts)
keymap("n", "<leader><M-Right>", "16<C-w>>", opts)
-- split creation
keymap("n", "<leader><PageDown>", ":split scratch", opts)
keymap("n", "<leader><End>", ":vsplit scratch", opts)

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
keymap("n", "<leader>hb", ":lua require('harpoon.ui').toggle_quick_menu<CR>", opts)
keymap("n", "<leader>ha", ":lua require('harpoon.mark').add_file()<CR>", opts)
keymap("n", "<leader>hf", ":lua require('harpoon.ui').toggle_quick_menu()<CR>", opts)
keymap("n", "<leader>h<Left>", ":lua require('harpoon.ui').nav_file(1)<CR>", opts)
keymap("n", "<leader>h<Down>", ":lua require('harpoon.ui').nav_file(2)<CR>", opts)
keymap("n", "<leader>h<Up>", ":lua require('harpoon.ui').nav_file(3)<CR>", opts)
keymap("n", "<leader>h<Right>", ":lua require('harpoon.ui').nav_file(4)<CR>", opts)

-- lightspeed
keymap("n", "s", "<Plug>Lightspeed_s", opts)
keymap("n", "S", "<Plug>Lightspeed_S", opts)

-- nnn
keymap("n", "<C-t>", ":NnnExplorer<CR>", opts)

-- telescope
keymap("n", "<leader>b", ":Telescope buffers<CR>", opts)
keymap("n", "<leader>f", ":Telescope find_files<CR>", opts)
keymap("n", "<leader>og", ":Telescope live_grep<CR>", opts)
keymap("n", "<leader>ot", ":Telescope help_tags<CR>", opts)
keymap("n", "<leader>oh", ":Telescope harpoon marks<CR>", opts)

-- toggling various plugins
keymap("n", "<leader>tc", ":HexokinaseToggle<CR>", opts)
keymap("n", "<leader>ts", ":setlocal spell! spelllang=en_gb<CR>", opts)
keymap("n", "<leader>thn", ":call SynGroup()<CR>", opts)
keymap("n", "<leader>tht", ":TSHighlightCapturesUnderCursor<CR>", opts)
