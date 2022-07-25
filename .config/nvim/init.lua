require "impatient"

-- core
require "autocommands"
require "keymap"
require "options"

-- plugins
require "plugins"
require "packer_compiled"
require "treesitter"
require "autopairs"
require "nvim-cmp"
require "luasnip"
-- require "harpoon" -- no worky for now
require "lsp"

-- colour scheme
vim.cmd('source $XDG_CONFIG_HOME/nvim/lua/colours/xresources.vim')
