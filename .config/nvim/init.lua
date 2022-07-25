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
require "harpoonconfig"
require "lsp"
require "gitsignsconfig"

-- colour scheme and statusline
-- vim.cmd('source $XDG_CONFIG_HOME/nvim/lua/colours/xresources.vim')
require "colours.xres"
require "statusline"
