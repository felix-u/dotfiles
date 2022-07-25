local fn = vim.fn

-- automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
  print "Installing packer close and reopen Neovim..."
  vim.cmd [[packadd packer.nvim]]
end

-- reload neovim whenever plugins.lua saved
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

-- use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- have packer use a popup window
packer.init {
  display = {
    open_fn = function()
      return require("packer.util").float { border = "rounded" }
    end,
  },
}

-- Plugins --
return packer.startup{function(use, vim)

    use { "lewis6991/impatient.nvim" } -- spEEEEED

    use { -- packer can manage itself
        "wbthomason/packer.nvim",
        -- cmd = { "PackerCompile", "PackerInstall", "PackerStatus", "PackerSync" },
    }

    use { "windwp/nvim-autopairs" } -- automatic syntax pairs, with treesitter and cmp integration

    -- cmp plugins
    use "hrsh7th/nvim-cmp"
    use "hrsh7th/cmp-buffer"
    use { "mtoohey31/cmp-fish", ft = "fish" }
    use "hrsh7th/cmp-path"
    use "hrsh7th/cmp-cmdline"
    use { "hrsh7th/cmp-nvim-lua", ft = "lua" }
    use "hrsh7th/cmp-nvim-lsp"
    use "hrsh7th/cmp-calc"
    use "f3fora/cmp-spell"
    use "lukas-reineke/cmp-rg"
    use "hrsh7th/cmp-emoji"
    use { "kdheepak/cmp-latex-symbols", ft = "tex" }
    use "onsails/lspkind.nvim"

    -- snippets
    use "L3MON4D3/LuaSnip"
    use "saadparwaiz1/cmp_luasnip"
    use "rafamadriz/friendly-snippets"

    -- treesitter (eye candy galore)
    use {
        'nvim-treesitter/nvim-treesitter',
        requires = {
            -- 'nvim-treesitter/nvim-treesitter-refactor',
            'nvim-treesitter/nvim-treesitter-textobjects',
            'nvim-treesitter/playground'
        },
        -- run = ':TSUpdate',
    }

    use { -- colour preview
        "RRethy/vim-hexokinase",
        run = 'make hexokinase',
        cmd  = {"HexokinaseToggle"},
        opt = true
    }
    use { "tpope/vim-surround" } -- easily manipulate surroundings

    use { -- nnn as my file tree
        "luukvbaal/nnn.nvim",
        config = function() require("nnn").setup({
            picker = {
                style = {
                    border = "rounded",
                },
            },
            auto_open = {
                empty = true,
            },
            auto_close = true,
        }) end,
        opt = true,
        cmd = { "NnnExplorer", "NnnPicker" }
    }


  -- automatically set up your configuration after cloning packer.nvim
  -- put this at the end after all plugins
  if PACKER_BOOTSTRAP then
      require("packer").sync()
  end
end,
config = {
        compile_path = '~/.config/nvim/lua/packer_compiled.lua'
    }
}
