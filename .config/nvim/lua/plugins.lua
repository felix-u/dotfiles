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

    -- more vim objects
    use { "wellle/targets.vim" }

    -- git signs (like gitgutter, but better)
    use {
        'lewis6991/gitsigns.nvim',
        requires = {
            'nvim-lua/plenary.nvim'
        },
    }

    -- better marks
    use {
        "chentoast/marks.nvim",
        config = function()
            require"marks".setup {
                default_mappings = true,
                builtin_marks = {},
            }
        end
    }

    use { "windwp/nvim-autopairs" } -- automatic syntax pairs, with treesitter and cmp integration

    -- base lsp
    use {
        "williamboman/mason.nvim",
        "williamboman/mason-lspconfig.nvim",
        "neovim/nvim-lspconfig",
    }

    -- fuzzy finding and other stuff. pretty kool
    use {
        'nvim-telescope/telescope.nvim',
        requires = {
            'nvim-lua/plenary.nvim',
            'nvim-telescope/telescope-media-files.nvim'
        }
    }
    use 'nvim-lua/popup.nvim'

    -- -- status line :)))))
    -- use {
    --     'nvim-lualine/lualine.nvim',
    --     requires = {
    --         'kyazdani42/nvim-web-devicons',
    --         'SmiteshP/nvim-navic', -- shows location in code structure
    --         opt = true
    --     },
    -- }

    -- -- lsp eyecandy
    -- use({
    --   "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    --   config = function()
    --     require("lsp_lines").setup()
    --   end,
    -- })
    -- -- use { "RRethy/vim-illuminate" }

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

    -- syntax highlighting for odin (no treesitter support that I've found)
    use "Tetralux/odin.vim"

    -- syntax highlighting for eww's yuck
    use "elkowar/yuck.vim"

    use {
        "iamcco/markdown-preview.nvim",
        run = function()
            vim.fn["mkdp#util#install"]()
        end,
    }

    use { -- delete, don't cut
        "gbprod/cutlass.nvim",
        config = function ()
            require"cutlass".setup()
        end,
    }

    -- what it says on the tin
    use 'ggandor/lightspeed.nvim'

    -- like which-key in emacs
    -- Lua
    use {
      "folke/which-key.nvim",
      config = function()
        require("which-key").setup()
      end
    }

    -- use {
    --     "lukas-reineke/indent-blankline.nvim",
    --     config = function ()
    --         require("indent_blankline").setup {
    --             -- for example, context is off by default, use this to turn it on
    --             show_current_context = true,
    --             show_current_context_start = false,
    --         }
    --     end,
    -- }

    use { -- colour preview
        "RRethy/vim-hexokinase",
        run = 'make hexokinase',
        cmd  = {"HexokinaseToggle"},
        opt = true
    }
    use { "tpope/vim-surround" } -- easily manipulate surroundings

    -- allow plugins to tap into vim repeat functionality
    use 'tpope/vim-repeat'

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

    use { -- incredible file navigation system
        "ThePrimeagen/harpoon",
        requires = "nvim-lua/plenary.nvim",
    }

    -- better comments
    use {
        "numToStr/Comment.nvim",
        config = function()
            require('Comment').setup()
        end
    }

    -- latex, along with better text wrapping when in tex mode
    use {
       "lervag/vimtex",
       ft = "tex",
    }
    -- use {
    --     "reedes/vim-pencil",
    --     ft = { "markdown", "tex", }
    -- }

    -- just some colour schemes to muck about with.
    -- I don't actually use any though :P
    use {
        "overcache/NeoSolarized",
        "ellisonleao/gruvbox.nvim",
        "folke/tokyonight.nvim",
        "rafi/awesome-vim-colorschemes", -- this is a collection
        "pineapplegiant/spaceduck",
        "phha/zenburn.nvim",
        "olivertaylor/vacme",
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
