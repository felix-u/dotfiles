return require('packer').startup{function(use, vim)

    -- spEEd. I am spEEEEED.
    use 'lewis6991/impatient.nvim'

    -- packer-ception
    use 'wbthomason/packer.nvim'

    -- colour
    use {
        'RRethy/vim-hexokinase',
        run = 'make hexokinase',
        cmd  = {"HexokinaseToggle"}
    }

    -- LaTeX babyyyy
    use 'lervag/vimtex'

    -- better comments
    use 'numToStr/Comment.nvim'

    -- easily manipulate surroundings
    use 'tpope/vim-surround'

    -- HTML
    use 'othree/html5.vim'
    use 'mattn/emmet-vim'
    -- working with tags
    use 'alvan/vim-closetag'
    use 'gregsexton/MatchTag'

    -- file tree
    -- switched to nnn and ditch nvim-tree.
    -- makes more sense for my use cases.
    use {
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
        }) end
    }

    -- fuzyy finding and other stuff. pretty kool
    use {
        'nvim-telescope/telescope.nvim',
        requires = {
            'nvim-lua/plenary.nvim',
            'nvim-telescope/telescope-media-files.nvim'
        }
    }
    use 'nvim-lua/popup.nvim'

    -- LSP --------------------------------------
    use 'neovim/nvim-lspconfig'
    -- syntax completion and tab autocomplete
    -- use { 'ms-jpq/coq_nvim', branch = 'coq' }
    -- use { 'ms-jpq/coq.artifacts', branch = 'artifacts' }
    -- use { 'ms-jpq/coq.thirdparty', branch = '3p' }

    -- TODO switch to nvim-cmp
    use "hrsh7th/nvim-cmp"
    use "hrsh7th/cmp-buffer"
    use "hrsh7th/cmp-path"
    use "hrsh7th/cmp-cmdline"
    use "hrsh7th/cmp-nvim-lua"
    use "hrsh7th/cmp-nvim-lsp"
    use "onsails/lspkind.nvim"
    -- closely related: luasnip
    use "L3MON4D3/LuaSnip"
    use "saadparwaiz1/cmp_luasnip"

    -- better text wrapping
    use 'reedes/vim-pencil'

    -- status line :)))))
    use {
        'nvim-lualine/lualine.nvim',
        requires = {
            'kyazdani42/nvim-web-devicons',
            'SmiteshP/nvim-gps', -- shows location in code structure
            opt = true
        }
    }

    -- kmonad support
    use 'kmonad/kmonad-vim'

    -- treesitter (eye candy galore)
    use {
        'nvim-treesitter/nvim-treesitter',
        requires = {
            'nvim-treesitter/nvim-treesitter-refactor',
            'nvim-treesitter/nvim-treesitter-textobjects',
            'nvim-treesitter/playground'
        },
        run = ':TSUpdate',
    }

    -- syntax highlighting for kerboscript (syntax used in kOS scripts)
    use {
        'KSP-KOS/EditorTools',
        rtp = 'VIM/vim-kerboscript'
    }

    -- colour schemes (which I probably won't actually use)
    use 'rktjmp/lush.nvim'
    use 'sainnhe/gruvbox-material'
    use 'npxbr/gruvbox.nvim'
    use 'ghifarit53/tokyonight-vim'
    use 'overcache/NeoSolarized'
    use 'chriskempson/base16-vim'
    use 'Shatur/neovim-ayu'

    -- more vim objects
    use 'wellle/targets.vim'

    -- automatic syntax pairing (may move to windwp/nvim-autopairs)
    -- use 'jiangmiao/auto-pairs'
    use {
        'windwp/nvim-autopairs',
        wants = "nvim-treesitter",
        module = { "nvim-autopairs.completion.cmp", "nvim-autopairs" },
        config = function()
            require('nvim-autopairs').setup()
        end,
        event = "InsertEnter",
    }

    -- better marks
    use 'chentau/marks.nvim'

    -- git signs (like gitgutter, but better)
    use {
        'lewis6991/gitsigns.nvim',
        requires = {
            'nvim-lua/plenary.nvim'
        },
    }

    -- allow plugins to tap into vim repeat functionality
    use 'tpope/vim-repeat'

    -- godot support
    use 'habamax/vim-godot'

    -- what it says on the tin
    use 'ggandor/lightspeed.nvim'

    -- neorg (like org mode but for nvim)
    use {
        "nvim-neorg/neorg",
        config = function()
            require('neorg').setup {
                load = {
                    ["core.defaults"] = {}, -- load default modules
                    ["core.keybinds"] = { -- load default keybindings
                        config = {
                            default_keybinds = true,
                            neorg_leader = "<Leader>o"
                        }
                    },
                    -- TODO too slow for now
                    -- ["core.norg.concealer"] = {}, -- allows the use of icons
                    ["core.norg.dirman"] = { -- manage directories with neorg
                        config = {
                            workspaces = {
                                uni_2022_spring = "~/uni/2022/spring/agenda"
                            }
                        }
                    },
                    ["core.integrations.telescope"] = {}, -- Enable the telescope module
                }
            }
        end,
        requires = {
            "nvim-lua/plenary.nvim",
            "nvim-neorg/neorg-telescope", -- telescope integration
        },
    }

    -- delete, don't cut
    use 'gbprod/cutlass.nvim'

    -- lsp diagnostics and whatnot
    use {
        "folke/trouble.nvim",
        requires = "kyazdani42/nvim-web-devicons",
    }

    -- support for yuck (syntax used by eww)
    use 'elkowar/yuck.vim'

    -- transparency
    use 'xiyaowong/nvim-transparent'

    -- harpooon
    use 'ThePrimeagen/harpoon'
    -- get good at vim through practice games
    use 'ThePrimeagen/vim-be-good'

    -- self-explanatory
    use {
        'iamcco/markdown-preview.nvim',
        run = "cd app && yarn install"
    }

    -- zettelkasten note-taking
    use {
        'renerocksai/telekasten.nvim',
        requires = {
            'renerocksai/calendar-vim'
        }
    }

end,
config = {
        compile_path = '~/.config/nvim/lua/packer_compiled.lua'
    }
}


