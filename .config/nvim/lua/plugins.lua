return require('packer').startup{function(use, vim)

    -- spEEd. I am spEEEEED.
    use "lewis6991/impatient.nvim"

    -- packer-ception
    use "wbthomason/packer.nvim"

    -- colour
    use {
        "RRethy/vim-hexokinase",
        run = 'make hexokinase',
        cmd  = {"HexokinaseToggle"},
        opt = true
    }

    -- LaTeX babyyyy
    use {
        "lervag/vimtex",
        opt = true
    }

    -- better comments
    use 'numToStr/Comment.nvim'
    -- todo comments
    use {
        "folke/todo-comments.nvim",
        requires = "nvim-lua/plenary.nvim",
        config = function()
        require("todo-comments").setup {
            colors = {
                error = { "#224750", "DiagnosticError", "ErrorMsg", },
                warning = { "#224750", "DiagnosticWarning", "WarningMsg", },
                info = { "#224750", "DiagnosticInfo", },
                hint = { "#224750", "DiagnosticHint", },
                default = { "#224750", "Identifier", },
              },
              highlight = {
                  after = "",
              },
        }
        end
    }

    -- easily manipulate surroundings
    use 'tpope/vim-surround'

    -- HTML
    use {
        "othree/html5.vim",
        opt = true
    }
    use {
        "mattn/emmet-vim",
        opt = true
    }
    -- working with tags
    use {
        "alvan/vim-closetag",
        opt = true
    }
    use {
        "gregsexton/MatchTag",
        opt = true
    }

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
        }) end,
        opt = true,
        cmd = { "NnnExplorer", "NnnPicker" }
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
    use "tamago324/cmp-zsh"
    use "hrsh7th/cmp-emoji"
    use { "kdheepak/cmp-latex-symbols", ft = "tex" }

    -- add icons to completion menu
    use "onsails/lspkind.nvim"

    -- snippets
    use "L3MON4D3/LuaSnip"
    use "saadparwaiz1/cmp_luasnip"
    -- lots of snippets for different languages
    use "rafamadriz/friendly-snippets"

    -- better text wrapping
    use {
        'reedes/vim-pencil',
        opt = true,
        ft = { "markdown", "tex",  }
    }

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

    -- -- syntax highlighting for kerboscript (syntax used in kOS scripts)
    -- use {
    --     'KSP-KOS/EditorTools',
    --     rtp = 'VIM/vim-kerboscript'
    -- }

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
    use 'chentoast/marks.nvim'

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
    use {
        'habamax/vim-godot',
        opt = true
    }

    -- what it says on the tin
    use 'ggandor/lightspeed.nvim'

    -- like which-key in emacs
    -- Lua
    use {
      "folke/which-key.nvim",
      config = function()
        require("which-key").setup {
          -- your configuration comes here
          -- or leave it empty to use the default settings
          -- refer to the configuration section below
        }
      end
    }

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
                    -- NOTE: too slow for now
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
    use 'glepnir/lspsaga.nvim'
    -- use {
    --     "RishabhRD/nvim-lsputils",
    --     requires = "RishabhRD/popfix",
    --     config = function()
    --         vim.lsp.handlers['textDocument/codeAction'] = require'lsputil.codeAction'.code_action_handler
    --         vim.lsp.handlers['textDocument/references'] = require'lsputil.locations'.references_handler
    --         vim.lsp.handlers['textDocument/definition'] = require'lsputil.locations'.definition_handler
    --         vim.lsp.handlers['textDocument/declaration'] = require'lsputil.locations'.declaration_handler
    --         vim.lsp.handlers['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
    --         vim.lsp.handlers['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
    --         vim.lsp.handlers['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
    --         vim.lsp.handlers['workspace/symbol'] = require'lsputil.symbols'.workspace_handler
    --     end,
    -- }

    -- support for yuck (syntax used by eww)
    use {
        'elkowar/yuck.vim',
        opt = true
    }

    -- transparency
    use {
        'xiyaowong/nvim-transparent',
        opt = true,
        cmd = 'TransparentToggle',
        config = function()
            require("transparent").setup({
              enable = true, -- boolean: enable transparent
              extra_groups = { -- table/string: additional groups that should be clear
                -- In particular, when you set it to 'all', that means all avaliable groups

                "all",
                -- example of akinsho/nvim-bufferline.lua
                "BufferLineTabClose",
                "BufferlineBufferSelected",
                "BufferLineFill",
                "BufferLineBackground",
                "BufferLineSeparator",
                "BufferLineIndicatorSelected",
                "indentLine_bgcolor_term",
                "indentLine_bgcolor_gui"
              },
              exclude = {}, -- table: groups you don't want to clear
            })
        end
    }

    -- harpooon
    use 'ThePrimeagen/harpoon'

    -- self-explanatory
    use({
        "iamcco/markdown-preview.nvim",
        run = function() vim.fn["mkdp#util#install"]() end,
    })

    -- zettelkasten note-taking
    use {
        'renerocksai/telekasten.nvim',
        requires = {
            'renerocksai/calendar-vim'
        },
        opt = true,
        config = function ()
            local home = vim.fn.expand("~/uni/zettelkasten")
            require('telekasten').setup({
                home         = home,

                -- if true, telekasten will be enabled when opening a note within the
                -- configured home
                take_over_my_home = true,

                -- auto-set telekasten filetype: if false, the telekasten filetype will
                -- not be used and thus the telekasten syntax will not be loaded either
                auto_set_filetype = true,

                -- dir names for special notes (absolute path or subdir name)
                dailies      = home .. '/' .. 'daily',
                weeklies     = home .. '/' .. 'weekly',
                templates    = home .. '/' .. 'templates',

                -- image (sub)dir for pasting
                -- dir name (absolute path or subdir name)
                -- or nil if pasted images shouldn't go into a special subdir
                image_subdir = "img",

                -- markdown file extension
                extension    = ".md",

                -- following a link to a non-existing note will create it
                follow_creates_nonexisting = true,
                dailies_create_nonexisting = true,
                weeklies_create_nonexisting = true,

                -- template for new notes (new_note, follow_link)
                -- set to `nil` or do not specify if you do not want a template
                -- template_new_note = home .. '/' .. 'templates/new_note.md',
                template_new_note = nil,

                -- template for newly created daily notes (goto_today)
                -- set to `nil` or do not specify if you do not want a template
                -- template_new_daily = home .. '/' .. 'templates/daily.md',
                template_new_daily = nil,

                -- template for newly created weekly notes (goto_thisweek)
                -- set to `nil` or do not specify if you do not want a template
                -- template_new_weekly= home .. '/' .. 'templates/weekly.md',
                template_new_weekly= nil,

                -- image link style
                -- wiki:     ![[image name]]
                -- markdown: ![](image_subdir/xxxxx.png)
                image_link_style = "markdown",

                -- integrate with calendar-vim
                plug_into_calendar = true,
                calendar_opts = {
                    -- calendar week display mode: 1 .. 'WK01', 2 .. 'WK 1', 3 .. 'KW01', 4 .. 'KW 1', 5 .. '1'
                    weeknm = 4,
                    -- use monday as first day of week: 1 .. true, 0 .. false
                    calendar_monday = 1,
                    -- calendar mark: where to put mark for marked days: 'left', 'right', 'left-fit'
                    calendar_mark = 'left-fit',
                },

                -- telescope actions behavior
                close_after_yanking = false,
                insert_after_inserting = true,

                -- tag notation: '#tag', ':tag:', 'yaml-bare'
                tag_notation = "#tag",

                -- command palette theme: dropdown (window) or ivy (bottom panel)
                command_palette_theme = "dropdown",

                -- tag list theme:
                -- get_cursor: small tag list at cursor; ivy and dropdown like above
                show_tags_theme = "dropdown",

                -- when linking to a note in subdir/, create a [[subdir/title]] link
                -- instead of a [[title only]] link
                subdirs_in_links = true,

                -- template_handling
                -- What to do when creating a new note via `new_note()` or `follow_link()`
                -- to a non-existing note
                -- - prefer_new_note: use `new_note` template
                -- - smart: if day or week is detected in title, use daily / weekly templates (default)
                -- - always_ask: always ask before creating a note
                template_handling = "smart",

                -- path handling:
                --   this applies to:
                --     - new_note()
                --     - new_templated_note()
                --     - follow_link() to non-existing note
                --
                --   it does NOT apply to:
                --     - goto_today()
                --     - goto_thisweek()
                --
                --   Valid options:
                --     - smart: put daily-looking notes in daily, weekly-looking ones in weekly,
                --              all other ones in home, except for notes/with/subdirs/in/title.
                --              (default)
                --
                --     - prefer_home: put all notes in home except for goto_today(), goto_thisweek()
                --                    except for notes with subdirs/in/title.
                --
                --     - same_as_current: put all new notes in the dir of the current note if
                --                        present or else in home
                --                        except for notes/with/subdirs/in/title.
                new_note_location = "smart",

                -- should all links be updated when a file is renamed
                rename_update_links = true,
            })
        end
    }

end,
config = {
        compile_path = '~/.config/nvim/lua/packer_compiled.lua'
    }
}


