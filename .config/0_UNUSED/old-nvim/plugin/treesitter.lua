require('nvim-treesitter.configs').setup {
    ensure_installed = { "norg", "norg_meta", "norg_table",
                         "bash", "fish", "comment",
                         "commonlisp",
                         "c",  "cmake",
                         "bibtex", "latex",
                         "gdscript",
                         "json", "lua", "nix", "toml", "yaml",
                         "javascript", "html", "css", "scss", "typescript",
                         "rust", "cpp",
                         "python",
                         "scss"
                       },
    highlight = {
        enable = true,
    },
    autopairs = {
        enable = false,
    },
    playground = {
        enable = true,
        disable = {},
        updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
        persist_queries = false, -- Whether the query persists across vim sessions
        keybindings = {
            toggle_query_editor = 'o',
            toggle_hl_groups = 'i',
            toggle_injected_languages = 't',
            toggle_anonymous_nodes = 'a',
            toggle_language_display = 'I',
            focus_language = 'f',
            unfocus_language = 'F',
            update = 'R',
            goto_node = '<cr>',
            show_help = '?',
        },
    },
    refactor = {
        highlight_definitions = {
            enable = true,
                -- Set to false if you have an `updatetime` of ~100.
                clear_on_cursor_move = true,
            },
    },
}
