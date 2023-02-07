local status_ok, configs = pcall(require, "nvim-treesitter.configs")
if not status_ok then
	return
end

configs.setup({
	ensure_installed = "", -- one of "all" or a list of languages
	ignore_install = { "" }, -- List of parsers to ignore installing
	highlight = {
		enable = true, -- false will disable the whole extension
		-- disable = { "css" }, -- list of language that will be disabled
	},
	autopairs = {
		enable = true,
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
	indent = { -- I'm a sadomasochist and want *everything* to indent by 4 spaces,
             -- as configured in my core nvim settings. Very few languages
             -- actually care anyway
    -- HOWEVER, I'm giving this a shot FOR NOW
    enable = true,
    -- disable = { "python", "css" }
  },
})
