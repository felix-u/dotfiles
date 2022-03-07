local utils = require 'lualine.utils.utils'
local gps = require 'nvim-gps'

local function user_host()
	return vim.fn.getenv('USER') .. '@' .. vim.fn.hostname()
end

-- Get the colors to create theme
-- stylua: ignore
local colors = {
  blue  = utils.extract_color_from_hllist('fg', { 'Title' }, '#000000'),
  green  = utils.extract_color_from_hllist('fg', { 'String', 'MoreMsg' }, '#000000'),
  orange = utils.extract_color_from_hllist('fg', { 'Number', 'Type' }, '#000000'),
  cyan  = utils.extract_color_from_hllist('fg', { 'Special', 'Boolean', 'Constant' }, '#000000'),
  red = utils.extract_color_from_hllist('fg', { 'Identifier' }, '#000000'),
  purple = utils.extract_color_from_hllist('fg', { 'Keyword' }, '#000000'),
  bg   = utils.extract_color_from_hllist('bg', { 'Normal', 'StatusLineNC' }, '#000000'),
  fg   = utils.extract_color_from_hllist('fg', { 'phpParent' }, '#000000'),
  color15    = utils.extract_color_from_hllist('fg', { 'Normal', 'StatusLine' }, '#000000'),
  color8   = utils.extract_color_from_hllist('bg', { 'StatusLine' }, '#000000'),
  color0   = utils.extract_color_from_hllist('bg', { 'Folded' }, '#000000'),
  color7   = utils.extract_color_from_hllist('fg', { 'StatusLine' }, '#000000'),
}

local none = colors.color0

require('lualine').setup {
	options = {
        theme = {
            normal = {
                a = { bg = colors.blue, fg = colors.color0, gui = 'bold' },
                b = { bg = colors.bg, fg = colors.blue },
                c = { bg = none, fg = colors.color15 },
            },
            insert = {
                a = { bg = colors.green, fg = colors.color0, gui = 'bold' },
                b = { bg = colors.bg, fg = colors.green },
                c = { bg = none, fg = colors.color15 },
              },
            replace = {
              a = { bg = colors.orange, fg = colors.color0, gui = 'bold' },
              b = { bg = colors.bg, fg = colors.orange },
              c = { bg = none, fg = colors.color15 },
            },
            visual = {
              a = { bg = colors.cyan, fg = colors.color0, gui = 'bold' },
              b = { bg = colors.bg, fg = colors.cyan },
              c = { bg = none, fg = colors.color15 },
            },
            command = {
              a = { bg = colors.red, fg = colors.color0, gui = 'bold' },
              b = { bg = colors.bg, fg = colors.red },
              c = { bg = none, fg = colors.color15 },
            }
        },
		disabled_filetypes = { 'NvimTree' },
		component_separators = ' ',
		section_separators = '',
		icons_enabled = true
	},

	sections = {
		lualine_a = {
			{ 'mode', fmt = string.upper,
                color = { gui = 'italic,bold' } }
		},

		lualine_b = {
			{ 'branch', fmt = string.lower, color = { gui = 'bold' } }
		},

		lualine_c = {
			{ 'filename', fmt = string.lower,
                color = { bg = colors.color8, fg = colors.color15 } },
            {gps.get_location, cond = gps.is_available, color = { fg = colors.green, bg = colors.color0, gui = 'italic' }},
			{ 'diff', color = { bg = colors.color0 } }
		},

		lualine_x = {
            { user_host, color = { fg = colors.color7, bg = colors.color0 }},
			{ 'diagnostics', color = { gui = 'italic,bold' } },
		},

		lualine_y = {
			{ 'filetype', color = { gui = 'italic,bold' } }
		},
		lualine_z = {
			{ 'location', color = { gui = 'bold' } }
		}
	}
}
