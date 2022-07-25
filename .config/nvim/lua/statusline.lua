local utils = require 'lualine.utils.utils'
local gps = require 'nvim-navic'

local function user_host()
	return vim.fn.getenv('USER') .. '@' .. vim.fn.hostname()
end

-- Get the colors to create theme
-- stylua: ignore
local palette = require "colours.palette"
local colors = {
  blue  = palette.blue04[1],
  green  = palette.green02[1],
  orange = palette.orange[1],
  cyan  = palette.cyan06[1],
  red = palette.red01[1],
  purple = palette.magenta05[1],
  bg   = palette.background[1],
  fg   = palette.foreground[1],
  color15    = palette.white15[1],
  color8   = palette.black08[1],
  color0   = palette.black00[1],
  color7   = palette.grey07[1],
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
                color = { gui = '', fg = colors.bg }
            },
		},

		lualine_b = {
			{ 'filename', fmt = string.lower,
                color = { bg = colors.color0, fg = colors.fg }
            },
		},

		lualine_c = {
            {gps.get_location, cond = gps.is_available, color = { fg = colors.green, bg = colors.color0, gui = 'italic' }},
			{ 'diff', color = { bg = colors.color0 } }
		},

		lualine_x = {
            -- { user_host, color = { fg = colors.color7, bg = colors.color0 }},
			{ 'diagnostics', color = { gui = 'italic,bold' } },
		},

		lualine_y = {
			{ 'branch', fmt = string.lower, color = { bg = colors.color0, gui = 'bold', fg = colors.cyan } },
            { 'filetype', color = { gui = 'italic', bg = colors.color0, fg = colors.color15 } }
		},
		lualine_z = {
			{ 'location', color = {bg = colors.color0, fg = colors.fg } }
		}
	}
}

