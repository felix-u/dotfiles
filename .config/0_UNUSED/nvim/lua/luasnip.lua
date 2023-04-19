local ls = require("luasnip")
local s = ls.s
local sn = ls.sn
local t = ls.t
local i = ls.i
local f = ls.f
local c = ls.c
local d = ls.d
local pi = ls.parent_indexer
local isn = require("luasnip.nodes.snippet").ISN
local psn = require("luasnip.nodes.snippet").PSN
local l = require'luasnip.extras'.l
local r = require'luasnip.extras'.rep
local p = require("luasnip.extras").partial
local types = require("luasnip.util.types")
local events = require("luasnip.util.events")
local util = require("luasnip.util.util")


-- all
--
-- insert space when character after snippet is letter
  s("mk", {
    t("$"), i(1), t("$")
  },{
    callbacks = {
      -- index `-1` means the callback is on the snippet as a whole
      [-1] = {
        [events.leave] = function ()
          vim.cmd([[
            autocmd InsertCharPre <buffer> ++once lua _G.if_char_insert_space()
          ]])
        end
      }
    }
  })
  _G.if_char_insert_space = function ()
  if string.find(vim.v.char, "%a") then
    vim.v.char = " " .. vim.v.char
  end
end


-- LaTeX
--
-- displaymath when typing dm
local tex = {}
tex.in_mathzone = function()
        return vim.fn['vimtex#syntax#in_mathzone']() == 1
end
tex.in_text = function()
        return not tex.in_mathzone()
end
s("dm", {
	t({ "\\[", "\t" }),
	i(1),
	t({ "", "\\]" }),
}, { condition = tex.in_text })

