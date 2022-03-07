local awful = require("awful")
local bling = require("bling")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
--local awestore = require("awestore")
local helpers = require("helpers")
--

-- Animation settings ---------------------------------------------------------
--
--local anim_x = awestore.tweened(-1010, {
    --duration = 225,
    --easing = awestore.easing.cubic_in_out
--})

--local anim_y = awestore.tweened(-1120, {
    --duration = 225,
    --easing = awestore.easing.circ_in_out
--})

--
scr_w = 3840
scr_h = 2160
len_x = 1500
len_y = 1500

-- Discord scratchpad ---------------------------------------------------------
--
discord_scratch = bling.module.scratchpad:new{
    command = "Discord",
    rule = {instance = "discord"},
    sticky = false,
    autoclose = false,
    floating = true,
    geometry = {x = (scr_w / 2 - len_x / 2), y = (scr_h / 2 - len_y / 2), height = len_y, width = len_x},
    reapply = true,
    --awestore = {y = anim_y}
}
awesome.connect_signal("scratch::discord",
    function() discord_scratch:toggle() end)

-- Alacritty scratchpad ---------------------------------------------------------
--
term_scratch = bling.module.scratchpad:new{
    command = "alacritty --class 'term_scratch'",
    rule = {instance = "alacritty"},
    sticky = false,
    autoclose = false,
    floating = true,
    geometry = {x = dpi(936), y = dpi(464), height = dpi(800), width = dpi(1200)},
    reapply = true,
    --awestore = {y = anim_y}
}
awesome.connect_signal("scratch::alacritty",
    function() term_scratch:toggle() end)

-- Zathura scratchpad ---------------------------------------------------------
--
zathura_scratch = bling.module.scratchpad:new{
    command = "zathura",
    rule = {instance = "zathura"},
    sticky = false,
    autoclose = false,
    floating = true,
    geometry = {x = (scr_w / 2 - len_x / 2), y = (scr_h / 2 - len_y / 2), height = len_y, width = len_x},
    reapply = true,
    --awestore = {y = anim_y}
}
awesome.connect_signal("scratch::zathura",
    function() zathura_scratch:toggle() end)

-- Qalculate scratchpad ---------------------------------------------------------
--
qlen_x = len_x / 1.3
qlen_y = len_y / 2
qscr_w = scr_w - 10
qscr_h = scr_h - 10
qalculate_scratch = bling.module.scratchpad:new{
    command = "qalculate-gtk",
    rule = {instance = "qalculate"},
    sticky = false,
    autoclose = false,
    floating = true,
    geometry = {x = scr_w - qlen_x - 20, y = 70, height = qlen_y, width = qlen_x},
    reapply = true,
    --awestore = {y = anim_y}
}
awesome.connect_signal("scratch::qalculate",
    function() qalculate_scratch:toggle() end)

