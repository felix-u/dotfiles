local awful = require("awful")
local gears = require("gears")
local gfs = gears.filesystem
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = require("beautiful.xresources").apply_dpi
local bling = require("bling")
--

-- Better mouse resizing on tiled
-- https://github.com/larkery/awesome/blob/master/better-resize.lua
require("module.better-resize")
--

-- No borders if only one tiled window
require("module.oneclient-noborder")
--

-- Save window positions in floating layout
-- https://github.com/larkery/awesome/blob/master/savefloats.lua-
require("module.savefloats")

-- Titlebars only when floating
require("module.titlebar-float")

-- Focused and unfocused border on floating windows as well
require("module.focus-colour-float")

-- Set layouts
tag.connect_signal("request::default_layouts", function()
    awful.layout.append_default_layouts({

        awful.layout.suit.tile,
        awful.layout.suit.tile.left,
        awful.layout.suit.tile.bottom,
        awful.layout.suit.tile.top,

        bling.layout.centered,
        awful.layout.suit.fair,
        awful.layout.suit.corner.nw,
        awful.layout.suit.corner.ne,
        bling.layout.equalarea,

        bling.layout.mstab,
        awful.layout.suit.fair.horizontal,
        awful.layout.suit.spiral,
        awful.layout.suit.spiral.dwindle,
        awful.layout.suit.floating,
        awful.layout.suit.max,
    })
end)
--
