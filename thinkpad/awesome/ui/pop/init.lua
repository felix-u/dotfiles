--local awful = require("awful")
--local gears = require("gears")
--local wibox = require "wibox"
----local exit_manager = require(... .. ".exitscreen")
--local start = require(... .. ".start")
---- local dash_manager = require(... .. ".dash")
---- local notif = require(... .. ".notif")
--local beautiful = require("beautiful")
--local xresources = require("beautiful.xresources")
--local dpi = xresources.apply_dpi

--[>
--awesome.connect_signal("widgets::dashboard::show",
--function() dash_manager.dash_show() end)
----]]

--[>
--awesome.connect_signal("widgets::notif_panel::show", function(s)
    --notif.screen = s
    --notif.visible = not notif.visible
    --awesome.emit_signal("widgets::notif_panel::status", notif.visible)
--end)
----]]

----awesome.connect_signal("widgets::exit_screen::toggle",
                       ----function() exit_manager.exit_screen_show() end)

--awesome.connect_signal("widgets::start::toggle", function()
    --if not start.visible then
        --start.visible = true
    --else
        ----local unsub_strut
        ----unsub_strut = function() unsub_strut() end
        ----local unsub_panel
        ----unsub_panel = function()
                ----start.visible = false
                ----unsub_panel()
            ----end
        --start.visible = false
    --end

    --awesome.emit_signal("widgets::start::status", start.visible)
--end)

--local peek = require(... .. ".peek")
--awesome.connect_signal("widgets::peek", function() peek.run() end)
