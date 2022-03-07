-- rc.lua

-- Handle LuaRocks
pcall(require, "luarocks.loader")

-- Standard awesome library
local gfs = require("gears.filesystem")
local gears = require("gears")
local awful = require("awful")
local naughty= require("naughty")
require("awful.autofocus")
--

-- Widget and layout library
local wibox = require("wibox")
--

-- Declarative object management
local ruled = require("ruled")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
--

-- Enable hotkeys help widget for VIM and other apps
require("awful.hotkeys_popup.keys")
--

---- Disable naughty notifications (in favour of dunst)
--local _dbus = dbus; dbus = nil
--local naughty = require("naughty")
--dbus = _dbus
-- Config error handling
naughty.connect_signal("request::display_error", function(message, startup)
    naughty.notification {
        urgency = "critical",
        title   = "Oops, an error happened"..(startup and " during startup!" or "!"),
        message = message
    }
end)
--

-- Initialise theme
local beautiful = require("beautiful")
local theme = "clean"
beautiful.init(gfs.get_configuration_dir() .. "themes/" .. theme .. "/theme.lua")
local bling = require("bling")
--

-- DPI
local dpi = beautiful.xresources.apply_dpi
--
-- Import configuration files
require("configuration")

-- Import daemons and widgets
require("ui")

-- Garbage collector
collectgarbage("setpause", 110)
collectgarbage("setstepmul", 1000)
--

-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}
mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })
mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
--

-- Spawn new windows as slaves, not masters
client.connect_signal( "manage",
    function(c)
        if not awesome.startup then
            awful.client.setslave(c)
        end
    end
)
