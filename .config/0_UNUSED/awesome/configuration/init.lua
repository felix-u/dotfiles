local awful = require("awful")
local gears = require('gears')
local gfs = gears.filesystem
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

-- Autostart apps
require("configuration.autostart")
--

-- Default applications
terminal = "alacritty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " start " .. editor
browser = "firefox"
filemanager = "thunar"
discord = "discord"
launcher = "rofi -show drun"
--

-- Global vars
screen_width = awful.screen.focused().geometry.width
screen_height = awful.screen.focused().geometry.height
--

-- Default keys
modkey = "Mod4"
altkey = "Mod1"
shift  = "Shift"
ctrl   = "Control"
--

-- Keybinds
require("configuration.keys")

-- Rules
require("configuration.ruled")

-- Layouts and window things
require("configuration.window")

-- Scratchpad
require("configuration.scratchpad")
