-- autostart.lua
local awful = require("awful")
local gears = require("gears")
--

require("module.tilebg")

--
local function run_once(cmd)
    local findme = cmd
    local firstspace = cmd:find(' ')
    if firstspace then findme = cmd:sub(0, firstspace - 1) end
    awful.spawn.easy_async_with_shell(string.format(
                                          'pgrep -u $USER -x %s > /dev/null || (%s)',
                                          findme, cmd))
end
--

-- Random tiled wallpaper at startup
-- tilebg()

-- LuaFormatter off
-- Add apps to autostart here
autostart_apps = {

    -- Compositor
    "picom &",

    "setxkbmap -option compose:ralt",
    -- Faster key repetition
    "xset r rate 200 50",

    -- Network and bluetooth applets
    "nm-applet &",
    "blueman-applet &",

    -- Keyring
    "eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)",
    "export SSH_AUTH_SOCK",
    "source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh",

    -- Random wallpaper on startup
    "exec ~/.config/bspwm/scripts/randwall.sh ~/dotfiles/Pictures/cafe-walls &",
    --

    -- Cursor
    "xsetroot -cursor_name left_ptr &",
    "unclutter --hide-on-touch &",

    -- Polkit agent for sudo privileges in certain apps
    "polkit-dumb-agent &",
    --

    -- Gestures
    "touchegg &",

    -- emacs
    "emacs --daemon &",

    -- "picom --experimental-backends --animations --animation-stiffness 600 --animation-for-open-window zoom --animation-window-mass 0.5",

}
-- LuaFormatter on

for app = 1, #autostart_apps do run_once(autostart_apps[app]) end

-- EOF ------------------------------------------------------------------------
