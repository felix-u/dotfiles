---------------------------
-- awesome theme --
---------------------------

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local xrdb = xresources.get_current_theme()
local rnotification = require("ruled.notification")
local dpi = xresources.apply_dpi
local helpers = require("helpers")
local wibox = require("wibox")

local gears = require("gears")
local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

local theme = {}

theme.font          = "Fira Sans Medium 12"
theme.font_sans     = "Fira Sans Medium 12"
theme.font_sans_base= "Fira Sans Medium"
theme.font_base     = "Fira Code"
theme.font_nerd     = "FiraCode Nerd Font"

theme.xbackground   = xrdb.background
theme.xforeground   = xrdb.foreground
theme.xcolor0 = xrdb.color0
theme.xcolor1 = xrdb.color1
theme.xcolor2 = xrdb.color2
theme.xcolor3 = xrdb.color3
theme.xcolor4 = xrdb.color4
theme.xcolor5 = xrdb.color5
theme.xcolor6 = xrdb.color6
theme.xcolor7 = xrdb.color7
theme.xcolor8 = xrdb.color8
theme.xcolor9 = xrdb.color9
theme.xcolor10 = xrdb.color10
theme.xcolor11 = xrdb.color11
theme.xcolor12 = xrdb.color12
theme.xcolor13 = xrdb.color13
theme.xcolor14 = xrdb.color14
theme.xcolor15 = xrdb.color15
theme.bg_normal     = xrdb.color0.."00" or "#2E3440"
theme.bg_focus      = xrdb.color0.."ff" or "#5e81ac"
theme.bg_urgent     = xrdb.color1 or "#BF616A"
theme.bg_minimize   = xrdb.color1.."20" or "#4C566A"
theme.bg_systray    = theme.bg_normal

-- Wibar
--
theme.wibar_height = dpi(38)
-- theme.wibar_bg = theme.xbackground .. "bf"
theme.wibar_bg = theme.xbackground .. "e5"
theme.wibar_pos = "bottom"
theme.widget_border_color   = theme.bg_normal
theme.widget_bg             = theme.xcolor0
theme.widget_light_bg       = theme.xcolor8


theme.border_radius       = 16

theme.fg_normal     = xrdb.foreground or "#D8DEE9"
theme.fg_focus      = xrdb.color15 or "#ECEFF4"
theme.fg_urgent     = xrdb.color1  or "#ECEFF4"
theme.fg_minimize   = xrdb.color15 or "#ECEFF4"

theme.useless_gap         = dpi(7)
theme.useless_gap_max     = dpi(225)
theme.border_width        = dpi(2)
theme.border_color_normal = xrdb.color0 or "#2e3440"
theme.border_color_active = xrdb.color4 or "#81A1C1"
theme.border_color_marked = xrdb.color2 or "#5E81AC"

theme.gap_single_client = false

theme.notification_font = theme.font_sans
theme.notification_bg = theme.xcolor0
theme.notification_border_color = theme.xcolor0
theme.notification_border_width = theme.border_width
theme.notification_spacing = theme.useless_gap
theme.notification_border_radius = theme.border_radius
local notification_icon = gfs.get_configuration_dir() .. "icons/notif-center/notification.png"
theme.notification_icon = gears.color.recolor_image(notification_icon, theme.xcolor1)
theme.notif_timeout = 7

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_bg_normal = theme.xbackground
theme.titlebar_bg_focus = theme.xbackground

local circle = gears.surface.load_uncached(
    themes_path.."clean/titlebar/circle.png")

theme.titlebar_close_button_normal = themes_path.."default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themes_path.."default/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path.."default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path.."default/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path.."default/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = themes_path.."default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path.."default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = themes_path.."default/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path.."default/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = themes_path.."default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path.."default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = themes_path.."default/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path.."default/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = themes_path.."default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path.."default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = themes_path.."default/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path.."default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path.."default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path.."default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themes_path.."default/titlebar/maximized_focus_active.png"

theme.wallpaper = themes_path.."default/background.png"

-- You can use your own layout icons like this:
theme.layout_fairh = themes_path.."default/layouts/fairhw.png"
theme.layout_fairv = themes_path.."default/layouts/fairvw.png"
theme.layout_floating  = themes_path.."default/layouts/floatingw.png"
theme.layout_magnifier = themes_path.."default/layouts/magnifierw.png"
theme.layout_max = themes_path.."default/layouts/maxw.png"
theme.layout_fullscreen = themes_path.."default/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path.."default/layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path.."default/layouts/tileleftw.png"
theme.layout_tile = themes_path.."default/layouts/tilew.png"
theme.layout_tiletop = themes_path.."default/layouts/tiletopw.png"
theme.layout_spiral  = themes_path.."default/layouts/spiralw.png"
theme.layout_dwindle = themes_path.."default/layouts/dwindlew.png"
theme.layout_cornernw = themes_path.."default/layouts/cornernww.png"
theme.layout_cornerne = themes_path.."default/layouts/cornernew.png"
theme.layout_cornersw = themes_path.."default/layouts/cornersww.png"
theme.layout_cornerse = themes_path.."default/layouts/cornersew.png"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = "/usr/share/icons/Numix-Circle/"

-- Icons for Notif Center
--
local icon_path = gfs.get_configuration_dir() .. "icons/"
theme.clear_icon = icon_path .. "notif-center/clear.png"
theme.clear_grey_icon = icon_path .. "notif-center/clear_grey.png"
theme.notification_icon = icon_path .. "notif-center/notification.png"
theme.delete_icon = icon_path .. "notif-center/delete.png"
theme.delete_grey_icon = icon_path .. "notif-center/delete_grey.png"


-- Systray
--
theme.systray_icon_spacing = dpi(7)
theme.bg_systray = theme.xcolor0 .. "ff"
theme.systray_icon_size = dpi(20)

-- Bling task preview
--
theme.task_preview_widget_border_radius = theme.border_radius          -- Border radius of the widget (With AA)
theme.task_preview_widget_bg = theme.xbackground             -- The bg color of the widget
theme.task_preview_widget_border_color = theme.xcolor0   -- The border color of the widget
theme.task_preview_widget_border_width = theme.border_width           -- The border width of the widget
theme.task_preview_widget_margin = dpi(theme.useless_gap)

-- Bling tag preview
--
theme.tag_preview_widget_border_radius = theme.border_radius          -- Border radius of the widget (With AA)
theme.tag_preview_client_border_radius = theme.border_radius / 2          -- Border radius of each client in the widget (With AA)
theme.tag_preview_client_opacity = 1              -- Opacity of each client
theme.tag_preview_client_bg = theme.xbackground             -- The bg color of each client
theme.tag_preview_client_border_color = theme.xcolor8   -- The border color of each client
theme.tag_preview_client_border_width = theme.border_width           -- The border width of each client
theme.tag_preview_widget_bg = theme.xbackground.."aa"            -- The bg color of the widget
theme.tag_preview_widget_border_color = theme.xcolor0   -- The border color of the widget
theme.tag_preview_widget_border_width = theme.border_width           -- The border width of the widget
theme.tag_preview_widget_margin = dpi(theme.useless_gap / 2)

-- Set different colors for urgent notifications.
rnotification.connect_signal('request::rules', function()
    rnotification.append_rule {
        rule       = { urgency = 'critical' },
        properties = { bg = "#BF616A", fg = '#eceff4' }
    }
end)

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
