local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local helpers = require("helpers")
local dpi = beautiful.xresources.apply_dpi
require("signal.volume")

local width = dpi(600)
local height = dpi(150)
local screen = awful.screen.focused()

local active_color_1 = {
    type = 'linear',
    from = {0, 0},
    to = {width, height},
    stops = {{0, beautiful.xcolor4}, {0.50, beautiful.xcolor6}, {1.00, beautiful.xcolor3}}
}

local volume_icon = wibox.widget {
    markup = "<span foreground='" .. beautiful.xcolor4 .. "'></span>",
    align = 'center',
    valign = 'center',
    font = beautiful.font_nerd.." 30",
    widget = wibox.widget.textbox
}

local volume_adjust = wibox({
    screen = screen.primary,
    type = "notification",
    x = screen.geometry.width / 2 - width / 2,
    y = screen.geometry.height / 2 - height / 2 + 300,
    width = width,
    height = height,
    visible = false,
    ontop = true,
    bg = beautiful.xbackground .. "00"
})

local volume_bar = wibox.widget {
    widget = wibox.widget.progressbar,
    shape = gears.shape.rounded_bar,
    bar_shape = gears.shape.rounded_bar,
    color = active_color_1,
    background_color = beautiful.xcolor8,
    max_value = 150,
    value = 0
}

volume_adjust:setup{
    {
        layout = wibox.layout.align.vertical,
        {
            volume_icon,
            top = dpi(15),
            left = dpi(50),
            right = dpi(50),
            bottom = dpi(15),
            widget = wibox.container.margin
        },
        {
            volume_bar,
            left = dpi(25),
            right = dpi(25),
            bottom = dpi(30),
            widget = wibox.container.margin
        }

    },
    shape = helpers.rrect(beautiful.client_radius),
    bg = beautiful.xcolor0,
    border_width = beautiful.widget_border_width,
    border_color = beautiful.widget_border_color,
    widget = wibox.container.background
}

-- create a 3 second timer to hide the volume adjust
-- component whenever the timer is started
local hide_volume_adjust = gears.timer {
    timeout = 3,
    autostart = true,
    callback = function() volume_adjust.visible = false end
}

local firstSignal = true
 --show volume-adjust when "volume_change" signal is emitted
awesome.connect_signal("signal::volume", function(vol, muted)
    volume_bar.value = vol
    if muted or vol == 0 then
        volume_icon.markup = "<span foreground='" .. beautiful.xcolor3 ..
                                 "'><b>ﳌ</b></span>"
        volume_bar.color = beautiful.xcolor1
    else
        volume_icon.markup = "<span foreground='" .. beautiful.xcolor4 ..
                                 "'><b></b></span>"
        volume_bar.color = active_color_1
    end

    if not firstSignal then
        if volume_adjust.visible then
            hide_volume_adjust:again()
        else
            volume_adjust.visible = true
            hide_volume_adjust:start()
        end
    end

    firstSignal = false

end)

