local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local helpers = require("helpers")
local dpi = beautiful.xresources.apply_dpi

local width = dpi(600)
local height = dpi(150)
local screen = awful.screen.focused()

require("signal.brightness")

local active_color_1 = {
    type = 'linear',
    from = {0, 0},
    to = {width, height},
    stops = {{0, beautiful.xcolor4}, {0.50, beautiful.xcolor6}, {1.00, beautiful.xcolor3}}
}

local bright_icon = wibox.widget {
    markup = "<span foreground='" .. beautiful.xcolor4 .. "'><b></b></span>",
    align = 'center',
    valign = 'center',
    font = beautiful.font_nerd .. ' 30',
    widget = wibox.widget.textbox
}

-- create the bright_adjust component
local bright_adjust = wibox({
    screen = screen.primary,
    type = "notification",
    x = screen.geometry.width / 2 - width / 2,
    y = screen.geometry.height / 2 - height / 2 + 300,
    width = width,
    height = height,
    visible = false,
    ontop = true,
    bg = beautiful.xbackground.."00"
})

local bright_bar = wibox.widget {
    widget = wibox.widget.progressbar,
    shape = gears.shape.rounded_bar,
    bar_shape = gears.shape.rounded_bar,
    color = active_color_1,
    background_color = beautiful.xcolor8,
    max_value = 100,
    value = 0
}

bright_adjust:setup{
    {
        layout = wibox.layout.align.vertical,
        {
            bright_icon,
            top = dpi(15),
            left = dpi(50),
            right = dpi(50),
            bottom = dpi(15),
            widget = wibox.container.margin
        },
        {
            bright_bar,
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
local hide_bright_adjust = gears.timer {
    timeout = 3,
    autostart = true,
    callback = function() bright_adjust.visible = false end
}

local firstSignal = true
awesome.connect_signal("signal::brightness", function(value)
    bright_bar.value = value
    if not firstSignal then
        if bright_adjust.visible then
            hide_bright_adjust:again()
        else
            bright_adjust.visible = true
            hide_bright_adjust:start()
        end
    end

    firstSignal = false
end)
