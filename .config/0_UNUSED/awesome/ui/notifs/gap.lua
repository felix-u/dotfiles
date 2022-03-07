local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local helpers = require("helpers")
local dpi = beautiful.xresources.apply_dpi

local width = dpi(600)
local height = dpi(150)
local screen = awful.screen.focused()

local active_color_1 = {
    type = 'linear',
    from = {0, 0},
    to = {width, height},
    stops = {{0, beautiful.xcolor4}, {0.50, beautiful.xcolor6}, {1.00, beautiful.xcolor3}}
}

local gap_icon = wibox.widget {
    markup = "<span foreground='" .. beautiful.xcolor6 .. "'></span>",
    align = 'center',
    valign = 'center',
    font = beautiful.font_nerd.." 30",
    widget = wibox.widget.textbox
}

local gap_adjust = wibox({
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

local gap_bar = wibox.widget {
    widget = wibox.widget.progressbar,
    shape = gears.shape.rounded_bar,
    bar_shape = gears.shape.rounded_bar,
    color = active_color_1,
    background_color = beautiful.xcolor8,
    max_value = beautiful.useless_gap_max,
    value = 0
}

gap_adjust:setup{
    {
        layout = wibox.layout.align.vertical,
        {
            gap_icon,
            top = dpi(15),
            left = dpi(50),
            right = dpi(50),
            bottom = dpi(15),
            widget = wibox.container.margin
        },
        {
            gap_bar,
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

-- create a 3 second timer to hide the gap adjust
-- component whenever the timer is started
local hide_gap_adjust = gears.timer {
    timeout = 3,
    autostart = true,
    callback = function() gap_adjust.visible = false end
}

-- show gap_adjust when gap is changed
local update_gap_bar = function()
    gap_bar.value = awful.tag.gap

    if gap_adjust.visible then
        hide_gap_adjust:again()
    else
        gap_adjust.visible = true
        hide_gap_adjust:start()
    end
end

local gap_increment = dpi(5)

-- local current_gap = #awful.screen.focused().selected_tag:gap
change_gap = function(operation)
    if operation == "increment" then
        awful.tag.incgap(gap_increment)
        -- if current_gap > beautiful.useless_gap_max then
        --     current_gap = beautiful.useless_gap_max
        -- end
        update_gap_bar()
    end
    if operation == "decrement" then
        awful.tag.incgap(-gap_increment)
        -- if awful.tag.gap < beautiful.useless_gap then
        --     awful.tag.gap = beautiful.useless_gap
        -- end
        update_gap_bar()
    end

    awful.layout.arrange(awful.screen.focused())
end
