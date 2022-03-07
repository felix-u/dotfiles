-- wibar.lua
local awful = require("awful")
local gears = require("gears")
local gfs = require("gears.filesystem")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local helpers = require("helpers")

local systray_margin = (beautiful.wibar_height - beautiful.systray_icon_size) / 2


-- Date widget ----------------------------------------------------------------
--
local dateText = wibox.widget {
    font = beautiful.font,
    format = "%d.%m",
    align = "center",
    valign = "center",
    widget = wibox.widget.textclock
}

dateText.markup = "<span foreground='" .. beautiful.xcolor11 .. "'>" ..
                    dateText.text .. "</span>"

dateText:connect_signal("widget::redraw_needed", function()
    dateText.markup = "<span foreground='" .. beautiful.xcolor11 .. "'>" ..
                        dateText.text .. "</span>"
end)

local dateIcon = wibox.widget {
    font = beautiful.font_base .. " 16",
    markup = "<span foreground='" .. beautiful.xcolor11 .. "'></span>",
    align = 'center',
    valign = 'center',
    widget = wibox.widget.textbox
}

local datePill = wibox.widget {
    {
        {dateIcon, top = dpi(1), widget = wibox.container.margin},
        helpers.horizontal_pad(10),
        {dateText, top = dpi(1), widget = wibox.container.margin},
        layout = wibox.layout.fixed.horizontal
    },
    left = dpi(10),
    right = dpi(10),
    widget = wibox.container.margin
}

-- Time widget ----------------------------------------------------------------
--
local timeText = wibox.widget {
    font = beautiful.font,
    format = "%H:%M",
    align = "center",
    valign = "center",
    widget = wibox.widget.textclock
}

timeText.markup = "<span foreground='" .. beautiful.xcolor5 .. "'>" ..
                    timeText.text .. "</span>"

timeText:connect_signal("widget::redraw_needed", function()
    timeText.markup = "<span foreground='" .. beautiful.xcolor5 .. "'>" ..
                        timeText.text .. "</span>"
end)

local timeIcon = wibox.widget {
    font = beautiful.font_base .. " 16",
    markup = "<span foreground='" .. beautiful.xcolor5 .. "'></span>",
    align = 'center',
    valign = 'center',
    widget = wibox.widget.textbox
}

local timePill = wibox.widget {
    {
        {timeIcon, top = dpi(1), widget = wibox.container.margin},
        helpers.horizontal_pad(10),
        {timeText, top = dpi(1), widget = wibox.container.margin},
        layout = wibox.layout.fixed.horizontal
    },
    left = dpi(10),
    right = dpi(10),
    widget = wibox.container.margin
}

-- Systray --------------------------------------------------------------------
--
local mysystray = wibox.widget.systray()
mysystray:set_base_size(beautiful.systray_icon_size)

local mysystrayContainer = {
    mysystray,
    left = dpi(8),
    right = dpi(8),
    widget = wibox.container.margin
}

-- Tasklist buttons -----------------------------------------------------------
--
local tasklistButtons = gears.table.join(
                            awful.button({}, 1, function(c)
    if c == client.focus then
            c.minimized = true
        else
            c:emit_signal("request::activate", "tasklist", {raise = true})
        end
    end), awful.button({}, 3, function()
        awful.menu.client_list({theme = {width = 250}})
    end), awful.button({}, 4, function() awful.client.focus.byidx(1) end),
                             awful.button({}, 5, function()
        awful.client.focus.byidx(-1)
    end))

-- Create wibar ---------------------------------------------------------------
--
screen.connect_signal("request::desktop_decoration", function(s)
    -- Create promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    --Create layoutbox widget
    s.mylayoutbox = awful.widget.layoutbox(s)
    
    -- Create wibox
    s.mywibox = awful.wibar({position = "top", screen = s, type = "dock"})

    -- Remove wibar on fullscreen
    local function removeWibar(c)
        if c.fullscreen or c.maximized then
            c.screen.mywibox.visible = false
        else
            c.screen.mywibox.visible = true
        end
    end

    local function addWibar(c)
        if c.fullscreen or c.maximized then
            c.screen.mywibox.visible = true
        end
    end

    client.connect_signal("property::fullscreen", remove_wibar)
    client.connect_signal("request::unmanage", add_wibar)
    
    -- Create taglist widget
        s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = {
            awful.button({ }, 1, function(t) t:view_only() end),
            awful.button({ modkey }, 1, function(t)
                                            if client.focus then
                                                client.focus:move_to_tag(t)
                                            end
                                        end),
            awful.button({ }, 3, awful.tag.viewtoggle),
            awful.button({ modkey }, 3, function(t)
                                            if client.focus then
                                                client.focus:toggle_tag(t)
                                            end
                                        end),
            awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
            awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end),
        }
    }

    -- Create tasklist widget
    -- TODO
    
    local finalSystray = wibox.widget {
        {mysystrayContainer, top = dpi(5), layout = wibox.container.margin},
        bg = beautiful.xcolor0,
        shape = helpers.rrect(beautiful.border_radius - 3),
        widget = wibox.container.background
    }

    -- Add widgets to the wibox
    s.mywibox:setup{
        layout = wibox.layout.align.vertical,
        nil,
        {
            layout = wibox.layout.align.horizontal,
            expand = "none",
            {
                layout = wibox.layout.fixed.horizontal,
                {
                    {
                        {
                            s.mytaglist,
                            spacing = 15,
                            spacing_widget = {
                                color = beautiful.xcolor8,
                                shape = gears.shape.powerline,
                                widget = wibox.widget.separator
                            },
                            layout = wibox.layout.fixed.horizontal
                        },
                        bg = beautiful.xcolor0,
                        shape = helpers.rrect(beautiful.border_radius - 3),
                        widget = wibox.container.background
                    },
                    top = dpi(10),
                    left = dpi(10),
                    right = dpi(5),
                    bottom = dpi(0),
                    widget = wibox.container.margin
                },
                s.mypromptbox,
            },
            --{
                --{
                    --{
                        --s.mytasklist,
                        --bg = beautiful.xcolor0 .. "00",
                        --shape = helpers.rrect(beautiful.border_radius - 3),
                        --widget = wibox.container.background
                    --},
                    --top = dpi(10),
                    --left = dpi(5),
                    --bottom = dpi(0),
                    --right = dpi(5),
                    --widget = wibox.container.margin
                --},
                --widget = wibox.container.constraint
            --},
            {
                {
                    {
                        timePill,
                        bg = beautiful.xcolor0,
                        shape = helpers.rrect(beautiful.border_radius - 3),
                        widget = wibox.container.background
                    },
                    top = dpi(10),
                    left = dpi(5),
                    bottom = dpi(0),
                    right = dpi(5),
                    widget = wibox.container.margin
                },

                {
                    {
                        datePill,
                        bg = beautiful.xcolor0,
                        shape = helpers.rrect(beautiful.border_radius - 3),
                        widget = wibox.container.background
                    },
                    top = dpi(10),
                    left = dpi(5),
                    bottom = dpi(0),
                    right = dpi(5),
                    widget = wibox.container.margin
                },
                {
                    awful.widget.only_on_screen(finalSystray, screen[1]),
                    top = dpi(10),
                    left = dpi(5),
                    bottom = dpi(0),
                    right = dpi(5),
                    widget = wibox.container.margin
                },
                {
                    {
                        {
                            s.mylayoutbox,
                            top = dpi(4),
                            bottom = dpi(4),
                            right = dpi(7),
                            left = dpi(7),
                            widget = wibox.container.margin
                        },
                        bg = beautiful.xcolor0,
                        shape = helpers.rrect(beautiful.border_radius - 3),
                        widget = wibox.container.background
                    },
                    top = dpi(10),
                    bottom = dpi(0),
                    left = dpi(5),
                    right = dpi(10),
                    widget = wibox.container.margin
                },
                layout = wibox.layout.fixed.horizontal
            }
        },
        {
            widget = wibox.container.background,
            bg = beautiful.widget_border_color,
            forced_height = 0
        }
}


end)
