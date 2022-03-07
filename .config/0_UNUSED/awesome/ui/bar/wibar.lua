-- wibar.lua
local awful = require("awful")
local gears = require("gears")
local gfs = require("gears.filesystem")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local helpers = require("helpers")
local bling = require("bling")

local systray_margin = (beautiful.wibar_height - beautiful.systray_icon_size) / 2
local widget_bg = beautiful.xcolor0


-- Function to display widgets as pills ---------------------------------------
--
function Pillify(inputWidget, leftpad, rightpad)
    local widgetPill = wibox.widget {
        {
            inputWidget,
            left = dpi(leftpad),
            right= dpi(rightpad),
            widget = wibox.container.margin
        },
        shape = helpers.rrect(beautiful.border_radius + dpi(9)),
        bar_shape = helpers.rrect(beautiful.border_radius + dpi(9)),
        bg                 = widget_bg,
        shape_border_color = beautiful.xcolor4,
        shape_border_width = 0,
        widget             = wibox.container.background
    }
    return wibox.layout.margin(widgetPill, dpi(5), dpi(5), dpi(5), dpi(5))
end



-- Awesome Panel -----------------------------------------------------------
--
local icon1 = wibox.widget {
    widget = wibox.widget.textbox,
    font = beautiful.font_base .. " 20",
    markup = "<span foreground='" .. beautiful.xcolor4 .. "'></span>",
    align = 'center',
    valign = 'center'
}

local awesome_icon = wibox.widget {
    {
        icon1,
        top = dpi(0),
        bottom = dpi(1),
        left = dpi(4),
        right = dpi(4),
        widget = wibox.container.margin
    },
    bg = beautiful.xcolor0,
    widget = wibox.container.background
}
--awesome_icon = Pillify(awesome_icon, 10, 10)

awesome_icon:buttons(gears.table.join(awful.button({}, 1, function()
    awesome.emit_signal("widgets::start::toggle", mouse.screen)
end)))



-- Date widget ----------------------------------------------------------------
--
local dateText = wibox.widget {
    font = beautiful.font_sans,
    format = "%d.%m",
    align = "center",
    valign = "center",
    widget = wibox.widget.textclock
}

dateText.markup = "<span foreground='" .. beautiful.xcolor4 .. "'>" ..
                    dateText.text .. "</span>"

dateText:connect_signal("widget::redraw_needed", function()
    dateText.markup = "<span foreground='" .. beautiful.xcolor4 .. "'>" ..
                        dateText.text .. "</span>"
end)

local dateIcon = wibox.widget {
    font = beautiful.font_base .. " 16",
    markup = "<span foreground='" .. beautiful.xcolor4 .. "'></span>",
    align = 'center',
    valign = 'center',
    widget = wibox.widget.textbox
}

local datePill = wibox.widget {
    {dateIcon, top = dpi(1), widget = wibox.container.margin},
    helpers.horizontal_pad(10),
    {dateText, top = dpi(1), widget = wibox.container.margin},
    layout = wibox.layout.fixed.horizontal
}
datePill = Pillify(datePill, 10, 10)


-- Time widget ----------------------------------------------------------------
--
local timeText = wibox.widget {
    font = beautiful.font_sans,
    format = "%H:%M",
    align = "center",
    valign = "center",
    widget = wibox.widget.textclock
}

timeText.markup = "<span foreground='" .. beautiful.xcolor6 .. "'>" ..
                    timeText.text .. "</span>"

timeText:connect_signal("widget::redraw_needed", function()
    timeText.markup = "<span foreground='" .. beautiful.xcolor6 .. "'>" ..
                        timeText.text .. "</span>"
end)

local timeIcon = wibox.widget {
    font = beautiful.font_base .. " 16",
    markup = "<span foreground='" .. beautiful.xcolor6 .. "'></span>",
    align = 'center',
    valign = 'center',
    widget = wibox.widget.textbox
}

local timePill = wibox.widget {
        {timeIcon, top = dpi(1), widget = wibox.container.margin},
        helpers.horizontal_pad(10),
        {timeText, top = dpi(1), widget = wibox.container.margin},
        layout = wibox.layout.fixed.horizontal
}
timePill = Pillify(timePill, 10, 10)



-- Systray --------------------------------------------------------------------
--
local mysystray = wibox.widget.systray()
mysystray:set_base_size(beautiful.systray_icon_size)
local mysystrayPill = {
    {
        mysystray,
        left = dpi(9),
        right = dpi(4),
        top = dpi(5),
        bottom = dpi(5),
        widget = wibox.container.margin
    },
    shape = helpers.rrect(beautiful.border_radius + dpi(9)),
    bar_shape = helpers.rrect(beautiful.border_radius + dpi(9)),
    bg                 = widget_bg,
    shape_border_color = beautiful.xcolor6,
    shape_border_width = 0,
    widget             = wibox.container.background
}
mysystrayPill = wibox.layout.margin(mysystrayPill, dpi(5), dpi(5), dpi(5), dpi(5))



-- Playerctl widget -----------------------------------------------------------
--
-- Title Widget
local song_title = wibox.widget {
    markup = "Nothing Playing",
    align = "center",
    valign = "center",
    widget = wibox.widget.textbox
}

local song_artist = wibox.widget {
    markup = "nothing playing",
    align = "center",
    valign = "center",
    widget = wibox.widget.textbox
}

local song_logo = wibox.widget {
    markup = '<span foreground="' .. beautiful.xcolor9 .. '"></span>',
    font = beautiful.font_base.."12",
    align = "center",
    valign = "center",
    widget = wibox.widget.textbox
}

local playerctl_bar = wibox.widget {
    {
        {
            song_logo,
            top = dpi(2),
            left = dpi(0),
            right = dpi(8),
            bottom = dpi(1),
            widget = wibox.container.margin
        },
        {
            {
                song_title,
                expand = "outside",
                layout = wibox.layout.align.vertical
            },
            top = dpi(1),
            left = dpi(9),
            right = dpi(9),
            widget = wibox.container.margin
        },
        {
            {
                song_artist,
                expand = "outside",
                layout = wibox.layout.align.vertical
            },
            top = dpi(1),
            left = dpi(9),
            widget = wibox.container.margin
        },
        spacing = dpi(9),
        spacing_widget = {
            {
                bg = beautiful.xcolor8,
                shape = gears.shape.circle,
                widget = wibox.container.background
            },
            top = dpi(2),
            widget = wibox.container.margin
        },
        layout = wibox.layout.fixed.horizontal
    },
    left = dpi(10),
    right = dpi(13),
    widget = wibox.container.margin
}
playerctl_bar = Pillify(playerctl_bar, 4, 10)

playerctl_bar.visible = false

bling.signal.playerctl.enable()
awesome.connect_signal("bling::playerctl::no_players",
                       function() playerctl_bar.visible = false end)

-- Get Title
awesome.connect_signal("bling::playerctl::title_artist_album",
                       function(title, artist, _)
    playerctl_bar.visible = true
    song_title.markup = '<span foreground="' .. beautiful.xcolor10 .. '">' ..
                            title .. "</span>"

    song_artist.markup = '<span foreground="' .. beautiful.xcolor7 .. '">' ..
                             artist .. "</span>"
end)



-- Tasklist Buttons -----------------------------------------------------------
--
local tasklist_buttons = gears.table.join(
                             awful.button({}, 1, function(c)
        if c == client.focus then
            c.minimized = true
        else
            c:emit_signal("request::activate", "tasklist", {raise = true})
        end
    end), awful.button({}, 3, function()
        awful.menu.client_list({theme = {width = dpi(215), height = dpi(30)}})
    end), awful.button({}, 4, function() awful.client.focus.byidx(1) end),
                             awful.button({}, 5, function()
        awful.client.focus.byidx(-1)
    end))

bling.widget.task_preview.enable {
    height = dpi(200),                 -- The height of the popup
    width = dpi(200),                  -- The width of the popup
    placement_fn = function(c)    -- Place the widget using awful.placement (this overrides x & y)
        awful.placement.top(c, {
            margins = {
                top = dpi(beautiful.useless_gap / 2) + beautiful.wibar_height
            }
        })
    end
}


-- Tags -----------------------------------------------------------------------
--
screen.connect_signal("request::desktop_decoration", function(s)
    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6"}, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

-- Layout icon ----------------------------------------------------------------
--
    s.mylayoutbox = awful.widget.layoutbox {
        screen  = s,
        buttons = {
            awful.button({ }, 1, function () awful.layout.inc( 1) end),
            awful.button({ }, 3, function () awful.layout.inc(-1) end),
            awful.button({ }, 4, function () awful.layout.inc(-1) end),
            awful.button({ }, 5, function () awful.layout.inc( 1) end),
        }
    }

    local layoutPill = {
        {
            s.mylayoutbox,
            left = dpi(9),
            right = dpi(9),
            top = dpi(6),
            bottom = dpi(6),
            widget = wibox.container.margin
        },
        shape = helpers.rrect(beautiful.border_radius + dpi(9)),
        bar_shape = helpers.rrect(beautiful.border_radius + dpi(9)),
        bg                 = widget_bg,
        shape_border_color = beautiful.xcolor6,
        shape_border_width = 0,
        widget             = wibox.container.background
    }

    layoutPill = wibox.layout.margin(layoutPill, dpi(5), dpi(5), dpi(4), dpi(4))


    -- Taglist ----------------------------------------------------------------

    bling.widget.tag_preview.enable {
    show_client_content = true,  -- Whether or not to show the client content
    scale = 0.25,                 -- The scale of the previews compared to the screen
    honor_padding = true,        -- Honor padding when creating widget size
    honor_workarea = true,       -- Honor work area when creating widget size
    placement_fn = function(c)    -- Place the widget using awful.placement (this overrides x & y)
        awful.placement.top_left(c, {
            margins = {
                top = dpi(beautiful.useless_gap / 2) + beautiful.wibar_height,
                left = dpi(beautiful.useless_gap / 2)
            }
        })
    end
    }

    s.mytaglist = require("ui.widgets.taglist")(s)
    s.mytaglist = wibox.widget {
        awesome_icon,
            {
                s.mytaglist,
                layout = wibox.layout.fixed.horizontal
            },
            spacing = dpi(4),
            spacing_widget = {
                color = beautiful.xcolor8.."00",
                shape = gears.shape.powerline,
                widget = wibox.widget.separator
            },
            layout = wibox.layout.fixed.horizontal
        }

    local taglistPill = {
        {
            s.mytaglist,
            left = dpi(9),
            right = dpi(9),
            top = dpi(5),
            bottom = dpi(5),
            widget = wibox.container.margin
        },
        shape = helpers.rrect(beautiful.border_radius + dpi(9)),
        bar_shape = helpers.rrect(beautiful.border_radius + dpi(9)),
        bg                 = widget_bg,
        shape_border_color = beautiful.xcolor6,
        shape_border_width = 0,
        widget             = wibox.container.background
    }

    taglistPill = wibox.layout.margin(taglistPill, dpi(5), dpi(5), dpi(5), dpi(5))

    --  Tasklist --------------------------------------------------------------
        s.mytasklist = awful.widget.tasklist {
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = tasklist_buttons,
            bg = beautiful.xcolor0.."00",
            style = {bg = beautiful.xcolor0.."00"},
            layout = {
                spacing = dpi(0),
                spacing_widget = {
                    bg = beautiful.xcolor0,
                    widget = wibox.container.background
                },
                layout = wibox.layout.fixed.horizontal
            },
            widget_template = {
            {
                {
                    {
                        {
                            {
                                {
                                    awful.widget.clienticon,
                                    top = dpi(2),
                                    bottom = dpi(2),
                                    right = dpi(1),
                                    left = dpi(1),
                                    layout = wibox.container.margin,
                                    id = 'clienticon'
                                },
                                helpers.horizontal_pad(6),
                                -- Uncomment this to see the client name
                                --{id = 'text_role', widget = wibox.widget.textbox},
                                layout = wibox.layout.fixed.horizontal
                            },
                            top = dpi(2),
                            bottom = dpi(1),
                            left = dpi(9),
                            right = dpi(5),
                            widget = wibox.container.margin
                        },
                        id = "background_role",
                        widget = wibox.container.background
                    },
                    shape = helpers.rrect(beautiful.border_radius + dpi(9)),
                    bar_shape = helpers.rrect(beautiful.border_radius + dpi(9)),
                    bg                 = beautiful.bg_normal,
                    shape_border_color = beautiful.xcolor4,
                    shape_border_width = 0,
                    widget             = wibox.container.background,
                },
                left = dpi(5),
                right = dpi(5),
                top = dpi(5),
                bottom = dpi(5),
                layout = wibox.container.margin,
            },
            nil,
            create_callback = function(self, c, index, objects) --luacheck: no unused args
                self:get_children_by_id('clienticon')[1].client = c

                -- BLING: Toggle the popup on hover and disable it off hover
                self:connect_signal('mouse::enter', function()
                        awesome.emit_signal("bling::task_preview::visibility", s,
                                            true, c)
                    end)
                    self:connect_signal('mouse::leave', function()
                        awesome.emit_signal("bling::task_preview::visibility", s,
                                            false, c)
                    end)
            end,
            layout = wibox.layout.align.vertical,
        }
    }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top" })

    -- Add widgets to the wibox
    s.mywibox.widget = {
        {
            {
                layout = wibox.layout.align.horizontal,
                expand = "none",
                { -- Left widgets
                    layout = wibox.layout.fixed.horizontal,
                    taglistPill,
                    playerctl_bar,
                    s.mypromptbox,
                },
                { -- Middle widgets
                    layout = wibox.layout.fixed.horizontal,
                    s.mytasklist,
                },
                { -- Right widgets
                    layout = wibox.layout.fixed.horizontal,
                    timePill,
                    datePill,
                    mysystrayPill,
                    layoutPill,
                },
            },
            left = dpi(5),
            right = dpi(5),
            layout = wibox.container.margin,
        },
        bottom = beautiful.border_width,
        color = beautiful.xcolor0,
        widget = wibox.container.margin,
    }

end)
--



-- Titlebars ------------------------------------------------------------------
--
-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = {
        awful.button({ }, 1, function()
            c:activate { context = "titlebar", action = "mouse_move"  }
        end),
        awful.button({ }, 3, function()
            c:activate { context = "titlebar", action = "mouse_resize"}
        end),
    }

    awful.titlebar(c, {size=dpi(36)}).widget = {
        { -- Left
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                --layout  = wibox.layout.fixed.horizontal
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            --awful.titlebar.widget.minimizebutton(c),
            --awful.titlebar.widget.maximizedbutton(c),
            --awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)
