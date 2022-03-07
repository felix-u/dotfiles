local gears = require("gears")
local gfs = require("gears.filesystem")
local naughty = require("naughty")

local display_high = true
local display_low = true
local display_charge = true
local beautiful = require("beautiful")

require("signal.battery")

awesome.connect_signal("signal::battery", function(percentage, state)
    local value = percentage

    -- only display message if its not charging and low
    if value < 20 and display_low and state == 2 then
        -- naughty.notification({
        --     title = "Battery Status",
        --     text = "Running low at " .. value .. "%",
        --     image = gears.color.recolor_image(
        --         gfs.get_configuration_dir() .. "icons/notif-center/battery.png",
        --         beautiful.xcolor1
        --     )
        -- })
        display_low = false
    end

    -- only display message once if its fully charged and high
    if display_high and state == 4 and value > 90 then
        naughty.notification({
            title = "Battery Status",
            text = "Fully charged!",
            image = gears.color.recolor_image(
                gfs.get_configuration_dir() .. "icons/notif-center/battery.png",
                beautiful.xcolor6
            )

        })
        display_high = false
    end

    -- only display once if charging
    if display_charge and state == 1 then
        naughty.notification({
            title = "Battery Status",
            text = "Charging",
            image = gears.color.recolor_image(
                gfs.get_configuration_dir() .. "icons/notif-center/battery_charging.png",
                beautiful.xcolor10
            )
        })
        display_charge = false
    end

    if value < 88 and value > 18 then
        display_low = true
        display_high = true
    end

    if state == 2 then display_charge = true end

end)
