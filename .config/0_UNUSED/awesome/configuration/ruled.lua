local awful = require("awful")
local beautiful = require("beautiful")
local ruled = require("ruled")
--

-- Rules
ruled.client.connect_signal("request::rules", function()

    -- Global
    ruled.client.append_rule {
        id         = "global",
        rule       = { },
        properties = {
            focus     = awful.client.focus.filter,
            raise     = true,
            screen    = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen
        }
    }
    --

    -- Floating clients
    ruled.client.append_rule {
        id       = "floating",
        rule_any = {
            instance = { "copyq", "pinentry" },
            class    = {
                "Arandr", "Blueman-manager"
            },
            name    = {
                "Event Tester",  -- xev.
                "Steam - News",
                "dragon"
            },
            role    = {
                "pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = { floating = true, placement = awful.placement.centered }
    }
    --

    -- Add titlebars to normal clients and dialogs
    ruled.client.append_rule {
        id         = "titlebars",
        rule_any   = { type = { "normal", "dialog" } },
        properties = { titlebars_enabled = false      }
    }
    --

    -- Center Placement
    ruled.client.append_rule {
        id = "center_placement",
        rule_any = {
            type = {"dialog", "_NET_WM_WINDOW_TYPE_DIALOG", "_NET_WM_WINDOW_TYPE_NORMAL"},
            class = {
                "Steam", "discord", "music", "markdown_input", "scratchpad", "firefox"
            },
            instance = {"music", "markdown_input", "scratchpad"},
            role = {"GtkFileChooserDialog", "conversation", "Dialog"}
        },
        properties = {placement = awful.placement.centered}
    }
    --
end)
