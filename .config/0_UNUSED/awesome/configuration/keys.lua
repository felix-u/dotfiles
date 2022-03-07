-- keys.lua
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup")
--local awestore = require("awestore")
local bling = require("bling")

require("module.tilebg")
--

-- Mouse bindings
awful.mouse.append_global_mousebindings({
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewprev),
    awful.button({ }, 5, awful.tag.viewnext),
})
--

-- setup for useless_gap shortuct
require("ui.notifs.gap")

-- Keyboard bindings
awful.keyboard.append_global_keybindings({
    --awful.key({ modkey, "Mod1" }, "w",      hotkeys_popup.show_help,
              --{description="show help", group="awesome"}),
    awful.key({ modkey, "Mod1" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey }, "d", function ()
        awful.util.spawn("rofi -show drun") end),
    awful.key({ modkey, "Mod1" }, "w",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey, "Mod1"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),
    --awful.key({ modkey }, "x",
              --function ()
                  --awful.prompt.run {
                    --prompt       = "Run Lua code: ",
                    --textbox      = awful.screen.focused().mypromptbox.widget,
                    --exe_callback = awful.util.eval,
                    --history_path = awful.util.get_cache_dir() .. "/history_eval"
                  --}
              --end,
              --{description = "lua execute prompt", group = "awesome"}),
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    --awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              --{description = "run prompt", group = "launcher"}),
    -- SHORTCUTS
    -- Super + D --> rofi
    --awful.key({ "Mod1" }, "Tab", function ()
        --awful.util.spawn("rofi -show window") end),
    awful.key({ modkey, "Shift" }, "y", function ()
        awful.util.spawn("rofi -show emoji") end),
    awful.key({ modkey }, "b", function ()
        -- awful.spawn.with_shell("GDK_DPI_SCALE=1 GDK_SCALE=1 firefox") end),
        awful.spawn.with_shell("firefox") end),
    awful.key({ modkey }, "r", function ()
        awful.util.spawn("alacritty --command 'ranger'") end),
    awful.key({ modkey }, "a", function ()
        awful.util.spawn("thunar") end),
    awful.key({ modkey }, "z", function ()
        awful.util.spawn("zathura") end),
    awful.key({ modkey, "Shift" }, "b", function ()
        awful.spawn.with_shell("exec ~/.config/bspwm/scripts/randwall.sh ~/dotfiles/Pictures/cafe-walls &") end),

    awful.key({ modkey, "Shift" }, "s", function ()
        awful.spawn.with_shell("maim -s -u | xclip -selection clipboard -t image/png -i && notify-send 'Screenshot saved to clipboard'") end),
    awful.key({ modkey, "Control" }, "s", function ()
        awful.spawn.with_shell("maim -u | xclip -selection clipboard -t image/png -i && notify-send 'Fullscreen screenshot saved to clipboard'") end),
    awful.key({ modkey, "Shift", "Control" }, "s", function ()
        awful.spawn.with_shell("maim -u ~/Pictures/screenshots/$(date +%s).png && notify-send 'Fullscreen screenshot saved to ~/Pictures/screenshots'") end),
    awful.key({ modkey, "Shift" }, "a", function ()
        awful.spawn.with_shell("maim -s -u ~/Pictures/screenshots/$(date +%s).png && notify-send 'Screenshot saved to ~/Pictures/screenshots'") end),
    awful.key({ modkey, "Control" }, "a", function ()
        awful.util.spawn("flameshot gui") end),

    -- emacs
    awful.key({ modkey }, "y", function ()
        awful.spawn("emacsclient -a '' -c") end),



        -- Random tiled wallpapers ------------------------------------------------
    awful.key({ modkey, "Shift" }, "t", function ()
       tilebg()
    end),
    awful.key({ modkey, "Shift" }, "c", function ()
       tilebgClr()
    end),

    -- Change useless_gap on the fly
    awful.key({ modkey, }, "g", function ()
        change_gap("increment")
    end),
    awful.key({ modkey, "Shift" }, "g", function ()
        change_gap("decrement")
    end),


    awful.key({}, "XF86AudioRaiseVolume", function () awful.util.spawn("pulsemixer --change-volume +5") end),
    awful.key({}, "XF86AudioLowerVolume", function () awful.util.spawn("pulsemixer --change-volume -5") end),
    awful.key({}, "XF86AudioMute",        function () awful.util.spawn("pulsemixer --toggle-mute") end),

    -- Brightness
    --
    awful.key({}, "XF86MonBrightnessUp", function () awful.util.spawn("brightnessctl set +5%") end),
    awful.key({}, "XF86MonBrightnessDown", function () awful.util.spawn("brightnessctl set 5%-") end),

    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"}),

    -- Screen locking
    --
    awful.key({ modkey }, "x", function ()
        awful.spawn.with_shell("~/dotfiles/misc/lock.sh") end)
})

-- Tags related keybindings
awful.keyboard.append_global_keybindings({
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),
})

-- Focus related keybindings
awful.keyboard.append_global_keybindings({
   awful.key({ modkey,           }, "m",
        function ()
            awful.client.focus.bydirection("left")
            if client.focus then client.focus:raise() end
        end,
        {description = "focus left", group = "client"}
    ),
    awful.key({ modkey,           }, "n",
        function ()
            awful.client.focus.bydirection("down")
            if client.focus then client.focus:raise() end
        end,
        {description = "focus down", group = "client"}
    ),
    awful.key({ modkey,           }, "e",
        function ()
            awful.client.focus.bydirection("up")
            if client.focus then client.focus:raise() end
        end,
        {description = "focus up", group = "client"}
    ),
    awful.key({ modkey,           }, "i",
        function ()
            awful.client.focus.bydirection("right")
            if client.focus then client.focus:raise() end
        end,
        {description = "focus right", group = "client"}
    ),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),
    awful.key({ modkey, "Control" }, "n", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "e", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey, "Shift" }, "h",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:activate { raise = true, context = "key.unminimize" }
                  end
              end,
              {description = "restore minimized", group = "client"}),
})

-- Layout related keybindings
awful.keyboard.append_global_keybindings({
    awful.key({ modkey, "Shift"   }, "m", function () awful.client.swap.bydirection("left")    end,
    {description = "swap with client on the left", group = "client"}),
    awful.key({ modkey, "Shift"   }, "n", function () awful.client.swap.bydirection("down")    end,
    {description = "swap with client upwards", group = "client"}),
    awful.key({ modkey, "Shift"   }, "e", function () awful.client.swap.bydirection("up")    end,
    {description = "swap with client downwards", group = "client"}),
    awful.key({ modkey, "Shift"   }, "i", function () awful.client.swap.bydirection("right")    end,
    {description = "swap with client on the right", group = "client"}),

    awful.key({ modkey, "Mod1"    }, "m", function () awful.tag.incmwfact( -0.05) end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Mod1"    }, "i", function () awful.tag.incmwfact(  0.05) end,
              {description = "increase master width factor", group = "layout"}),

    --awful.key({ modkey, "Mod1"    }, "j", awful.client.width = awful.client.widte - 10 ),

    awful.key({ modkey,           }, "j", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey, }, "l",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, }, "u",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "i",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "m",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),
})


awful.keyboard.append_global_keybindings({
    awful.key {
        modifiers   = { modkey },
        keygroup    = "numrow",
        description = "only view tag",
        group       = "tag",
        on_press    = function (index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                tag:view_only()
            end
        end,
    },
    awful.key {
        modifiers   = { modkey, "Control" },
        keygroup    = "numrow",
        description = "toggle tag",
        group       = "tag",
        on_press    = function (index)
            local screen = awful.screen.focused()
            local tag = screen.tags[index]
            if tag then
                awful.tag.viewtoggle(tag)
            end
        end,
    },
    awful.key {
        modifiers = { modkey, "Shift" },
        keygroup    = "numrow",
        description = "move focused client to tag",
        group       = "tag",
        on_press    = function (index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:move_to_tag(tag)
                end
            end
        end,
    },
    awful.key {
        modifiers   = { modkey, "Control", "Shift" },
        keygroup    = "numrow",
        description = "toggle focused client on tag",
        group       = "tag",
        on_press    = function (index)
            if client.focus then
                local tag = client.focus.screen.tags[index]
                if tag then
                    client.focus:toggle_tag(tag)
                end
            end
        end,
    },
    awful.key {
        modifiers   = { modkey },
        keygroup    = "numpad",
        description = "select layout directly",
        group       = "layout",
        on_press    = function (index)
            local t = awful.screen.focused().selected_tag
            if t then
                t.layout = t.layouts[index] or t.layout
            end
        end,
    }
})

client.connect_signal("request::default_mousebindings", function()
    awful.mouse.append_client_mousebindings({
        awful.button({ }, 1, function (c)
            c:activate { context = "mouse_click" }
        end),
        awful.button({ modkey }, 1, function (c)
            c:activate { context = "mouse_click", action = "mouse_move"  }
        end),
        awful.button({ modkey }, 3, function (c)
            c:activate { context = "mouse_click", action = "mouse_resize"}
        end),
    })
end)

client.connect_signal("request::default_keybindings", function()
    awful.keyboard.append_client_keybindings({
        awful.key({ modkey,           }, "f",
            function (c)
                c.fullscreen = not c.fullscreen
                c:raise()
            end,
            {description = "toggle fullscreen", group = "client"}),
        awful.key({ modkey,   }, "w",      function (c) c:kill()                         end,
                {description = "close", group = "client"}),
        awful.key({ modkey }, "s",  awful.client.floating.toggle                     ,
                {description = "toggle floating", group = "client"}),
        awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
                {description = "move to master", group = "client"}),
        --awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
                --{description = "move to screen", group = "client"}),
        awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
                {description = "toggle keep on top", group = "client"}),
        awful.key({ modkey,           }, "h",
            function (c)
                -- The client currently has the input focus, so it cannot be
                -- minimized, since minimized clients can't have the focus.
                c.minimized = true
            end ,
            {description = "minimize", group = "client"}),
        awful.key({ modkey,           }, "v",
            function (c)
                c.maximized = not c.maximized
                c:raise()
            end ,
            {description = "(un)maximize", group = "client"}),
        awful.key({ modkey, "Control" }, "v",
            function (c)
                c.maximized_vertical = not c.maximized_vertical
                c:raise()
            end ,
            {description = "(un)maximize vertically", group = "client"}),
        awful.key({ modkey, "Shift"   }, "v",
            function (c)
                c.maximized_horizontal = not c.maximized_horizontal
                c:raise()
            end ,
            {description = "(un)maximize horizontally", group = "client"}),
    })
end)
--
