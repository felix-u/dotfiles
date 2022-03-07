
local awful = require("awful")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local bling = require("bling")

-- Random tiled wallpapers ------------------------------------------------
--

function genIcon ()
    num = math.random(1,12)
    if num == 1 then
        tileIcon = "" 
    elseif num == 2 then
        tileIcon = "" 
    elseif num == 3 then
        tileIcon = "" 
    elseif num == 4 then
        tileIcon = "" 
    elseif num == 5 then
        tileIcon = "כּ" 
    elseif num == 6 then
        tileIcon = "" 
    elseif num == 7 then
        tileIcon = "" 
    elseif num == 8 then
        tileIcon = "" 
    elseif num == 9 then
        tileIcon = "" 
    elseif num == 10 then
        tileIcon = ""
    elseif num == 11 then
        tileIcon = ""
    elseif num == 12 then
        tileIcon = "" end
end


function tilebg ()
    genIcon()
    awful.screen.connect_for_each_screen(function(s)  -- that way the wallpaper is applied to every screen
        bling.module.tiled_wallpaper(tileIcon, s, {        -- call the actual function ("x" is the string that will be tiled)
            fg = beautiful.xcolor0,  -- define the foreground color
            bg = beautiful.xbackground,  -- define the background color
            offset_y = dpi(12),   -- set a y offset
            offset_x = dpi(12),   -- set a x offset
            font = "Fira Code Nerd Font",   -- set the font (without the size)
            font_size = dpi(25),  -- set the font size
            padding = dpi(100),   -- set padding (default is 100)
            zickzack = true  -- rectangular pattern or criss cross
        })
    end)
end

function tilebgClr ()
    genIcon()
    function genClr ()
        fgClr = math.random(0,6)
        if fgClr == 0 then
            fgClr = beautiful.xcolor0
        elseif fgClr == 1 then
            fgClr = beautiful.xcolor1
        elseif fgClr == 2 then
            fgClr = beautiful.xcolor2
        elseif fgClr == 3 then
            fgClr = beautiful.xcolor3
        elseif fgClr == 4 then
            fgClr = beautiful.xcolor4
        elseif fgClr == 5 then
            fgClr = beautiful.xcolor5
        elseif fgClr == 6 then
            fgClr = beautiful.xcolor6
        end

        bgClr = math.random(0,6)
        if bgClr == 0 then
            bgClr = beautiful.xbackground
        elseif bgClr == 1 then
            bgClr = beautiful.xcolor1
        elseif bgClr == 2 then
            bgClr = beautiful.xcolor2
        elseif bgClr == 3 then
            bgClr = beautiful.xcolor3
        elseif bgClr == 4 then
            bgClr = beautiful.xcolor4
        elseif bgClr == 5 then
            bgClr = beautiful.xcolor5
        elseif bgClr == 6 then
            bgClr = beautiful.xcolor6
        end

        if fgClr == bgClr then
            genClr()
        end
    end

    genClr()

    awful.screen.connect_for_each_screen(function(s)  -- that way the wallpaper is applied to every screen
        bling.module.tiled_wallpaper(tileIcon, s, {        -- call the actual function ("x" is the string that will be tiled)
            fg = fgClr,  -- define the foreground color
            bg = bgClr,  -- define the background color
            offset_y = dpi(12),   -- set a y offset
            offset_x = dpi(12),   -- set a x offset
            font = "Fira Code Nerd Font",   -- set the font (without the size)
            font_size = dpi(25),  -- set the font size
            padding = dpi(100),   -- set padding (default is 100)
            zickzack = true  -- rectangular pattern or criss cross
        })
    end)
end

