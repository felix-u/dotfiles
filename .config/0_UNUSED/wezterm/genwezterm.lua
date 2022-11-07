local wezterm = require 'wezterm';

return {
    alternate_buffer_wheel_scroll_speed = 1,
    colors = {
        foreground = "$(wq foreground)",
        background = "$(wq background)",
        cursor_bg = "$(wq color15)",
        cursor_fg = "$(wq background)",
        cursor_border = "$(wq color15)",
        selection_fg = "$(wq color15)",
        selection_bg = "$(wq background)",
        scrollbar_thumb = "$(wq color4)",
        split = "$(wq color0)",
        ansi = {
            "$(wq color0)", "$(wq color1)", "$(wq color2)", "$(wq color3)",
            "$(wq color4)", "$(wq color5)", "$(wq color6)", "$(wq color7)",
        },
        brights = {
            "$(wq color8)", "$(wq color9)", "$(wq color10)", "$(wq color11)",
            "$(wq color12)", "$(wq color13)", "$(wq color14)", "$(wq color15)",
        },
        compose_cursor = "$(wq color2)",
    },
    custom_block_glyphs = false,
    enable_tab_bar = false,
    enable_wayland = true,
    default_cursor_style = "SteadyBar",
    font = wezterm.font("$(wq fontmono) Nerd Font", {weight="Medium"}),
    font_size = 12, -- this is also the default
    freetype_load_target = "Light",
    window_close_confirmation = "NeverPrompt",
    window_padding = {
        left = "18pt", right = "18pt",
        top = "21pt", bottom = "21pt"
    },
}
