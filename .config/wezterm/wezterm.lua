local wezterm = require 'wezterm';

return {
    alternate_buffer_wheel_scroll_speed = 1,
    colors = {
        foreground = "#93a1a1",
        background = "#002b36",
        cursor_bg = "#839496",
        cursor_fg = "#002b36",
        cursor_border = "#839496",
        selection_fg = "#839496",
        selection_bg = "#002b36",
        scrollbar_thumb = "#268bd2",
        split = "#073642",
        ansi = {
            "#073642", "#dc322f", "#859900", "#b58900",
            "#268bd2", "#6c71c4", "#2aa198", "#657b83",
        },
        brights = {
            "#224750", "#dc322f", "#859900", "#b58900",
            "#268bd2", "#6c71c4", "#2aa198", "#839496",
        },
        compose_cursor = "#859900",
    },
    custom_block_glyphs = false,
    enable_tab_bar = false,
    enable_wayland = true,
    default_cursor_style = "SteadyBar",
    font = wezterm.font("Iosevka Nerd Font", {weight="Medium"}),
    font_size = 12, -- this is also the default
    freetype_load_target = "Light",
    window_close_confirmation = "NeverPrompt",
    window_padding = {
        left = "18pt", right = "18pt",
        top = "21pt", bottom = "21pt"
    },
}
