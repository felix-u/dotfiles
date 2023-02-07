-- Treesitter is unstable and won't let me not use red, so I'm setting it to
-- be equal to white in the meantime.
local palette = {
    background  = {"$(wq background)", 0,  "background"},
    foreground  = {"$(wq foreground)", 16, "foreground"},
    black00     = {"$(wq color0)",  1,  "black00"},
    red01       = {"$(wq color1)",  2,  "red01"},
    green02     = {"$(wq color2)",  3,  "green02"},
    yellow03    = {"$(wq color3)",  4,  "yellow03"},
    blue04      = {"$(wq color4)",  5,  "blue04"},
    magenta05   = {"$(wq color5)",  6,  "magenta05"},
    cyan06      = {"$(wq color6)",  7,  "cyan06"},
    grey07      = {"$(wq color7)",  8,  "grey07"},
    black08     = {"$(wq color8)",  9,  "black08"},
    red09       = {"$(wq color9)",  10, "red09"},
    green10     = {"$(wq color10)", 11, "green10"},
    yellow11    = {"$(wq color11)", 12, "yellow11"},
    blue12      = {"$(wq color12)", 13, "blue12"},
    magenta13   = {"$(wq color13)", 14, "magenta13"},
    cyan14      = {"$(wq color14)", 15, "cyan14"},
    white15     = {"$(wq color15)", 16, "white15"},
}

return palette
