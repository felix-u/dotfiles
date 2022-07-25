local palette = {
    background  = {"$(wq background)", 0,  "background"},
    foreground  = {"$(wq foreground)", 16, "foreground"},
    black00     = {"$(wq color0)", 1,  "black00"},
    red01       = {"$(wq color1)", 2,  "red01"},
    green02     = {"$(wq color2)", 3,  "green02"},
    yellow03    = {"$(wq color3)", 4,  "yellow03"},
    blue04      = {"$(wq color4)", 5,  "blue04"},
    magenta05   = {"$(wq color5)", 6,  "magenta05"},
    cyan06      = {"$(wq color6)", 7,  "cyan06"},
    grey07      = {"$(wq color7)", 8,  "grey07"},
    black08     = {"$(wq color0)", 9,  "black08"},
    red09       = {"$(wq color1)", 10, "red09"},
    green10     = {"$(wq color2)", 11, "green10"},
    yellow11    = {"$(wq color3)", 12, "yellow11"},
    blue12      = {"$(wq color4)", 13, "blue12"},
    magenta13   = {"$(wq color5)", 14, "magenta13"},
    cyan14      = {"$(wq color6)", 15, "cyan14"},
    white15     = {"$(wq color7)", 16, "white15"},

    orange = {
        "$(pastel mix $(wq color1) $(wq color3) | pastel saturate 0.09 | pastel rotate -7 | pastel darken 0.02 | pastel format hex)",
        3,
        "orange",
    },
    pink = {
        "$(pastel mix $(wq color1) $(wq color5) | pastel saturate 0.25 | pastel rotate -8 | pastel darken 0.01 | pastel format hex)",
        1,
        "pink",
    },
}

return palette
