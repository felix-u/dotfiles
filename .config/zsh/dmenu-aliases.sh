#!/usr/bin/env sh

wq () {
    grep "$1:" ~/.Xresources | awk '{print $2}'
}

dmenu () {
    dmenu-wl -i -h 36 -fn "$(wq fontsans) Medium 12" -nb "$(wq background)" \
        -nf "$(wq color15)" -sb "$(wq color0)" -sf "$(wq color4)"
}

alias dmenu-wl="dmenu"
