#!/usr/bin/env sh

TR="ff"
swaylock -e -F \
    --ring-clear-color "#${CLR_04}$TR" --ring-ver-color "#${CLR_02}$TR" \
    --ring-color "#${CLR_08}$TR" \
    --ring-wrong-color "#${CLR_01}$TR" --line-color "#${CLR_08}$TR" \
    --key-hl-color "#${CLR_07}$TR" --bs-hl-color "#${CLR_08}$TR" \
    --separator-color "#${CLR_08}$TR" \
    --indicator-radius 150 --indicator-thickness 20 \
    --text-color "#${CLR_FG}$TR" \
    --font-size 40 --font "$FONT_SANS" \
    --color "#$CLR_08" \
    --inside-color "#${CLR_00}$TR" \
    --inside-ver-color "#${CLR_02}$TR" \
    --inside-wrong-color "#${CLR_01}$TR" \
    --inside-clear-color "#${CLR_04}$TR" \
    --timestr "%a %d" --datestr "%H:%M" \
    --indicator \
    --clock
