#!/usr/bin/env sh
wq () {
    grep "$1:" "$XRESOURCES" | awk '{print $2}'
}

SCREENBG="$(wq color8)"
WBACKGROUND="$(wq color7)"
WFOREGROUND="$(wq foreground)"
WCOLOR0="$(wq color0)"
WCOLOR1="$(wq color1)"
WCOLOR2="$(wq color2)"
WCOLOR4="$(wq color4)"
WCOLOR6="$(wq color6)"
WCOLOR7="$(wq color7)"
WCOLOR8="$(wq color8)"

TR="ff"
swaylock -e -F \
    --ring-clear-color "${WCOLOR4}$TR" --ring-ver-color "${WCOLOR2}$TR" \
    --ring-color "${WCOLOR8}$TR" \
    --ring-wrong-color "${WCOLOR1}$TR" --line-color "${WCOLOR8}$TR" \
    --key-hl-color "${WCOLOR7}$TR" --bs-hl-color "${WCOLOR8}$TR" \
    --separator-color "${WCOLOR8}$TR" \
    --indicator-radius 150 --indicator-thickness 20 \
    --text-color "${WFOREGROUND}$TR" \
    --font-size 40 --font "$(wq fontsans)" \
    --color "$SCREENBG" \
    --inside-color "${WCOLOR0}$TR" \
    --inside-ver-color "${WCOLOR2}$TR" \
    --inside-wrong-color "${WCOLOR1}$TR" \
    --inside-clear-color "${WCOLOR4}$TR" \
    --timestr "%a %d" --datestr "%H:%M" \
    --indicator \
    --clock
    # -i "$LOCKWALL"
    # --fade-in 0.1 
    # --screenshots --effect-blur 15x7
