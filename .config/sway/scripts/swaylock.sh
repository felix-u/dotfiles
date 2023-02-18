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

# LOCKWALL=$(find ~/dotfiles/Pictures/cafe-walls/ -name "*.jpg" -type f | shuf -n 1)
swaylock -e -F \
    --ring-clear-color "${WCOLOR4}ff" --ring-ver-color "${WCOLOR2}ff" \
    --ring-color "${WCOLOR8}ff" \
    --ring-wrong-color "${WCOLOR1}ff" --line-color "${WCOLOR8}ff" \
    --key-hl-color "${WCOLOR7}ff" --bs-hl-color "${WCOLOR8}ff" \
    --separator-color "${WCOLOR8}ff" \
    --indicator-radius 150 --indicator-thickness 20 \
    --text-color "${WFOREGROUND}ff" \
    --font-size 40 --font "$(wq fontsans)" \
    --color "$SCREENBG" \
    --inside-color "${WCOLOR0}80" \
    --inside-ver-color "${WCOLOR2}80" \
    --inside-wrong-color "${WCOLOR1}80" \
    --inside-clear-color "${WCOLOR4}80" \
    --fade-in 0.1 \
    --timestr "%a %d" --datestr "%H:%M" \
    --indicator \
    --clock
    # --screenshots --effect-blur 15x7
    # -i "$LOCKWALL"
