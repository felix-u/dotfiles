#!/usr/bin/env sh
wq () {
    grep "$1:" ~/.Xresources | awk '{print $2}'
}

WBACKGROUND="$(wq color7)"
WCOLOR0="$(wq color0)"
WCOLOR1="$(wq color1)"
WCOLOR2="$(wq color2)"
WCOLOR4="$(wq color4)"
WCOLOR6="$(wq color6)"
WCOLOR8="$(wq color8)"

# LOCKWALL=$(find ~/dotfiles/Pictures/cafe-walls/ -name "*.jpg" -type f | shuf -n 1)
swaylock -e -F \
    --inside-color ${WCOLOR0}80 --ring-color ${WCOLOR8}ff \
    --inside-ver-color ${WCOLOR2}80 --ring-ver-color ${WCOLOR2}ff \
    --inside-wrong-color ${WCOLOR1}80 \
    --inside-clear-color ${WCOLOR4}80 --ring-clear-color ${WCOLOR4}ff \
    --ring-wrong-color ${WCOLOR1}ff --line-color ${WCOLOR0}ff \
    --key-hl-color ${WCOLOR6}ff --bs-hl-color ${WCOLOR8}ff \
    --separator-color ${WCOLOR0}ff \
    --indicator-radius 150 --indicator-thickness 20 \
    --text-color ${WBACKGROUND}ff \
    --font-size 40 --font 'Fira Sans' \
    --clock --indicator --timestr "%a %d" --datestr "%H:%M"  \
    --screenshots --effect-blur 15x7 --fade-in 0.1
    # -i "$LOCKWALL"
