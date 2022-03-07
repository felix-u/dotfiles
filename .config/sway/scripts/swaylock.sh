#!/usr/bin/env sh
wq () {
    grep "$1:" ~/.Xresources | awk '{print $2}'
}

WBACKGROUND="$(wq background)"
WCOLOR0="$(wq color0)"
WCOLOR1="$(wq color1)"
WCOLOR2="$(wq color2)"
WCOLOR4="$(wq color4)"
WCOLOR6="$(wq color6)"
WCOLOR8="$(wq color8)"


LOCKWALL=$(find ~/dotfiles/Pictures/cafe-walls/ -name "*.jpg" -type f | shuf -n 1)
swaylock -e -i "$LOCKWALL" -F \
    --inside-color ${WCOLOR0}40 --ring-color ${WBACKGROUND}ff \
    --inside-ver-color ${WCOLOR2}80 --ring-ver-color ${WCOLOR2}ff \
    --inside-wrong-color ${WCOLOR1}80 \
    --inside-clear-color ${WCOLOR4}80 --ring-clear-color ${WCOLOR4}ff \
    --ring-wrong-color ${WCOLOR1}ff --line-color ${WCOLOR0}ff \
    --key-hl-color ${WCOLOR6}ff --bs-hl-color ${WCOLOR8}ff \
    --separator-color ${WCOLOR0}ff \
    --indicator-radius 150 --indicator-thickness 20 \
    --text-color ${WBACKGROUND}ff \
    --font-size 40 --font 'Fira Sans'
