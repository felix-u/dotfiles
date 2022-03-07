#!/bin/sh
xquery () {
    xrdb -query | grep $1 | awk '{print $NF; exit}'
}
LOCKWALL=$(find ~/dotfiles/Pictures/cafe-walls/ -name "*.jpg" -type f | shuf -n 1)
i3lock -e -i "$LOCKWALL" -F \
    --inside-color=$(xquery background)00 --ring-color=$(xquery background)ff \
    --insidever-color=$(xquery color2)00 --ringver-color=$(xquery color2)ff \
    --insidewrong-color=$(xquery color1)00 \
    --ringwrong-color=$(xquery color1)ff --line-color=$(xquery color0)ff \
    --keyhl-color=$(xquery color6)ff --bshl-color=$(xquery color8)ff \
    --separator-color=$(xquery color0)ff --verif-color=$(xquery color15)ff \
    --wrong-color=$(xquery color15)ff --verif-font=Ubuntu --wrong-font=Ubuntu \
    --bar-indicator --bar-color=00000000
