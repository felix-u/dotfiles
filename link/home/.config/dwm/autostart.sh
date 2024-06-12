#!/usr/bin/env sh

xset r rate 200 50

hsetroot -solid "#$CLR_08"

pkill picom; picom -b --vsync
