#!/usr/bin/env sh

xset r rate 200 50
hsetroot -solid "#$CLR_08"
xsetroot -cursor_name left_ptr

pkill picom; picom -b --vsync
