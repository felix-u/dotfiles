#!/usr/bin/env sh
pkill swaybg
[ -d "$1" ] && swaybg -m fill -i "$(find "$1" -name "*.jpg" -o -name "*.png" -type f | shuf -n 1)" &
