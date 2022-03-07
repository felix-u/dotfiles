#!/bin/sh
[ -d "$1" ] && xwallpaper --zoom "$(find "$1" -name "*.jpg" -o -name "*.png" -type f | shuf -n 1)"
