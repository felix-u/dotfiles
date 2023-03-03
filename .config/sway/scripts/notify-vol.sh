#!/usr/bin/env sh

wq () {
    grep "$1:" "$XRESOURCES" | awk '{print $2}'
}

VOLUME=$(pulsemixer --get-volume | cut -f 1 -d " ")
notify-send -h int:value:"$VOLUME" -h "string:hlcolor:$(wq color4)" "Volume"
