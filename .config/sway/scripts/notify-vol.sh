#!/usr/bin/env sh

VOLUME=$(pulsemixer --get-volume | cut -f 1 -d " ")
notify-send -h int:value:"$VOLUME" -h "string:hlcolor:$(wq color4)" "Volume"
