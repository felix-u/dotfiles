#!/usr/bin/env sh

dunstctl close-all
MUTE=$(pulsemixer --get-mute)
VOLUME=$(pulsemixer --get-volume | cut -f 1 -d " ")
if [ "$MUTE" = 0 ]; then
    notify-send -h int:value:"$VOLUME" -h "string:hlcolor:$(wq color4)" "Unmute"
else
    notify-send -h int:value:"$VOLUME" -h "string:hlcolor:$(wq color1)" "Mute"
fi
