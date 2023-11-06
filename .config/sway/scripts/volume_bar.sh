#!/usr/bin/env sh

VOLUME="$(pulsemixer --get-volume | cut -f 1 -d ' ')"
MUTE="$(pulsemixer --get-mute | tr -d '\n')"

if [ "$MUTE" = "1" ]; then
    echo $VOLUME ${CLR_BG}ff ${CLR_FG}ff ${CLR_08}ff > /tmp/bar
else
    echo "$VOLUME" > /tmp/bar
fi
