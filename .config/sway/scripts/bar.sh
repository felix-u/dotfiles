#!/usr/bin/env sh

if [ "$1" = "battery" ]; then
    BAT="$("$DOTFILES"/scripts/bat/bat0.sh)"
fi

DATE="$(date +'%a %d  %H:%M')"

VOL="$(pulsemixer --get-volume | cut -f 1 -d " " | tr -d '\n')"

SHAREDBAR="VOL:${VOL}%    $DATE   "

if [ "$1" = "battery" ]; then
    echo "$BAT |   $SHAREDBAR"
else
    echo "$SHAREDBAR"
fi

