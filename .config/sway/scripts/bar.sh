#!/usr/bin/env sh

if [ "$1" = "battery" ]; then
    BAT="$("$DOTFILES"/scripts/bat/bat0.sh)"
fi

DATE="$(date +'%a %d  %H:%M')"

if [ "$1" = "battery" ]; then
    echo "$BAT |   $DATE   "
else
    echo "$DATE   "
fi

