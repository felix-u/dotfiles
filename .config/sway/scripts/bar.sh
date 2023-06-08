#!/usr/bin/env sh

if [ "$1" = "battery" ]; then
    BATNUM="$(cat /sys/class/power_supply/BAT0/capacity)"
    BATPERCENTAGE="${BATNUM}%"
    BATSTATE="$(cat /sys/class/power_supply/BAT0/status)"
    if [ "$BATSTATE" = "Charging" ]; then
        BATICON="⠔"
    elif [ "$BATNUM" -ge 80 ]; then
        BATICON="⣿"
    elif [ "$BATNUM" -ge 70 ]; then
        BATICON="⣾"
    elif [ "$BATNUM" -ge 60 ]; then
        BATICON="⣶"
    elif [ "$BATNUM" -ge 50 ]; then
        BATICON="⣦"
    elif [ "$BATNUM" -ge 40 ]; then
        BATICON="⣤"
    elif [ "$BATNUM" -ge 30 ]; then
        BATICON="⣄"
    elif [ "$BATNUM" -ge 20 ]; then
        BATICON="⣀"
    else
        BATICON="!"
    fi
fi

SEP="\t    "

DATE="$(date +'%a %d  %H:%M')"

VOL="$(pulsemixer --get-volume | cut -f 1 -d " " | tr -d '\n')"

SHAREDBAR="<${VOL}%>${SEP}${DATE}      "

if [ "$1" = "battery" ]; then
    echo -e "$BATICON $BATPERCENTAGE${SEP}$SHAREDBAR"
else
    echo -e "$SHAREDBAR"
fi

