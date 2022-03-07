#!/bin/bash

# see if muted
if [ $(pacmd list-sinks | awk '/muted/ { print $2 }' | tail -n 1) == "no" ]
then # not muted
    awk -F"[][]" '/Left:/ { print $2 }' <(amixer sget Master)
elif [ $(pacmd list-sinks | awk '/muted/ { print $2 }' | tail -n 1) == "yes" ]
then # muted
    echo "$(awk -F"[][]" '/Left:/ { print $2 }' <(amixer sget Master)) ()"
fi
