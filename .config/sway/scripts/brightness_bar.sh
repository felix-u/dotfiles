#!/usr/bin/env sh

level="$(brightnessctl info | awk \
    'match($0, /\([^)]*\)/) { print substr($0, RSTART + 1, RLENGTH - 2) }' | \
    tr -d '%' | tr -d '\n')"

if [ "$level" -le "5" ]; then
    brightness_step=1
elif [ "$level" -le "10" ]; then
    brightness_step=2
elif [ "$level" -le "20" ]; then
    brightness_step=3
elif [ "$level" -le "40" ]; then
    brightness_step=5
elif [ "$level" -le "65" ]; then
    brightness_step=7
else
    brightness_step=10
fi

if [ "$1" = "up" ]; then
    brightnessctl set +${brightness_step}%
elif [ "$1" = "down" ]; then
    brightnessctl set ${brightness_step}%-
fi

level="$(brightnessctl info | awk \
    'match($0, /\([^)]*\)/) { print substr($0, RSTART + 1, RLENGTH - 2) }' | \
    tr -d '%' | tr -d '\n')"

echo "$level" > /tmp/bar
