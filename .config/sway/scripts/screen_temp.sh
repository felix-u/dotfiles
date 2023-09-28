#!/usr/bin/env sh

OPTIONS=$(cat <<-END
time-sensitive
3000 K (warmest)
3500 K
4000 K
4500 K
5000 K
5500 K
6000 K
6500 K (off)
7000 K (coldest)
END
)

if [ "$1" = "default" ]; then
    OPT="time"
else
    MENU="$XDG_CONFIG_HOME"/sway/scripts/general_menu.sh
    OPT="$(printf "%s" "$OPTIONS" | "$MENU" temp | head -c 4)"
fi

[ "$OPT" = "" ] && exit

if [ "$OPT" = "time" ]; then
    LOW_TEMP=4500
    if [ "$(hostname)" = "alpinebtw" ]; then
        HIGH_TEMP=5500
    elif [ "$(hostname)" = "thonkpad" ]; then
        HIGH_TEMP=6000
    fi
    pkill wlsunset
    wlsunset -S 06:30 -s 18:00 -T "$HIGH_TEMP" -t "$LOW_TEMP" &
else
    OPT_MAX=$((OPT+1))
    pkill wlsunset
    wlsunset -T "$OPT_MAX" -t "$OPT" &
fi
