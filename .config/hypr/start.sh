#!/usr/bin/env sh

# GTK fixes
gsettings set org.gnome.desktop.interface cursor-theme Adwaita &
gsettings set org.gnome.desktop.interface cursor-size 24 &
gsettings set org.gnome.desktop.interface icon-theme "elementary" &

wq () {
    grep "$1:" "$XRESOURCES" | awk '{print $2}'
}
wqs () {
    wq "$1" | tr -d \#
}

# Wallpaper
"$XDG_CONFIG_HOME"/sway/scripts/randwall.sh "$DOTFILES"/Pictures/cafe-walls &

# Warm the display based on time.
pkill wlsunset
"$XDG_CONFIG_HOME"/sway/scripts/screen_temp.sh default &

# Save theme colours.
WFG="$(wq foreground)"
WBG="$(wq background)"
W00="$(wq color0)"
W01="$(wq color1)"
W02="$(wq color2)"
W03="$(wq color3)"
W04="$(wq color4)"
W05="$(wq color5)"
W06="$(wq color6)"
W07="$(wq color7)"
W08="$(wq color8)"
W15="$(wq color15)"
WSFG="$(wqs foreground)"
WSBG="$(wqs background)"
WS00="$(wqs color0)"
WS01="$(wqs color1)"
WS02="$(wqs color2)"
WS03="$(wqs color3)"
WS04="$(wqs color4)"
WS05="$(wqs color5)"
WS06="$(wqs color6)"
WS07="$(wqs color7)"
WS08="$(wqs color8)"
WS15="$(wqs color15)"

# Apps
term="foot"
files="io.elementary.files"

# Bar
# TODO

# Thinkpad
if [ "$(cat /proc/sys/kernel/hostname)" = "thonkpad" ]; then
    dpi=2

    # Brightness control
    hyprctl keyword bind ", XF86MonBrightnessUp, exec, brightnessctl set +3%" &
    hyprctl keyword bind ", XF86MonBrightnessDown, exec, brightnessctl set 3%-" &

    # Battery check (temporary)
    hyprctl keyword bind "SUPER ALT, B, exec, notify-send \$(cat /sys/class/power_supply/BAT0/capacity)%" &

    # Display output
    hyprctl keyword monitor ", 3840x2400@60, 0x0, $dpi" &
fi
