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
hyprctl keyword misc:disable_hyprland_logo true &
hyprctl keyword misc:disable_splash_rendering true &
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
browser="firefox"
files="io.elementary.files"
term="foot"

# Bar
# TODO

# Thinkpad
if [ "$(cat /proc/sys/kernel/hostname)" = "thonkpad" ]; then
    dpi="2"

    # Brightness control
    hyprctl keyword bind ", XF86MonBrightnessUp, exec, brightnessctl set +3%" &
    hyprctl keyword bind ", XF86MonBrightnessDown, exec, brightnessctl set 3%-" &

    # Battery check (temporary)
    hyprctl keyword bind "SUPER ALT, B, exec, notify-send \$(cat /sys/class/power_supply/BAT0/capacity)%" &

    # Display output
    hyprctl keyword monitor ", 3840x2400@60, 0x0, $dpi" &

    # Gestures
    hyprctl keyword gestures:workspace_swipe on &

    # Keybindings
    "$XDG_CONFIG_HOME"/hypr/bind.sh "qwerty" &
elif [ "$(cat /proc/sys/kernel/hostname)" = "nixbtw" ]; then
    dpi="1.3"
    
    # Display output
    hyprctl keyword monitor ", 3840x2160@60, 0x0, $dpi" &

    # Keybindings
    "$XDG_CONFIG_HOME"/hypr/bind.sh "colemak-dh" &
fi

# Notifications
pkill dunst; dunst &

# Gaps
gaps=9
hyprctl keyword general:gaps_in "$gaps" &
hyprctl keyword general:gaps_out "$((gaps * 2))" &

# Window borders
hyprctl keyword general:border_size 3 &
hyprctl keyword general:col.active_border "rgb($WSFG)" &
hyprctl keyword general:col.inactive_border "rgb($WS06)" &
hyprctl keyword decoration:rounding 0 &
hyprctl keyword decoration:drop_shadow no &

# Blur
hyprctl keyword decoration:blur no &

# Animations
hyprctl keyword animations:enabled no &

# Layout behaviour
hyprctl keyword general:layout dwindle &
hyprctl keyword dwindle:force_split 2 &
hyprctl keyword dwindle:pseudotile no &
hyprctl keyword dwindle:preserve_split yes &
hyprctl keyword misc:enable_swallow true &

# Polkit
/run/current-system/sw/libexec/polkit-gnome-authentication-agent-1 &
