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

    # Bar
    pkill waybar; waybar -c "$XDG_CONFIG_HOME"/waybar/thinkpad.json &

    # Gestures
    hyprctl keyword gestures:workspace_swipe on &

    # Keybindings
    "$XDG_CONFIG_HOME"/hypr/bind.sh "qwerty" &

elif [ "$(cat /proc/sys/kernel/hostname)" = "nixbtw" ]; then
    dpi="1.3"
    
    # Bar
    pkill waybar; waybar -c "$XDG_CONFIG_HOME"/waybar/desktop.json &

    # Keybindings
    "$XDG_CONFIG_HOME"/hypr/bind.sh "colemak-dh" &
fi

# Display output
hyprctl keyword monitor ", hires, auto, $dpi"
hyprctl keyword exec-once "xprop -root -f _XWAYLAND_GLOBAL_OUTPUT_SCALE 32c -set _XWAYLAND_GLOBAL_OUTPUT_SCALE $dpi"
hyprctl keyword env "GDK_SCALE, $dpi"
hyprctl keyword env "XCURSOR_SIZE, 24"

# Input
hyprctl keyword input:kb_layout "us" &
hyprctl keyword input:repeat_rate 50 &
hyprctl keyword input:repeat_delay 200 &
hyprctl keyword input:follow_mouse 2 &
hyprctl keyword input:sensitivity 0 &

hyprctl keyword input:touchpad:disable_while_typing true &
hyprctl keyword input:touchpad:natural_scroll yes &
hyprctl keyword input:mouse:natural_scroll yes &
hyprctl keyword input:mouse:accel_profile flat &

hyprctl keyword general:cursor_inactive_timeout 9 &
hyprctl keyword general:no_cursor_warps true &

# Notifications
pkill dunst; dunst &

# Gaps
gaps=0
hyprctl keyword general:gaps_in "$gaps" &
hyprctl keyword general:gaps_out "$((gaps * 2))" &

# Window borders
hyprctl keyword general:border_size 3 &
hyprctl keyword general:col.active_border "rgb($WSFG)" &
hyprctl keyword general:col.inactive_border "rgb($WS08)" &
hyprctl keyword decoration:rounding 0 &
hyprctl keyword decoration:drop_shadow no &
hyprctl keyword decoration:dim_inactive true &
hyprctl keyword decoration:dim_strength 0.05 &

# Blur
hyprctl keyword decoration:blur no &

# Animations
hyprctl keyword animations:enabled no &

# Layout behaviour
hyprctl keyword general:layout dwindle &
hyprctl keyword dwindle:force_split 2 &
hyprctl keyword dwindle:no_gaps_when_only true &
hyprctl keyword dwindle:pseudotile no &
hyprctl keyword dwindle:preserve_split yes &
hyprctl keyword misc:enable_swallow true &

# Polkit
/run/current-system/sw/libexec/polkit-gnome-authentication-agent-1 &
