#!/usr/bin/env bash

wq () {
    grep "$1:" "$XRESOURCES" | awk '{print $2}'
}

wqs () {
    wq "$1" | tr -d \#
}

# wallpaper
pkill swaybg; swaybg -c "$(wq background)" &
# ~/.config/sway/scripts/randwall.sh ~/dotfiles/Pictures/cafe-walls &

pkill wlsunset
"$XDG_CONFIG_HOME"/sway/scripts/screen_temp.sh default &

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

MOD="Mod4"
ALT="Mod1"
TERM="foot"

swaymsg "bar std mode invisible" &
swaymsg "bar std font pango:$(wq fontmono) Bold 12" &
swaymsg "bar std position top" &
swaymsg "bar std hidden_state hide" &
swaymsg "bar std status_padding 0" &
swaymsg "bar std height 35" &
swaymsg "bar std colors background $WBG" &
swaymsg "bar std colors statusline $WFG" &
swaymsg "bar std colors focused_workspace ${W00}ff ${W00}ff $WFG" &
swaymsg "bar std colors inactive_workspace ${WBG}ff ${WBG}ff $W07" &
swaymsg "bar std mode dock" &
swaymsg "bar modifier $ALT" &

#
#  |   |   _)      |                 |
#   _|   \  |   \  | / _ \  _` |  _` |
# \__|_| _|_|_| _|_\_\.__/\__,_|\__,_|
#                    _|
#
#
#
if [[ $(cat /proc/sys/kernel/hostname) == "thonkpad" ]]; then

    "$XDG_CONFIG_HOME"/sway/scripts/binds qwerty "$MOD" "$ALT" "$TERM" &

    WDPI=2

    swaymsg "bindsym XF86MonBrightnessUp exec brightnessctl set +3%" &
    swaymsg "bindsym XF86MonBrightnessDown exec brightnessctl set 3%-" &

    swaymsg "output * resolution 3840x2400 position 3840 0 scale $WDPI" &
    swaymsg "seat seat0 xcursor_theme 'Adwaita' 24" &
    swaymsg "xwayland scale=$WDPI" &

    # pkill waybar
    # waybar -c ~/.config/waybar/thinkpad.json &
    swaymsg "bar std status_command \"while $XDG_CONFIG_HOME/sway/scripts/bar.sh battery; do sleep 1; done\"" &

#  ____   ____
# |  _ \ / ___|
# | |_) | |
# |  __/| |___
# |_|    \____|
#
#
#
#
elif [[ $(cat /proc/sys/kernel/hostname) == "nixbtw" ]]; then

    "$XDG_CONFIG_HOME"/sway/scripts/binds colemak "$MOD" "$ALT" "$TERM" &

    WDPI="1.3"

    swaymsg "output * resolution 3840x2160 position 3840 0 scale $WDPI" &
    swaymsg "seat seat0 xcursor_theme 'Adwaita' 24" &
    swaymsg "xwayland scale=$WDPI" &

    # pkill waybar
    # waybar -c ~/.config/waybar/desktop.json &
    swaymsg "bar std status_command \"while $XDG_CONFIG_HOME/sway/scripts/bar.sh; do sleep 1; done\"" &
fi

FILES='pcmanfm'
SLURP="slurp -d -b '${WS07}40' -c '${WS07}' -w 3"

swaymsg "bindsym $MOD+Return exec $TERM" &
swaymsg "bindsym $MOD+d exec ~/.config/sway/scripts/menu.sh" &

#
# #####  # #    # #####  # #    #  ####   ####
# #    # # ##   # #    # # ##   # #    # #
# #####  # # #  # #    # # # #  # #       ####
# #    # # #  # # #    # # #  # # #  ###      #
# #    # # #   ## #    # # #   ## #    # #    #
# #####  # #    # #####  # #    #  ####   ####
#
#
#
#

# apps and scripts
# when having xwayland installed, some apps may prefer X even if working fine
# on wayland. launch them with DISPLAY=wayland-0
swaymsg "bindsym $MOD+comma bar mode dock" &
swaymsg "bindsym $MOD+Shift+comma bar mode invisible" &
swaymsg "bindsym $MOD+$ALT+comma bar mode hide" &
swaymsg "bindsym $MOD+Shift+v exec emacsclient -c" &
swaymsg "bindsym $MOD+$ALT+b exec MOZ_ENABLE_WAYLAND=1 firefox" &
swaymsg "bindsym $MOD+a exec $FILES" &
#
swaymsg "bindsym $MOD+Shift+b exec ~/.config/sway/scripts/randwall.sh \
    ~/dotfiles/Pictures/cafe-walls" &
#
swaymsg "bindsym XF86AudioMute exec pulsemixer --toggle-mute && $XDG_CONFIG_HOME/sway/scripts/notify-mute.sh" &
swaymsg "bindsym XF86AudioRaiseVolume exec pulsemixer --change-volume +5 && $XDG_CONFIG_HOME/sway/scripts/notify-vol.sh" &
swaymsg "bindsym XF86AudioLowerVolume exec pulsemixer --change-volume -5 && $XDG_CONFIG_HOME/sway/scripts/notify-vol.sh" &
#
# lock system
swaymsg "bindsym $MOD+x exec ~/.config/sway/scripts/swaylock.sh" &

# screen temp
swaymsg "bindsym $MOD+$ALT+t exec $XDG_CONFIG_HOME/sway/scripts/screen_temp.sh" &

# sway functionality
swaymsg "bindsym $MOD+w kill" &
swaymsg "floating_modifier $MOD normal" &
swaymsg "bindsym $MOD+$ALT+r reload" &
swaymsg "bindsym $MOD+$ALT+q exit" &
#

for i in {1..9}
do
    swaymsg "bindsym $MOD+$i workspace number $i" &
    swaymsg "bindsym $MOD+Shift+$i move container to workspace number $i" &
done
# also use 0 for 10
swaymsg "bindsym $MOD+0 workspace number 10" &
swaymsg "bindsym $MOD+Shift+0 move container to workspace number 10" &

#
swaymsg "bindsym $MOD+z layout stacking" &
swaymsg "bindsym $MOD+r layout tabbed" &
swaymsg "bindsym $MOD+t layout toggle split" &
swaymsg "bindsym $MOD+f fullscreen" &
swaymsg "bindsym $MOD+s floating toggle" &
# make floating windows sticky
# swaymsg "bindsym $MOD+Shift+t sticky toggle" &
# swap focus between tiling and floating area
swaymsg "bindsym $MOD+v focus mode_toggle" &
swaymsg "bindsym $MOD+p focus parent" &
swaymsg "bindsym $MOD+Shift+p focus child" &
swaymsg "bindsym $MOD+Tab workspace back_and_forth" &
#
# centre floating window
swaymsg "bindsym $MOD+q move position center" &
# you can move floating windows with the same shortcuts that swap them around
# in tiling mode
#
# screenshot and screen recording
swaymsg "bindsym $MOD+Shift+d exec $SLURP | grim -g - \
    ~/Pictures/screenshots/\$(date +%Y-%m-%d-%H%M).png" &
swaymsg "bindsym $MOD+Shift+s exec $SLURP | grim -g - /tmp/screenshot.png && \
    cat /tmp/screenshot.png | wl-copy -t image/png" &
swaymsg "bindsym $MOD+$ALT+d exec grim \
   ~/Pictures/screenshots/\$(date +%Y-%m-%d-%H%M).png" &
swaymsg "bindsym $MOD+$ALT+s exec grim /tmp/screenshot.png && \
    cat /tmp/screenshot.png | wl-copy -t image/png" &
# notifs
swaymsg "bindsym $MOD+c exec dunstctl close" &
swaymsg "bindsym $MOD+Shift+c exec dunstctl close-all" &
swaymsg "bindsym $MOD+$ALT+c exec dunstctl history-pop" &
# inner gaps
swaymsg "bindsym $MOD+g gaps inner current plus 15" &
swaymsg "bindsym $MOD+Shift+g gaps inner current minus 15" &
swaymsg "bindsym $MOD+$ALT+g gaps inner current set 15" &
# outer gaps
swaymsg "bindsym $MOD+period gaps outer current plus 15" &
swaymsg "bindsym $MOD+Shift+period gaps outer current minus 15" &
swaymsg "bindsym $MOD+$ALT+period gaps outer current set 0" &
# swap between qwerty and colemak bindings on the fly
swaymsg "bindsym $MOD+Shift+t exec $XDG_CONFIG_HOME/sway/scripts/bindswitch" &

# Cursor hiding (disable when gaming!)
swaymsg "bindsym $MOD+$ALT+v exec $XDG_CONFIG_HOME/sway/scripts/cursor show && \
    notify-send Cursor shown" &
swaymsg "bindsym $MOD+$ALT+Shift+v exec $XDG_CONFIG_HOME/sway/scripts/cursor hide && \
    notify-send Cursor hidden" &

#
#  _.._ ._  _  _..__.._  _ _
# (_||_)|_)(/_(_||(_|| |(_(/_
#    |  |
#
#
#
#    # when using border images
# swaymsg "gaps inner 50" &
# swaymsg "smart_gaps on" &
# otherwise
swaymsg "gaps inner 15" &
#
swaymsg "font pango:$(wq fontmono) Bold 12" &
swaymsg "title_format %app_id" &

CLRFOCUSED="$WFG"
CLRUNFOCUSED="$W06"
TEXTFOCFG="$WFG"
TEXTFOCBG="$W00"
TEXTUNFOCFG="$W07"
TEXTUNFOCBG="$WBG"
TEXTFOCINACTIVEBG="$W04"
TEXTFOCINACTIVEFG="$WBG"
swaymsg "default_border pixel 3"
#               class        border       background             text                indicator      child border
swaymsg "client.focused "$TEXTFOCBG"     "$TEXTFOCBG"        "$TEXTFOCFG"          "$CLRFOCUSED"   "$CLRFOCUSED"" &
swaymsg "client.focused_inactive \
                    "$TEXTFOCINACTIVEBG" "$TEXTFOCINACTIVEBG" "$TEXTFOCINACTIVEFG" "$CLRUNFOCUSED" "$CLRUNFOCUSED"" &
swaymsg "client.unfocused \
                        "$TEXTUNFOCBG"   "$TEXTUNFOCBG"   "$TEXTUNFOCFG"          "$W15"        "$CLRUNFOCUSED"" &
swaymsg "client.urgent  "$W01" "$CLRUNFOCUSED" "$WFG" "$W15" "$CLRUNFOCUSED"" &
# swaymsg "client.placeholder \
#                   "$WBG" "$WBG" "$WFG" "$WBG" "$WBG"" &

# # #
# # rounded borders with sway-borders
# swaymsg "default_border none" # remove titlebar
# swaymsg "border_images.focused ~/.config/sway/borders/v3/color4.png"
# swaymsg "border_images.focused_inactive ~/.config/sway/borders/v3/color0.png"
# swaymsg "border_images.unfocused ~/.config/sway/borders/v3/color0.png"
# swaymsg "border_images.urgent ~/.config/sway/borders/v3/color0.png"
# # 3

#
#  _.  _|_ _  __|_ _..__|_
# (_||_||_(_)_> |_(_||  |_
#
#
#
#
# autotiling &
# polkit-dumb-agent &
/run/current-system/sw/libexec/polkit-gnome-authentication-agent-1 &

pkill dunst; dunst &

# Fix for first workspace having gaps 0 on startup
swaymsg gaps inner current set 15 &
