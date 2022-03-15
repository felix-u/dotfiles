#!/usr/bin/env bash

wq () {
    grep "$1:" "$XRESOURCES" | awk '{print $2}'
}

wqs () {
    wq "$1" | tr -d \#
}

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

MOD='Mod4'
ALT='Mod1'

TERM='foot'
FILES='thunar'
SLURP="slurp -d -b '${WS07}40' -c '${WS07}' -w 3"

swaymsg "bindsym $MOD+Return exec $TERM" &
swaymsg "bindsym $MOD+d exec ~/.config/sway/scripts/menu.sh" &

#
#####  # #    # #####  # #    #  ####   ####
#    # # ##   # #    # # ##   # #    # #
#####  # # #  # #    # # # #  # #       ####
#    # # #  # # #    # # #  # # #  ###      #
#    # # #   ## #    # # #   ## #    # #    #
#####  # #    # #####  # #    #  ####   ####
#
#
#
#

# apps and scripts
# when having xwayland installed, some apps may prefer X even if working fine
# on wayland. launch them with DISPLAY=wayland-0
swaymsg "bindsym $MOD+b exec $BROWSER" &
swaymsg "bindsym $MOD+$ALT+b exec MOZ_ENABLE_WAYLAND=1 firefox" &
swaymsg "bindsym $MOD+a exec $FILES" &
swaymsg "bindsym $MOD+z exec zathura" &
#
swaymsg "bindsym $MOD+Shift+b exec ~/.config/sway/scripts/randwall.sh \
    ~/dotfiles/Pictures/cafe-walls" &
#
swaymsg "bindsym XF86AudioMute exec pulsemixer --toggle-mute" &
swaymsg "bindsym XF86AudioRaiseVolume exec pulsemixer --change-volume +10" &
swaymsg "bindsym XF86AudioLowerVolume exec pulsemixer --change-volume -10" &
#
swaymsg "bindsym $MOD+x exec ~/.config/sway/scripts/swaylock.sh"

# sway functionality
swaymsg "bindsym $MOD+w kill" &
swaymsg "floating_modifier $MOD normal" &
swaymsg "bindsym $MOD+$ALT+r reload" &
swaymsg "bindsym $MOD+$ALT+q exit" &
#
for i in {1..8}
do
    swaymsg "bindsym $MOD+$i workspace number $i" &
    swaymsg "bindsym $MOD+Shift+$i move container to workspace number $i" &
done
#
swaymsg "bindsym $MOD+v layout stacking" &
swaymsg "bindsym $MOD+r layout tabbed" &
swaymsg "bindsym $MOD+t layout toggle split" &
swaymsg "bindsym $MOD+f fullscreen" &
swaymsg "bindsym $MOD+s floating toggle" &
    # swap focus between tiling and floating area
# swaymsg "bindsym $MOD+space focus mode_toggle" &
swaymsg "bindsym $MOD+p focus parent" &
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
swaymsg "bindsym $MOD+$ALT+g gaps inner current set 0" &
# outer gaps
swaymsg "bindsym $MOD+period gaps outer current plus 15" &
swaymsg "bindsym $MOD+Shift+period gaps outer current minus 15" &
swaymsg "bindsym $MOD+$ALT+period gaps outer current set 0" &
# colour picker FIXME
# swaymsg "bindsym $MOD+Shift+p exec $SLURP | grim -g -t ppm - | \
    # convert - -format '%[pixel:p{0,0}]' txt:-"


#
# _.._ ._  _  _..__.._  _ _
#(_||_)|_)(/_(_||(_|| |(_(/_
#   |  |
#
#
#
#    # when using border images
# swaymsg "gaps inner 50" &
# swaymsg "smart_gaps on" &
  # otherwise
swaymsg "gaps inner 0" &
swaymsg "smart_gaps off" &
#
swaymsg "font pango:Fira Sans Medium 12" &
swaymsg "title_format %app_id" &

CLRFOCUSED="$W08"
CLRUNFOCUSED="$W00"
swaymsg "default_border pixel 3"
#               class        border       background       text     indicator    child border
swaymsg "client.focused "$CLRUNFOCUSED" "$CLRFOCUSED" "$WFG" "$CLRFOCUSED" "$CLRFOCUSED"" &
swaymsg "client.focused_inactive \
                        "$CLRUNFOCUSED" "$CLRUNFOCUSED" "$WFG" "$CLRUNFOCUSED" "$CLRUNFOCUSED"" &
swaymsg "client.unfocused \
                        "$CLRUNFOCUSED" "$WBG" "$WFG" "$W15" "$CLRUNFOCUSED"" &
swaymsg "client.urgent  "$W01" "$CLRUNFOCUSED" "$WFG" "$W15" "$CLRUNFOCUSED"" &
swaymsg "client.placeholder \
                  "$WBG" "$WBG" "$WFG" "$WBG" "$WBG"" &
# # #
# # rounded borders with sway-borders
# swaymsg "default_border none" # remove titlebar
# swaymsg "border_images.focused ~/.config/sway/borders/v3/color4.png"
# swaymsg "border_images.focused_inactive ~/.config/sway/borders/v3/color0.png"
# swaymsg "border_images.unfocused ~/.config/sway/borders/v3/color0.png"
# swaymsg "border_images.urgent ~/.config/sway/borders/v3/color0.png"
# # 3


#
# |   |   _)      |                 |
#  _|   \  |   \  | / _ \  _` |  _` |
#\__|_| _|_|_| _|_\_\.__/\__,_|\__,_|
#                   _|
#
#
#
if [[ $(cat /proc/sys/kernel/hostname) == "thonkpad" ]]; then
    LEFT='h'
    DOWN='j'
    UP='k'
    RIGHT='l'

    RETURN="Semicolon"
    SCRATCH="n"

    SPLITV='u'
    SPLITH='o'
    swaymsg "bindsym $MOD+$SPLITH splith" &
    swaymsg "bindsym $MOD+$SPLITV splitv" &

    WDPI=2

    swaymsg "bindsym XF86MonBrightnessUp exec brightnessctl set +5%" &
    swaymsg "bindsym XF86MonBrightnessDown exec brightnessctl set 5%-" &

    swaymsg "output * resolution 3840x2400 position 3840 0 scale $WDPI" &
    swaymsg "seat seat0 xcursor_theme "Adwaita" 24" &
    swaymsg "xwayland scale=$WDPI" &

    pkill waybar
    waybar -c ~/.config/waybar/thinkpad.json &
fi

# ____   ____
#|  _ \ / ___|
#| |_) | |
#|  __/| |___
#|_|    \____|
#
#
#
#
if [[ $(cat /proc/sys/kernel/hostname) == "archbtw" ]]; then
    LEFT='m'
    DOWN='n'
    UP='e'
    RIGHT='i'

    RETURN='o'
    SCRATCH="h"

    SPLITV='l'
    SPLITH='y'
    swaymsg "bindsym $MOD+$SPLITH splith" &
    swaymsg "bindsym $MOD+$SPLITV splitv" &

    WDPI="1.3"

    swaymsg "output * resolution 3840x2160 position 3840 0 scale $WDPI" &
    swaymsg "seat seat0 xcursor_theme "Adwaita" 24" &
    swaymsg "xwayland scale=$WDPI" &

    pkill waybar
    waybar -c ~/.config/waybar/desktop.json &
fi

swaymsg "bindsym $MOD+$RETURN exec $TERM" &
# navigation
swaymsg "bindsym $MOD+$LEFT focus left" &
swaymsg "bindsym $MOD+$DOWN focus down" &
swaymsg "bindsym $MOD+$UP focus up" &
swaymsg "bindsym $MOD+$RIGHT focus right" &
swaymsg "bindsym $MOD+Shift+$LEFT move left 50px" &
swaymsg "bindsym $MOD+Shift+$DOWN move down 50px" &
swaymsg "bindsym $MOD+Shift+$UP move up 50px" &
swaymsg "bindsym $MOD+Shift+$RIGHT move right 50px" &
# layout stuff
swaymsg "bindsym $MOD+Control+t splitt" &
# resizing
swaymsg "bindsym $MOD+$ALT+$LEFT  resize shrink width  50px" &
swaymsg "bindsym $MOD+$ALT+$DOWN  resize grow   height 50px" &
swaymsg "bindsym $MOD+$ALT+$UP    resize shrink height 50px" &
swaymsg "bindsym $MOD+$ALT+$RIGHT resize grow   width  50px" &
#
swaymsg "bindsym $MOD+Shift+$SCRATCH move scratchpad" &
swaymsg "bindsym $MOD+$SCRATCH scratchpad show" &
# wallpaper
~/.config/sway/scripts/randwall.sh ~/dotfiles/Pictures/cafe-walls &

#
# _.  _|_ _  __|_ _..__|_
#(_||_||_(_)_> |_(_||  |_
#
#
#
#
# autotiling &
polkit-dumb-agent &

pkill dunst
dunst &
