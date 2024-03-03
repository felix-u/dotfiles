#!/usr/bin/env bash

"$XDG_CONFIG_HOME"/sway/scripts/wp "$CLR_BG" &

SWAYFONT="$FONT_SANS Semi-Bold 12"

rm -f /tmp/bar && mkfifo /tmp/bar && tail -f /tmp/bar | wob &

swaybar_status="$XDG_CONFIG_HOME"/sway/scripts/bar.sh
swaymsg "bar std font pango:$SWAYFONT"
swaymsg "bar std colors background $CLR_00"
swaymsg "bar std colors focused_workspace ${CLR_08}ff ${CLR_08}ff $CLR_FG"
swaymsg "bar std colors inactive_workspace ${CLR_00}ff ${CLR_00}ff $CLR_07"
swaymsg "bar std colors statusline $CLR_FG"

pkill wlsunset
"$XDG_CONFIG_HOME"/sway/scripts/screen_temp.sh default &

MOD="Mod4"
ALT="Mod1"

if [[ $(cat /proc/sys/kernel/hostname) == "thonkpad" ]]; then
    swaymsg "bar std status_command \
        \"while $swaybar_status battery; do sleep 1; done\""

    "$XDG_CONFIG_HOME"/sway/scripts/binds qwerty "$MOD" "$ALT" &

    WDPI=2

    brightness_adjust="$XDG_CONFIG_HOME/sway/scripts/brightness_bar.sh"
    swaymsg "bindsym XF86MonBrightnessUp exec $brightness_adjust up" &
    swaymsg "bindsym XF86MonBrightnessDown exec $brightness_adjust down" &

    swaymsg "output * resolution 3840x2400 position 3840 0 scale $WDPI" &
    swaymsg "seat seat0 xcursor_theme 'Adwaita' 24" &

elif [[ $(cat /proc/sys/kernel/hostname) == "pc" ]]; then
    swaymsg "bar std status_command \
        \"while $swaybar_status; do sleep 1; done\""

    "$XDG_CONFIG_HOME"/sway/scripts/binds colemak "$MOD" "$ALT" &

    WDPI=2

    swaymsg "output * resolution 3840x2160 position 3840 0 scale $WDPI" &
    swaymsg "seat seat0 xcursor_theme 'Adwaita' 24" &
fi

SLURP="slurp -d -b '${CLR_07}40' -c '${CLR_07}' -w 3"

swaymsg "bindsym $MOD+Return exec $TERMINAL" &
swaymsg "bindsym $MOD+d exec '$XDG_CONFIG_HOME/sway/scripts/menu.sh'" &

# focus over parent containers
swaymsg "bindsym $MOD+Home exec swaymsg focus parent && swaymsg focus left && swaymsg focus child" &
swaymsg "bindsym $MOD+Page_Down exec swaymsg focus parent && swaymsg focus down && swaymsg focus child" &
swaymsg "bindsym $MOD+Page_Up exec swaymsg focus parent && swaymsg focus up && swaymsg focus child" &
swaymsg "bindsym $MOD+End exec swaymsg focus parent && swaymsg focus right && swaymsg focus child" &

swaymsg "bindsym $MOD+$ALT+b exec \$XDG_CONFIG_HOME/sway/scripts/wp" &

swaymsg "bindsym $MOD+comma bar mode dock" &
swaymsg "bindsym $MOD+Shift+comma bar mode invisible" &
swaymsg "bindsym $MOD+$ALT+comma bar mode hide" &
swaymsg "bindsym $MOD+b exec $BROWSER" &

volume_bar="$XDG_CONFIG_HOME/sway/scripts/volume_bar.sh"
swaymsg "bindsym XF86AudioMute exec pulsemixer --toggle-mute && $volume_bar" & 
swaymsg "bindsym XF86AudioRaiseVolume exec pulsemixer --change-volume +10 && $volume_bar" &
swaymsg "bindsym XF86AudioLowerVolume exec pulsemixer --change-volume -10 && $volume_bar" & 
swaymsg "bindsym XF86AudioPlay exec playerctl play-pause" &
swaymsg "bindsym XF86AudioNext exec playerctl next" &
swaymsg "bindsym XF86AudioPrev exec playerctl previous" &

swaymsg "bindsym $MOD+x exec ~/.config/sway/scripts/swaylock.sh" &

swaymsg "bindsym $MOD+$ALT+t exec $XDG_CONFIG_HOME/sway/scripts/screen_temp.sh" &

swaymsg "bindsym $MOD+w kill" &
swaymsg "floating_modifier $MOD normal" &
swaymsg "bindsym $MOD+$ALT+r reload" &
swaymsg "bindsym $MOD+$ALT+q exit" &

for i in {1..9}
do
    swaymsg "bindsym $MOD+$i workspace number $i" &
    swaymsg "bindsym $MOD+Shift+$i move container to workspace number $i" &
done
# swaymsg "bindsym $MOD+0 workspace number 10" &
# swaymsg "bindsym $MOD+Shift+0 move container to workspace number 10" &

swaymsg "bindsym $MOD+a layout stacking" &
swaymsg "bindsym $MOD+r layout tabbed" &
swaymsg "bindsym $MOD+t layout toggle split" &
swaymsg "bindsym $MOD+f fullscreen" &
swaymsg "bindsym $MOD+s floating toggle" &
swaymsg "bindsym $MOD+v focus mode_toggle" &
swaymsg "bindsym $MOD+p focus parent" &
swaymsg "bindsym $MOD+Shift+p focus child" &
swaymsg "bindsym $MOD+Tab workspace back_and_forth" &
swaymsg "bindsym $MOD+q move position center" &

swaymsg "bindsym $MOD+Shift+d exec $SLURP | grim -g - \
    ~/Pictures/\$(date +%Y-%m-%d-%H%M%S).png" &
swaymsg "bindsym $MOD+Shift+s exec $SLURP | grim -g - /tmp/screenshot.png && \
    cat /tmp/screenshot.png | wl-copy -t image/png" &
swaymsg "bindsym $MOD+$ALT+d exec grim \
   ~/Pictures/\$(date +%Y-%m-%d-%H%M%S).png" &
swaymsg "bindsym $MOD+$ALT+s exec grim /tmp/screenshot.png && \
    cat /tmp/screenshot.png | wl-copy -t image/png" &

swaymsg "bindsym $MOD+g gaps inner current plus 15" &
swaymsg "bindsym $MOD+Shift+g gaps inner current minus 15" &
swaymsg "bindsym $MOD+$ALT+g gaps inner current set 15" &
swaymsg "bindsym $MOD+period gaps outer current plus 15" &
swaymsg "bindsym $MOD+Shift+period gaps outer current minus 15" &
swaymsg "bindsym $MOD+$ALT+period gaps outer current set 0" &

swaymsg "bindsym $MOD+Shift+t exec $XDG_CONFIG_HOME/sway/scripts/bindswitch" &

# Disable cursor hiding when gaming!
swaymsg "bindsym $MOD+$ALT+v exec $XDG_CONFIG_HOME/sway/scripts/cursor show && \
    notify-send Cursor shown" &
swaymsg "bindsym $MOD+$ALT+Shift+v exec $XDG_CONFIG_HOME/sway/scripts/cursor hide && \
    notify-send Cursor hidden" &

swaymsg "smart_gaps on" &
DEFAULT_GAPS=0
swaymsg "gaps inner $DEFAULT_GAPS" &
swaymsg "font pango:$SWAYFONT" &
swaymsg "title_format %app_id" &

# Fix for first workspace having gaps 0 on startup
swaymsg gaps inner current set $DEFAULT_GAPS &
