#!/usr/bin/env bash

source "$XDG_CONFIG_HOME"/sh/env.sh
source "$XDG_CONFIG_HOME"/sh/aliases_funcs.sh

pkill swaybg; swaybg -c "#$CLR_08" &

SWAYFONT="$FONT_SANS Semi-Bold 12"

# swaymsg "bar std mode invisible"
# swaymsg "bar std position top"
# swaymsg "bar std font pango:$SWAYFONT"
# swaymsg "bar std hidden_state hide"
# swaymsg "bar std status_padding 0"
# swaymsg "bar std height 35"
# swaymsg "bar std colors background $WBG"
# swaymsg "bar std colors statusline $WFG"
# swaymsg "bar std colors focused_workspace ${W00}ff ${W00}ff $WFG"
# swaymsg "bar std colors inactive_workspace ${WBG}ff ${WBG}ff $W07"
# swaymsg "bar modifier $ALT"

pkill wlsunset
"$XDG_CONFIG_HOME"/sway/scripts/screen_temp.sh default &

pgrep "pipewire" > /dev/null || /usr/libexec/pipewire-launcher &
pgrep "xdg-desktop-portal-gtk" > /dev/null || \
    /usr/libexec/xdg-desktop-portal-gtk &
pgrep "xdg-desktop-portal-wlr" > /dev/null || \
    /usr/libexec/xdg-desktop-portal-wlr &

gsettings set org.gnome.desktop.wm.preferences titlebar-font "$FONT_SANS 12" &
gsettings set org.gnome.desktop.interface font-name "$FONT_SANS 12" &
gsettings set org.gnome.desktop.interface document-font-name "$FONT_SANS 12" &
gsettings set org.gnome.desktop.interface monospace-font-name "$FONT_MONO 12" &

MOD="Mod4"
ALT="Mod1"

if [[ $(cat /proc/sys/kernel/hostname) == "thonkpad" ]]; then

    "$XDG_CONFIG_HOME"/sway/scripts/binds qwerty "$MOD" "$ALT" &

    WDPI=2

    swaymsg "bindsym XF86MonBrightnessUp exec brightnessctl set +3%" &
    swaymsg "bindsym XF86MonBrightnessDown exec brightnessctl set 3%-" &

    swaymsg "output * resolution 3840x2400 position 3840 0 scale $WDPI" &
    swaymsg "seat seat0 xcursor_theme 'Adwaita' 24" &

    pkill waybar
    waybar -c ~/.config/waybar/thinkpad.json &

    # swaymsg "bar std status_command \"while $XDG_CONFIG_HOME/sway/scripts/bar.sh battery; do sleep 1; done\"" &

elif [[ $(cat /proc/sys/kernel/hostname) == "alpinebtw" ]]; then

    "$XDG_CONFIG_HOME"/sway/scripts/binds colemak "$MOD" "$ALT" &

    WDPI="1.3"

    swaymsg "output * resolution 3840x2160 position 3840 0 scale $WDPI" &
    swaymsg "seat seat0 xcursor_theme 'Adwaita' 24" &

    pkill waybar
    waybar -c ~/.config/waybar/desktop.json &

    # swaymsg "bar std status_command \"while $XDG_CONFIG_HOME/sway/scripts/bar.sh; do sleep 1; done\"" &
fi

SLURP="slurp -d -b '${CLR_07}40' -c '${CLR_07}' -w 3"

swaymsg "bindsym $MOD+Return exec $TERM" &
swaymsg "bindsym $MOD+d exec '. $XDG_CONFIG_HOME/sh/env.sh && $XDG_CONFIG_HOME/sway/scripts/menu.sh'" &

# focus over parent containers
swaymsg "bindsym $MOD+Home exec swaymsg focus parent && swaymsg focus left && swaymsg focus child" &
swaymsg "bindsym $MOD+Page_Down exec swaymsg focus parent && swaymsg focus down && swaymsg focus child" &
swaymsg "bindsym $MOD+Page_Up exec swaymsg focus parent && swaymsg focus up && swaymsg focus child" &
swaymsg "bindsym $MOD+End exec swaymsg focus parent && swaymsg focus right && swaymsg focus child" &

swaymsg "bindsym $MOD+comma bar mode dock" &
swaymsg "bindsym $MOD+Shift+comma bar mode invisible" &
swaymsg "bindsym $MOD+$ALT+comma bar mode hide" &
swaymsg "bindsym $MOD+b exec $BROWSER" &

swaymsg "bindsym XF86AudioMute exec pulsemixer --toggle-mute" & 
swaymsg "bindsym XF86AudioRaiseVolume exec pulsemixer --change-volume +5" &
swaymsg "bindsym XF86AudioLowerVolume exec pulsemixer --change-volume -5" & 

swaymsg "bindsym $MOD+x exec ~/.config/sway/scripts/swaylock.sh" &

swaymsg "bindsym $MOD+$ALT+t exec $XDG_CONFIG_HOME/sway/scripts/screen_temp.sh" &

swaymsg "bindsym $MOD+w kill" &
swaymsg "floating_modifier $MOD normal" &
swaymsg "bindsym $MOD+$ALT+r reload" &
swaymsg "bindsym $MOD+$ALT+q exit" &

for i in {1..4}
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
    ~/Pictures/screenshots/\$(date +%Y-%m-%d-%H:%M:%S).png" &
swaymsg "bindsym $MOD+Shift+s exec $SLURP | grim -g - /tmp/screenshot.png && \
    cat /tmp/screenshot.png | wl-copy -t image/png" &
swaymsg "bindsym $MOD+$ALT+d exec grim \
   ~/Pictures/screenshots/\$(date +%Y-%m-%d-%H:%M:%S).png" &
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
DEFAULT_GAPS=10
swaymsg "gaps inner $DEFAULT_GAPS" &
swaymsg "font pango:$SWAYFONT" &
swaymsg "title_format %app_id" &

CLRFOCUSED="#$CLR_08"
CLRUNFOCUSED="#$CLR_00"
TEXTFOCFG="#$CLR_FG"
TEXTFOCBG="#$CLR_08"
TEXTUNFOCFG="#$CLR_07"
TEXTUNFOCBG="#$CLR_00"
TEXTFOCINACTIVEFG="#$CLR_FG"
TEXTFOCINACTIVEBG="#$CLR_00"
swaymsg "default_border pixel 4"
#               class        border       background             text                indicator      child border
swaymsg "client.focused "$TEXTFOCBG"     "$TEXTFOCBG"        "$TEXTFOCFG"          "$CLRFOCUSED"   "$CLRFOCUSED"" &
swaymsg "client.focused_inactive \
                    "$TEXTFOCINACTIVEBG" "$TEXTFOCINACTIVEBG" "$TEXTFOCINACTIVEFG" "$CLRUNFOCUSED" "$CLRUNFOCUSED"" &
swaymsg "client.unfocused \
                        "$TEXTUNFOCBG"   "$TEXTUNFOCBG"   "$TEXTUNFOCFG"          "$CLR_15"        "$CLRUNFOCUSED"" &
swaymsg "client.urgent  "$CLR_01" "$CLRUNFOCUSED" "$CLR_FG" "$CLR_15" "$CLRUNFOCUSED"" &

# # Fix for first workspace having gaps 0 on startup
# swaymsg gaps inner current set $DEFAULT_GAPS &
