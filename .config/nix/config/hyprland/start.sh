#!/usr/bin/env sh

alt=ALT
mod=SUPER
hypr_conf="$XDG_CONFIG_HOME"/nix/config/hyprland

pkill swaybg; swaybg -c "#$CLR_08" &

pkill wlsunset; "$XDG_CONFIG_HOME"/sway/scripts/screen_temp.sh default &

rm -f /tmp/bar && mkfifo /tmp/bar && tail -f /tmp/bar | wob &

hostname=$(cat /proc/sys/kernel/hostname)
if [ "$hostname" = "thonkpad" ]; then
    echo TODO bar
    RETURN=Semicolon
    LEFT=H
    DOWN=J
    UP=K
    RIGHT=L

    WDPI=2

    hyprctl setcursor Adwaita 24 &
elif [ "$hostname" = "pc" ]; then
    echo TODO bar

    RETURN=O
    LEFT=M
    DOWN=N
    UP=E
    RIGHT=I

    WDPI=2

    hyprctl setcursor Adwaita 24 &
fi


hyprctl keyword bind "$mod","$RETURN",exec,"$TERMINAL" &

hyprctl keyword bind "$mod","$LEFT",movefocus,l &
hyprctl keyword bind "${mod} SHIFT","$LEFT",movewindow,l &
hyprctl keyword bind "$mod","$DOWN",movefocus,d &
hyprctl keyword bind "${mod} SHIFT","$DOWN",movewindow,d &
hyprctl keyword bind "$mod","$UP",movefocus,u &
hyprctl keyword bind "${mod} SHIFT","$UP",movewindow,u &
hyprctl keyword bind "$mod","$RIGHT",movefocus,r &
hyprctl keyword bind "${mod} SHIFT","$RIGHT",movewindow,r &

hyprctl keyword binde "${mod} ${alt}","$LEFT",resizeactive,"-10% 0" &
hyprctl keyword binde "${mod} ${alt}","$DOWN",resizeactive,"0 -20%" &
hyprctl keyword binde "${mod} ${alt}","$UP",resizeactive,"0 20%" &
hyprctl keyword binde "${mod} ${alt}","$RIGHT",resizeactive,"10% 0" &


hyprctl keyword bind "$mod",B,exec,"$BROWSER" &
hyprctl keyword bind "${mod} ${alt}",B,exec,"$hypr_conf"/scripts/wp &
hyprctl keyword bind "$mod",D,exec,"$XDG_CONFIG_HOME"/sway/scripts/menu.sh &
hyprctl keyword bind "$mod",W,killactive &

volume_bar="$XDG_CONFIG_HOME/sway/scripts/volume_bar.sh"
hyprctl keyword bindl  ",XF86AudioMute,exec,pulsemixer --toggle-mute && $volume_bar" &
hyprctl keyword bindel ",XF86AudioRaiseVolume,exec,pulsemixer --change-volume +10 && $volume_bar" &
hyprctl keyword bindel ",XF86AudioLowerVolume,exec,pulsemixer --change-volume -10 && $volume_bar" &
hyprctl keyword bindl  ",XF86AudioPlay,exec,playerctl play-pause" &
hyprctl keyword bindl  ",XF86AudioNext,exec,playerctl next" &
hyprctl keyword bindl  ",XF86AudioPrev,exec,playerctl previous" &

# TODO: why lock no vork?
# hyprctl keyword bind "$mod",X,exec,"$XDG_CONFIG_HOME"/sway/scripts/swaylock.sh &

hyprctl keyword bind "${mod} ${alt}",T,exec,"$XDG_CONFIG_HOME"/sway/scripts/screen_temp.sh &

mouse_button_left=272
mouse_button_right=273
hyprctl keyword bindm "$mod",mouse:"$mouse_button_left",movewindow &
hyprctl keyword bindm "$mod",mouse:"$mouse_button_right",resizewindow &
hyprctl keyword bind "$mod $alt",R,exec,"hyprctl reload && ${hypr_conf}/start.sh" &
hyprctl keyword bind "$mod $alt",Q,exit &

for i in `seq 1 9`; do
    hyprctl keyword bind "$mod","$i",workspace,"$i" &
    hyprctl keyword bind "${mod} SHIFT","$i",movetoworkspace,"$i" &
done

# TODO: layouts & groups
# TODO: fuzzy picker to focus & centre any window
hyprctl keyword bind "$mod",S,togglefloating &
hyprctl keyword bind "$mod",F,fullscreen,0 &
hyprctl keyword bind "$mod SHIFT",F,fullscreen,2 &
hyprctl keyword bind "$mod",C,centerwindow &

hyprctl keyword bind "$alt",Tab,focuscurrentorlast &

SLURP="slurp -d -b '${CLR_07}40' -c '${CLR_07}' -w 3"
hyprctl keyword bind "$mod SHIFT",D,exec,"$SLURP | grim -g - ~/Pictures/\$(date +%Y-%m-%d-%H%M%S).png" &
hyprctl keyword bind "$mod SHIFT",S,exec,"$SLURP | grim -g - /tmp/screenshot.png && cat /tmp/screenshot.png | wl-copy -t image/png" &
hyprctl keyword bind "$mod  $alt",D,exec,"grim ~/Pictures/\$(date +%Y-%m-%d-%H%M%S).png" &
hyprctl keyword bind "$mod  $alt",S,exec,"grim /tmp/screenshot.png && cat /tmp/screenshot.png | wl-copy -t image/png" &


hyprctl keyword bezier ease_out_expo,0.16,1,0.3,1
nice=1,2.5,ease_out_expo
fast=1,1,ease_out_expo
hyprctl keyword animation global,$fast &
hyprctl keyword animation windowsMove,$nice &
hyprctl keyword animation border,0 &
hyprctl keyword animation fade,0 &


hyprctl keyword animations:first_launch_animation false &
hyprctl keyword animations:enabled true &


hyprctl keyword decoration:rounding 0 &
hyprctl keyword decoration:drop_shadow false &
hyprctl keyword decoration:blur:enabled false &


hyprctl keyword dwindle:force_split 2 & # always to the right/down
hyprctl keyword dwindle:no_gaps_when_only 1 &


hyprctl keyword general:border_size 3 &
hyprctl keyword general:col.active_border "rgb($CLR_FG)" &
hyprctl keyword general:col.inactive_border "rgb($CLR_BG)" &
hyprctl keyword general:gaps_in 5 &
hyprctl keyword general:gaps_out 10 &


hyprctl keyword input:accel_profile flat &
hyprctl keyword input:mouse_refocus false &
hyprctl keyword input:follow_mouse 2 &
hyprctl keyword input:repeat_delay 200 &
hyprctl keyword input:repeat_rate 50 &


hyprctl keyword monitor ,highres,auto,"$WDPI" &


hyprctl keyword xwayland:force_zero_scaling true &
