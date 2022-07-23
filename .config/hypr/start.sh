#!/usr/bin/env bash

systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP &
gsettings set org.gnome.desktop.interface cursor-theme Adwaita &
gsettings set org.gnome.desktop.interface cursor-size 24 &
gsettings set org.gnome.desktop.interface icon-theme 'Adwaita' &

# wallpaper
~/.config/sway/scripts/randwall.sh ~/dotfiles/Pictures/cafe-walls &

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

TERM="foot"

FILES='pcmanfm'
SLURP="slurp -d -b '${WS07}40' -c '${WS07}' -w 3"

#
# |   |   _)      |                 |
#  _|   \  |   \  | / _ \  _` |  _` |
#\__|_| _|_|_| _|_\_\.__/\__,_|\__,_|
#                   _|
#
#
#
if [[ $(cat /proc/sys/kernel/hostname) == "thonkpad" ]]; then

    WDPI=2

    hyprctl keyword bind ",XF86MonBrightnessUp,exec,brightnessctl set +5%" &
    hyprctl keyword bind ",XF86MonBrightnessDown,exec,brightnessctl set 5%-" &

    hyprctl keyword monitor ",3840x2400@60,0x0,$WDPI"
    Xwayland &

    killall -s SIGKILL waybar; killall -s SIGKILL .waybar-wrapped
    waybar -c ~/.config/waybar/thinkpad.json &

    hyprctl keyword gestures:workspace_swipe 1 &
    hyprctl keyword gestures:workspace_swipe_fingers 3 &
    hyprctl keyword gestures:workspace_swipe_distance 200 &
    hyprctl keyword gestures:workspace_swipe_min_speed_to_force 20 &
        
    VIM_L="H"
    VIM_D="J"
    VIM_U="K"
    VIM_R="L"
    HI_L="Y"
    HI_D="U"
    HI_U="I"
    HI_R="O"
    RET="semicolon"
    
    hyprctl keyword bind "SUPER,$RET,exec,$TERM" &
    hyprctl keyword bind "SUPER,$VIM_L,movefocus,l" &
    hyprctl keyword bind "SUPER,$VIM_D,movefocus,d" &
    hyprctl keyword bind "SUPER,$VIM_U,movefocus,u" &
    hyprctl keyword bind "SUPER,$VIM_R,movefocus,r" &

    hyprctl keyword bind "SUPERSHIFT,$VIM_L,movewindow,l" &
    hyprctl keyword bind "SUPERSHIFT,$VIM_D,movewindow,d" &
    hyprctl keyword bind "SUPERSHIFT,$VIM_U,movewindow,u" &
    hyprctl keyword bind "SUPERSHIFT,$VIM_R,movewindow,r" &

    hyprctl keyword bind "SUPER,$HI_L,resizeactive,-40 0" &
    hyprctl keyword bind "SUPER,$HI_D,resizeactive,0 -40" &
    hyprctl keyword bind "SUPER,$HI_U,resizeactive,0 40" &
    hyprctl keyword bind "SUPER,$HI_R,resizeactive,40 0" &
    

# ____   ____
#|  _ \ / ___|
#| |_) | |
#|  __/| |___
#|_|    \____|
#
#
#
#
elif [[ $(cat /proc/sys/kernel/hostname) == "nixbtw" ]]; then

    WDPI="1.3"

    hyprctl keyword monitor ",3840x2160@60,0x0,$WDPI"
    Xwayland &

    killall -s SIGKILL waybar; killall -s SIGKILL .waybar-wrapped
    waybar -c ~/.config/waybar/desktop.json &

    VIM_L="M"
    VIM_D="N"
    VIM_U="E"
    VIM_R="I"            
    HI_L="J"
    HI_D="L"
    HI_U="U"
    HI_R="Y"
    RET="O"
    
    hyprctl keyword bind "SUPER,$RET,exec,$TERM" &
    hyprctl keyword bind "SUPER,$VIM_L,movefocus,l" &
    hyprctl keyword bind "SUPER,$VIM_D,movefocus,d" &
    hyprctl keyword bind "SUPER,$VIM_U,movefocus,u" &
    hyprctl keyword bind "SUPER,$VIM_R,movefocus,r" &

    hyprctl keyword bind "SUPERSHIFT,$VIM_L,movewindow,l" &
    hyprctl keyword bind "SUPERSHIFT,$VIM_D,movewindow,d" &
    hyprctl keyword bind "SUPERSHIFT,$VIM_U,movewindow,u" &
    hyprctl keyword bind "SUPERSHIFT,$VIM_R,movewindow,r" &

    hyprctl keyword bind "SUPER,$HI_L,resizeactive,-40 0" &
    hyprctl keyword bind "SUPER,$HI_D,resizeactive,0 -40" &
    hyprctl keyword bind "SUPER,$HI_U,resizeactive,0 40" &
    hyprctl keyword bind "SUPER,$HI_R,resizeactive,40 0" &
fi

hyprctl keyword bind "SUPER,left,moveactive,-40 0" &
hyprctl keyword bind "SUPER,down,moveactive,0 40" &
hyprctl keyword bind "SUPER,up,moveactive,0 -40" &
hyprctl keyword bind "SUPER,right,moveactive,40 0" &
hyprctl keyword bind "SUPERSHIFT,left,movewindow,l" &
hyprctl keyword bind "SUPERSHIFT,right,movewindow,r" &
hyprctl keyword bind "SUPERSHIFT,up,movewindow,u" &
hyprctl keyword bind "SUPERSHIFT,down,movewindow,d" &
hyprctl keyword bind "SUPERALT,left,resizeactive,-40 0" &
hyprctl keyword bind "SUPERALT,down,resizeactive,0 40" &
hyprctl keyword bind "SUPERALT,up,resizeactive,0 -40" &
hyprctl keyword bind "SUPERALT,right,resizeactive,40 0" &

hyprctl keyword bind "SUPER,return,exec,$TERM" &
hyprctl keyword bind "SUPER,D,exec,rofi -show drun" &


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
hyprctl keyword bind "SUPER,B,exec,$BROWSER" &
hyprctl keyword bind "SUPERALT,B,exec,MOZ_ENABLE_WAYLAND=1 firefox" &
hyprctl keyword bind "SUPER,A,exec,$FILES" &
#
hyprctl keyword bind "SUPERSHIFT,B,exec,~/.config/sway/scripts/randwall.sh \
    ~/dotfiles/Pictures/cafe-walls" &
#
hyprctl keyword bind ",XF86AudioMute,exec,pulsemixer --toggle-mute" &
hyprctl keyword bind ",XF86AudioRaiseVolume,exec,pulsemixer --change-volume +10" &
hyprctl keyword bind ",XF86AudioLowerVolume,exec,pulsemixer --change-volume -10" &
#
# lock system
hyprctl keyword bind "SUPER,X,exec,~/.config/sway/scripts/swaylock.sh"
#
hyprctl keyword bind "SUPER,F,fullscreen" &
hyprctl keyword bind "SUPER,S,togglefloating" &
hyprctl keyword bind "SUPERSHIFT,T,pseudo" &
hyprctl keyword bind "SUPER,tab,cyclenext,prev" &
hyprctl keyword bind "SUPER,W,killactive" &
hyprctl keyword bind "SUPERALT,Q,exit" &
hyprctl keyword bind "SUPERALT,R,exec,~/.config/hypr/start.sh" &
#

for i in {1..9}
do
    hyprctl keyword bind "SUPER,$i,workspace,$i" &
done
# also use 0 for 10
hyprctl keyword bind "SUPER,0,workspace,10" &
# move to workspace
hyprctl keyword bind "SUPERSHIFT,exclam,movetoworkspace,1"
hyprctl keyword bind "SUPERSHIFT,at,movetoworkspace,2"
hyprctl keyword bind "SUPERSHIFT,numbersign,movetoworkspace,3"
hyprctl keyword bind "SUPERSHIFT,dollar,movetoworkspace,4"
hyprctl keyword bind "SUPERSHIFT,percent,movetoworkspace,5"
hyprctl keyword bind "SUPERSHIFT,asciicircum,movetoworkspace,6"
hyprctl keyword bind "SUPERSHIFT,ampersand,movetoworkspace,7"
hyprctl keyword bind "SUPERSHIFT,asterisk,movetoworkspace,8"
hyprctl keyword bind "SUPERSHIFT,parenleft,movetoworkspace,9"
hyprctl keyword bind "SUPERSHIFT,parenright,movetoworkspace,10"
#

#
# screenshot and screen recording
hyprctl keyword bind "SUPERSHIFT,D,exec,$SLURP | grim -g - ~/Pictures/screenshots/\$(date ,%Y-%m-%d-%H%M).png" &
hyprctl keyword bind "SUPERSHIFT,S,exec,$SLURP | grim -g - /tmp/screenshot.png && cat /tmp/screenshot.png | wl-copy -t image/png" &
hyprctl keyword bind "SUPER,ALT,D,exec,grim ~/Pictures/screenshots/\$(date ,%Y-%m-%d-%H%M).png" &
hyprctl keyword bind "SUPER,ALT,S,exec,grim /tmp/screenshot.png && cat /tmp/screenshot.png | wl-copy -t image/png" &
# notifs
hyprctl keyword bind "SUPER,C,exec,dunstctl close" &
hyprctl keyword bind "SUPERSHIFT,C,exec,dunstctl close-all" &
hyprctl keyword bind "SUPER,ALT,C,exec,dunstctl history-pop" &

#
# _.._ ._  _  _..__.._  _ _
#(_||_)|_)(/_(_||(_|| |(_(/_
#   |  |
#
#
#
hyprctl keyword general:sensitivity 1.0 &
hyprctl keyword general:main_mod SUPER &
hyprctl keyword general:gaps_in 5 &
hyprctl keyword general:gaps_out 20 &
hyprctl keyword general:border_size 4 &
hyprctl keyword general:col.active_border 0xff"$WS08" &
hyprctl keyword general:col.inactive_border 0xff"$WS00" &
hyprctl keyword general:cursor_inactive_timeout 10 &
hyprctl keyword general:apply_sens_to_raw 0 &
hyprctl keyword general:damage_tracking full &
#
hyprctl keyword decoration:rounding 30 &
hyprctl keyword decoration:blur 1 &
hyprctl keyword decoration:blur_size 9 &
hyprctl keyword decoration:blur_passes 2 &
hyprctl keyword decoration:active_opacity 1 &
hyprctl keyword decoration:inactive_opacity 0.81 &
hyprctl keyword decoration:multisample_edges 0 &
hyprctl keyword decoration:drop_shadow 1 &
hyprctl keyword decoration:shadow_range 9 &
hyprctl keyword decoration:shadow_render_power 1 &
hyprctl keyword decoration:shadow_ignore_window 0 &
hyprctl keyword decoration:col.shadow 0x40000000 &
hyprctl keyword decoration:col.shadow_inactive 0x20000000 &
#
# animations configured in hyprland.conf
#
hyprctl keyword dwindle:pseudotile 1 &
hyprctl keyword dwindle:force_split 2 &

#
# _.  _|_ _  __|_ _..__|_
#(_||_||_(_)_> |_(_||  |_
#
#
#
#
/run/current-system/sw/libexec/polkit-gnome-authentication-agent-1 &

pkill dunst
dunst &
