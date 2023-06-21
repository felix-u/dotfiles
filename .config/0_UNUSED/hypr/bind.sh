#!/usr/bin/env sh

alias bind="hyprctl keyword bind"

wq () {
    grep "$1:" "$XRESOURCES" | awk '{print $2}'
}
wqs () {
    wq "$1" | tr -d \#
}

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

# Layout-independent bindings

# Apps and scripts
browser="firefox"
files="io.elementary.files"
term="foot"
bind "SUPER, B, exec, $browser" &
bind "SUPER, A, exec, $files" &
bind "SUPER, Return, exec, $term" &
bind "SUPER, D, exec, $XDG_CONFIG_HOME/sway/scripts/menu.sh" &
bind "SUPER SHIFT, T, exec, $XDG_CONFIG_HOME/sway/scripts/screen_temp.sh" &
bind "SUPER, X, exec, $XDG_CONFIG_HOME/sway/scripts/swaylock.sh" &
bind "SUPER SHIFT, B, exec, $XDG_CONFIG_HOME/sway/scripts/randwall.sh $DOTFILES/Pictures/cafe-walls" &

# Window management
bind "SUPER, W, killactive," &
bind "SUPER ALT, Q, exit" &
bind "SUPER, S, togglefloating," &
bind "SUPER, T, layoutmsg, togglesplit," &
bind "SUPER, F, fullscreen, 0" &
bind "SUPER ALT, F, fullscreen, 1" &
bind "SUPER SHIFT, F, fakefullscreen," &
bind "SUPER, Q, centerwindow," &
bind "SUPER, tab, focuscurrentorlast," &
hyprctl keyword bindm "SUPER,mouse:272, movewindow" &
hyprctl keyword bindm "SUPER,mouse:273, resizewindow" &

# Screenshots
slurp_cmd="slurp -d -b '${WS07}40' -c '${WS07}' -w 3"
pic_dir="$HOME/Pictures/screenshots/"
bind "SUPER SHIFT, D, exec, $slurp_cmd | grim -g - $pic_dir/\$(date +%Y-%m-%d-%H:%M:%S).png" &
bind "SUPER SHIFT, S, exec, $slurp_cmd | grim -g - /tmp/screenshot.png && cat /tmp/screenshot.png | \
    wl-copy -t image/png" &
bind "SUPER ALT, D, exec, grim $pic_dir/\$(date +%Y-%m-%d-%H:%M:%S).png" &
bind "SUPER ALT, S, exec, grim /tmp/screenshot.png && cat /tmp/screenshot.png | wl-copy -t image/png" &

# Notifications
bind "SUPER, C, exec, dunstctl close" &
bind "SUPER SHIFT, C, exec, dunstctl close-all" &
bind "SUPER ALT, C, exec, dunstctl history-pop" &

# Volume
bind ", XF86AudioMute, exec, pulsemixer --toggle-mute" &
bind ", XF86AudioRaiseVolume, exec, pulsemixer --change-volume +5" &
bind ", XF86AudioLowerVolume, exec, pulsemixer --change-volume -5" &

# Layout-dependent bindings

if [ "$1" = "qwerty" ]; then
    left="H"
    down="J"
    up="K"
    right="L"

    home="Y"
    pgdown="U"
    pgup="I"
    end="O"

    return="semicolon"
elif [ "$1" = "colemak-dh" ]; then
    left="M"
    down="N"
    up="E"
    right="I"

    home="J"
    pgdown="L"
    pgup="U"
    end="Y"

    return="O"
fi

bind "SUPER, $home, layoutmsg, preselect l" &
bind "SUPER, $pgdown, layoutmsg, preselect d" &
bind "SUPER, $pgup, layoutmsg, preselect u" &
bind "SUPER, $end, layoutmsg, preselect r" &

bind "SUPER, $return, exec, $term" &

# Window management

for i in $(seq 1 9); do
    bind "SUPER, $i, workspace, $i" &
    bind "SUPER SHIFT, $i, movetoworkspace, $i" &
done

bind "SUPER, $left, movefocus, l" &
bind "SUPER, $down, movefocus, d" &
bind "SUPER, $up, movefocus, u" &
bind "SUPER, $right, movefocus, r" &

bind "SUPER SHIFT, $left, swapwindow, l" &
bind "SUPER SHIFT, $down, swapwindow, d" &
bind "SUPER SHIFT, $up, swapwindow, u" &
bind "SUPER SHIFT, $right, swapwindow, r" &

resize_amount=100
bind "SUPER ALT, $left, resizeactive, -$resize_amount 0" &
bind "SUPER ALT, $down, resizeactive, 0 $resize_amount" &
bind "SUPER ALT, $up, resizeactive, 0 -$resize_amount" &
bind "SUPER ALT, $right, resizeactive, $resize_amount 0" &
