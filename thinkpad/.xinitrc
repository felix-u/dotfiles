#!/bin/sh

xrdb -merge ~/.Xresources
xset s off -dpms

# faster character repetition
xset r rate 200 50

# GTK scaling
export GDK_DPI_SCALE=0.5
export GDK_SCALE=2

# network and bluetooth applets
# nm-applet &
# blueman-applet &

# keyring management
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY

# BSPWM
polybar laptop &
# sxhkd -c $HOME/.config/sxhkd/sxhkdrc &
exec bspwm

# awesomewm
# exec awesome

# sway
# exec sway

# export _JAVA_AWT_WM_NONREPARENTING=1
# export AWT_TOOLKIT=MToolkit

# export GDK_DPI_SCALE=1
# export GDK_BACKEND=wayland # unset for electron apps
# export QT_QPA_PLATFORM=wayland
# export CLUTTER_BACKEND=wayland
# export SDL_VIDEODRIVER=wayland

# export XDG_SESSION_TYPE=wayland
# export XDG_SESSION_DESKTOP=sway
# export XDG_CURRENT_DESKTOP=sway
