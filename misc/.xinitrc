#!/bin/sh

xrdb -merge ~/.Xresources
xset s off -dpms

# hide cursor when not in use
unclutter &

# faster character repetition
xset r rate 200 50

# network and bluetooth applets
# nm-applet &
# blueman-applet &

export GDK_DPI_SCALE=1.25

# keyring management
# eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
# export SSH_AUTH_SOCK
# source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

# BSPWM
#sxhkd -c $HOME/.config/sxhkd/sxhkdrc $HOME/.config/sxhkd/bspwm &
exec bspwm

# awesomewm
#exec herbstluftwm --locked
#exec awesome
