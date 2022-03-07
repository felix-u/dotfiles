#!/bin/sh

alias wgen="~/Desktop/xgen/xgen-wayland"

wq () {
    grep "$1:" ~/.Xresources | awk '{print $2}'
}

# foot
wgen "$XDG_CONFIG_HOME"/foot/gencolours.ini "$XDG_CONFIG_HOME"/foot/colours.ini &
echo "foot"

# cava
wgen "$XDG_CONFIG_HOME"/cava/genconfig "$XDG_CONFIG_HOME"/cava/config
# pkill -USR1 cava # reloads whole config
pkill -USR2 cava # reloads colours only
echo "cava"

# darktable
wgen "$XDG_CONFIG_HOME"/darktable/genuser.css "$XDG_CONFIG_HOME"/darktable/user.css &
echo "darktable"

# dunst
wgen "$XDG_CONFIG_HOME"/dunst/gendunstrc "$XDG_CONFIG_HOME"/dunst/dunstrc
pkill dunst
dunst &
echo "dunst"

# firefox
wgen ~/dotfiles/.config/firefox/chrome/genuserChrome.css ~/dotfiles/.config/firefox/chrome/userChrome.css &
echo "firefox"

# nvim
DIR=$(pwd)
cd "$XDG_CONFIG_HOME"/nvim/colors/ || exit
exec ./schemescript.sh &
cd "$DIR" || exit
echo "nvim"

# zathura
wgen "$XDG_CONFIG_HOME"/zathura/genzathurarc "$XDG_CONFIG_HOME"/zathura/zathurarc &
echo "zathura"

# css
DIR=$(pwd)
cd ~/dotfiles/misc/css || exit
wgen gendiscord.css discord.css &
wgen gengithub.css github.css &
wgen genelement.css element.css &
cd "$DIR" || exit
echo "css"

# qutebrowser
wgen "$XDG_CONFIG_HOME"/qutebrowser/genconfig.py "$XDG_CONFIG_HOME"/qutebrowser/config.py &
echo "qutebrowser"

# wallpaper
convert -size 3840x2400 xc:"$(wq background)" /tmp/wp.png
pkill swaybg
swaybg -m fill -i /tmp/wp.png &
echo "wallpaper"


# no longer in use but here if needed
# alias xgen="~/Desktop/xgen/xgen-nonposix"
#
#
# # alacritty
# wgen "$XDG_CONFIG_HOME"/alacritty/genalacritty.yml "$XDG_CONFIG_HOME"/alacritty/alacritty.yml &
# echo "alacritty"
#
# # picom
# xgen "$XDG_CONFIG_HOME"/picom/genpicom.conf "$XDG_CONFIG_HOME"/picom/picom.conf
# pkill picom && picom &
#
# # bspwm
# exec "$XDG_CONFIG_HOME"/bspwm/scripts/setborders.sh &
# echo "bspwm" echo "picom"
#
# # polybar
# xgen "$XDG_CONFIG_HOME"/polybar/genconfig "$XDG_CONFIG_HOME"/polybar/config
# if [[ $(cat /proc/sys/kernel/hostname) == "thonkpad" ]]; then
#     killall -q polybar
#     polybar laptop -q &
# elif [[ $(cat /proc/sys/kernel/hostname) == "archbtw" ]]; then
#     killall -q polybar
#     polybar desktop -q &
# fi
# echo "polybar"
#
# # rofi
# xgen "$XDG_CONFIG_HOME"/rofi/genconfig.rasi "$XDG_CONFIG_HOME"/rofi/config.rasi &
# echo "rofi"
#
# # emacs
# ~/Desktop/xgen/old/xgen ~/dotfiles/.config/.doom.d/manual/gendoom-xresources-theme.el ~/dotfiles/.config/.doom.d/manual/doom-xresources-theme.el
# echo "emacs"
#
# # wallpaper
# XRES=$(xrandr | grep \* | awk '{print $1}')
# convert -size $XRES xc:$(xquery background) /tmp/wp.png
# xwallpaper --zoom /tmp/wp.png
# echo "wallpaper"
