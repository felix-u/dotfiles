#!/bin/sh

# foot
shgen "$DOTSCONF"/foot/gencolours.ini "$DOTSCONF"/foot/colours.ini &
echo "foot"

# cava
shgen "$DOTSCONF"/cava/genconfig "$DOTSCONF"/cava/config
# pkill -USR1 cava # reloads whole config
pkill -USR2 cava # reloads colours only
echo "cava"

# darktable
shgen "$XDG_CONFIG_HOME"/darktable/genuser.css "$XDG_CONFIG_HOME"/darktable/user.css &
echo "darktable"

# dunst
shgen "$DOTSCONF"/dunst/gendunstrc "$DOTSCONF"/dunst/dunstrc
pkill dunst
dunst &
echo "dunst"

# firefox
shgen ~/dotfiles/firefox/chrome/genuserChrome.css ~/dotfiles/firefox/chrome/userChrome.css &
echo "firefox"

# nvim
DIR=$(pwd)
cd "$DOTSCONF"/nvim/colors/ || exit
exec ./schemescript.sh &
cd "$DIR" || exit
echo "nvim"

# zathura
shgen "$DOTSCONF"/zathura/genzathurarc "$DOTSCONF"/zathura/zathurarc &
echo "zathura"

# css
DIR=$(pwd)
cd ~/dotfiles/misc/css || exit
shgen gendiscord.css discord.css &
shgen gengithub.css github.css &
shgen genelement.css element.css &
cd "$DIR" || exit
echo "css"

# qutebrowser
shgen "$DOTSCONF"/qutebrowser/genconfig.py "$DOTSCONF"/qutebrowser/config.py &
shgen "$DOTSCONF"/qutebrowser/greasemonkey/gendarkreader.js.disabled \
     "$DOTSCONF"/qutebrowser/greasemonkey/darkreader.js &
shgen "$DOTSCONF"/qutebrowser/greasemonkey/gendiscordtheme.js.disabled \
     "$DOTSCONF"/qutebrowser/greasemonkey/discordtheme.js &
shgen "$DOTSCONF"/qutebrowser/greasemonkey/genelementtheme.js.disabled \
     "$DOTSCONF"/qutebrowser/greasemonkey/elementtheme.js &
echo "qutebrowser"

# waybar
shgen "$DOTSCONF"/waybar/genstyle.css "$DOTSCONF"/waybar/style.css &
echo "waybar"

# theming stuff
shgen ~/dotfiles/scripts/theme/genlighttheme ~/dotfiles/scripts/theme/lighttheme &
shgen ~/dotfiles/scripts/theme/gendarktheme ~/dotfiles/scripts/theme/darktheme &
echo "terminal theme files"

# wallpaper
pkill swaybg
swaybg -c "$(wq background)" &
echo "wallpaper"


# no longer in use but here if needed
# alias xgen="~/Desktop/xgen/xgen-nonposix"
#
#
# # alacritty
# shgen "$DOTSCONF"/alacritty/genalacritty.yml "$DOTSCONF"/alacritty/alacritty.yml &
# echo "alacritty"
#
# # picom
# xgen "$DOTSCONF"/picom/genpicom.conf "$DOTSCONF"/picom/picom.conf
# pkill picom && picom &
#
# # bspwm
# exec "$DOTSCONF"/bspwm/scripts/setborders.sh &
# echo "bspwm" echo "picom"
#
# # polybar
# xgen "$DOTSCONF"/polybar/genconfig "$DOTSCONF"/polybar/config
# if [[ $(cat /proc/sys/kernel/hostname) == "thonkpad" ]]; then
#     killall -q polybar
#     polybar laptop -q &
# elif [[ $(cat /proc/sys/kernel/hostname) == "nixbtw" ]]; then
#     killall -q polybar
#     polybar desktop -q &
# fi
# echo "polybar"
#
# # rofi
# xgen "$DOTSCONF"/rofi/genconfig.rasi "$DOTSCONF"/rofi/config.rasi &
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
