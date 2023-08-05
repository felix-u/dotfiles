#!/usr/bin/env sh

# fontconfig
shgen "$DOTSCONF"/fontconfig/genfonts.conf "$DOTSCONF"/fontconfig/fonts.conf &

# foot
shgen "$DOTSCONF"/foot/gencolours.ini "$DOTSCONF"/foot/colours.ini &

# darktable
shgen "$XDG_CONFIG_HOME"/darktable/genuser.css "$XDG_CONFIG_HOME"/darktable/user.css &

# firefox
shgen ~/dotfiles/firefox/chrome/genuserChrome.css ~/dotfiles/firefox/chrome/userChrome.css &

# imv
shgen "$XDG_CONFIG_HOME"/imv/genconfig "$XDG_CONFIG_HOME"/imv/config &

# waybar
shgen "$DOTSCONF"/waybar/genstyle.css "$DOTSCONF"/waybar/style.css &

# zathura
shgen "$DOTSCONF"/zathura/genzathurarc "$DOTSCONF"/zathura/zathurarc &
