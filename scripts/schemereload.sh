#!/usr/bin/env sh

# castor
shgen "$DOTSCONF"/castor/gensettings.toml "$DOTSCONF"/castor/settings.toml &

# fontconfig
shgen "$DOTSCONF"/fontconfig/genfonts.conf "$DOTSCONF"/fontconfig/fonts.conf &

# foot
shgen "$DOTSCONF"/foot/gencolours.ini "$DOTSCONF"/foot/colours.ini &

# darktable
shgen "$XDG_CONFIG_HOME"/darktable/genuser.css "$XDG_CONFIG_HOME"/darktable/user.css &

# firefox
shgen ~/dotfiles/firefox/chrome/genuserChrome.css ~/dotfiles/firefox/chrome/userChrome.css &

# gtk
shgen "$DOTSCONF"/gtk-3.0/gengtk.css "$DOTSCONF"/gtk-3.0/gtk.css &
shgen "$DOTSCONF"/gtk-4.0/gengtk.css "$DOTSCONF"/gtk-4.0/gtk.css &

# imv
shgen "$XDG_CONFIG_HOME"/imv/genconfig "$XDG_CONFIG_HOME"/imv/config &

# waybar
shgen "$DOTSCONF"/waybar/genstyle.css "$DOTSCONF"/waybar/style.css &

# zathura
shgen "$DOTSCONF"/zathura/genzathurarc "$DOTSCONF"/zathura/zathurarc &

# theming stuff
shgen ~/dotfiles/scripts/theme/genlighttheme ~/dotfiles/scripts/theme/lighttheme &
shgen ~/dotfiles/scripts/theme/gendarktheme ~/dotfiles/scripts/theme/darktheme &
