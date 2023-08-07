#!/usr/bin/env sh

NIXDIR="$XDG_CONFIG_HOME/nix/config"

# darktable
shgen "$XDG_CONFIG_HOME"/darktable/genuser.css "$XDG_CONFIG_HOME"/darktable/user.css &

# imv
shgen "$XDG_CONFIG_HOME"/imv/genconfig "$XDG_CONFIG_HOME"/imv/config &

# waybar
shgen "$DOTSCONF"/waybar/genstyle.css "$DOTSCONF"/waybar/style.css &
