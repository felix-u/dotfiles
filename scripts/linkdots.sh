#!/usr/bin/env sh

# save current working directory
CWD=$(pwd)

# get dotfiles directory
cd "$HOME/dotfiles" || exit

# symlink configs
stow --restow --target="$XDG_CONFIG_HOME" .config

# firefox config
if [ -d "$HOME"/.mozilla/firefox/ ]; then
    for dir in "$HOME"/.mozilla/firefox/*.default*; do
        stow --restow --target="$dir" firefox
    done
fi

# symlink some other stuff that is "naked" in home dir
stow --restow --target="$HOME" home


# return to initial dir
cd "$CWD" || exit
