#!/usr/bin/env sh

# save current working directory
CWD=$(pwd)

# get dotfiles directory
cd "$HOME/dotfiles" || exit

# symlink configs
stow --restow --target="$XDG_CONFIG_HOME" .config

# symlink some other stuff that is "naked" in home dir
stow --restow --target="$HOME" home

# return to initial dir
cd "$CWD" || exit
