#!/usr/bin/env sh

current_dir=$(pwd)

cd "$HOME"/dotfiles/link || exit
stow --restow --target="$HOME" home

cd "$current_dir"
