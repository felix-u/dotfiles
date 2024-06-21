#!/usr/bin/env sh

current_dir=$(pwd)

mkdir -p "$HOME"/.config

cd "$HOME"/dotfiles/link || exit
stow --restow --target="$HOME" home

cd "$current_dir"
