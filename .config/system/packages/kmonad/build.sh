#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin/
mkdir -p "$install_dir"

pkg_ver="0.4.1"
pkg_bin="kmonad-$pkg_ver-linux"

wget "https://github.com/kmonad/kmonad/releases/download/$pkg_ver/$pkg_bin"

install -m 755 "$pkg_bin" "$install_dir"/kmonad

rm "$pkg_bin"
