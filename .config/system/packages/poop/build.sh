#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin/
mkdir -p "$install_dir"

pkg_ver="0.4.0"
pkg_arch="x86_64"
poop_bin="$pkg_arch-linux-poop"

wget "https://github.com/andrewrk/poop/releases/download/$pkg_ver/$poop_bin"

install -m 755 "$poop_bin" "$install_dir"/poop

rm "$poop_bin"
