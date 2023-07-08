#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin
mkdir -p "$install_dir"

pkg_name="imgclr"
pkg_ver="v0.1"
pkg_arch="x86_64"
bin="$pkg_name-$pkg_ver-$pkg_arch-linux"

wget "https://github.com/felix-u/$pkg_name/releases/download/$pkg_ver/$bin"

install -m 755 "$bin" "$install_dir"/"$pkg_name"

rm "$bin"
