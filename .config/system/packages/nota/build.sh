#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin/
mkdir -p "$install_dir"

pkg_ver="v0.3"
pkg_arch="x86_64"
bin="nota-$pkg_ver-$pkg_arch-linux"

wget "https://github.com/felix-u/nota/releases/download/$pkg_ver/$bin"

install -m 755 "$bin" "$install_dir"/nota

rm "$bin"
