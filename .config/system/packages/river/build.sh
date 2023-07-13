#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin
mkdir -p "$install_dir"

pkg_name="river"
pkg_ver="master"

wget "https://github.com/riverwm/${pkg_name}/archive/refs/heads/${pkg_ver}.zip"
unzip "$pkg_ver.zip"

cd "$pkg_name-$pkg_ver"
git submodule update --init --recursive
zig-0.10.1 build -Drelease-safe -Dxwayland --prefix ~/.local install
cd ..

rm -rf "$pkg_ver.zip" "$pkg_name-$pkg_ver"
