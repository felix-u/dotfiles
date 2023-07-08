#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin
mkdir -p "$install_dir"

pkg_name="shgen"
pkg_ver="master"
archive="$pkg_ver.zip"
pkg_dir="shgen-$pkg_ver"

wget "https://github.com/felix-u/$pkg_name/archive/refs/heads/$archive"
unzip "$archive"

install -m 755 "$pkg_dir/$pkg_name" "$install_dir"/"$pkg_name"

rm -rf "$archive" "$pkg_dir"
