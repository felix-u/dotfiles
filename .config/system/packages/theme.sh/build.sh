#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin/
mkdir -p "$install_dir"

pkg_ver="1.1.5"
archive="v$pkg_ver.tar.gz"
pkg_dir="theme.sh-$pkg_ver"

wget "https://github.com/lemnos/theme.sh/archive/refs/tags/$archive"
tar xf "$archive"

install -m 755 "$pkg_dir"/bin/theme.sh "$install_dir"/

rm -rf "$archive" "$pkg_dir"
