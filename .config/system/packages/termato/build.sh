#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin
mkdir -p "$install_dir"

pkg_name="termato"
pkg_ver="0.4"
archive="v$pkg_ver.tar.gz"
src_dir="$pkg_name-$pkg_ver"

wget "https://github.com/felix-u/$pkg_name/archive/refs/tags/$archive"
tar xfv "$archive"

cd "$src_dir"
make release NAME="$pkg_name"
cd ..

install -m 755 "$src_dir"/"$pkg_name" "$install_dir"/"$pkg_name"

rm -rf "$archive" "$src_dir"
