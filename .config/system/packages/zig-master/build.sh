#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin/
mkdir -p "$install_dir"

pkg_ver="0.11.0-dev.3970+7a8002a5c"
pkg_arch="x86_64"
src_dir="zig-linux-$pkg_arch-$pkg_ver"
archive="$src_dir.tar.xz"

wget "https://ziglang.org/builds/$archive"
tar xfv "$archive"
zig_bin="$(realpath "$src_dir/zig")"

chmod +x "$zig_bin"
ln -s "$zig_bin" "$install_dir"

rm "$archive"
