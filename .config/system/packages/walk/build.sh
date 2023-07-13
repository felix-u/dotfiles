#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin/
mkdir -p "$install_dir"
srcdir="walk"

git clone --depth 1 "https://github.com/google/$srcdir"

cd walk

make

install -m 755 ./walk "$install_dir"/walk
install -m 755 ./sor "$install_dir"/sor

cd ..

rm -rf "$srcdir"
