#!/usr/bin/env sh
set -e

install_dir="$HOME"/.local/bin/
mkdir -p "$install_dir"

git clone https://gitlab.com/snakedye/kile.git

cd kile
git submodule update --init --recursive
cargo build --release

install -m 755 target/release/kile  "$install_dir"/kile

cd .. 
