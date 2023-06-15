{ pkgs ? import <nixos-unstable> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    cairo
    json_c
    meson
    pcre2
    pkg-config
    wayland
    wayland-protocols
    wlroots_0_16
    libxkbcommon
    libevdev
    pixman
  ];
}
