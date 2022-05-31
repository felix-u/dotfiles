{ pkgs ? import <nixpkgs> {} }:
let
  raylib-wayland = pkgs.raylib.overrideAttrs (oldAttrs: rec {
    # buildInputs = with pkgs; (oldAttrs.buildInputs or []) ++
    #                          [ glfw-wayland wayland libxkbcommon ];
    buildInputs = with pkgs; [
      mesa libGLU glfw-wayland wayland libxkbcommon
      alsa-lib libpulseaudio
    ];

    cmakeFlags = (oldAttrs.cmakeFlags or []) ++
                 [ "-DCUSTOMIZE_BUILD=ON"
                   "-DGLFW_BUILD_WAYLAND=ON"
                   "-DGLFW_BUILD_X11=OFF"
                   "-DUSE_WAYLAND_DISPLAY=ON"
                   "-DINCLUDE_EVERYTHING=OFF"
                 ];
  });
in pkgs.mkShell {
  nativeBuildInputs = [ raylib-wayland ];
}
