let
  pkgs = import <nixpkgs> { };

  godot4-bin = pkgs.fetchurl {
    url = "https://downloads.tuxfamily.org/godotengine/4.0/alpha8/Godot_v4.0-alpha8_linux.64.zip";
    sha256 = "59f5ad39a0f1d13b6ed950c657fda8a6887df79e6f17c0b422c29e33b22c60ec";
  };
in
pkgs.runCommand "godot4" {}
    ''
      #!${pkgs.stdenv.shell}
      ${pkgs.unzip}/bin/unzip ${godot4-bin}
      mkdir -p $out/bin
      cp Godot_v4.0-alpha8_linux.64 $out/bin/godot4-bin
      printf "#\!/usr/bin/env sh\nsteam-run godot4-bin" > $out/bin/godot4-run
      chmod +x $out/bin/*
    ''
