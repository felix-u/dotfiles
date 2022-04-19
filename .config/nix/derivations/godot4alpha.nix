# NOT WORKING!!!

let
  pkgs = import <nixpkgs> { };

  godot4-bin = pkgs.fetchurl {
    url = "https://downloads.tuxfamily.org/godotengine/4.0/alpha6/Godot_v4.0-alpha6_linux.64.zip";
    sha256 = "8dbf10157425c49a5d4d98dd6aab5f44f175571223faa831a334498867b38124";
  };
in
pkgs.runCommand "godot4" {}
    ''
      #!${pkgs.stdenv.shell}
      ${pkgs.unzip}/bin/unzip ${godot4-bin}
      mkdir -p $out/bin
      cp Godot_v4.0-alpha6_linux.64 $out/bin/godot
      chmod +x $out/bin/*
    ''
