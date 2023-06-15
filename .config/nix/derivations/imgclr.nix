let
  pkgs = import <nixpkgs> { };

  imgclr-bin = pkgs.fetchurl {
    url = "https://github.com/felix-u/imgclr/releases/download/v0.1/imgclr-v0.1-x86_64-linux";
    sha256 = "sha256-QgHn6ACqqzpXcNZPBfT4yZ97yTzQ2jeYMyhoe8rkQ88=";
  };
in
pkgs.runCommand "imgclr" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/bin
    cp ${imgclr-bin} $out/bin/imgclr
    chmod +x $out/bin/*
  ''
