let
  pkgs = import <nixpkgs> { };

  poop-bin = pkgs.fetchurl {
    url = "https://github.com/andrewrk/poop/releases/download/0.2.0/x86_64-linux-poop";
    sha256 = "0d83878e108844fb30eb5ebae9d34d828dd93f4b9bcdd193f200f7291b1cd5bb";
  };
in
pkgs.runCommand "poop" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/bin
    cp ${poop-bin} $out/bin/poop
    chmod +x $out/bin/*
  ''
