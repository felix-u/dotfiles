let
  pkgs = import <nixpkgs> { };

  imgclr-bin = pkgs.fetchurl {
    url = "https://github.com/felix-u/imgclr/releases/download/v0.2/imgclr-v0.2-x86_64-linux";
    sha256 = "sha256-guebyzpmWs0r9jYSD8TN4R9h/ZEAZibI7pLD3ZKAHqA=";
  };
in
pkgs.runCommand "imgclr" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/bin
    cp ${imgclr-bin} $out/bin/imgclr
    chmod +x $out/bin/*
  ''
