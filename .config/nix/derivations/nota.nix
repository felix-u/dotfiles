let
  pkgs = import <nixpkgs> { };

  nota-bin = pkgs.fetchurl {
    url = "https://github.com/felix-u/nota/releases/download/v0.3/nota-v0.3-x86_64-linux";
    sha256 = "sha256-6JSmZHikj1+cgeMUD5pTX02PvgDsHxkyGkT9xoncC8I=";
  };
in
pkgs.runCommand "nota" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/bin
    cp ${nota-bin} $out/bin/nota
    chmod +x $out/bin/*
  ''
