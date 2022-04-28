  let
    pkgs = import <nixpkgs> { };

    themesh-bin = pkgs.fetchurl {
      url = "https://git.io/JM70M";
      sha256 = "25cae4e35944c03b04301c6e75dfdb95077a0c10bc80970891578af496f68644";
    };
  in
  pkgs.runCommand "themesh" {}
      ''
        #!${pkgs.stdenv.shell}
        mkdir -p $out/bin
        cp ${themesh-bin} $out/bin/theme.sh
        chmod +x $out/bin/*
      ''
