let
  pkgs = import <nixpkgs> { };

  medio-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/medio_0200.zip";
    sha256 = "sha256:157hpygn33w9kj5x5jbr60ki2p36gihc127dcfc6vwd4gw6635qm";
  };
in
pkgs.runCommand "medio" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${medio-bin}
    mv *.otf $out/share/fonts/opentype
  ''
