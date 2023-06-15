let
  pkgs = import <nixpkgs> { };

  seshat-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/seshat_0100.zip";
    sha256 = "sha256:03qlq4h993g4c8djfa47ms234zxq4dzx6vrvqqh1fpvmwvhrvnfa";
  };
in
pkgs.runCommand "seshat" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${seshat-bin}
    mv *.otf $out/share/fonts/opentype
  ''
