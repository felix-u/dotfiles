let
  pkgs = import <nixpkgs> { };

  bebas-neue-bin = pkgs.fetchurl {
    url = "https://dl.dafont.com/dl/?f=bebas_neue";
    sha256 = "sha256-Tgf3A7S7CAZhMnTRHHroYfZWZZMYSY7wtGjbr5Sj4hM=";
  };
in
pkgs.runCommand "bebas-neue" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/truetype
    ${pkgs.unzip}/bin/unzip ${bebas-neue-bin}
    mv *.ttf $out/share/fonts/truetype
  ''
