let
  pkgs = import <nixpkgs> { };

  melete-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/melete_0200.zip";
    sha256 = "sha256:0qv3n5yk68hg25mcilpr90k30flmnvl6pbznbv11i8rixgllchzv";
  };
in
pkgs.runCommand "melete" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${melete-bin}
    mv *.otf $out/share/fonts/opentype
  ''
