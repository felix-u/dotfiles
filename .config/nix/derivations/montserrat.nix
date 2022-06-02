let
  pkgs = import <nixpkgs> { };

  montserrat-bin = builtins.fetchurl {
    url = "https://github.com/JulietaUla/Montserrat/archive/refs/heads/master.zip";
  };
in
pkgs.runCommand "montserrat" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${montserrat-bin}
      mv Montserrat-master/fonts/otf/*.otf $out/share/fonts/opentype
    ''
