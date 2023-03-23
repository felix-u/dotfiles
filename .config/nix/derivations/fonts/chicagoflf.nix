let
  pkgs = import <nixpkgs> { };

  chicagoflf-bin = pkgs.fetchurl {
    url = "https://fontsarena.com/wp-content/uploads/2018/08/ChicagoFLF.ttf.zip";
    sha256 = "sha256-WZKU22CBnBQ2FaM8BX7a8iFtt6T+K2GN+IOFUHH4BeQ=";
  };
in
pkgs.runCommand "chicagoflf" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/truetype
      ${pkgs.unzip}/bin/unzip ${chicagoflf-bin}
      mv ChicagoFLF.ttf $out/share/fonts/truetype/
    ''
