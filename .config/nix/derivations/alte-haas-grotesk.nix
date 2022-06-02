let
  pkgs = import <nixpkgs> { };

  alte-haas-grotesk-bin = pkgs.fetchurl {
    url = "http://dl.1001fonts.com/download/alte-haas-grotesk.zip";
    sha256 = "e87ce5f6b722e6b5a6ffd8d853bad836c2069037a9cd9ab66ddccb92ef37e1e8";
  };
in
pkgs.runCommand "alte-haas-grotesk" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts
      ${pkgs.unzip}/bin/unzip ${alte-haas-grotesk-bin} -d $out/share/fonts/truetype
    ''
