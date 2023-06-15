let
  pkgs = import <nixpkgs> { };

  rev = "235d3ac02bfa724aa7652b9b425c4de40263d063";

  playfair-bin = pkgs.fetchurl {
    url = "https://github.com/clauseggers/Playfair/archive/${rev}.zip";
    sha256 = "sha256-YmLBtd4bD29oeopk5pwx/Jawkq/WYAwb6C02ouVedLM=";
  };
in
pkgs.runCommand "playfair" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/truetype
    ${pkgs.unzip}/bin/unzip ${playfair-bin}
    mv Playfair-${rev}/fonts/VF-TTF/*.ttf $out/share/fonts/truetype
  ''
