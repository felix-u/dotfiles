let
  pkgs = import <nixpkgs> { };

  rev = "c593a9cd9b6e13592ff09924e3947bffab5ea013";

  lora-bin = pkgs.fetchurl {
    url = "https://github.com/cyrealtype/Lora-Cyrillic/archive/${rev}.zip";
    sha256 = "sha256-plxajkdFt5aC2tqYi5Tjp2vX1oeWV4ncj42XeK+qRp4=";
  };
in
pkgs.runCommand "lora" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${lora-bin}
      mv Lora-Cyrillic-${rev}/fonts/otf/*.otf $out/share/fonts/opentype
    ''
