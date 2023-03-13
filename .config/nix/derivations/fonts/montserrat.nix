let
  pkgs = import <nixpkgs> { };

  rev = "b54c0978c3fd840c9ccd864600608e593fffee5a";

  montserrat-bin = pkgs.fetchurl {
    url = "https://github.com/JulietaUla/Montserrat/archive/${rev}.zip";
    sha256 = "sha256-skj6AaEmxBUrImLQ2afzO0tAPOfuHoa64C5iTLva8RY=";
  };
in
pkgs.runCommand "montserrat" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${montserrat-bin}
      mv Montserrat-${rev}/fonts/otf/*.otf $out/share/fonts/opentype
    ''
