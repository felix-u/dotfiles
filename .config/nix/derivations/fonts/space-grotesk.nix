let
  pkgs = import <nixpkgs> { };

  rev = "03507d024a01282884232081fc6011c09ff4e849";

  space-grotesk-bin = pkgs.fetchurl {
    url = "https://github.com/floriankarsten/space-grotesk/archive/${rev}.zip";
    sha256 = "sha256-tK0Lk+9ap92JV+pyeaRgwh4Q5d4Yl1KrmFjjzTRMag4=";
  };
in
pkgs.runCommand "space-grotesk" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${space-grotesk-bin}
    mv space-grotesk-${rev}/fonts/otf/*.otf $out/share/fonts/opentype
  ''
