let
  pkgs = import <nixpkgs> { };

  space-grotesk-bin = builtins.fetchurl {
    url = "https://github.com/floriankarsten/space-grotesk/archive/refs/heads/master.zip";
  };
in
pkgs.runCommand "space-grotesk" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${space-grotesk-bin}
      mv space-grotesk-master/fonts/otf/*.otf $out/share/fonts/opentype
    ''
