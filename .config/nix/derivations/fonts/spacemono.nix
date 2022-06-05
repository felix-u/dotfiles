let
  pkgs = import <nixpkgs> { };

  spacemono-bin = builtins.fetchurl {
    url = "https://github.com/googlefonts/spacemono/archive/refs/heads/main.zip";
  };
in
pkgs.runCommand "spacemono" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/truetype
      ${pkgs.unzip}/bin/unzip ${spacemono-bin}
      mv spacemono-main/fonts/*.ttf $out/share/fonts/truetype
    ''
