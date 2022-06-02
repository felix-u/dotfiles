let
  pkgs = import <nixpkgs> { };

  playfair-bin = builtins.fetchurl {
    url = "https://github.com/clauseggers/Playfair/archive/refs/heads/master.zip";
  };
in
pkgs.runCommand "playfair" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/truetype
      ${pkgs.unzip}/bin/unzip ${playfair-bin}
      mv Playfair-master/fonts/VF-TTF/*.ttf $out/share/fonts/truetype
    ''
