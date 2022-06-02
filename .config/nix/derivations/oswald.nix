let
  pkgs = import <nixpkgs> { };

  oswald-bin = builtins.fetchurl {
    url = "https://github.com/vernnobile/OswaldFont/archive/refs/heads/master.zip";
  };
in
pkgs.runCommand "oswald" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/truetype
      ${pkgs.unzip}/bin/unzip ${oswald-bin}
      mv OswaldFont-master/3.0/*/*/*.ttf $out/share/fonts/truetype
    ''
