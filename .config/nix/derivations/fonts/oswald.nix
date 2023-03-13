let
  pkgs = import <nixpkgs> { };

  rev = "9dd0521c8c06dd24998fe5d9cd644dab9cbbacca";

  oswald-bin = pkgs.fetchurl {
    url = "https://github.com/vernnobile/OswaldFont/archive/${rev}.zip";
    sha256 = "sha256-Cm3xzxe37VKbYZC2NDoHTbiNK46XwPvHtFW0ygB/hlc=";
  };
in
pkgs.runCommand "oswald" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/truetype
      ${pkgs.unzip}/bin/unzip ${oswald-bin}
      mv OswaldFont-${rev}/3.0/*/*/*.ttf $out/share/fonts/truetype
    ''
