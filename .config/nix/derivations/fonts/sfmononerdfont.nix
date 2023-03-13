let
  pkgs = import <nixpkgs> { };

  rev = "a95ddff08857fd156d14d15b986a80ce3dd0f075";

  sf-mono-bin = pkgs.fetchurl {
    url = "https://github.com/epk/SF-Mono-Nerd-Font/archive/${rev}.zip";
    sha256 = "sha256-hHffgOMgvCLBjz49ZKQs8soz7bY8aNL2tFo4SasAAzE=";
  };
in
pkgs.runCommand "sf-mono" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${sf-mono-bin}
      mv SF-Mono-Nerd-Font-${rev}/*.otf $out/share/fonts/opentype
    ''
