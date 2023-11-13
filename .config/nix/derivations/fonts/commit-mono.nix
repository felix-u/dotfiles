let
  pkgs = import <nixpkgs> { };

  rev = "0c8c288c0d2d43f403b3b9ba216ef54a5fd7cced";

  commit-mono-bin = pkgs.fetchurl {
    url = "https://github.com/felix-u/commit-mono/archive/${rev}.zip";
    sha256 = "sha256-gbsFq8CZS+itvGbfb26vRdIb/ZUFnRMDiia2P8zktWw=";
  };
in
pkgs.runCommand "commit-mono" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${commit-mono-bin}
    mv commit-mono-${rev}/*.otf $out/share/fonts/opentype
  ''
