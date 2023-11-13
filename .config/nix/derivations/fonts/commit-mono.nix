let
  pkgs = import <nixpkgs> { };

  rev = "d675d62d8fc4bf7882ddb6bb63300a1036ec9816";

  commit-mono-bin = pkgs.fetchurl {
    url = "https://github.com/felix-u/commit-mono/archive/${rev}.zip";
    sha256 = "sha256-QK0P0npHBBjtSqTl+mtlu4PXsvgpBYL0aKtXvlbktyQ=";
  };
in
pkgs.runCommand "commit-mono" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${commit-mono-bin}
    mv commit-mono-${rev}/*.otf $out/share/fonts/opentype
  ''
