let
  pkgs = import <nixpkgs> { };

  rev = "1.132";

  commit-mono-bin = pkgs.fetchurl {
    url = "https://github.com/eigilnikolajsen/commit-mono/releases/download/${rev}/CommitMono-${rev}.zip";
    sha256 = "sha256-VyssKNTZk//nMjgw/KtSHkml23n+4YE96+W9vit8/Ns=";
  };
in
pkgs.runCommand "commit-mono" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${commit-mono-bin}
    mv *.otf $out/share/fonts/opentype
  ''
