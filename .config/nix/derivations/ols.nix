let
  pkgs = import <nixpkgs> { };

  ols-bin = builtins.fetchurl {
    url = "https://github.com/DanielGavin/ols/releases/download/nightly/ols-x86_64-unknown-linux-gnu.zip";
  };
in
pkgs.runCommand "ols" {}
    ''
      #!${pkgs.stdenv.shell}
      ${pkgs.unzip}/bin/unzip ${ols-bin}
      mkdir -p $out/bin
      cp -r * $out/bin/
      printf "#\!/usr/bin/env sh\nsteam-run ols-x86_64-unknown-linux-gnu" > $out/bin/ols
      chmod +x $out/bin/*
    ''
