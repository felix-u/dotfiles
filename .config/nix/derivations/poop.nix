let
  pkgs = import <nixpkgs> { };

  poop-bin = pkgs.fetchurl {
    url = "https://github.com/andrewrk/poop/releases/download/0.3.0/x86_64-linux-poop";
    sha256 = "sha256-5ohSOOIpEBjpNSqoBIpHUFIIgW6eMc97tPgRj54+8KA=";
  };
in
pkgs.runCommand "poop" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/bin
    cp ${poop-bin} $out/bin/poop
    chmod +x $out/bin/*
  ''
