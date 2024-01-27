let
  pkgs = import <nixpkgs> { };

  truly-bin = pkgs.fetchurl {
    url = "https://github.com/felix-u/truly/releases/download/v1.0/truly";
    sha256 = "sha256-e9aHQ1yPN/9UZ9dg6afchCiNSTg55IKFMMTsBQPHce0=";
  };
in
pkgs.runCommand "truly" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/bin
    cp ${truly-bin} $out/bin/truly
    chmod +x $out/bin/*
  ''
