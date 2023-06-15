let
  pkgs = import <nixpkgs> { };

  route159-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/route159_110.zip";
    sha256 = "sha256:1rdvf4yndb8q8f4py1a47lqfp36cyi77azyfbq4rh9pcjkxagkyh";
  };
in
pkgs.runCommand "route159" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${route159-bin}
    mv *.otf $out/share/fonts/opentype
  ''
