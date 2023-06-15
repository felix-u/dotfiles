let
  pkgs = import <nixpkgs> { };

  nacelle-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/nacelle_100.zip";
    sha256 = "sha256:0ra018s5q80bvizaq8gfpq1mhynlqn7g832jghd4wp5i48ggq8af";
  };
in
pkgs.runCommand "nacelle" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${nacelle-bin}
    mv *.otf $out/share/fonts/opentype
  ''
