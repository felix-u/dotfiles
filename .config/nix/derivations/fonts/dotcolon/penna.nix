let
  pkgs = import <nixpkgs> { };

  penna-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/penna_0100.zip";
    sha256 = "sha256:1vg9j19sj2z86185la7gv3r33f97jr4m9kn6lvp27qvhvp56payl";
  };
in
pkgs.runCommand "penna" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${penna-bin}
    mv *.otf $out/share/fonts/opentype
  ''
