let
  pkgs = import <nixpkgs> { };

  metropolis-bin = builtins.fetchurl {
    url = "https://github.com/dw5/Metropolis/archive/refs/heads/master.zip";
  };
in
pkgs.runCommand "metropolis" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${metropolis-bin}
      mv Metropolis-master/Fonts/OpenType/*.otf $out/share/fonts/opentype
    ''
