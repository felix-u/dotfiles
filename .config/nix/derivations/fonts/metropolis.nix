let
  pkgs = import <nixpkgs> { };

  rev = "85fa62524e9eb40047bbf64830022e59a02cbaed";

  metropolis-bin = pkgs.fetchurl {
    url = "https://github.com/dw5/Metropolis/archive/${rev}.zip";
    sha256 = "sha256-vQMwru85LuL2m1kX2KwhfoXwEY1oiOdD0sDqS6W8IWo=";
  };
in
pkgs.runCommand "metropolis" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${metropolis-bin}
      mv Metropolis-${rev}/Fonts/OpenType/*.otf $out/share/fonts/opentype
    ''
