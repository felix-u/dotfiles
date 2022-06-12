let
  pkgs = import <nixpkgs> { };

  sf-mono-bin = builtins.fetchurl {
    url = "https://github.com/epk/SF-Mono-Nerd-Font/archive/refs/heads/master.zip";
  };
in
pkgs.runCommand "sf-mono" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${sf-mono-bin}
      mv SF-Mono-Nerd-Font-master/*.otf $out/share/fonts/opentype
    ''
