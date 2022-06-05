let
  pkgs = import <nixpkgs> { };

  league-spartan-bin = builtins.fetchurl {
    url = "https://github.com/theleagueof/league-spartan/archive/refs/heads/master.zip";
  };
in
pkgs.runCommand "league-spartan" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${league-spartan-bin}
      mv league-spartan-master/fonts/otf/*.otf $out/share/fonts/opentype
    ''
