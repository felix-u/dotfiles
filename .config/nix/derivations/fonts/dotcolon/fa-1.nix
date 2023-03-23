let
  pkgs = import <nixpkgs> { };

  fa-1-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/fa_1_0100.zip";
    sha256 = "sha256:106nd01b3zl5h0za8w7vz5x30sx8js9bl3r6jkdv4dr4fhzx6fpq";
  };
in
pkgs.runCommand "fa-1" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${fa-1-bin}
      mv *.otf $out/share/fonts/opentype
    ''
