let
  pkgs = import <nixpkgs> { };

  rev = "27341b9bf93a2c7faa140538a64ce342486c5fb5";

  league-spartan-bin = pkgs.fetchurl {
    url = "https://github.com/theleagueof/league-spartan/archive/${rev}.zip";
    sha256 = "sha256-CBqlSB/NKIeNYQdErxs07bGitXIcmo9So48tKef8pNI=";
  };
in
pkgs.runCommand "league-spartan" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${league-spartan-bin}
      mv league-spartan-${rev}/fonts/otf/*.otf $out/share/fonts/opentype
    ''
