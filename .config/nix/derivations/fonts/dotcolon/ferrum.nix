let
  pkgs = import <nixpkgs> { };

  ferrum-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/ferrum_0200.zip";
    sha256 = "sha256:0v60fpkvyh2v2n33mrijnzr551x913bqfjwbiccy2mx07c4w5p26";
  };
in
pkgs.runCommand "ferrum" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${ferrum-bin}
    mv *.otf $out/share/fonts/opentype
  ''
