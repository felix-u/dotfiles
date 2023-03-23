let
  pkgs = import <nixpkgs> { };

  tenderness-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/tenderness_0601.zip";
    sha256 = "sha256:14bq0crcmy0b2cfkhp3ac5yprprhmwc0kf4m6xiy6nnai8wi5mg9";
  };
in
pkgs.runCommand "tenderness" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${tenderness-bin}
      mv *.otf $out/share/fonts/opentype
    ''
