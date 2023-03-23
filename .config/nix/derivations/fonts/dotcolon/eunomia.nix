let
  pkgs = import <nixpkgs> { };

  eunomia-bin = builtins.fetchurl {
    url = "https://dotcolon.net/download/fonts/eunomia_0200.zip";
    sha256 = "sha256:1r4ynrzrwakhdig6wvknyq1yarlw1dcdw501gm9xgkwj4l78lwf1";
  };
in
pkgs.runCommand "eunomia" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${eunomia-bin}
      mv *.otf $out/share/fonts/opentype
    ''
