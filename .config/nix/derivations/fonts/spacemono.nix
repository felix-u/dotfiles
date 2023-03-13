let
  pkgs = import <nixpkgs> { };

  rev = "f5ebc1e1c0eca03079cb830f167695751d3e0353";

  spacemono-bin = pkgs.fetchurl {
    url = "https://github.com/googlefonts/spacemono/archive/${rev}.zip";
    sha256 = "sha256-NKVzj4o1d7TcroHepgt4CqqaxjcRi29Np4oH+lIPpMk=";
  };
in
pkgs.runCommand "spacemono" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/truetype
      ${pkgs.unzip}/bin/unzip ${spacemono-bin}
      mv spacemono-${rev}/fonts/*.ttf $out/share/fonts/truetype
    ''
