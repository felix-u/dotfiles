let
  pkgs = import <nixpkgs> { };

  beteckna-bin = pkgs.fetchurl {
    url = "https://github.com/jeffmcneill/beteckna/archive/refs/tags/v0.1.zip";
    sha256 = "e427050139a0b436b2366bc97c0d064c215aea8646de7c7b8e8785c849fe118a";
  };
in
pkgs.runCommand "beteckna" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/truetype
      ${pkgs.unzip}/bin/unzip ${beteckna-bin}
      mv beteckna-0.1/geometric-sans/*.ttf $out/share/fonts/truetype
    ''
