let
  pkgs = import <nixpkgs> { };

  fragment-mono-bin = pkgs.fetchurl {
    url = "https://github.com/weiweihuanghuang/fragment-mono/releases/download/1.011/fragment-mono-fonts.zip";
    sha256 = "sha256-XmDAx2he0okzRfpyOSwm/gflOtEVfd3u/tw5kdInVdA=";
  };
in
pkgs.runCommand "fragment-mono" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/truetype
    ${pkgs.unzip}/bin/unzip ${fragment-mono-bin}
    mv fragment-mono-fonts/fonts/ttf/*.ttf $out/share/fonts/truetype
  ''
