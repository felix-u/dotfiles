{ pkgs ? import <nixpkgs> { } }:

let
  version = "2022-05-12";
in
pkgs.stdenv.mkDerivation {
  name = "blinkenlights";

  src = pkgs.fetchurl {
    url = "https://justine.lol/blinkenlights/blinkenlights-${version}.com";
    sha256 = "sha256-2NXgmqcerXLTD6qSFnx58LjxB/w2JEMpQvJ82XOLn2k=";
  };

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/blinkenlights
    chmod +x $out/bin/blinkenlights
  '';
}
