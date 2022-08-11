let
  pkgs = import <nixpkgs> { };

  wasm4-bin = pkgs.fetchurl {
    url = "https://github.com/aduros/wasm4/releases/download/v2.5.1/w4-linux.zip";
    sha256 = "677efe4d8831553a9be7bb4ff9f0e360966519f457b144b6e6f49161bdc93e00";
  };
in
pkgs.runCommand "wasm4" {}
    ''
      #!${pkgs.stdenv.shell}
      ${pkgs.unzip}/bin/unzip ${wasm4-bin}
      mkdir -p $out/bin
      cp w4 $out/bin/
      # cp w4 $out/bin/wasm4-bin
      # printf "#\!${pkgs.bash}/bin/bash\nsteam-run wasm4-bin $@" > $out/bin/w4
      chmod +x $out/bin/*
    ''

#
# {
#   stdenv,
#   fetchzip,
#   autoPatchelfHook,
# }:
# stdenv.mkDerivation (finalAttrs: {
#   pname = "w4";
#   version = "2.5.1";
#
#   src = fetchzip {
#     url = "https://github.com/aduros/wasm4/releases/download/v${finalAttrs.version}/w4-linux.zip";
#     hash = "sha256-2fpsHM1792z00/eMRA4c9yH9i9ufTfM6+S3hJRh7oZs=";
#   };
#
#   nativeBuildInputs = [
#     autoPatchelfHook
#   ];
#
#   buildInputs = [
#     stdenv.cc.cc.lib
#   ];
#
#   installPhase = ''
#     install -Dm555 w4 $out/bin/w4
#   '';
# })
