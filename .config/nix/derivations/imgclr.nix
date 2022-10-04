{ stdenv, fetchgit }:

let
    pkgs = import <nixpkgs> { };
in

stdenv.mkDerivation rec {
    pname = "imgclr";
    version = "0.1";

    src = fetchgit {
        url = "https://github.com/felix-u/imgclr";
        sha256 = "sha256-bMA+TGoQm2j10F3LgF8zzRufEC7fnUZr7US0jNzVQaI=";
        rev = "92e4c383e970e3c8007b4854c7964b6e199004cc";
    };

    buildPhase = ''
        make release
    '';

    installPhase = ''
        mkdir -p $out/bin
        cp ./imgclr $out/bin/
    '';
}
