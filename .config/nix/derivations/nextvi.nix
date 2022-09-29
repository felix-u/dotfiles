{ stdenv, fetchgit }:

let
    pkgs = import <nixpkgs> { };
in

stdenv.mkDerivation rec {
    pname = "nextvi";
    version = "2022";

    src = fetchgit {
        url = "https://github.com/kyx0r/nextvi";
        sha256 = "sha256-GqTN6kbBVksh1uXUTGs2unyl2GXlz+Vogqr0kLcIgGs=";
        rev = "5ccc9f3a18be9d78a72a91c432ab3aac50c69647";
    };

    buildPhase = ''
        ./build.sh
    '';

    installPhase = ''
        mkdir -p $out/bin
        cp ./vi $out/bin/
    '';
}
