{ stdenv, fetchgit }:

let
    pkgs = import <nixpkgs> { };
in

stdenv.mkDerivation rec {
    pname = "imgclr";
    version = "0.1";

    src = fetchgit {
        url = "https://github.com/felix-u/imgclr";
        sha256 = "sha256-kqM/3S0XQnULz4XBvVPkNnNaQ8utWljNwL/PBWGlt0U=";
        rev = "401dc2920f397eb83a08fe9343a3736805e858b4";
    };

    buildPhase = ''
        make release
    '';

    installPhase = ''
        mkdir -p $out/bin
        cp ./imgclr $out/bin/
    '';
}
