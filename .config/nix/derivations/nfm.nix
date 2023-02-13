{ stdenv, fetchgit }:

let
    pkgs-unstable = import <nixos-unstable> { };
in
stdenv.mkDerivation rec {
    pname = "nfm";
    version = "master";

    src = fetchgit {
        url = "https://git.sr.ht/~leon_plickat/nfm";
        sha256 = "sha256-CB5mE3eZkxBKCPQDdU77liaEoSi673d54diL2o8k/N8=";
        rev = "50a0a21017af7e431d4024a4517374c588fda196";
        fetchSubmodules = true;
    };

    nativeBuildInputs = [ pkgs-unstable.zig ];

    dontConfigure = true;
    preBuild = ''
        export HOME=$TMPDIR
        export XDG_CACHE_HOME=$TMPDIR
    '';

    installPhase = ''
        runHook preInstall
        mkdir -p $out/bin
        zig build -Drelease-safe=true
        mv zig-out/bin/nfm $out/bin
        runHook postInstall
    '';
}
