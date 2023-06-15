let
  pkgs = import <nixpkgs> { };

  dmenu-wl-bin = pkgs.fetchurl {
    url = "https://github.com/nyyManni/dmenu-wayland/archive/304c8e917651ee02b16ebf0b7097a5c53fa2236b.zip";
    sha256 = "52771212147c14ea7ddca0b4c1d4dc1a90cadbde6d044119e126c78bbecc8f90";
  };
in
pkgs.runCommand "dmenu-wl" { }
  ''
    #!${pkgs.stdenv.shell}
    ${pkgs.unzip}/bin/unzip ${dmenu-wl-bin}
    mkdir -p $out/bin
    cp dmenu-wayland-304c8e917651ee02b16ebf0b7097a5c53fa2236b/dmenu-wl_run $out/bin/dmenu-wl_run
    chmod +x $out/bin/*
  ''
