let
  pkgs = import <nixpkgs> { };

  rev = "3934+ba6e5e65a";

  zig-bin = builtins.fetchurl {
    url = "https://ziglang.org/builds/zig-linux-x86_64-0.11.0-dev.${rev}.tar.xz";
  };
in
pkgs.runCommand "zig" { }
  ''
    #!${pkgs.stdenv.shell}
    ${pkgs.gnutar}/bin/tar xf ${zig-bin}
    mkdir -p $out/bin
    cp -r zig-linux-x86_64-0.11.0-dev.${rev}/*  $out/bin/
    chmod +x $out/bin/zig
  ''
