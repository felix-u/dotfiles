let
  pkgs = import <nixpkgs> { };

  rev = "0.13.0-dev.245+5fe9f88b1";

  zig-bin = builtins.fetchurl {
    url = "https://ziglang.org/builds/zig-linux-x86_64-${rev}.tar.xz";
  };
in
pkgs.runCommand "zig" { }
  ''
    #!${pkgs.stdenv.shell}
    ${pkgs.gnutar}/bin/tar xf ${zig-bin}
    mkdir -p $out/bin
    cp -r zig-linux-x86_64-${rev}/*  $out/bin/
    chmod +x $out/bin/zig
  ''
