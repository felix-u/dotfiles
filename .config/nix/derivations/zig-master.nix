let
  pkgs = import <nixpkgs> { };

  zig-bin = builtins.fetchurl {
    url = "https://ziglang.org/builds/zig-linux-x86_64-0.11.0-dev.1836+28364166e.tar.xz";
  };
in
pkgs.runCommand "zig" {}
    ''
      #!${pkgs.stdenv.shell}
      ${pkgs.gnutar}/bin/tar xf ${zig-bin}
      mkdir -p $out/bin
      cp -r zig-linux-x86_64-0.11.0-dev.1836+28364166e/*  $out/bin/
      chmod +x $out/bin/zig
    ''
