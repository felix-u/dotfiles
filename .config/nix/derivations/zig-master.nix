let
  pkgs = import <nixpkgs> { };

  zig-bin = builtins.fetchurl {
    url = "https://ziglang.org/builds/zig-linux-x86_64-0.10.0-dev.4418+99c3578f6.tar.xz";
  };
in
pkgs.runCommand "zig" {}
    ''
      #!${pkgs.stdenv.shell}
      ${pkgs.gnutar}/bin/tar xf ${zig-bin}
      mkdir -p $out/bin
      cp -r zig-linux-x86_64-0.10.0-dev.4418+99c3578f6/*  $out/bin/
      chmod +x $out/bin/zig
    ''
