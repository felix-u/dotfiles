let
  pkgs = import <nixpkgs> { };

  zig-bin = builtins.fetchurl {
    url = "https://ziglang.org/builds/zig-linux-x86_64-0.11.0-dev.1390+74b72a766.tar.xz";
  };
in
pkgs.runCommand "zig" {}
    ''
      #!${pkgs.stdenv.shell}
      ${pkgs.gnutar}/bin/tar xf ${zig-bin}
      mkdir -p $out/bin
      cp -r zig-linux-x86_64-0.11.0-dev.1390+74b72a766/*  $out/bin/
      chmod +x $out/bin/zig
    ''
