let
  pkgs = import <nixpkgs> { };

  rev = "0.12.0-dev.3142+9d500bda2";

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
