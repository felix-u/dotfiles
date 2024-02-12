let
  pkgs = import <nixpkgs> { };

  rev = "0.12.0-dev.2063+804cee3b9";

  zig-bin = builtins.fetchurl {
    url = "https://ziglang.org/builds/zig-linux-x86_64-${rev}.tar.xz";
  };
in
pkgs.runCommand "zig" { }
  ''
    #!${pkgs.stdenv.shell}
    ${pkgs.gnutar}/bin/tar xf ${zig-bin}
    mkdir -p $out/bin
    cp zig-linux-x86_64-${rev}/zig $out/bin/zigmach
    cp -r zig-linux-x86_64-${rev}/*  $out/bin/
    chmod +x $out/bin/zigmach
  ''
