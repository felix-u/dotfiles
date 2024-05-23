let
  pkgs = import <nixpkgs> { };

  rev = "0.12.0-dev.3180+83e578a18";

  zig-bin = builtins.fetchurl {
    url = "https://pkg.machengine.org/zig/zig-linux-x86_64-${rev}.tar.xz";
  };
in
pkgs.runCommand "zig" { }
  ''
    #!${pkgs.stdenv.shell}
    ${pkgs.gnutar}/bin/tar xf ${zig-bin}
    mkdir -p $out/bin
    mv zig-linux-x86_64-${rev}/zig $out/bin/zigmach
    cp -r zig-linux-x86_64-${rev}/*  $out/bin/
    chmod +x $out/bin/zigmach
  ''
