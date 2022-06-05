let
  pkgs = import <nixpkgs> { };

  imgclr-bin = builtins.fetchurl {
    url = "https://github.com/felix-u/imgclr/archive/refs/heads/rust.zip";
  };
in
pkgs.runCommand "imgclr" {}
    ''
      #!${pkgs.stdenv.shell}
      ${pkgs.unzip}/bin/unzip ${imgclr-bin}
      cd imgclr-rust
      ${pkgs.cargo}/bin/cargo build --release
      mkdir -p $out/bin
      mv imgclr-rust/target/release/imgclr $out/bin
      chmod +x $out/bin/*
    ''
