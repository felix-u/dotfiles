  let
    pkgs = import <nixpkgs> { };

    nvim-bin = pkgs.fetchurl {
      url = "https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.tar.gz";
      sha256 = "9bd79c32bbbb9e87cbd88bad319034bead2626f5b168fce1027e29673c44c035";
    };
  in
  pkgs.runCommand "nvim" {}
      ''
        #!${pkgs.stdenv.shell}
        ${pkgs.gnutar}/bin/tar xzvf ${nvim-bin}
        mkdir -p $out/bin
        cp nvim-linux64/bin/nvim $out/bin/nvim-bin
        printf "#\!/usr/bin/env sh\nsteam-run nvim-bin" > $out/bin/nvim
        chmod +x $out/bin/*
      ''
