  let
    pkgs = import <nixpkgs> { };

    shgen-bin = pkgs.fetchurl {
      url = "https://github.com/felix-u/shgen/archive/refs/heads/master.zip";
      sha256 = "716c533da9b34d0484ef15e4010340cc42538255f760d7e8bc1dcb9afd23a6da";
    };
  in
    pkgs.runCommand "shgen" {}
      ''
        #!${pkgs.stdenv.shell}
        ${pkgs.unzip}/bin/unzip ${shgen-bin}
        mkdir -p $out/bin
        cp shgen-master/shgen $out/bin/
        chmod +x $out/bin/*
      ''
