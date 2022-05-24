  let
    pkgs = import <nixpkgs> { };

    shgen-bin = builtins.fetchurl {
      url = "https://github.com/felix-u/shgen/archive/refs/heads/master.zip";
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
