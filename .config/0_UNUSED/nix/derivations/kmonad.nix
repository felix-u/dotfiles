  let
    pkgs = import <nixpkgs> { };

    kmonad-bin = pkgs.fetchurl {
      url = "https://github.com/kmonad/kmonad/releases/download/0.4.1/kmonad-0.4.1-linux";
      # sha256 = "4545b0823dfcffe0c4f0613916a6f38a0ccead0fb828c837de54971708bafc0b";
      sha256 = "839e58e7cc23d6dd06846f343c0c9be0f927698189e497da35ef5196703f7a8f";
    };
  in
  pkgs.runCommand "kmonad" {}
      ''
        #!${pkgs.stdenv.shell}
        mkdir -p $out/bin
        cp ${kmonad-bin} $out/bin/kmonad
        chmod +x $out/bin/*
      ''
