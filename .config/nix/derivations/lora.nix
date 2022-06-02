let
  pkgs = import <nixpkgs> { };

  lora-bin = builtins.fetchurl {
    url = "https://github.com/cyrealtype/Lora-Cyrillic/archive/refs/heads/master.zip";
  };
in
pkgs.runCommand "lora" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/share/fonts/opentype
      ${pkgs.unzip}/bin/unzip ${lora-bin}
      mv Lora-Cyrillic-master/fonts/otf/*.otf $out/share/fonts/opentype
    ''
