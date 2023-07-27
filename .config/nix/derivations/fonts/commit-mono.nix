let
  pkgs = import <nixpkgs> { };

  rev = "bc83e6dee609780659051469c72fba188be9aca5";

  commit-mono-bin = pkgs.fetchurl {
    url = "https://github.com/felix-u/commit-mono/archive/${rev}.zip";
    sha256 = "sha256-Y3GvkU6qZpGszI4aTpXlQzAnvb1sn74hlDHjenDtHqw=";
  };
in
pkgs.runCommand "commit-mono" { }
  ''
    #!${pkgs.stdenv.shell}
    mkdir -p $out/share/fonts/opentype
    ${pkgs.unzip}/bin/unzip ${commit-mono-bin}
    mv commit-mono-${rev}/*.otf $out/share/fonts/opentype
  ''
