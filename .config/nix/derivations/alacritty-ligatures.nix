let
  pkgs = import <nixpkgs> { };
in
{rustPlatform}:
rustPlatform.buildRustPackage {
    pname = "alacritty";
    version = "0.1";
    src = pkgs.fetchFromGitHub {
        owner = "zenixls2";
        repo = "alacritty";
        branch = "ligature"; # this doesn't work for now
        rev = "7291702f6b4fff10f2470f084abe0785b95659a0";
        sha256 = "sha256-fire7O6BIHiUNtCrG2gfsLMRbq+TzSj/ywRf7JBUwKY=";
    };
    cargoSha256 = "sha256-secSfiWVYcbKmhCmMdo9/g6LppGnztP9sXr4LgJVHm0=";
}
