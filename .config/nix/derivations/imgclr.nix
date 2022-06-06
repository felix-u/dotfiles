let
  pkgs = import <nixpkgs> { };
in
{rustPlatform}:
rustPlatform.buildRustPackage {
    pname = "imgclr";
    version = "0.1";
    src = pkgs.fetchFromGitHub {
        owner = "felix-u";
        repo = "imgclr";
        rev = "0a261c64b4319567f13dde8e3b827ae19ee49334";
        sha256 = "sha256-01Fe7O6BIHiUNtCrG2gfsLMRbq+TzSj/ywRf7JBUwKY=";
    };
    cargoSha256 = "sha256-YkQSfiWVYcbKmhCmMdo9/g6LppGnztP9sXr4LgJVHm0=";
}
