let
  pkgs = import <nixpkgs> { };
in
{rustPlatform}:
rustPlatform.buildRustPackage {
    pname = "imgclr";
    version = "0.pre";
    src = pkgs.fetchFromGitHub {
        owner = "felix-u";
        repo = "imgclr";
        rev = "9830c55cbb9154734c99aad70127319c51e2cfe9";
        sha256 = "sha256-7L2RkqJt7BPQz99jJ3PpOkifKO4KmVK34ljMTo8D7IA=";
    };
    cargoSha256 = "sha256-Jh/0JyccqslFvGuxKru3V4ZwCC2V547Pk+av3FIr8V0=";
}
