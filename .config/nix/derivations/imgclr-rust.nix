let
  pkgs = import <nixpkgs> { };
in
{ rustPlatform }:
rustPlatform.buildRustPackage {
  pname = "imgclr";
  version = "0.1";
  src = pkgs.fetchFromGitHub {
    owner = "felix-u";
    repo = "imgclr";
    rev = "e107cb51513f00fb2d237630eca4a7067451af09";
    sha256 = "sha256-QiT/GU+coaeSqGMatq76L1Fr4OMss8xhoHG402AowNM=";
  };
  cargoSha256 = "sha256-3rd4WuktomiC8QNEWus1nupqU69gjpJYhf0hL2vSTNI=";
}
