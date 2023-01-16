# Compilation errors :((((

with import <nixpkgs> {};

rustPlatform.buildRustPackage {

    pname = "wl-screenrec";
    version = "0.1";

    src = pkgs.fetchFromGitHub {
        owner = "russelltg";
        repo = "wl-screenrec";
        rev = "ccfc6defe261a3c78b83fa1b7fab9bf620b68356";
        sha256 = "sha256-HgluUHe/mME3oHatGCJpsccpM6/1LhDFQ/PSVVyEgOk=";
    };

    cargoSha256 = "sha256-DcRrOeFIaAXgFnnbvoK/K4TFa2qMJUiy39d/hZdmGpY=";

    LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";
    buildInputs = with pkgs; [ ffmpeg-full ];
    nativeBuildInputs = with pkgs; [ pkg-config ffmpeg clang ];
}
