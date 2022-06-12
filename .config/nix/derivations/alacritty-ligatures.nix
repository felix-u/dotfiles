let
    pkgs = import <nixpkgs> { };
    rpathLibs = with pkgs; [
        expat fontconfig freetype libGL xorg.libX11 xorg.libXcursor xorg.libXi
        xorg.libXrandr xorg.libXxf86vm xorg.libxcb libxkbcommon wayland
        egl-wayland
    ];
in
{stdenv, lib, rustPlatform}:
rustPlatform.buildRustPackage rec {
    pname = "alacritty";
    version = "0.1";
    src = pkgs.fetchFromGitHub {
        owner = "zenixls2";
        repo = "alacritty";
        rev = "7291702f6b4fff10f2470f084abe0785b95659a0";
        sha256 = "sha256-hosAMy4/iSP/Q6ce8ueROKHPTXMIivWNdH4/R4BkofQ=";
    };
    cargoSha256 = "sha256-4S57QOBcwwI0AcxqoBeKZYjXRrHVcbi3DMGugat3Z78=";
    nativeBuildInputs = with pkgs; [
        cmake installShellFiles makeWrapper ncurses pkg-config python3
    ];
    buildInputs = rpathLibs;
}
