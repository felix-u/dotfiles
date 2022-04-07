{ pkgs, config, ... }:

{
    boot = {
        loader = {
            grub = {
                enable = true;
                version = 2;
                devices = [ "nodev" ];
                efiSupport = true;
                useOSProber = true;
                splashImage = /home/felix/dotfiles/Pictures/cafe-walls/cityfromabove.jpg;
                font = "${pkgs.iosevka}/share/fonts/truetype/iosevka-medium.ttf";
                fontSize = 32;
            };
            efi = { canTouchEfiVariables = true; };
        };
        kernelPackages = pkgs.linuxPackages_latest;
    };
}
