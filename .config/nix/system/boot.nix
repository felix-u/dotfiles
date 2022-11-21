{ pkgs, ... }:

{
    boot = {
        loader = {
            grub = {
                enable = true;
                version = 2;
                devices = [ "nodev" ];
                efiSupport = true;
                useOSProber = false;
                splashImage = /home/felix/dotfiles/Pictures/cafe-walls/corentin-wunsche-parking.jpg;
                font = "${pkgs.iosevka}/share/fonts/truetype/iosevka-medium.ttf";
                fontSize = 32;
            };
            efi = { canTouchEfiVariables = true; };
        };
        kernelPackages = pkgs.linuxPackages_latest;
        kernelParams = [
            "vt.cur_default=0"
        ];
    };

    systemd = {
        services = {
            NetworkManager-wait-online.enable = false;
        };
    };
}
