{ config, pkgs, ... }:

{
    imports =
        [   # results of the hardware scan
            /etc/nixos/hardware-configuration.nix

            # grub, boot paramaters, etc.
            ../system/boot.nix

            # time, localisation, TTY
            ../system/localisation.nix

            # fonts
            ../system/fonts.nix

            # various desktop-related: sway, QT, printing, pipewire, etc.
            ../system/desktop.nix

            # user configuration (groups, shell, etc.)
            ../system/users.nix

            # general packages
            ../system/packages.nix
            # desktop-specific packages
            ./packages.nix

        ];


    # networking
    networking = {
        hostName = "nixbtw";
        networkmanager.enable = true;
        useDHCP = false;
        interfaces.wlan0.useDHCP = true;
    };


    system.stateVersion = "21.11";
}
