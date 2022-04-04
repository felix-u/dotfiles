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
            };
            efi = { canTouchEfiVariables = true; };
        };
        kernelPackages = pkgs.linuxPackages_latest;
    };
}
