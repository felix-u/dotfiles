{ pkgs, ... }:

{
  boot = {
    loader = {
      # Grub is disabled in favour of systemd-boot
      grub = {
        enable = false;
        devices = [ "nodev" ];
        efiSupport = true;
        useOSProber = false;
        splashImage = /home/felix/dotfiles/Pictures/cafe-walls/corentin-wunsche-parking.jpg;
        font = "${pkgs.iosevka}/share/fonts/truetype/iosevka-medium.ttf";
        fontSize = 32;
      };
      systemd-boot = {
        enable = true;
        consoleMode = "max"; # 0, 1, 2, auto, max, keep (default)
      };
      efi = { canTouchEfiVariables = true; };
      timeout = 2;
    };
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [
      "vt.cur_default=0"
    ];
    supportedFilesystems = [ "ntfs" ];
  };

  systemd = {
    services = {
      NetworkManager-wait-online.enable = false;
    };
  };
}
