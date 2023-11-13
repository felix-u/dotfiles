{ pkgs, config, lib, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in
{
  # bluetooth
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
    settings = { General = { ControllerMode = "bredr"; }; };
  };
  services.blueman.enable = true;

  hardware.opengl.enable = true;

  services.xserver = {
    enable = true;
    desktopManager.pantheon.enable = true;
  };

  environment.systemPackages = with pkgs; [
    xclip
  ];

  # conflicts with TLP
  services.power-profiles-daemon.enable = false;

  networking = {
    dhcpcd = {
      # No need to wait for this to continue booting
      wait = "background";
      # ARP not needed on home networks; can boot a little faster without it
      extraConfig = "noarp";
    };
  };

}
