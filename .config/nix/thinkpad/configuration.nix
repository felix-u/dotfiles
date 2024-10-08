{ config, pkgs, ... }:

{
  imports =
    [
      # results of the hardware scan
      /etc/nixos/hardware-configuration.nix

      # grub, boot paramaters, etc.
      ../system/boot.nix

      ../system/env.nix

      # time, localisation, TTY
      ../system/localisation.nix

      # fonts
      ../system/fonts.nix

      # various desktop-related: sway, QT, printing, pipewire, etc.
      ../system/desktop-gnome.nix

      # user configuration (groups, shell, etc.)
      ../system/users.nix

      # general packages
      ../system/packages.nix
      # thinkpad-specific packages
      ./packages.nix

      # mimetypes
      ../system/mimetypes.nix

    ];


  # networking
  networking = {
    hostName = "thonkpad";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
  };
  programs.nm-applet.enable = true;


  # kmonad
  users.groups = { uinput = { }; };
  services.udev.extraRules = ''
    # KMonad user access to /dev/uinput
    KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
  '';
  systemd.services.thinkpadkbd = {
    wantedBy = [ "multi-user.target" ];
    description = "Start kmonad";
    serviceConfig = {
      Type = "simple";
      ExecStart = "/run/current-system/sw/bin/kmonad ${toString ../config/kmonad/thinkpad.kbd}";
      User = "felix";
    };
  };
  # TODO: decide whether to keep or not
  systemd.services.thinkpadkbd.enable = false;

  # libva
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
  ];

  # services.tlp = {
  #   enable = true;
  #   settings = {
  #     # START_CHARGE_THRESH_BAT0 = 70;
  #     STOP_CHARGE_THRESH_BAT0 = 75;
  #   };
  # };

  system.stateVersion = "21.11";
}
