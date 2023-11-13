{ pkgs, ... }:

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
      ../system/desktop_pantheon.nix

      # user configuration (groups, shell, etc.)
      ../system/users.nix

      # general packages
      ../system/packages.nix
      # desktop-specific packages
      ./packages.nix

      # mimetypes
      ../system/mimetypes.nix

    ];


  # networking
  networking = {
    hostName = "pc";
    networkmanager.enable = true;
  };


  # amdgpu stuff
  hardware.opengl = {
    # package = pkgs.mesa;
    driSupport = true;
    extraPackages = with pkgs; [
      rocm-opencl-icd
      rocm-opencl-runtime
      amdvlk
      vaapiVdpau
      libvdpau-va-gl
      mesa
      libdrm
    ];
  };


  system.stateVersion = "23.05";
}
