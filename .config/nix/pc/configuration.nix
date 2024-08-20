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
      ../system/desktop.nix

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

  nixpkgs.overlays = [
    # GNOME 46: triple-buffering-v4-46
    (final: prev: {
      gnome = prev.gnome.overrideScope (gnomeFinal: gnomePrev: {
        mutter = gnomePrev.mutter.overrideAttrs (old: {
          src = pkgs.fetchFromGitLab {
            domain = "gitlab.gnome.org";
            owner = "vanvugt";
            repo = "mutter";
            rev = "triple-buffering-v4-46";
            hash = "sha256-nz1Enw1NjxLEF3JUG0qknJgf4328W/VvdMjJmoOEMYs=";
          };
        });
      });
    })
  ];

  system.stateVersion = "23.05";
}
