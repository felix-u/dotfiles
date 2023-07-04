{ pkgs, config, lib, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

  flake-compat = builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz";

in
{

  # River

  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [
      swaylock-effects
    ];
  };

  environment.systemPackages =
    let
      river-git = pkgs-unstable.river.overrideAttrs (oldAttrs: rec {
        src = pkgs.fetchFromGitHub {
          owner = "riverwm";
          repo = "river";
          rev = "5ce2ca1bc0411b43e94e8a1dfdf3a90a5dc7fd20";
          sha256 = "sha256-sa5yWeuQzR/dcN74ok3QkP/FdiCcxifbmDVcHiAZkhU=";
          fetchSubmodules = true;
        };
      });
    in
    with pkgs; [
      glib
      grim
      polkit_gnome
      river-git
      slurp
      tofi
      unstable.rivercarro
      waybar
      wf-recorder
      wl-clipboard
      wlr-randr
      wlsunset
      xwayland
    ];

  environment.pathsToLink = [ "/libexec" ]; # for polkit
  environment.sessionVariables = { GTK_USE_PORTAL = "1"; };
  qt.platformTheme = "qt5ct";

  # pipewire
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # enable CUPS for printing
  services.printing.enable = true;

  # flatpak and xdg portals
  services.flatpak.enable = true;
  xdg.portal = {
    enable = true;
    # gtkUsePortal = true;
    wlr.enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-hyprland ];
  };

  # bluetooth
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
    settings = { General = { ControllerMode = "bredr"; }; };
  };
  services.blueman.enable = true;

  hardware.opengl.enable = true;

  networking = {
    dhcpcd = {
      # No need to wait for this to continue booting
      wait = "background";
      # ARP not needed on home networks; can boot a little faster without it
      extraConfig = "noarp";
    };
  };

}
