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
      swaybg
      swaylock-effects
    ];
  };

  environment.systemPackages =
    let
      river-git = pkgs-unstable.river.overrideAttrs (oldAttrs: rec {
        src = pkgs.fetchFromGitHub {
          owner = "riverwm";
          repo = "river";
          rev = "c16628c7f57c51d50f2d10a96c265fb0afaddb02";
          sha256 = "sha256-E3Xtv7JeCmafiNmpuS5VuLgh1TDAbibPtMo6A9Pz6EQ=";
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
      wf-recorder
      wl-clipboard
      wlr-randr
      wlsunset
      xwayland
    ];

  environment.pathsToLink = [ "/libexec" ]; # for polkit
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

  services.flatpak.enable = true;
  environment.sessionVariables = { GTK_USE_PORTAL = "1"; };
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
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
