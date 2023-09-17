{ pkgs, config, lib, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

  # flake-compat = builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz";

in
{

  services.xserver = {
    enable = false;
    displayManager.gdm.enable = false;
    desktopManager.gnome.enable = false;
  };
  # environment.gnome.excludePackages = (with pkgs; [
  #   gnome-tour
  # ]) ++ (with pkgs.gnome; [
  #   gnome-contacts
  #   epiphany
  # ]);
  # programs.dconf.enable = true;
  # environment.systemPackages = with pkgs; [
  #   # gnomeExtensions.appindicator
  #   gnome.gnome-tweaks
  # ];
  # services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];

  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [
      swaybg
      swaylock-effects
      glib
      grim
      polkit_gnome
      slurp
      tofi
      wf-recorder
      wl-clipboard
      wlr-randr
      wlsunset
      xwayland
    ];
  };

  environment.pathsToLink = [ "/libexec" ]; # for polkit

  # pipewire
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # enable CUPS for printing
  services.printing.enable = true;

  services.flatpak.enable = true;
  # environment.sessionVariables = { GTK_USE_PORTAL = "1"; };
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
