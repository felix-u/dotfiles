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

  programs.hyprland = {
    enable = true;
    package = pkgs-unstable.hyprland;
  };

  environment.systemPackages = [
    pkgs-unstable.hyprlock
    pkgs.glib
    pkgs.grim
    pkgs.playerctl
    pkgs.polkit_gnome
    pkgs.slurp
    pkgs.swaybg
    pkgs.swaylock-effects
    pkgs.tofi
    pkgs.wf-recorder
    pkgs.wl-clipboard
    pkgs.wlr-randr
    pkgs.wlsunset
  ];

  # programs.sway = {
  #   enable = true;
  #   extraPackages = with pkgs; [
  #     glib
  #     grim
  #     playerctl
  #     polkit_gnome
  #     slurp
  #     swaybg
  #     swaylock-effects
  #     tofi
  #     wf-recorder
  #     wl-clipboard
  #     wlr-randr
  #     wlsunset
  #     wob
  #     xwayland
  # ];
  # };

  environment.sessionVariables = rec {
    MOZ_ENABLE_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
    QT_QPA_PLATFORM = "wayland;xcb";
    SDL_VIDEODRIVER = "wayland";
    XDG_SESSION_TYPE = "wayland";
    XDG_CURRENT_DESKTOP = "Hyprland";
    XDG_SESSION_DESKTOP = "Hyprland";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    GDK_SCALE = "2";
    XCURSOR_SIZE = "24";
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
    package = pkgs.bluez;
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
