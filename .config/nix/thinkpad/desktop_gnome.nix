{ pkgs, config, ... }:

let
  # dmenu-wl_run = import ../derivations/dmenu-wl.nix;
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in
{

  environment.pathsToLink = [ "/libexec" ]; # for polkit

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
    extraPortals = [ pkgs.xdg-desktop-portal-gnome ];
  };

  # bluetooth
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
    hsphfpd.enable = false;
    settings = { General = { ControllerMode = "bredr"; }; };
  };
  services.blueman.enable = true;

  hardware.opengl.enable = true;

  hardware.pulseaudio.enable = false;
  services.xserver.enable = true;

  # Pantheon
  services.xserver.desktopManager.pantheon.enable = true;

  # # Gnome
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome = {
  #   enable = true;
  # };
  # environment.gnome.excludePackages = (with pkgs; [
  #   gnome-photos
  #   gnome-tour
  # ]) ++ (with pkgs.gnome; [
  #   # cheese # webcam tool
  #   gnome-music
  #   gnome-terminal
  #   gedit # text editor
  #   epiphany # web browser
  #   geary # email reader
  #   evince # document viewer
  #   gnome-characters
  #   gnome-software
  #   gnome-music
  #   simple-scan
  #   totem # video player
  #   tali # poker game
  #   iagno # go game
  #   hitori # sudoku game
  #   atomix # puzzle game
  # ]);

  services.power-profiles-daemon.enable = false;
}
