{ pkgs, config, lib, ... }:

let
in
{

  environment.pathsToLink = [ "/libexec" ]; # for polkit
  qt.platformTheme = "qt5ct";

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
  environment.sessionVariables = { GTK_USE_PORTAL = "1"; };
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gnome ];
  };

  # bluetooth
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
    settings = { General = { ControllerMode = "bredr"; }; };
  };
  services.blueman.enable = true;

  hardware.opengl.enable = true;

  services.xserver = {
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };

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
