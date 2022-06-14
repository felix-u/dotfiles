{ pkgs, config, ... }:

let
    dmenu-wl_run = import ../derivations/dmenu-wl.nix;
    unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {
    # wayland schtuff
    programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraPackages = with pkgs; [
            brightnessctl dmenu-wayland dmenu-wl_run dunst flashfocus grim polkit_gnome
            slurp swaybg swayidle swaylock-effects
            waybar wayland wf-recorder wl-clipboard xwayland
        ];
    };
    environment.pathsToLink = [ "/libexec" ]; # for polkit
    # programs.qt5ct.enable = true;

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
      gtkUsePortal = true;
      wlr.enable = true;
      # extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };

    # bluetooth
    hardware.bluetooth = {
      enable = true;
      package = pkgs.bluezFull;
      hsphfpd.enable = true;
      settings = { General = { ControllerMode = "bredr"; }; };
    };
    services.blueman.enable = true;

    hardware.opengl.enable = true;

    # gnome on x for testing
    hardware.pulseaudio.enable = false;
    services.xserver.enable = true;
    # services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome.enable = true;
    environment.gnome.excludePackages = (with pkgs; [
      gnome-photos
      gnome-tour
    ]) ++ (with pkgs.gnome; [
      cheese # webcam tool
      gnome-music
      gnome-terminal
      gedit # text editor
      epiphany # web browser
      geary # email reader
      evince # document viewer
      gnome-characters
      gnome-software
      gnome-music
      simple-scan
      totem # video player
      tali # poker game
      iagno # go game
      hitori # sudoku game
      atomix # puzzle game
    ]);

}
