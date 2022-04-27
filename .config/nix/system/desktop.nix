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
            waybar wf-recorder wl-clipboard xwayland
        ];
    };
    environment.pathsToLink = [ "/libexec" ]; # for polkit
    programs.qt5ct.enable = true;

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
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };

    # bluetooth
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;

    hardware.opengl.enable = true;

}
