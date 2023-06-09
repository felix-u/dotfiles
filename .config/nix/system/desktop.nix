{ pkgs, config, lib, ... }:

let
    unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

    flake-compat = builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz";

in {

    # Hyprland

    # nixpkgs.overlays = [
    #     (self: super: {
    #       waybar = super.waybar.overrideAttrs (oldAttrs: {
    #         mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
    #       });
    #     })
    # ];
    # programs.hyprland = {
    #     enable = true;
    #     package = pkgs-unstable.hyprland.override {
    #         enableXWayland = true;
    #         hidpiXWayland = true;
    #     }; 
    #     xwayland = {
    #         enable = true;
    #         hidpi = true;
    #     };
    # };
    # environment.systemPackages = with pkgs; [
    #     dunst glib grim polkit_gnome slurp swaybg swaylock-effects tofi
    #     waybar wf-recorder wl-clipboard wlsunset xorg.xprop
    # ];

    # Sway

    # nixpkgs.overlays = [
    #     (self: super: {
    #         sway = pkgs-unstable.swayfx;
    #     })
    # ];  
    programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraPackages = with pkgs; [
            # pkgs-unstable.swayfx
            dunst
            glib
            grim 
            polkit_gnome
            slurp 
            swaybg
            swaylock-effects 
            tofi
            waybar
            wayland
            wf-recorder 
            wl-clipboard 
            wlsunset 
            xwayland
        ];
    };

    # River

    # environment.systemPackages =
    # let
    #     river-git = pkgs-unstable.river.overrideAttrs (oldAttrs: rec {
    #         src = pkgs.fetchFromGitHub {
    #             owner = "riverwm";
    #             repo = "river";
    #             rev = "792d94253c191e653e4025a648d574d9f8ce99bf";
    #             sha256 = "sha256-4Gwi7PiITj6i41YnngecFWd/pt5UQwslOM71C7tUR4k=";
    #             fetchSubmodules = true;
    #         };
    #     });
    # in
    # with pkgs; [
    #     unstable.river-luatile
    #     river-git 
    #     unstable.rivercarro 
    #     unstable.river-tag-overlay
    #     wlr-randr
    # ];

    environment.pathsToLink = [ "/libexec" ]; # for polkit
    environment.sessionVariables = { GTK_USE_PORTAL="1"; };
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
      # hsphfpd.enable = true;
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

    # Automatically mount drives
    services.udisks2.enable = true;

}
