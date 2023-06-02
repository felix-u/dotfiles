{ pkgs, config, lib, ... }:

let
    unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

    # flake-compat = builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz";

in {

    # nixpkgs.overlays = [
    #     (self: super: {
    #         sway = pkgs-unstable.sway;
    #     })
    # ];

    # environment.systemPackages =
    # let
    #     swayfx-src = builtins.fetchTarball {
    #         url = "https://github.com/WillPower3309/swayfx/archive/ac31a612164828ca6ae5478332eee3cd2317ffdc.tar.gz";
    #     };
    #     swayfx-git = import flake-compat { src = swayfx-src; };
    # in with pkgs; [
    #     swayfx-git.defaultNix.packages.x86_64-linux.default
    # ];

    # wayland schtuff

    programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraPackages = with pkgs; [
            dunst
            glib
            grim polkit_gnome
            slurp swaybg swayidle
            swaylock-effects tofi
            # waybar
            wayland wf-recorder wl-clipboard wlsunset xwayland
        ];
    };
    environment.pathsToLink = [ "/libexec" ]; # for polkit
    environment.sessionVariables = { GTK_USE_PORTAL="1"; };
    qt.platformTheme = "qt5ct";

    environment.systemPackages =
    let
        river-git = pkgs-unstable.river.overrideAttrs (oldAttrs: rec {
            src = pkgs.fetchFromGitHub {
                owner = "riverwm";
                repo = "river";
                rev = "792d94253c191e653e4025a648d574d9f8ce99bf";
                sha256 = "sha256-4Gwi7PiITj6i41YnngecFWd/pt5UQwslOM71C7tUR4k=";
                fetchSubmodules = true;
            };

        });
        # kile-git = pkgs-unstable.kile-wl.overrideAttrs (oldAttrs: rec {
        #     src = pkgs.fetchgit {
        #         url = "https://gitlab.com/snakedye/kile";
        #         rev = "625f91010b920587dbf0ee23113eb8aa51cc6ec3";
        #         hash = "sha256-4sfzF2g2kSz+Q55gTCePjM+7kvfrEJ2uLKyy/V+SLF4=";
        #     };
        #     cargoSha256 = "5598a195360c96a337aca9399b074f9239184d418c597cc4dfbc8450d1c62443";
        # });
    in
    with pkgs; [
        unstable.river-luatile
        river-git 
        unstable.rivercarro 
        unstable.river-tag-overlay
        wlr-randr
    ];

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
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
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
