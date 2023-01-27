{ pkgs, config, lib, ... }:

let
    dmenu-wl_run = import ../derivations/dmenu-wl.nix;
    unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {

    nixpkgs.overlays = [
        (self: super: {
            sway = pkgs-unstable.sway;
        })
    ];

    # wayland schtuff

    programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraPackages = with pkgs; [
            dmenu-wayland dmenu-wl_run dunst
            flashfocus glib
            grim polkit_gnome
            slurp swaybg swayidle
            swaylock-effects
            waybar wayland wf-recorder wl-clipboard xwayland
        ];
    };
    environment.pathsToLink = [ "/libexec" ]; # for polkit
    environment.sessionVariables = { GTK_USE_PORTAL="1"; };
    qt5.platformTheme = "qt5ct";

    # environment.systemPackages =
    # let
    #     river-with-xwayland = pkgs-unstable.river.overrideAttrs (oldAttrs: rec {
    #         src = pkgs.fetchFromGitHub {
    #             owner = "riverwm";
    #             repo = "river";
    #             rev = "d4b2f2b0fc5766c8ae14a6f42fe76d058bfb3505";
    #             sha256 = "sha256-Sb2EoVW06Iq734PHTw8+F2Q3DdAolOfvmKebqmqMiTU=";
    #             fetchSubmodules = true;
    #         };
    #         buildInputs = with pkgs; [
    #             wayland-protocols
    #             wlroots
    #             libxkbcommon
    #             pixman
    #             udev
    #             libevdev
    #             libinput
    #             libGL
    #             xorg.libX11
    #         ];
    #         installPhase = ''
    #             runHook preInstall
    #             zig build -Drelease-safe -Dcpu=baseline -Dxwayland -Dman-pages --prefix $out install
    #             runHook postInstall
    #           '';
    #     });
    # in
    # with pkgs; [
    #     # unstable.kile-wl
    #     river-with-xwayland unstable.rivercarro wlr-randr
    # ];

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

    # # gnome on x for testing
    # hardware.pulseaudio.enable = false;
    # services.xserver.enable = true;
    # # services.xserver.displayManager.gdm.enable = true;
    # services.xserver.desktopManager.gnome.enable = true;
    # environment.gnome.excludePackages = (with pkgs; [
    #   gnome-photos
    #   gnome-tour
    # ]) ++ (with pkgs.gnome; [
    #   cheese # webcam tool
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
