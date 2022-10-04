{ pkgs, config, ... }:

let
    # dmenu-wl_run = import ../derivations/dmenu-wl.nix;
    unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {
    # wayland schtuff
    # programs.sway = {
    #     enable = true;
    #     wrapperFeatures.gtk = true;
    #     extraPackages = with pkgs; [
    #         brightnessctl dmenu-wayland dmenu-wl_run dunst flashfocus glib
    #         grim polkit_gnome
    #         slurp swaybg swayidle swaylock-effects
    #         wayland wf-recorder wl-clipboard xwayland
    #     ];
    # };
    environment.pathsToLink = [ "/libexec" ]; # for polkit
    # programs.qt5ct.enable = true;

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
    #     unstable.kile-wl
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
      gtkUsePortal = true;
      # wlr.enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gnome ];
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

    hardware.pulseaudio.enable = false;
    services.xserver.enable = true;
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome = {
        enable = true;
        extraGSettingsOverrides = ''
            [org.gnome.desktop.peripherals.keyboard]
            repeat-interval=25
            delay=200

            [org.gnome.desktop.background]
            picture-uri=none
            primary-color='#002731'
            color-shading-type='solid'

            [org.gnome.desktop.interface]
            enable-hot-corners=false

            [org.gnome.settings-daemon.plugins.media-keys]
            custom-keybindings=['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/',
                                '/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/',
                                '/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/']

            # [org.gnome.settings-daemon.plugins.media-keys.custom-keybindings.custom0]
            # binding='<Super><Return>'
            # command='foot'
            # name='Open terminal'
            #
            # [org.gnome.settings-daemon.plugins.media-keys.custom-keybindings.custom1]
            # binding='<Super><Ret>'
            # command='firefox'
            # name='Open terminal1'
            #
            # [org.gnome.settings-daemon.plugins.media-keys.custom-keybindings.custom2]
            # binding='<Super><Enter>'
            # command='chromium'
            # name='Open terminal2'
        '';
    };
    environment.gnome.excludePackages = (with pkgs; [
      gnome-photos
      gnome-tour
    ]) ++ (with pkgs.gnome; [
      # cheese # webcam tool
      gnome-music
      gnome-terminal
      # gedit # text editor
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
      # hitori # sudoku game
      # atomix # puzzle game
    ]);

    services.power-profiles-daemon.enable = false;
}