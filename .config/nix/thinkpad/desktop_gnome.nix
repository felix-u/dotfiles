{ pkgs, config, ... }:

let
    # dmenu-wl_run = import ../derivations/dmenu-wl.nix;
    unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {

    # nixpkgs.overlays = [
    #     (self: super: {
    #         gnome = pkgs-unstable.gnome;
    #     })
    # ];

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
      # gtkUsePortal = true;
      # wlr.enable = true;
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
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome = {
        enable = true;
        # extraGSettingsOverrides = ''
        #     [org.gnome.desktop.peripherals.keyboard]
        #     repeat-interval=25
        #     delay=200
        #
        #     [org.gnome.desktop.background]
        #     picture-uri=none
        #     primary-color='#ffffff'
        #     color-shading-type='solid'
        #
        #     [org.gnome.desktop.interface]
        #     enable-hot-corners=false
        #
        #     [org.gnome.settings-daemon.plugins.media-keys]
        #     # custom-keybindings=['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/',
        #     #                     '/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/',
        #     #                     '/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/']
        #     #
        #     # [org.gnome.settings-daemon.plugins.media-keys.custom-keybindings.custom0]
        #     # binding='<Super><Return>'
        #     # command='foot'
        #     # name='Open terminal'
        #     #
        #     # [org.gnome.settings-daemon.plugins.media-keys.custom-keybindings.custom1]
        #     # binding='<Super><Ret>'
        #     # command='firefox'
        #     # name='Open terminal1'
        #     #
        #     # [org.gnome.settings-daemon.plugins.media-keys.custom-keybindings.custom2]
        #     # binding='<Super><Enter>'
        #     # command='chromium'
        #     # name='Open terminal2'
        # '';
    };

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
