{ pkgs, config, lib, hm, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in
{

  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };

  environment.sessionVariables = rec {
    MOZ_ENABLE_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
    QT_QPA_PLATFORM = "wayland-egl";
    SDL_VIDEODRIVER = "wayland";
  };

  home-manager.users.felix.dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/peripherals/keyboard" = {
        delay = (lib.gvariant.mkUint32 200);
        repeat-interval = (lib.gvariant.mkUint32 20);
      };
      "org/gnome/desktop/peripherals/mouse" = {
        accel-profile = "flat";
        speed = -0.2;
      };
      "org/gnome/desktop/wm/preferences" = {
        resize-with-right-button = true;
        num-workspaces = 9;
      };
      "org/gnome/desktop/session" = {
        idle-delay = (lib.gvariant.mkUint32 0);
      };
      "org/gnome/desktop/wm/keybindings" = {
        close = [ "<Super>w" "<Alt>F4" ];
        move-to-workspace-1 = [ "<Super><Shift>1" ];
        move-to-workspace-2 = [ "<Super><Shift>2" ];
        move-to-workspace-3 = [ "<Super><Shift>3" ];
        move-to-workspace-4 = [ "<Super><Shift>4" ];
        move-to-workspace-5 = [ "<Super><Shift>5" ];
        move-to-workspace-6 = [ "<Super><Shift>6" ];
        move-to-workspace-7 = [ "<Super><Shift>7" ];
        move-to-workspace-8 = [ "<Super><Shift>8" ];
        move-to-workspace-9 = [ "<Super><Shift>9" ];
        panel-run-dialog = [ "<Super>r" ];
        toggle-fullscreen = [ "<Super>f" ];
        toggle-maximized = [ "<Super><Shift>f" ];
        switch-to-workspace-1 = [ "<Super>1" ];
        switch-to-workspace-2 = [ "<Super>2" ];
        switch-to-workspace-3 = [ "<Super>3" ];
        switch-to-workspace-4 = [ "<Super>4" ];
        switch-to-workspace-5 = [ "<Super>5" ];
        switch-to-workspace-6 = [ "<Super>6" ];
        switch-to-workspace-7 = [ "<Super>7" ];
        switch-to-workspace-8 = [ "<Super>8" ];
        switch-to-workspace-9 = [ "<Super>9" ];
      };
      "org/gnome/mutter" = {
        center-new-windows = true;
        edge-tiling = true;
        dynamic-workspaces = false;
      };
      "org/gnome/mutter/keybindings" = {
        toggle-tiled-left = [ "<Super><Shift>Left" "<Super><Shift>m" "<Super><Shift>h" ];
        toggle-tiled-right = [ "<Super><Shift>Right" "<Super><Shift>i" "<Super><Shift>l" ];
      };
      "org/gnome/settings-daemon/plugins/power" = {
        power-button-action = "interactive"; # power off
        sleep-inactive-ac-type = "nothing";
      };
      "org/gnome/shell" = {
        disable-user-extensions = false;
        enabled-extensions = with pkgs.gnomeExtensions; [
          blur-my-shell.extensionUuid
          light-style.extensionUuid
          user-themes.extensionUuid
          workspace-indicator.extensionUuid
        ];
      };
      "org/gnome/shell/keybindings" = {
        show-screenshot-ui = [ "<Shift><Super>s" ];
        show-screen-recording-ui = [ "<Shift><Super>r" ];
        switch-to-application-1 = [ ];
        switch-to-application-2 = [ ];
        switch-to-application-3 = [ ];
        switch-to-application-4 = [ ];
        switch-to-application-5 = [ ];
        switch-to-application-6 = [ ];
        switch-to-application-7 = [ ];
        switch-to-application-8 = [ ];
        switch-to-application-9 = [ ];
      };
    };
  };

  environment.systemPackages = with pkgs; [
    gnome.gnome-tweaks
    gnomeExtensions.blur-my-shell
    gnomeExtensions.light-style
    gnomeExtensions.user-themes
    gnomeExtensions.workspace-indicator
  ];

  # environment.pathsToLink = [ "/libexec" ]; # for polkit

  # # pipewire
  # hardware.pulseaudio.enable = false;
  # services.pipewire = {
  #   enable = true;
  #   alsa.enable = true;
  #   alsa.support32Bit = true;
  #   pulse.enable = true;
  # };

  # enable CUPS for printing
  services.printing.enable = true;

  # services.flatpak.enable = true;
  # # environment.sessionVariables = { GTK_USE_PORTAL = "1"; };
  # xdg.portal = {
  #   enable = true;
  #   wlr.enable = true;
  #   extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  # };

  # bluetooth
  hardware.bluetooth = {
    enable = true;
    # package = pkgs.bluez;
    settings = { General = { ControllerMode = "bredr"; }; };
  };
  # services.blueman.enable = true;

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
