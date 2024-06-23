{ pkgs, config, lib, hm, ... }:

let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  home = config.home-manager.users.felix.home.homeDirectory;
  theme = (import ./theme.nix) { config = config; };
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
      "org/gnome/Console" = {
        custom-font = "Monospace ${(toString theme.fontmonosize)}";
        theme = "auto";
        use-system-font = false;
      };
      "org/gnome/desktop/background" = {
        color-shading-type = "solid";
        picture-options = "zoom";
        picture-uri = "${home}/dotfiles/win/teal.png";
        picture-uri-dark = "${home}/dotfiles/win/teal.png";
      };
      "org/gnome/desktop/peripherals/keyboard" = {
        delay = (lib.gvariant.mkUint32 200);
        repeat-interval = (lib.gvariant.mkUint32 20);
      };
      "org/gnome/desktop/peripherals/mouse" = {
        accel-profile = "flat";
        speed = 0;
      };
      "org/gnome/desktop/screensaver" = {
        color-shading-type = "solid";
        picture-options = "zoom";
        picture-uri = "${home}/dotfiles/win/teal.png";
      };
      "org/gnome/desktop/wm/preferences" = {
        button-layout = "appmenu:minimize,maximize,close";
        num-workspaces = 9;
        resize-with-right-button = true;
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
      "org/gnome/settings-daemon/plugins/media-keys".custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3/"
      ];
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
        binding = "<Super>b";
        command = "firefox";
        name = "Browser";
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
        binding = "<Super>Return";
        command = "kgx";
        name = "Terminal";
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
        binding = "<Super>o";
        command = "kgx";
        name = "Terminal";
      };
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom3" = {
        binding = "<Super>semicolon";
        command = "kgx";
        name = "Terminal";
      };
      "org/gnome/settings-daemon/plugins/power" = {
        power-button-action = "interactive"; # power off
        sleep-inactive-ac-type = "nothing";
      };
      "org/gnome/shell" = {
        disable-user-extensions = false;
        enabled-extensions = with pkgs.gnomeExtensions; [
          blur-my-shell.extensionUuid
          dash-to-panel.extensionUuid
          light-style.extensionUuid
          user-themes.extensionUuid
          workspace-indicator.extensionUuid
        ];
      };
      "org/gnome/shell/extensions/dash-to-panel" = {
        dot-color-1 = "#${theme.cfg}";
        dot-color-2 = "#${theme.cfg}";
        dot-color-3 = "#${theme.cfg}";
        dot-color-4 = "#${theme.cfg}";
        dot-color-override = true;
        dot-size = 4;
        dot-style-focus = "DOTS";
        dot-style-unfocused = "DOTS";
        focus-highlight-opacity = 100;
        panel-element-positions = "{\"0\":[{\"element\":\"showAppsButton\",\"visible\":false,\"position\":\"stackedTL\"},{\"element\":\"activitiesButton\",\"visible\":false,\"position\":\"stackedTL\"},{\"element\":\"leftBox\",\"visible\":false,\"position\":\"stackedTL\"},{\"element\":\"taskbar\",\"visible\":true,\"position\":\"stackedTL\"},{\"element\":\"centerBox\",\"visible\":false,\"position\":\"stackedBR\"},{\"element\":\"rightBox\",\"visible\":true,\"position\":\"stackedBR\"},{\"element\":\"dateMenu\",\"visible\":true,\"position\":\"stackedBR\"},{\"element\":\"systemMenu\",\"visible\":true,\"position\":\"stackedBR\"},{\"element\":\"desktopButton\",\"visible\":true,\"position\":\"stackedBR\"}]}";
        panel-sizes = "{\"0\":42}";
        progress-show-count = true;
        show-tooltip = false;
        trans-panel-opacity = 0.8;
        trans-use-custom-opacity = true;
        window-preview-title-font-color = "#${theme.cfg}";
      };
      "org/gnome/shell" = {
        favorite-apps = [
          "org.gnome.Nautilus.desktop"
          "firefox.desktop"
          "org.gnome.Console.desktop"
        ];
      };
      "org/gnome/shell/keybindings" = {
        show-screenshot-ui = [ "<Shift><Super>s" ];
        show-screen-recording-ui = [ "<Shift><Super>r" ];
        switch-to-application-1 = [ "<Control><Super>1" ];
        switch-to-application-2 = [ "<Control><Super>2" ];
        switch-to-application-3 = [ "<Control><Super>3" ];
        switch-to-application-4 = [ "<Control><Super>4" ];
        switch-to-application-5 = [ "<Control><Super>5" ];
        switch-to-application-6 = [ "<Control><Super>6" ];
        switch-to-application-7 = [ "<Control><Super>7" ];
        switch-to-application-8 = [ "<Control><Super>8" ];
        switch-to-application-9 = [ "<Control><Super>9" ];
      };
      "org/gtk/gtk4/settings/file-chooser" = {
        show-hidden = true;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    gnome.gnome-tweaks
    gnomeExtensions.blur-my-shell
    gnomeExtensions.dash-to-panel
    gnomeExtensions.light-style
    gnomeExtensions.user-themes
    gnomeExtensions.workspace-indicator
    wl-clipboard
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
