{ pkgs, config, ... }:

let
  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  theme = (import ./theme.nix) { config = config; };
  config_home = config.home-manager.users.felix.xdg.configHome;
  nixconfig = "${config_home}/nix/config";
in
{

  imports = [ <home-manager/nixos> ];

  # user account
  users.users.felix = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "input" "uinput" "sway" ];
  };

  # shell
  users.defaultUserShell = pkgs.bash;

  # home-manager
  home-manager = {
    backupFileExtension = "backup";
    useUserPackages = true;
    useGlobalPkgs = true;
    verbose = true;
    users.felix = { pkgs, ... }: {
      home.username = "felix";
      home.homeDirectory = "/home/felix";
      home.stateVersion = "21.11";

      gtk = {
        enable = true;
        cursorTheme.name = "Adwaita";
        cursorTheme.size = 24;
        font.name = "${theme.fontsans} Medium";
        font.size = theme.fontsanssize;
        iconTheme.name = "elementary";
        theme.name = "io.elementary.stylesheet.slate";
      };

      xdg.userDirs = {
        enable = true;
        createDirectories = true;
        music = null;
        publicShare = null;
        templates = null;
        videos = null;
      };

      programs.bash = {
        enable = true;
        enableVteIntegration = true;
        historyControl = [ "ignoredups" ];
        initExtra = builtins.readFile (toString ../config/bash/.bashrc);
      };

      programs.chromium = {
        enable = true;
        package = pkgs.chromium;
        commandLineArgs = [
          "-enable-features=UseOzonePlatform"
          "-ozone-platform=wayland"
        ];
        extensions = [
          # ublock origin
          { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; }
          # surfingkeys
          { id = "gfbliohnnapiefjpjlpjnehglfpaknnc"; }
          # dark reader
          { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; }
          # decentraleyes
          { id = "ldpochfccmkkmhdbclfhpagapcfdljkj"; }
        ];
      };

      programs.firefox = {
        enable = true;
        profiles.default = {
          settings = import ../config/firefox/user.nix;
          userChrome = (import ../config/firefox/userChrome.nix) { config = config; };
        };
      };

      programs.fzf = {
        enable = true;
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableZshIntegration = true;
        defaultCommand = "fd";
        defaultOptions = [ "--color=16" ];
      };

      programs.neovim = {
        enable = true;
        defaultEditor = true;
        extraConfig = builtins.readFile (toString ../config/nvim/init.vim);
      };

      programs.waybar = {
        enable = true;
        package = pkgs-unstable.waybar;
        style = (import ../config/waybar/style.nix) { config = config; };
        settings = (import ../config/waybar/bar.nix) {
          config = config;
          pkgs = pkgs-unstable;
        };
      };

      programs.zathura = {
        enable = true;
        extraConfig = (import ../config/zathura/zathurarc.nix) { config = config; };
      };

      home.file =
        {
          "${config_home}/gf2_config.ini".text = (import ../config/gf2_config.nix) { config = config; };
          # "${config_home}/imv/config".text = (import ../config/imv/config.nix) { config = config; };
          "${config_home}/nvim/pack/plugins/start".source = ../config/nvim/start;
          "${config_home}/newsraft/config".text = ''
            set scrolloff 5
            set download-timeout 20
            bind o exec w3m "%l"
            bind i exec imv "%i"
            bind b exec firefox "%l"
            bind m exec mpv --ytdl-format='bestvideo[height<=?1200]+bestaudio/best' --ytdl-raw-options=sub-lang='en',write-sub=,write-auto-sub= --sid=1 --speed=2 "%l"
          '';
        };

      wayland.windowManager.hyprland = {
        enable = true;
        package = pkgs-unstable.hyprland;
        settings = {
          exec-once = [
            "${nixconfig}/hyprland/start.sh"
          ];
        };
      };

    };

  };

  # doas instead of sudo (why not)
  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [{
      users = [ "felix" ];
      keepEnv = true;
      persist = true;
    }];
  };

}
