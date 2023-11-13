{ pkgs, config, ... }:

let
  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  theme = import ./theme.nix;
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
        font.name = "Inter Medium";
        font.size = 12;
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
          userChrome = import ../config/firefox/userChrome.nix;
        };
      };

      programs.foot = {
        enable = true;
        settings = import ../config/foot/foot.nix;
      };

      programs.fzf = {
        enable = true;
        enableBashIntegration = true;
        defaultCommand = "fd";
        defaultOptions = [ "--color=16" ];
      };

      # programs.kitty = {
      #   enable = true;
      #   settings = {
      #     active_border_color = "#${theme.cfg}";
      #     background = "#${theme.cbg}";
      #     color0 = "#${theme.c00}";
      #     color1 = "#${theme.c01}";
      #     color10 = "#${theme.c10}";
      #     color11 = "#${theme.c11}";
      #     color12 = "#${theme.c12}";
      #     color13 = "#${theme.c13}";
      #     color14 = "#${theme.c14}";
      #     color15 = "#${theme.c15}";
      #     color2 = "#${theme.c02}";
      #     color3 = "#${theme.c03}";
      #     color4 = "#${theme.c04}";
      #     color5 = "#${theme.c05}";
      #     color6 = "#${theme.c06}";
      #     color7 = "#${theme.c07}";
      #     color8 = "#${theme.c08}";
      #     color9 = "#${theme.c09}";
      #     cursor = "none";
      #     font-family = "monospace Regular";
      #     font-size = 12;
      #     foreground = "#${theme.cfg}";
      #     inactive_border_color = "#${theme.c00}";
      #     window_border_width = "1.5pt";
      #     window_padding_width = 10;
      #   };
      #   shellIntegration.enableBashIntegration = true;
      # };

      programs.ncspot = {
        enable = true;
        settings = import ../config/ncspot/ncspot.nix;
      };

      programs.neovim = {
        enable = true;
        defaultEditor = true;
        extraConfig = builtins.readFile (toString ../config/nvim/init.vim);
      };

      programs.zathura = {
        enable = true;
        extraConfig = import ../config/zathura/zathurarc.nix;
      };

      home.file =
        let
          config_home = config.home-manager.users.felix.xdg.configHome;
        in
        {
          "${config_home}/wob/wob.ini".text = import ../config/wob/wob.ini.nix;
          "${config_home}/nvim/pack/plugins/start".source = ../config/nvim/start;
          "${config_home}/imv/config".text = import ../config/imv/config.nix;
          "${config_home}/newsraft/config".text = ''
            set scrolloff 5
            set download-timeout 20
            bind o exec w3m "%l"
            bind i exec imv "%i"
            bind b exec firefox "%l"
            bind m exec mpv --ytdl-format='bestvideo[height<=?1200]+bestaudio/best' --ytdl-raw-options=sub-lang='en',write-sub=,write-auto-sub= --sid=1 --speed=2 "%l"
          '';
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
