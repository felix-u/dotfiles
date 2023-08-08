{ pkgs, config, ... }:


let
  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in
{

  imports = [ <home-manager/nixos> ];

  # user account
  users.users.felix = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "input" "uinput" "sway" ];
  };

  # shell
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;

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
        enableZshIntegration = true;
        defaultCommand = "fd";
        defaultOptions = [ "--color=16" ];
      };

      programs.ncspot = {
        enable = true;
        settings = import ../config/ncspot/ncspot.nix;
      };

      programs.waybar = {
        enable = true;
        style = import ../config/waybar/style.nix;
        settings = (import ../config/waybar/bar.nix) { config = config; pkgs = pkgs; };
      };

      programs. zathura = {
        enable = true;
        extraConfig = import ../config/zathura/zathurarc.nix;
      };

      programs.zsh = {
        enable = true;
        autocd = true;
        cdpath = [ "/home/felix" ];
        history = {
          expireDuplicatesFirst = true;
          path = "${config.home-manager.users.felix.xdg.cacheHome}/zsh_history";
        };
        initExtra = builtins.readFile (toString ../config/zsh/.zshrc);
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
