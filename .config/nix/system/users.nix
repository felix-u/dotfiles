{ pkgs, config, ... }:


let
    homedir = config.home-manager.users.felix.home.homeDirectory;
    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {

    imports = [ <home-manager/nixos> ];

    # user account
    users.users.felix = {
        isNormalUser = true;
        extraGroups = [ "wheel" "networkmanager" "input" "uinput" "sway" ];
    };

    # shell
    programs.zsh = {
        enable = true;
        autosuggestions= {
            enable = true;
            strategy = [ "history" "completion" ];
        };
        histFile = "$XDG_CACHE_HOME/zsh_history";
        histSize = 10000;
        promptInit = ''
            any-nix-shell zsh --info-right | source /dev/stdin
        '';
        setOptions = [ "SHARE_HISTORY" "AUTO_CD" "AUTOMENU" "EXTENDEDGLOB" "GLOBDOTS" ];
        syntaxHighlighting = {
            enable = true;
            highlighters = [ "main" "brackets" ];
            styles = {
                "bracket-level-1" = "fg=magenta";
                "bracket-level-2" = "fg=magenta";
                "bracket-level-3" = "fg=magenta";
                "bracket-level-4" = "fg=magenta";
                "commandseparator" = "fg=blue";
                "single-quoted-argument" = "fg=cyan";
                "double-quoted-argument" = "fg=cyan";
                "reserved-word" = "fg=fg,bold";
            };
        };
    };
    users.defaultUserShell = pkgs.zsh;

    # home-manager
    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;
      verbose = true;
      users.felix = {pkgs, ...}: {
        home.username = "felix";
        home.homeDirectory = "/home/felix";
        home.stateVersion = "21.11";

        gtk = {
            enable = true;
            cursorTheme.name = "Adwaita";
            cursorTheme.size = 24;
            font.name = "FreeSans";
            font.size = 11;
            iconTheme.name = "Adwaita";
            theme.name = "adw-gtk3";
            gtk3.extraConfig = {
                gtk-decoration-layout = "menu:";
                # button-layout = "";
            };
        };

        xdg.userDirs.enable = true;
        xdg.userDirs.createDirectories = true;

        services.udiskie.enable = true;

        programs.neovim = {
            enable = true;
            package = pkgs-unstable.neovim-unwrapped;
            plugins = with pkgs.vimPlugins; [
                nvim-treesitter.withAllGrammars
            ];
        };

        programs.nnn = {
          enable = true;
          bookmarks = {
            d = "${homedir}/dotfiles";
            h = "${homedir}";
            m = "/mnt";
            r = "${homedir}/Desktop/recordings";
            s = "${homedir}/Pictures/screenshots";
            u = "${homedir}/uni/2022/spring";
          };
          plugins.mappings = {
            d = "dragdrop";
            p = "preview-tui";
            r = "imgresize";
            w = "waypaper";
            v = "imgview";
          };
        };

        programs.chromium = {
          enable = true;
          package = pkgs.chromium;
          commandLineArgs = [
            "-animation-duration-scale=0.5"
            "-enable-features=UseOzonePlatform"
            "-ozone-platform=wayland"
            "-homepage='https://search.brave.com'"
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
            # stylus
            { id = "clngdbkpkpeebahjckkjfobafhncgmne"; }
          ];
        };

        # w3m config
        home.file.".w3m/config".text = ''
                confirm_qq false
            '';

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
