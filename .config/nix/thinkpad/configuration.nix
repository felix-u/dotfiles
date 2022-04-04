# Edit this configuration file to define what should be installed on your
# system. Help is available in the configuration.nix(5) man page and in the
# NixOS manual (accessible by running 'nixos-help').

{ config, pkgs, ... }:

let kmonad = import ../derivations/kmonad.nix;
in {
    imports =
        [ # Include the results of the hardware scan
            /etc/nixos/hardware-configuration.nix
        ];

    boot = {

        loader = {
            grub = {
                enable = true;
                version = 2;
                devices = [ "nodev" ];
                efiSupport = true;
                useOSProber = true;
            };
            efi = { canTouchEfiVariables = true; };
        };
    };

    boot.kernelPackages = pkgs.linuxPackages_latest;

    # hostname
    networking.hostName = "thonkpad";
    # enable wireless via networkmanager
    networking.networkmanager.enable = true;
    programs.nm-applet.enable = true;

    # time zone
    time.timeZone = "America/New_York";

    networking.useDHCP = false;
    networking.interfaces.wlp0s20f3.useDHCP = true;

    # internationalisation
    i18n.defaultLocale = "en_GB.UTF-8";
    console = {
        font = "Lat2-Terminus16";
        keyMap = "us";
    };

    # services.xserver.enable = true;
    # services.xserver.displayManager.gdm.enable = true;
    # services.xserver.desktopManager.gnome.enable = true;
    # hardware.pulseaudio.enable = false;
    # services.xserver.libinput.enable = true;

    programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraPackages = with pkgs; [
            swaylock swaybg swayidle
            wl-clipboard dunst dmenu-wayland bemenu waybar
            slurp grim wf-recorder
            brightnessctl flashfocus
            xwayland
        ];
    };

    programs.qt5ct.enable = true;

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    # enable CUPS for printing
    services.printing.enable = true;

    # sound
    # sound.enable = true;

    # kmonad
    users.groups = { uinput = {}; };
    services.udev.extraRules =
      ''
        # KMonad user access to /dev/uinput
        KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
      '';
    systemd.services.thinkpadkbd = {
        wantedBy = [ "multi-user.target" ];
        description = "Start kmonad";
        serviceConfig = {
            Type = "oneshot";
            ExecStart = "/run/current-system/sw/bin/kmonad /home/felix/dotfiles/.config/kmonad/nixpad.kbd";
            User = "felix";
        };
    };
    systemd.services.thinkpadkbd.enable = true;

    # user account
    users.users.felix = {
        isNormalUser = true;
        extraGroups = [ "wheel" "networkmanager" "input" "uinput" ];
    };

    # shell
    programs.zsh.enable = true;
    users.defaultUserShell = pkgs.zsh;

    nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
            experimental-features = nix-command flakes
        '';

        autoOptimiseStore = true;
        checkConfig = true;

        gc = {
            automatic = true;
            persistent = true;
            dates = "weekly";
            options = "--delete-older-than 30d";
        };

        optimise.automatic = true;

        trustedUsers = [ "root" "felix" ];
    };

    nixpkgs.config.allowUnfree = true;

    fonts.fonts = with pkgs; [
        nerdfonts fira noto-fonts-emoji
    ];

    environment.systemPackages =
    let unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    in
    with pkgs; [

        # TEMP
        unstable.helix

        # ESSENTIAL
        foot starship
        wget git gh stow
        kmonad neofetch

        # DEV
        gcc gnumake android-tools godot
        python3Full python39Packages.pip go
        home-manager

        # NEOVIM
        unstable.neovim vimPlugins.packer-nvim tree-sitter
        nodePackages.npm nodejs nodePackages.bash-language-server
        cmake-language-server
        nodePackages.vscode-langservers-extracted sumneko-lua-language-server
        rnix-lsp nodePackages.pyright rust-analyzer
        nodePackages.vim-language-server

        # TERMINAL MISC
        pipes-rs bat bc lolcat cava figlet handlr
        dragon-drop nnn tlp glow htop hyperfine
        libqalculate lowdown ncdu ncspot onefetch
        oneshot pastel pdftk termdown tldr tmux
        udiskie udisks unrar ytfzf unzip zip fzf
        skim

        # INTERNET & BLUETOOTH
        firefox ungoogled-chromium qutebrowser
        blueberry blueman bluez

        # DESKTOP
        pulsemixer pavucontrol
        libsForQt5.qtstyleplugin-kvantum
        libsForQt5.polkit-kde-agent qt5ct
        xfce.thunar xfce.thunar-archive-plugin
        imv handlr imagemagick qalculate-gtk
        libreoffice mpv noisetorch obs-studio
        zathura wally-cli

        # VISUAL
        unstable.gnome.adwaita-icon-theme
        gtk-engine-murrine gtk_engines gsettings-desktop-schemas
        solarc-gtk-theme

        # PHOTO, GRAPHICS & VIDEO
        darktable hugin luminanceHDR
        unstable.gimp-with-plugins krita
        inkscape-with-extensions
        jpegoptim

        # GAMING
        steam steamPackages.steam-fonts lutris minecraft
        protonup protontricks proton-caller

        # LATEX
        biber texinfo texlab texlive.combined.scheme-full

        # (pkgs.neovim.override {
        #  configure = {
        #      packages.myPlugins = with pkgs.vimPlugins; {
        #          start = [
        #          (nvim-treesitter.withPlugins (
        #             plugins: with plugins; [
        #                 tree-sitter-norg
        #                 tree-sitter-bash
        #                 tree-sitter-comment
        #                 tree-sitter-c
        #                 tree-sitter-latex
        #                 tree-sitter-json
        #                 tree-sitter-lua
        #                 tree-sitter-nix
        #                 tree-sitter-toml
        #                 tree-sitter-yaml
        #                 tree-sitter-javascript
        #                 tree-sitter-html
        #                 tree-sitter-css
        #                 tree-sitter-typescript
        #                 tree-sitter-rust
        #                 tree-sitter-cpp
        #                 tree-sitter-python
        #             ]
        #             ))
        #          ];
        #      };
        #    };
        #  })

    ];

    system.stateVersion = "21.11";
}
