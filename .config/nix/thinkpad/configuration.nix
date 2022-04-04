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

    services.xserver.enable = true;
    services.xserver.displayManager.gdm.enable = true;
    # services.xserver.desktopManager.gnome.enable = true;
    # hardware.pulseaudio.enable = true;
    # services.xserver.libinput.enable = true;

    programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        # extraPackages = with pkgs; [
        #     swaylock swaybg swayidle
        #     wl-clipboard dunst dmenu-wayland waybar
        #     slurp grim wf-recorder
        #     brightnessctl flashfocus
        # ];
    };
    # rtkit is optional but recommended
    security.rtkit.enable = false;
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
            RemainAfterExit = "yes";
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

    environment.systemPackages =
    let unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    in
    with pkgs; [

        swaylock swaybg swayidle
        wl-clipboard dunst dmenu-wayland waybar
        slurp grim wf-recorder
        brightnessctl flashfocus

        # temp
        unstable.helix kmonad

        # ESSENTIAL UTILITIES
        foot starship
        nerdfonts fira iosevka
        wget git gh stow

        # NEOVIM
        unstable.neovim go vimPlugins.packer-nvim tree-sitter
        nodePackages.npm nodejs nodePackages.bash-language-server gcc cmake-language-server
        nodePackages.vscode-langservers-extracted sumneko-lua-language-server
        rnix-lsp nodePackages.pyright rust-analyzer
        nodePackages.vim-language-server

        # TERMINAL UTILS & MISC
        pipes-rs bat bc lolcat cava figlet neofetch handlr
        dragon-drop nnn tlp
        glow htop hyperfine jpegoptim libqalculate lowdown
        ncdu ncspot onefetch oneshot pastel pdftk
        termdown tldr tmux udiskie udisks unrar unzip ytfzf
        zip fzf skim

        # INTERNET & BLUETOOTH
        firefox ungoogled-chromium
        blueberry blueman bluez

        # DESKTOP AND RELATED UTILS
        pulsemixer
        libsForQt5.qtstyleplugin-kvantum
        pavucontrol libsForQt5.polkit-kde-agent qt5ct
        xfce.thunar xfce.thunar-archive-plugin
        imv handlr imagemagick

        # X11 UTILS
        # xclip xdg-user-dirs xdg-utils xdo xdotool xf86_input_wacom
        # xorg.xbacklight xorg.xev xorg.xkill xorg.xprop xorg.xrandr xorg.xrdb
        # xsel xwallpaper

        # DEV
        # python3Full python39Packages.pip pypy3
        # home-manager

        # MISC & ADDITIONAL
        # android-tools calibre etcher godot gparted qalculate-gtk libreoffice
        # mpv noisetorch noto-fonts-emoji obs-studio zathura wally-cli

        # PHOTO, GRAPHICS & VIDEO
        # darktable hugin luminanceHDR
        # gimp-with-plugins krita
        # inkscape-with-extensions
        # olive-editor

        # GAMING
        # steam steamPackages.steam-fonts lutris minecraft
        # protonup protontricks proton-caller

        # LaTeX
        biber texinfo texlab texlive.combined.scheme-full

        (pkgs.neovim.override {
         configure = {
             packages.myPlugins = with pkgs.vimPlugins; {
                 start = [
                 (nvim-treesitter.withPlugins (
                    plugins: with plugins; [
                        tree-sitter-norg
                        tree-sitter-bash
                        tree-sitter-comment
                        tree-sitter-c
                        tree-sitter-latex
                        tree-sitter-json
                        tree-sitter-lua
                        tree-sitter-nix
                        tree-sitter-toml
                        tree-sitter-yaml
                        tree-sitter-javascript
                        tree-sitter-html
                        tree-sitter-css
                        tree-sitter-typescript
                        tree-sitter-rust
                        tree-sitter-cpp
                        tree-sitter-python
                    ]
                    ))
                 ];
             };
           };
         })
    ];

    system.stateVersion = "21.11";
}
