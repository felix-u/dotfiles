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
        kernelParams = [
            "i915.enable_dc=2"
        ];

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

    # enable xorg
    services.xserver.enable = true;
    # startx
    services.xserver.displayManager.startx.enable = true;
    # bspwm
    services.xserver.windowManager.bspwm.enable = true;

    # enable CUPS for printing
    services.printing.enable = true;

    # sound
    sound.enable = true;
    hardware.pulseaudio.enable = true;

    # touchpad support
    services.xserver.libinput.enable = true;

    # kmonad
    users.groups = { uinput = {}; };
    services.udev.extraRules =
      ''
        # KMonad user access to /dev/uinput
        KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
      '';
    # systemd.services.thinkpadkbd = {
    #     description = "Kmonad";
    #     serviceConfig = {
    #         Type = "oneshot";
    #         ExecStart = "/usr/bin/env kmonad /home/felix/dotfiles/.config/kmonad/nixpad.kbd";
    #         User = "felix";
    #         WantedBy = "multi-user.target";
    #     };
    #     # WantedBy = "multi-user.target";
    # };
    # systemd.services.thinkpadkbd.enable = true;

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

        # ESSENTIAL UTILITIES
        alacritty starship
        nerdfonts fira
        wget git gh
        kmonad

        # NEOVIM
        unstable.neovim
        nodePackages.bash-language-server clang cmake-language-server
        nodePackages.vscode-langservers-extracted sumneko-lua-language-server
        rnix-lsp nodePackages.pyright rust-analyzer
        nodePackages.vim-language-server

        # TERMINAL UTILS & MISC
        pipes-rs bat bc lolcat cava colorpicker devour exa figlet fontpreview
        ranger dragon-drop sxiv ueberzug
        giph glow go htop hyperfine jpegoptim libqalculate neofetch lowdown
        maim ncdu ncspot ntfs3g onefetch oneshot pastel pdftk playerctl
        powerline-fonts termdown tldr tmux udiskie udisks unrar unzip ytfzf
        zip

        # INTERNET & BLUETOOTH
        firefox ungoogled-chromium
        blueberry blueman bluez

        # DESKTOP AND RELATED UTILS
        pulsemixer dunst polybar rofi rofi-emoji eww i3lock-color
        libsForQt5.qtstyleplugin-kvantum numix-icon-theme-circle lxappearance
        pavucontrol picom-next libsForQt5.polkit-kde-agent qt5ct sxhkd
        xfce.thunar xfce.thunar-archive-plugin unclutter-xfixes wmctrl
        wmutils-core wmutils-opt bspwm

        # X11 UTILS
        xclip xdg-user-dirs xdg-utils xdo xdotool xf86_input_wacom
        xorg.xbacklight xorg.xev xorg.xkill xorg.xprop xorg.xrandr xorg.xrdb
        xsel xwallpaper

        # DEV
        nodePackages.npm python3Full python39Packages.pip pypy3 rpi-imager lua
        home-manager

        # MISC & ADDITIONAL
        android-tools calibre etcher godot gparted qalculate-gtk libreoffice
        mpv noisetorch noto-fonts-emoji obs-studio zathura wally-cli

        # PHOTO, GRAPHICS & VIDEO
        darktable hugin luminanceHDR
        gimp-with-plugins krita
        inkscape-with-extensions
        olive-editor

        # GAMING
        steam steamPackages.steam-fonts lutris minecraft
        protonup protontricks proton-caller

        # LaTeX
        biber texinfo texlab texlive.combined.scheme-full

    ];

    system.stateVersion = "21.11";
}
