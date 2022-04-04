{ config, pkgs, ... }:

let kmonad = import ../derivations/kmonad.nix;
in {
    imports =
        [ # Include the results of the hardware scan
            /etc/nixos/hardware-configuration.nix
        ];


    # bootloader
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
        kernelPackages = pkgs.linuxPackages_latest;
    };


    # networking
    networking = {
        hostName = "thonkpad";
        networkmanager.enable = true;
        useDHCP = false;
        interfaces.wlp0s20f3.useDHCP = true;
    };


    # time and internationalisation
    time.timeZone = "America/New_York";
    i18n.defaultLocale = "en_GB.UTF-8";
    # larger font, because 4K
    console = {
        earlySetup = true;
        font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
        packages = with pkgs; [ terminus_font ];
        keyMap = "us";
    };


    # fonts
    fonts.fonts = with pkgs; [
        fira nerdfonts noto-fonts-emoji
    ];


    # wayland schtuff
    programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraPackages = with pkgs; [
            bemenu brightnessctl dmenu-wayland dunst flashfocus grim slurp
            swaybg swayidle swaylock waybar wf-recorder wl-clipboard xwayland
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
    # NUR
    nixpkgs.config.packageOverrides = pkgs: {
        nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
            inherit pkgs;
        };
    };


    environment.systemPackages =
    let unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    in
    with pkgs; [

        # TEMP
        unstable.helix

        # ESSENTIAL
        foot gh git kmonad neofetch starship stow wget

        # DEV
        android-tools gcc gnumake go godot home-manager libresprite
        python39Packages.pip python3Full

        # NEOVIM
        cmake-language-server nodePackages.bash-language-server
        nodePackages.js-beautify
        nodePackages.npm nodePackages.pyright nodePackages.vim-language-server
        nodePackages.vscode-langservers-extracted nodejs rnix-lsp
        rust-analyzer sumneko-lua-language-server tree-sitter unstable.neovim
        vimPlugins.packer-nvim

        # TERMINAL MISC
        bat bc catimg cava cmatrix dict dragon-drop entr figlet ffmpeg fzf glow handlr htop
        hunspell hunspellDicts.en-gb-ise hyperfine
        libqalculate lolcat lowdown ncdu ncspot nnn onefetch oneshot pastel
        pdftk pipes-rs skim termdown tldr tlp tmux ttyper udiskie udisks unrar unzip
        w3m ytfzf zip _7zz

        # INTERNET & BLUETOOTH
        blueberry blueman bluez firefox qutebrowser ungoogled-chromium

        # DESKTOP
        handlr imagemagick imv libreoffice libsForQt5.polkit-kde-agent
        libsForQt5.qtstyleplugin-kvantum mpv noisetorch obs-studio pavucontrol
        pulsemixer qalculate-gtk qt5ct unstable.rnote wally-cli xfce.thunar
        xfce.thunar-archive-plugin xournalpp zathura

        # VISUAL
        gsettings-desktop-schemas gtk-engine-murrine gtk_engines
        solarc-gtk-theme unstable.gnome.adwaita-icon-theme

        # PHOTO, GRAPHICS & VIDEO
        darktable hugin inkscape-with-extensions jpegoptim krita luminanceHDR
        unstable.gimp-with-plugins

        # GAMING
        lutris minecraft proton-caller protontricks protonup steam
        steamPackages.steam-fonts unstable.heroic

        # LATEX
        biber texinfo texlab texlive.combined.scheme-full

    ];

    system.stateVersion = "21.11";
}
