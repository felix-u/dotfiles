{ pkgs, config, lib, ... }:

let kmonad = import ../derivations/kmonad.nix;
in {
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
        nur = import
            (builtins.fetchTarball
                "https://github.com/nix-community/NUR/archive/master.tar.gz") {
            inherit pkgs;
        };
    };


    # packages for all systems
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
        python39Packages.pip python3Full unstable.deadnix unstable.statix

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
        pdftk pipes-rs skim termdown tldr tmux ttyper udiskie udisks unrar unzip
        w3m youtube-dl ytfzf zip _7zz

        # INTERNET & BLUETOOTH
        blueberry blueman bluez firefox qutebrowser ungoogled-chromium

        # DESKTOP
        handlr imagemagick imv libreoffice libsForQt5.polkit-kde-agent
        libsForQt5.qtstyleplugin-kvantum mpv noisetorch obs-studio pavucontrol
        pulsemixer qalculate-gtk qt5ct wally-cli xfce.thunar
        xfce.thunar-archive-plugin zathura

        # VISUAL
        gsettings-desktop-schemas gtk-engine-murrine gtk_engines
        solarc-gtk-theme unstable.gnome.adwaita-icon-theme

        # PHOTO, GRAPHICS & VIDEO
        darktable hugin inkscape-with-extensions jpegoptim krita luminanceHDR
        mediainfo unstable.gimp-with-plugins

        # GAMING
        lutris minecraft proton-caller protontricks protonup steam
        steamPackages.steam-fonts unstable.heroic

        # LATEX
        biber texinfo texlab texlive.combined.scheme-full

    ];
}
