{ pkgs, config, lib, ... }:

{
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
    let
        unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

        # effectively "symlink" sudo to doas
        doas-as-sudo = (pkgs.writeShellScriptBin "sudo" ''
            echo "Warning: \"sudo\" runs \"doas\""
            doas "$@"
        '');

        # latest aseprite, since even "unstable" version is awfully behind
        # asepriteLatest = pkgs.aseprite-unfree.overrideAttrs (oldAttrs: rec {
        #     version = "1.3-beta14";
        #     src = pkgs.fetchFromGitHub {
        #         owner = "aseprite";
        #         repo = "aseprite";
        #         rev = "v${version}";
        #         fetchSubmodules = true;
        #         sha256 = "sha256-F8/UmgG2yRLDZnBZaNJTAHDcXyoC3ePMhdEcTHlNR8E=";
        #       };
        #     # patches = [];
        #     buildInputs = with pkgs; (oldAttrs.buildInputs or []) ++
        #         [ xorg.libXi gn (harfbuzz.override {withIcu=true;}) ];
        #     postPatch = '''';
        #     skia = pkgs.callPackage /home/felix/.config/nix/system/customPackages/skia/skia.nix {};
        #     cmakeFlags = (oldAttrs.cmakeFlags or []) ++
        #         [
        #           "-DSKIA_DIR=${skia}"
        #           "-DSKIA_LIBRARY_DIR=${skia}/out/Release"
        #           # "-DSKIA_LIBRARY=${skia}/out/Release/libskia.a"
        #           # "-DSKIA_LIBRARY=${skia}"
        #         ];
        # });

        godot4-alpha = import ../derivations/godot4alpha.nix;

    in
    with pkgs; [

        # ESSENTIAL
        foot unstable.gh git neofetch starship stow wget

        # DEV
        android-tools cargo clang-tools cmake gcc gnumake go home-manager
        libresprite man-pages-posix openssl_3_0 pkg-config python39Packages.pip
        python3Full unstable.rustc shellcheck
        unstable.clang unstable.deadnix unstable.godot unstable.statix yarn
        godot4-alpha
        # asepriteLatest
        unstable.aseprite-unfree

        # MATHS
        bc gnuplot libqalculate maxima qalculate-gtk wxmaxima

        # NEOVIM
        cmake-language-server nodePackages.bash-language-server
        nodePackages.js-beautify
        nodePackages.npm nodePackages.pyright nodePackages.vim-language-server
        nodePackages.vscode-langservers-extracted nodejs rnix-lsp
        rust-analyzer sumneko-lua-language-server tree-sitter
        unstable.neovim

        # TERMINAL MISC
        bat catimg cava cmatrix dict doas-as-sudo dragon-drop entr figlet file
        ffmpeg fzf
        glow handlr htop
        hunspell hunspellDicts.en-gb-ise hyperfine
        lm_sensors lolcat lowdown ncdu ncspot nvd onefetch oneshot pastel # nnn
        pandoc pdftk pipes-rs ripgrep skim termdown tldr tmux tty-clock ttyper
        udiskie udisks unrar unzip
        w3m xdg-utils youtube-dl unstable.ytfzf zip _7zz

        # INTERNET & BLUETOOTH
        blueberry blueman bluez bluez-tools firefox newsboat qutebrowser

        # DESKTOP
        appimage-run
        bitwarden calibre handlr imagemagick imv libreoffice
        libsForQt5.polkit-kde-agent
        libnotify libsForQt5.qtstyleplugin-kvantum libva libva-utils
        mpv mpvScripts.youtube-quality noisetorch
        obs-studio obs-studio-plugins.wlrobs
        pavucontrol profanity pulsemixer qt5ct wally-cli xfce.thunar
        xfce.thunar-archive-plugin zathura

        # VISUAL
        gsettings-desktop-schemas gtk-engine-murrine gtk_engines
        solarc-gtk-theme unstable.gnome.adwaita-icon-theme

        # PHOTO, GRAPHICS & VIDEO
        unstable.darktable hugin inkscape-with-extensions jpegoptim krita luminanceHDR
        mediainfo unstable.gimp-with-plugins

        # GAMING
        lutris minecraft proton-caller protontricks protonup
        steamPackages.steam-fonts unstable.heroic

        # LATEX
        biber texinfo texlab texlive.combined.scheme-full

    ];

    # steam here, not working in packages
    programs.steam.enable = true;

}
