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

    # emacs setup and overlay
    # services.emacs.package = pkgs.emacsPgtkNativeComp;
    # services.emacs.enable = true;
    nixpkgs.overlays = [
      (import (builtins.fetchGit {
        url = "https://github.com/nix-community/emacs-overlay.git";
        ref = "master";
        rev = "b324b27d58fe93add90d80e081c39d452ae1cb98";
      }))
    ];

    # packages for all systems
    environment.systemPackages =
    let

        dmux = (pkgs.callPackage ../derivations/dmux.nix {});

        # effectively "symlink" sudo to doas
        doas-as-sudo = (pkgs.writeShellScriptBin "sudo" ''
            echo "Warning: \"sudo\" runs \"doas\""
            doas "$@"
        '');

        flake-compat = builtins.fetchTarball {
            url = "https://github.com/edolstra/flake-compat/archive/b4a34015c698c7793d592d66adbab377907a2be8.tar.gz";
            sha256 = "sha256:1qc703yg0babixi6wshn5wm2kgl5y1drcswgszh4xxzbrwkk9sv7";
        };

        godot4-alpha = import ../derivations/godot4alpha.nix;

        helix-src = builtins.fetchTarball {
            url = "https://github.com/helix-editor/helix/archive/5b3b6ffc9e9b34fbbb39ad33cd29c8dec78ac231.tar.gz";
            sha256 = "sha256:1ix5x3b8prq5gn1rx8578jasx39mmx8jx1hhv0hwkx1dwn6y71y9";
        };
        helix-git = import flake-compat { src = helix-src; };

        imgclr = (pkgs.callPackage ../derivations/imgclr.nix {});

        shgen = import ../derivations/shgen.nix;

        themesh = import ../derivations/themesh.nix;

        unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

    in
    with pkgs; [

        # ESSENTIAL
        foot unstable.gh git neofetch pfetch starship stow wezterm wget

        # UTILS IN RUST
        bat # cat
        bottom # top
        fd # find
        # unstable.helix # nvim (ish!)
        helix-git.defaultNix.defaultPackage.x86_64-linux
        imgclr # haha, c'est a moi :D
        lsd # ls
        pipes-rs # pipes
        procs # ps
        ripgrep # grep
        skim # fzf
        ttyper

        # DEV
        android-tools any-nix-shell bsdiff cargo clang-tools cmake gcc gnumake go
        home-manager
        libresprite libxkbcommon man-pages-posix mold nix-index
        openssl_3_0 pkg-config protobuf
        python39Packages.bsdiff4 python39Packages.pip
        python3Full python39Packages.python-lsp-server
        python39Packages.termcolor
        so sccache shellcheck unstable.rustc
        unstable.clang unstable.deadnix unstable.godot unstable.statix
        valgrind
        yarn
        godot4-alpha
        # asepriteLatest
        unstable.aseprite-unfree
        # experimenting with lisp:
        cachix
        # clisp emacsPgtkNativeComp emacs-all-the-icons-fonts libvterm-neovim sbcl

        # NEOVIM
        cmake-language-server nodePackages.bash-language-server
        nodePackages.js-beautify
        nodePackages.npm nodePackages.pyright nodePackages.vim-language-server
        nodePackages.vscode-langservers-extracted nodejs rnix-lsp
        rust-analyzer sumneko-lua-language-server tree-sitter
        unstable.neovim

        # MATHS
        bc gnuplot libqalculate maxima qalculate-gtk wxmaxima

        # TERMINAL MISC
        catimg cava cmatrix dict doas-as-sudo xdragon entr figlet file
        ffmpeg fzf
        glow handlr htop
        hunspell hunspellDicts.en-gb-ise hyperfine killall
        lm_sensors lolcat lowdown unstable.lynis ncdu ncspot nvd onefetch oneshot pastel
        pandoc pdftk poppler_utils sdcv shgen
        termdown themesh tldr tmux tty-clock
        udiskie udisks unrar unzip
        v4l-utils libv4l
        w3m xdg-utils youtube-dl unstable.ytfzf zip zsh _7zz

        # INTERNET & BLUETOOTH
        blueberry blueman bluez bluez-tools firefox newsboat qutebrowser

        # DESKTOP
        appimage-run
        bitwarden calibre font-manager
        handlr imagemagick imv libreoffice
        libnotify libsForQt5.qtstyleplugin-kvantum libva libva-utils
        mpv mpvScripts.youtube-quality
        obs-studio obs-studio-plugins.wlrobs
        pavucontrol profanity pulsemixer signal-desktop
        qt5ct wally-cli
        # xfce.thunar xfce.thunar-archive-plugin
        pcmanfm
        zathura

        # VISUAL
        gsettings-desktop-schemas gtk-engine-murrine gtk_engines
        solarc-gtk-theme unstable.gnome.adwaita-icon-theme

        # PHOTO, GRAPHICS & VIDEO
        unstable.darktable hugin inkscape-with-extensions jpegoptim krita luminanceHDR
        mediainfo unstable.gimp-with-plugins

        # GAMING
        lutris minecraft unstable.optifine minetest
        proton-caller protontricks protonup
        unstable.heroic mangohud

        # LATEX
        biber texinfo texlab texlive.combined.scheme-full

        # KERNEL
        config.boot.kernelPackages.v4l2loopback

    ];

    # steam here, not working in packages
    programs.steam.enable = true;

}
