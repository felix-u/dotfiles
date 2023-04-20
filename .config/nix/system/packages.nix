{ pkgs, config, lib, ... }:

let
    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    flake-compat = builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
    # hyprland = (import flake-compat {
    #     src = builtins.fetchTarball "https://github.com/hyprwm/Hyprland/archive/master.tar.gz";
    # }).defaultNix;
in {
    # imports = [
    #     hyprland.nixosModules.default
    # ];
    nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
            experimental-features = nix-command flakes
        '';
        checkConfig = true;
        gc = {
            automatic = true;
            persistent = true;
            dates = "weekly";
            options = "--delete-older-than 30d";
        };
        optimise.automatic = true;
        settings = {
            auto-optimise-store = true;
            trusted-users = [ "root" "felix" ];
        };
    };
    nixpkgs.config.allowUnfree = true;
    # # NUR
    # nixpkgs.config.packageOverrides = pkgs: {
    #     nur = import
    #         (builtins.fetchTarball
    #             "https://github.com/nix-community/NUR/archive/master.tar.gz") {
    #         inherit pkgs;
    #     };
    # };

    # # Overlays
    # services.emacs.package = pkgs.emacsPgtkNativeComp;
    # services.emacs.enable = false;
    # nixpkgs.overlays = [
    #   (import (builtins.fetchGit {
    #     url = "https://github.com/nix-community/emacs-overlay.git";
    #     ref = "master";
    #     rev = "b324b27d58fe93add90d80e081c39d452ae1cb98";
    #   }))
    #     hyprland.overlays.default
    #     (self: super: {
    #         waybar = super.waybar.overrideAttrs (oldAttrs: {
    #             mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
    #         });
    #     })
    #     (import (builtins.fetchTarball {
    #       url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    #     }))
    # ];

    # packages for all systems
    environment.systemPackages =
    let

        unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

        # "sudo" runs doas
        doas-as-sudo = (pkgs.writeShellScriptBin "sudo" ''
            echo "Warning: \"sudo\" runs \"doas\""
            doas "$@"
        '');

        helix-src = builtins.fetchTarball {
            url = "https://github.com/helix-editor/helix/archive/2bd8bc8d84.tar.gz";
        };
        helix-git = import flake-compat { src = helix-src; };

        jdtls = (pkgs.writeShellScriptBin "jdtls" ''
            jdt-language-server "$@"
        '');

        odin-dev = pkgs.odin.overrideAttrs (oldAttrs: rec {
            nativeBuildInputs = with pkgs; oldAttrs.nativeBuildInputs ++ [
                which llvmPackages.llvm.dev
            ];
            preBuild = ''
              patchShebangs build_odin.sh
            '';
            src = pkgs.fetchFromGitHub {
                owner = "odin-lang";
                repo = "Odin";
                rev = "dev-2023-03";
                sha256 = "sha256-SIU1VZgac0bL6byai2vMvgl3nrWZaU9Hn0wRqazzxn4=";
            };
            installPhase = ''
                mkdir -p $out/bin
                cp odin $out/bin/odin
                cp -r core $out/bin/core
                # CWD="$(pwd)"
                # cd vendor/stb/src && ${pkgs.gnumake}/bin/make
                # cd "$CWD"
                # cp -r vendor $out/bin/vendor
                wrapProgram $out/bin/odin --prefix PATH : ${lib.makeBinPath (with pkgs.llvmPackages; [
                  bintools
                  llvm
                  clang
                  lld
                ])}
            '';
        });

        # broken
        nfm = pkgs.callPackage ../derivations/nfm.nix {};

        nota = import ../derivations/nota.nix;

        ols = import ../derivations/ols.nix;

        imgclr = import ../derivations/imgclr.nix;

        r_with_packages = pkgs.rWrapper.override{ packages = with pkgs.rPackages; [ weights ]; };

        shgen = import ../derivations/shgen.nix;

        signal-desktop = pkgs.signal-desktop.overrideAttrs (oldAttrs: rec {
            runtimeDependencies = oldAttrs.runtimeDependencies ++ [ pkgs.wayland ];
        });

        termato = pkgs.callPackage ../derivations/termato.nix {};

        themesh = import ../derivations/themesh.nix;

        wl-screenrec = import ../derivations/wl-screenrec.nix;

        zig-master = import ../derivations/zig-master.nix;

        zls-src = builtins.fetchTarball { url = "https://github.com/zigtools/zls/archive/master.tar.gz"; };
        zls-master = import flake-compat { src = zls-src; };

    in
    with pkgs; [

        # ESSENTIAL
            foot gh git neofetch nvi stow wget

        # UTILS IN RUST
            helix-git.defaultNix.packages.x86_64-linux.default

        # DEV AND PROGRAMMING
        # misc
            ascii bviplus
            # android-tools bsdiff
            # libresprite pixelorama rx
            # openssl_3_0 pkg-config protobuf unstable.godot godot4-alpha
        # c
            binutils-unwrapped-all-targets clang clang-tools cppcheck
            cmake cmake-language-server gcc gdb gnumake man-pages-posix
            rr tinycc
            valgrind
        # go
            go gopls
        # java
            jdk11
        # web (HTML, CSS, JS)
            nodejs yarn
            nodePackages.npm
        # lua
            lua sumneko-lua-language-server
        # nix
            any-nix-shell cachix home-manager deadnix nix-index statix rnix-lsp
        # odin
            odin-dev # ols
        # plan9 from user space
            plan9port
        # python
            python3Full nodePackages.pyright
        # r
            r_with_packages
        # rust
            cargo clippy rust-analyzer # sccache
        # shell
            nodePackages.bash-language-server shellcheck
        # vim
            # neovim itself is managed by home-manager in system/users.nix
            nodePackages.vscode-langservers-extracted
            nodePackages.vim-language-server
        # zig
            zig-master
            # zls-master.defaultNix.packages.x86_64-linux.default
        # MATHS
            bc gnuplot libqalculate maxima octaveFull

        # TERMINAL MISC
            _7zz catimg cmatrix doas-as-sudo entr ffmpeg figlet file gomuks
            handlr htop hunspell hunspellDicts.en-gb-ise hyperfine jq killall
            libv4l lm_sensors lolcat moreutils ncdu nota nvd onefetch themesh
            tldr tmux tty-clock udiskie udisks unrar unzip v4l-utils w3m
            xdg-utils xdragon youtube-dl ytfzf zip
            imgclr # haha, c'est a moi :D
            # irssi
            (pkgs.symlinkJoin { # fzf should always run with '--color=16'
                name = "fzf";
                paths = [ pkgs.fzf ];
                buildInputs = [ pkgs.makeWrapper ];
                postBuild = ''
                    wrapProgram $out/bin/fzf --add-flags "--color=16"
                    wrapProgram $out/bin/fzf-tmux --add-flags "--color=16"
                '';
            })

        # MUSIC & AUDIO
            cava cmus ncspot pavucontrol pulsemixer

        # INTERNET & BLUETOOTH
            blueberry bluez bluez-tools firefox newsboat
            # blueman

        # DESKTOP
            appimage-run anki-bin
            bitwarden calibre font-manager handlr
            imagemagick
            unstable.imv libreoffice libnotify
            # libsForQt5.qtstyleplugin-kvantum
            libva libva-utils mpv
            mpvScripts.youtube-quality obs-studio obs-studio-plugins.wlrobs
            signal-desktop
            qt5ct wally-cli
            pcmanfm xournalpp zathura

        # VISUAL
            gsettings-desktop-schemas gtk-engine-murrine gtk_engines
            adw-gtk3 gnome.adwaita-icon-theme

        # PHOTO, GRAPHICS & VIDEO
            asciinema asciinema-agg
            unstable.darktable hugin inkscape-with-extensions jpegoptim krita
            luminanceHDR mediainfo
            gimp-with-plugins

        # GAMING
            proton-caller
            protontricks protonup

        # LATEX
            biber texinfo texlab unstable.texlive.combined.scheme-full

        # KERNEL
            config.boot.kernelPackages.v4l2loopback

    ];

    # Isn't installed correctly if in package list
    programs.steam.enable = true;

}
