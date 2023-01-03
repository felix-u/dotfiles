{ pkgs, config, lib, ... }:

let
    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    flake-compat = builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
    hyprland = (import flake-compat {
        src = builtins.fetchTarball "https://github.com/hyprwm/Hyprland/archive/master.tar.gz";
    }).defaultNix;
in {
    imports = [
        hyprland.nixosModules.default
    ];
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
    # NUR
    nixpkgs.config.packageOverrides = pkgs: {
        nur = import
            (builtins.fetchTarball
                "https://github.com/nix-community/NUR/archive/master.tar.gz") {
            inherit pkgs;
        };
    };

    # emacs setup and overlay
    services.emacs.package = pkgs.emacsPgtkNativeComp;
    services.emacs.enable = false;
    nixpkgs.overlays = [
      (import (builtins.fetchGit {
        url = "https://github.com/nix-community/emacs-overlay.git";
        ref = "master";
        rev = "b324b27d58fe93add90d80e081c39d452ae1cb98";
      }))
        # hyprland.overlays.default
        # (self: super: {
        #     waybar = super.waybar.overrideAttrs (oldAttrs: {
        #         mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
        #     });
        # })
        (import (builtins.fetchTarball {
          url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
        }))
    ];

    # packages for all systems
    environment.systemPackages =
    let

        unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

        # "sudo" runs doas
        doas-as-sudo = (pkgs.writeShellScriptBin "sudo" ''
            echo "Warning: \"sudo\" runs \"doas\""
            doas "$@"
        '');

        godot4-alpha = import ../derivations/godot4alpha.nix;

        helix-src = builtins.fetchTarball {
            url = "https://github.com/helix-editor/helix/archive/ba3c24aa0268735ac57321442d458ab6a1ac662c.tar.gz";
        };
        helix-git = import flake-compat { src = helix-src; };

        nextvi = (pkgs.callPackage ../derivations/nextvi.nix {});

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
                rev = "dev-2022-12";
                sha256 = "sha256-FFAZLO2j0RjY2fvhyre+/efNpW36xK/wHnn1P9ZDzOI=";
            };
            installPhase = ''
                mkdir -p $out/bin
                cp odin $out/bin/odin
                cp -r core $out/bin/core
                CWD="$(pwd)"
                cd vendor/stb/src && ${pkgs.gnumake}/bin/make
                cd "$CWD"
                cp -r vendor $out/bin/vendor
                wrapProgram $out/bin/odin --prefix PATH : ${lib.makeBinPath (with pkgs.llvmPackages; [
                  bintools
                  llvm
                  clang
                  lld
                ])}
            '';
        });

        ols = import ../derivations/ols.nix;

        imgclr = (pkgs.callPackage ../derivations/imgclr.nix {});

        shgen = import ../derivations/shgen.nix;

        themesh = import ../derivations/themesh.nix;

        w4 = import ../derivations/wasm-4.nix;

        zig-master = import ../derivations/zig-master.nix;

    in
    with pkgs; [

        # ESSENTIAL
            foot gh git neofetch nvi starship stow wget

        # UTILS IN RUST
            helix-git.defaultNix.packages.x86_64-linux.default

        # DEV AND PROGRAMMING
        # misc
            # android-tools bsdiff
            bviplus libresprite
            # pixelorama rx
            # openssl_3_0 pkg-config protobuf unstable.godot godot4-alpha
        # c
            binutils-unwrapped-all-targets clang clang-tools cppcheck
            cmake cmake-language-server gcc gdb gnumake man-pages-posix tinycc
            valgrind
        # go
            go
        # web (HTML, CSS, JS)
            nodejs yarn
            nodePackages.npm nodePackages.js-beautify
        # lua
            lua sumneko-lua-language-server
        # nix
            any-nix-shell cachix home-manager deadnix nix-index statix rnix-lsp
        # odin
            odin-dev ols
        # python
            python3Full nodePackages.pyright
        # rust
            cargo clippy rust-analyzer
        # shell
            nodePackages.bash-language-server shellcheck
        # vim
            # neovim itself is managed by home-manager in system/users.nix
            tree-sitter
            nodePackages.vscode-langservers-extracted
            nodePackages.vim-language-server
        # zig
            zig-master

        # MATHS
            bc gnuplot libqalculate

        # TERMINAL MISC
            cmatrix doas-as-sudo xdragon entr figlet file ffmpeg fzf
            handlr htop hunspell hunspellDicts.en-gb-ise hyperfine jq killall
            lm_sensors lolcat ncdu nvd onefetch
            oneshot pastel pandoc pdftk poppler_utils shgen termdown
            themesh tldr tmux tty-clock udiskie udisks unrar unzip v4l-utils
            libv4l w3m xdg-utils youtube-dl ytfzf zip zsh _7zz
            imgclr # haha, c'est a moi :D

        # MUSIC & AUDIO
            cava cmus ncspot pavucontrol pulsemixer

        # INTERNET & BLUETOOTH
            blueberry blueman bluez bluez-tools firefox newsboat qutebrowser

        # DESKTOP
            appimage-run anki-bin bitwarden calibre font-manager gnome-solanum handlr
            imagemagick imv libreoffice libnotify
            # libsForQt5.qtstyleplugin-kvantum
            libva libva-utils mpv
            mpvScripts.youtube-quality obs-studio obs-studio-plugins.wlrobs
            signal-desktop qt5ct wally-cli
            pcmanfm zathura

        # VISUAL
            gsettings-desktop-schemas gtk-engine-murrine gtk_engines
            solarc-gtk-theme gnome.adwaita-icon-theme

        # PHOTO, GRAPHICS & VIDEO
            unstable.darktable hugin inkscape-with-extensions jpegoptim krita
            luminanceHDR mediainfo unstable.gimp-with-plugins

        # GAMING
            lutris minetest proton-caller
            protontricks protonup unstable.heroic mangohud

        # LATEX
            biber texinfo texlab unstable.texlive.combined.scheme-full

        # KERNEL
            config.boot.kernelPackages.v4l2loopback

    ];

    # steam here, not working in packages
    programs.steam.enable = true;

    # # dict
    # environment.etc."dict.conf".text = ''
    #     server localhost
    # '';
    # environment.etc."conf.d/dictd".text = ''
    #     DICTD_ARGS="--locale en_GB.UTF-8"
    # '';
    # services.dictd = {
    #     enable = false;
    #     DBs = with pkgs.dictdDBs; [ wiktionary fra2eng eng2fra ];
    # };

}
